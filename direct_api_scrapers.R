# Direct HTTP/API scrapers replacing RSelenium browser automation.
#
# Every institution/platform previously scraped via RSelemium + Firefox +
# geckodriver turned out to have a plain-HTTP path once actually
# investigated: a public JSON API (UW), an explicitly-published Atom feed
# (Eastern/Sheridan/Northwest, all on the same PeopleAdmin platform), an
# internal AJAX endpoint the site's own JS calls (LCCC/Casper/Central/
# Gillette/Western, all NEOGOV), a separate unprotected API host behind
# the same domain's Incapsula-protected UI (SchoolSpring), and a
# script-injection endpoint whose payload is plain embedded HTML
# (Applitrack). TedK12 already used plain rvest with no browser and needed
# no change. None of the functions below launch a browser.
#
# Each source is split into a fetch_*() function (does the HTTP call, the
# only part that needs live network) and a parse_*() function (pure logic
# on already-fetched text/JSON, testable against a static fixture with no
# network at all).
#
# TedK12 (fetch_tedk12_postings() below) was believed to need no change
# from plain rvest with no browser -- turned out to be wrong (found
# 2026-08-06): TedK12/PowerSchool Hire serves genuinely different content
# depending on the request's User-Agent, not just a plain "no browser
# needed" static page. See fetch_tedk12_postings()'s own comment for the
# full story.

suppressMessages({
  library(httr2)
  library(jsonlite)
  library(xml2)
  library(rvest)
  library(dplyr)
})

# ---------------------------------------------------------------------------
# University of Wyoming -- Oracle Cloud HCM Recruiting REST API
# ---------------------------------------------------------------------------

# Oracle's API caps at 25 results per page regardless of the requested
# `limit`, so this pages via `offset` until it has everything
# `TotalJobsCount` says exists. Oracle applies both pagination parameters
# only when they are finder arguments; top-level query arguments are ignored.
uw_finder <- function(site_number, limit, offset) {
  paste0(
    "findReqs;siteNumber=", site_number,
    ",limit=", as.integer(limit),
    ",offset=", as.integer(offset)
  )
}

fetch_uw_postings <- function(
    api_base = "https://eeik.fa.us2.oraclecloud.com/hcmRestApi/resources/latest/recruitingCEJobRequisitions",
    ui_base = "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1",
    site_number = "CX_1",
    page_size = 25,
    fetch_pay = TRUE,
    detail_delay = 0.2) {
  all_pages <- list()
  offset <- 0
  total <- NA_integer_
  seen_page_signatures <- character(0)
  seen_ids <- character(0)

  repeat {
    resp <- request(api_base) %>%
      req_url_query(
        onlyData = "true",
        expand = "requisitionList",
        finder = uw_finder(site_number, page_size, offset)
      ) %>%
      perform_with_retry()

    parsed <- resp_body_json(resp, simplifyVector = TRUE)
    item <- parsed$items[1, ]
    total <- as.integer(item$TotalJobsCount)

    reqs <- item$requisitionList[[1]]
    if (is.null(reqs) || nrow(reqs) == 0) break

    ids <- as.character(reqs$Id)
    if (anyNA(ids) || any(!nzchar(ids))) {
      stop("UW Oracle pagination returned a requisition without an Id at offset ", offset)
    }

    page_signature <- paste(ids, collapse = "\r")
    if (page_signature %in% seen_page_signatures) {
      stop(
        "UW Oracle pagination repeated a page at offset ", offset,
        "; refusing to emit duplicate postings."
      )
    }

    repeated_ids <- intersect(ids, seen_ids)
    if (length(repeated_ids) > 0) {
      stop(
        "UW Oracle pagination repeated requisition Id(s) at offset ", offset, ": ",
        paste(repeated_ids, collapse = ", "),
        "; refusing to emit duplicate postings."
      )
    }

    all_pages[[length(all_pages) + 1]] <- reqs
    seen_page_signatures <- c(seen_page_signatures, page_signature)
    seen_ids <- c(seen_ids, ids)
    offset <- offset + nrow(reqs)
    if (offset >= total) break
  }

  if (length(all_pages) == 0) {
    return(data.frame(Title = character(0), Location = character(0),
                       Posted_Date = character(0), Link = character(0),
                       stringsAsFactors = FALSE))
  }

  reqs <- bind_rows(all_pages)
  postings <- parse_uw_requisitions(reqs, ui_base)
  if (!fetch_pay) return(ensure_posted_pay_columns(postings))

  # The list carries no pay; each requisition's detail record (the call the
  # posting page makes) has a "Posted Salary Range" field. One request per
  # posting, spaced out; a failed call leaves only that posting's pay blank.
  detail_base <- sub("recruitingCEJobRequisitions$", "recruitingCEJobRequisitionDetails", api_base)
  pay <- dplyr::bind_rows(lapply(as.character(reqs$Id), function(id) {
    Sys.sleep(detail_delay)
    tryCatch({
      resp <- request(detail_base) %>%
        req_url_query(onlyData = "true", expand = "requisitionFlexFields",
                      finder = paste0('ById;Id="', id, '",siteNumber=', site_number)) %>%
        perform_with_retry()
      parse_uw_posting_pay(resp_body_string(resp))
    }, error = function(e) parse_uw_posting_pay(NA_character_))
  }))
  cbind(postings, pay)
}

# Pay from a UW requisition's detail record: the "Posted Salary Range" flex
# field, kept verbatim as Pay_Text. UW types it by hand, so it can lack the
# "$" ("42,504", "13.00", "10-15") or open with "Up to"; it is read as a
# labeled salary figure either way. With no unit stated, $7-$100 is hourly
# (the same rule as the PeopleAdmin pay fields) and $15,000+ is annual.
parse_uw_posting_pay <- function(json_text) {
  value <- NA_character_
  if (!is.na(json_text)) {
    flex <- jsonlite::fromJSON(json_text)$items$requisitionFlexFields[[1]]
    if (is.data.frame(flex) && all(c("Prompt", "Value") %in% names(flex))) {
      value <- trimws(flex$Value[match("Posted Salary Range", flex$Prompt)])
    }
  }
  if (is.na(value) || !nzchar(value)) return(extract_posted_pay(NA_character_))
  pay <- extract_posted_pay(paste("Salary:", sub("^(up to\\s*)?(\\d)", "\\1$\\2", value, ignore.case = TRUE)))
  if (is.na(pay$Pay_Min)) return(pay)
  if (is.na(pay$Pay_Unit) && pay$Pay_Max >= 7 && pay$Pay_Max <= 100) pay$Pay_Unit <- "hour"
  pay$Pay_Text <- value
  pay
}

parse_uw_requisitions <- function(reqs_df, ui_base) {
  data.frame(
    Title = reqs_df$Title,
    Location = reqs_df$PrimaryLocation,
    Posted_Date = reqs_df$PostedDate,
    Link = paste0(ui_base, "/job/", reqs_df$Id),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# PeopleAdmin (Eastern Wyoming, Sheridan, Northwest) -- published Atom feed
# ---------------------------------------------------------------------------

# The feed carries no pay, so with fetch_pay each posting's own page is read
# for it (parse_peopleadmin_posting_pay()): one request per posting, spaced
# out. A failed page leaves only that posting's pay and dates blank.
#
# The same page gives the posting's dates (parse_peopleadmin_posting_dates()).
# Its open date replaces the feed's <published>, which moves when a posting
# is re-published: 39 of 95 Western/Eastern/Sheridan/Northwest postings with
# an open date on 2026-11-04 differ from <published> (MT's MSU Billings and
# Great Falls: one by two years). Sheridan leaves "Job Posting Date" blank on
# 32 of 71 postings, which keep <published>.
fetch_peopleadmin_atom <- function(feed_url, location_fallback, fetch_pay = TRUE, detail_delay = 0.3) {
  resp <- request(feed_url) %>% perform_with_retry()
  postings <- parse_peopleadmin_atom(resp_body_string(resp), location_fallback)
  if (!fetch_pay || nrow(postings) == 0) {
    postings <- ensure_posted_pay_columns(postings)
    postings$Close_Date <- rep(NA_character_, nrow(postings))
    return(postings)
  }
  pages <- lapply(postings$Link, function(link) {
    Sys.sleep(detail_delay)
    tryCatch(resp_body_string(request(link) %>% perform_with_retry()), error = function(e) NA_character_)
  })
  pay <- dplyr::bind_rows(lapply(pages, parse_peopleadmin_posting_pay))
  dates <- dplyr::bind_rows(lapply(pages, parse_peopleadmin_posting_dates))
  postings$Posted_Date <- dplyr::coalesce(dates$Open_Date, as.character(postings$Posted_Date))
  postings$Close_Date <- dates$Close_Date
  cbind(postings, pay)
}

# A PeopleAdmin posting page's own open and close dates, as YYYY-MM-DD. Each
# college labels them its own way: MSU campuses, Western and Eastern "Open
# Date"/"Close Date", Northwest "Posting Date"/"Closing Date", Sheridan "Job
# Posting Date" and "Initial Review Date". A close date is kept even when
# "Open Until Filled" is also Yes (it is then the first-review deadline,
# like AppliTrack's "Closing Date"); with no close date, "Open Until Filled:
# Yes" gives Close_Date "Open Until Filled". Blank fields are NA.
PEOPLEADMIN_OPEN_LABELS <- c("Open Date", "Posting Date", "Job Posting Date")
PEOPLEADMIN_CLOSE_LABELS <- c("Close Date", "Closing Date", "Initial Review Date")

parse_peopleadmin_posting_dates <- function(html_text) {
  none <- data.frame(Open_Date = NA_character_, Close_Date = NA_character_, stringsAsFactors = FALSE)
  if (is.na(html_text)) return(none)
  rows <- rvest::html_elements(rvest::read_html(html_text), xpath = "//tr[th and td]")
  labels <- tolower(trimws(rvest::html_text2(rvest::html_element(rows, "th"))))
  values <- trimws(gsub("\\s+", " ", rvest::html_text2(rvest::html_element(rows, "td"))))
  first_date <- function(wanted) {
    v <- values[labels %in% tolower(wanted)]
    d <- suppressWarnings(as.Date(v, format = "%m/%d/%Y"))
    d <- d[!is.na(d) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", v)]
    if (length(d) == 0) NA_character_ else format(d[1])
  }
  close <- first_date(PEOPLEADMIN_CLOSE_LABELS)
  if (is.na(close) && any(labels == "open until filled" & grepl("^yes", values, ignore.case = TRUE))) {
    close <- "Open Until Filled"
  }
  data.frame(Open_Date = first_date(PEOPLEADMIN_OPEN_LABELS), Close_Date = close, stringsAsFactors = FALSE)
}

# Pay from a PeopleAdmin posting page. Each college labels its pay field
# differently (MSU campuses "Salary", Western "Estimated Salary", Sheridan
# "Hiring Range"/"Hourly Rate (if applicable)"/"Full Salary Range", Northwest
# "Starting Salary Range"); the first labeled field stating an amount wins,
# and its text is kept as Pay_Text. When the figure has no unit next to it,
# the unit comes from the posting's own "Compensation" field (MSU Billings,
# Northern, Great Falls: Annual / Hourly / Monthly / By Credit) or from an
# "Hourly Rate" label. Eastern has no pay field at all and puts pay in the job
# description ("Salary Hire Range: $60,598 - 66,889 annually"), so a page
# with no pay field is read from all its fields' text instead; a page that has
# one but states no amount ("commensurate with experience") stays NA.
PEOPLEADMIN_PAY_LABELS <- c("Salary", "Estimated Salary", "Hiring Range", "Starting Salary Range",
                            "Hourly Rate (if applicable)", "Full Salary Range")

parse_peopleadmin_posting_pay <- function(html_text) {
  none <- extract_posted_pay(NA_character_)
  if (is.na(html_text)) return(none)
  rows <- rvest::html_elements(rvest::read_html(html_text), xpath = "//tr[th and td]")
  labels <- trimws(rvest::html_text2(rvest::html_element(rows, "th")))
  values <- trimws(gsub("\\s+", " ", rvest::html_text2(rvest::html_element(rows, "td"))))

  pay_fields <- intersect(PEOPLEADMIN_PAY_LABELS, labels)
  if (length(pay_fields) == 0) return(extract_posted_pay(paste(values, collapse = ". ")))
  for (label in pay_fields) {
    value <- values[match(label, labels)]
    pay <- extract_posted_pay(value)
    if (is.na(pay$Pay_Min)) next
    if (is.na(pay$Pay_Unit)) {
      basis <- tolower(values[match("Compensation", labels)])
      pay$Pay_Unit <- dplyr::case_when(
        grepl("^hourly", label, ignore.case = TRUE) ~ "hour",
        basis %in% "annual" ~ "year",
        basis %in% "hourly" ~ "hour",
        basis %in% "monthly" ~ "month",
        basis %in% "by credit" ~ "credit hour",
        # A bare $7-$100 figure in a pay field ("Hiring Range: $18.44 -
        # $23.06") can only be an hourly wage.
        pay$Pay_Max >= 7 & pay$Pay_Max <= 100 ~ "hour",
        TRUE ~ NA_character_
      )
    }
    # A long salary paragraph keeps only the extractor's snippet around the amount.
    if (nchar(value) <= 200) pay$Pay_Text <- value
    return(pay)
  }
  none
}

parse_peopleadmin_atom <- function(xml_text, location_fallback) {
  doc <- xml2::read_xml(xml_text)
  ns <- c(a = "http://www.w3.org/2005/Atom")
  entries <- xml2::xml_find_all(doc, "//a:entry", ns)

  if (length(entries) == 0) {
    return(data.frame(Title = character(0), Location = character(0),
                       Posted_Date = character(0), Link = character(0),
                       stringsAsFactors = FALSE))
  }

  titles <- xml2::xml_text(xml2::xml_find_first(entries, "a:title", ns))
  links <- xml2::xml_attr(xml2::xml_find_first(entries, "a:link", ns), "href")
  published <- xml2::xml_text(xml2::xml_find_first(entries, "a:published", ns))
  posted_date <- as.Date(substr(published, 1, 10))

  data.frame(
    Title = titles,
    Location = location_fallback,
    Posted_Date = as.character(posted_date),
    Link = links,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# NEOGOV (LCCC, Casper, Central Wyoming, Gillette, Western) -- internal
# AJAX endpoint the page's own JS calls
# ---------------------------------------------------------------------------

# base_domain is e.g. "https://www.schooljobs.com" or
# "https://www.governmentjobs.com"; agency_slug is the short agency code
# from the site's own career-page URL (e.g. "cwc", "caspercollege").
# The X-Requested-With header is what makes the server return the AJAX
# job-list fragment instead of the full page shell around an empty
# container -- found by tracing the page's real network traffic with
# chromote, not by guessing.
#
# The listing is paginated at 10 per page (`page` query param, 1-indexed);
# the fragment gives no total-count field to check against, so this pages
# until a page comes back with zero cards. Confirmed live that omitting
# `page` entirely is byte-identical to `page = 1`, so every request goes
# through the same code path. Missing this loop previously silently
# truncated any agency with more than 10 open postings to just the first
# page (caught live: Central Wyoming College and Gillette College both
# currently have postings on page 2 that a single unpaginated request
# would have dropped).
fetch_neogov_postings <- function(base_domain, agency_slug) {
  all_pages <- list()
  page <- 1
  repeat {
    resp <- request(paste0(base_domain, "/careers/home/index")) %>%
      req_url_query(agency = agency_slug, sort = "PositionTitle", isDescendingSort = "false", page = page) %>%
      req_headers(`X-Requested-With` = "XMLHttpRequest") %>%
      perform_with_retry()

    page_result <- parse_neogov_html(resp_body_string(resp), base_domain)
    if (nrow(page_result) == 0) break

    all_pages[[length(all_pages) + 1]] <- page_result
    page <- page + 1
  }

  if (length(all_pages) == 0) {
    return(ensure_posted_pay_columns(
      data.frame(Title = character(0), Location = character(0),
                 Posted_Date = character(0), Link = character(0),
                 stringsAsFactors = FALSE)))
  }

  dplyr::bind_rows(all_pages)
}

parse_neogov_html <- function(html_text, base_domain) {
  soup <- rvest::read_html(html_text)

  # The fragment renders each job TWICE -- once in a card/list <ul> (with
  # location/date metadata) and again in a plain desktop <table> (title
  # only, no metadata) -- both populated regardless of viewport, not
  # switched by CSS. Scoping to just the card container avoids double-
  # counting every posting and matching the table version's rows, which
  # have no location/date to extract at all.
  container <- rvest::html_elements(soup, "ul.job-listing-container")
  nodes <- rvest::html_elements(container, ".item-details-link")

  if (length(nodes) == 0) {
    return(ensure_posted_pay_columns(
      data.frame(Title = character(0), Location = character(0),
                 Posted_Date = character(0), Link = character(0),
                 stringsAsFactors = FALSE)))
  }

  titles <- rvest::html_text2(nodes)
  links <- paste0(base_domain, rvest::html_attr(nodes, "href"))

  # Location/date nodes are matched positionally within the same
  # container; pad with NA if a page's markup omits one for some rows
  # rather than erroring on a length mismatch.
  pad_to <- function(x, n) if (length(x) == n) x else rep(NA_character_, n)
  n <- length(titles)
  locations <- pad_to(rvest::html_text2(rvest::html_elements(container, ".list-meta li:nth-child(1)")), n)
  posted_dates <- pad_to(rvest::html_text2(rvest::html_elements(container, ".list-entry-starts span")), n)

  # Pay is matched to each posting by job ID (the card's data-job-id is the
  # number in its /jobs/<id>/ link), not by position. If reading the pay
  # fails (a markup change), the postings still come through with NA pay.
  pay <- tryCatch(parse_neogov_card_pay(html_text), error = function(e) {
    message("NEOGOV card pay could not be read: ", conditionMessage(e))
    ensure_posted_pay_columns(data.frame(Job_ID = character(0), stringsAsFactors = FALSE))
  })
  idx <- match(sub("^.*/jobs/([0-9]+).*$", "\\1", links), pay$Job_ID)

  cbind(data.frame(Title = titles, Location = locations, Posted_Date = posted_dates,
                   Link = links, stringsAsFactors = FALSE),
        pay[idx, POSTED_PAY_COLUMNS, drop = FALSE], row.names = NULL)
}

# Posted pay from each card of a NEOGOV (schooljobs.com / governmentjobs.com)
# job-list fragment, keyed by the card's data-job-id. The card's meta line
# carries the board's pay field ("Full-Time - $61,356.00 - $76,695.00
# Annually"); "Depends on Qualifications" or "See Position Description" means
# none is stated, and then the card's description preview is read instead
# ("Current adjunct pay is $775 per credit hour"). Shared verbatim with the
# Montana dashboard's direct_api_scrapers.R.
parse_neogov_card_pay <- function(html_text) {
  container <- rvest::html_elements(rvest::read_html(html_text), "ul.job-listing-container")
  cards <- rvest::html_elements(container, "li.list-item")
  if (length(cards) == 0) {
    return(ensure_posted_pay_columns(data.frame(Job_ID = character(0), stringsAsFactors = FALSE)))
  }
  meta_pay <- vapply(cards, function(card) {
    lines <- rvest::html_text2(rvest::html_elements(card, ".list-meta li"))
    lines <- lines[grepl("$", lines, fixed = TRUE)]
    if (length(lines) == 0) NA_character_ else trimws(lines[1])
  }, character(1))
  preview <- rvest::html_text2(rvest::html_element(cards, ".list-entry"))
  pay <- extract_posted_pay(ifelse(is.na(meta_pay), preview, meta_pay))
  from_meta <- !is.na(meta_pay) & !is.na(pay$Pay_Min)
  pay$Pay_Text[from_meta] <- meta_pay[from_meta]
  # The pay field's unit is the board's pay type, which can be wrong: Gillette's
  # adjunct cards say "Adjunct - $900.00 Hourly" while the description says
  # "Grade: $900 per credit hour" (2026-11-10). When the field's unit is
  # rejected as implausible, the description preview's pay is used instead,
  # if it states a unit.
  no_unit <- which(from_meta & is.na(pay$Pay_Unit) & !is.na(preview))
  if (length(no_unit) > 0) {
    alt <- extract_posted_pay(preview[no_unit])
    use <- !is.na(alt$Pay_Min) & !is.na(alt$Pay_Unit)
    for (col in POSTED_PAY_COLUMNS) pay[[col]][no_unit[use]] <- alt[[col]][use]
  }
  cbind(data.frame(Job_ID = rvest::html_attr(cards, "data-job-id"), stringsAsFactors = FALSE), pay)
}

# ---------------------------------------------------------------------------
# SchoolSpring (K-12 districts) -- unprotected API host behind the
# Incapsula-protected UI domain
# ---------------------------------------------------------------------------

# domain_name is the district's own SchoolSpring hostname, e.g.
# "crook1.schoolspring.com" (from springer_job_links.csv's Job Link column,
# stripped of scheme/trailing slash). The interactive crook1.schoolspring.com
# site itself is Incapsula-protected, but api.schoolspring.com -- the same
# host its own React app calls -- is not.
# Registry Job_Link -> the domain_name fetch_schoolspring_postings() takes.
# Shared by Wy_ED_Jobs.Rmd and drift_check.R's resolve_scraper_call(), so
# the live check calls the scraper exactly the way the pipeline does.
schoolspring_domain <- function(job_link) sub("/$", "", sub("^https?://", "", job_link))

fetch_schoolspring_postings <- function(domain_name, page_size = 50) {
  all_pages <- list()
  page <- 1
  repeat {
    resp <- request("https://api.schoolspring.com/api/Jobs/GetPagedJobsWithSearch") %>%
      req_url_query(
        domainName = domain_name, keyword = "", location = "", category = "",
        gradelevel = "", jobtype = "", organization = "",
        page = page, size = page_size, sortDateAscending = "false"
      ) %>%
      perform_with_retry()

    page_df <- parse_schoolspring_json(resp_body_string(resp), domain_name)
    if (nrow(page_df) == 0) break

    all_pages[[length(all_pages) + 1]] <- page_df
    if (nrow(page_df) < page_size) break  # last page was partial -> no more pages
    page <- page + 1
  }

  if (length(all_pages) == 0) {
    return(data.frame(Title = character(0), Location = character(0),
                       Posted_Date = character(0), Posting_ID = character(0),
                       Link = character(0),
                       stringsAsFactors = FALSE))
  }
  dplyr::bind_rows(all_pages)
}

parse_schoolspring_json <- function(json_text, domain_name) {
  parsed <- jsonlite::fromJSON(json_text, simplifyVector = TRUE)
  jobs <- parsed$value$jobsList

  if (is.null(jobs) || length(jobs) == 0 || nrow(jobs) == 0) {
    return(data.frame(Title = character(0), Location = character(0),
                       Posted_Date = character(0), Posting_ID = character(0),
                       Link = character(0),
                       stringsAsFactors = FALSE))
  }

  # SchoolSpring occasionally exposes its own example vacancies as active
  # records. These are placeholder text, not jobs candidates can apply for.
  jobs <- jobs[!grepl("^Sample (Classified|Certified) Position$", jobs$title,
                      ignore.case = TRUE), , drop = FALSE]
  if (nrow(jobs) == 0) {
    return(data.frame(Title = character(0), Location = character(0),
                      Posted_Date = character(0), Posting_ID = character(0),
                      Link = character(0),
                      stringsAsFactors = FALSE))
  }

  data.frame(
    Title = jobs$title,
    Location = jobs$location,
    Posted_Date = substr(jobs$displayDate, 1, 10),
    Posting_ID = paste0("schoolspring:", tolower(domain_name), ":", jobs$jobId),
    Link = paste0("https://", domain_name, "/jobs/", jobs$jobId),
    stringsAsFactors = FALSE
  )
}

# One posting's own record (api.schoolspring.com/api/Jobs/<jobId>, the call
# the posting page makes; no login). It carries what the listing doesn't:
#
# - Close_Date: the deadline applicants see (formattedExternalCloseDate,
#   "Nov 13, 2026 11:59 PM (Mountain Standard Time)"). The raw closeDate is
#   UTC (2026-11-14T06:59), and "Posted until filled" is stored as
#   2050-01-01, so it is NA here.
# - Job_Status: jobStatusID 30 posted, 40 review (closed to applicants),
#   50 filled, 60 canceled. Live listings only show 30; the record keeps its
#   status after the posting leaves the board, which is what tells "filled"
#   from "canceled" (posting-dates plan, step 4).
# - Posted pay from payMin/payMax/salaryCode when payDisplay is on (88 of
#   107 MT postings, 2026-11-03; the listing has none). 0 means "not given".
#   salaryCode is the district's own label, so units are read with the
#   amount: "Per Year" under $10,000 is a coaching/extra-duty stipend
#   (Ronan $1,919.10), "Hourly and Certified Pay" holds both teacher
#   salaries ($45,450) and hourly rates ($24.38), and "Substitute" (WY: $150,
#   $14.95, $1,132) doesn't say per hour, day or season, so it is left
#   without pay, as is any other label ("Please see our Salary Schedule").
#   WY 2026-11-04: 132 of 214 postings show pay; 19 have a dated close.
#
# displayDate (the listing's posted date, kept as Posted_Date) stays the
# start date: postDate is when the record was created, often a few days
# before it went public and sometimes re-saved years later.
SCHOOLSPRING_STATUS <- c(`30` = "posted", `40` = "review", `50` = "filled", `60` = "canceled")

parse_schoolspring_job_json <- function(json_text) {
  v <- jsonlite::fromJSON(json_text, simplifyVector = TRUE)$value
  ji <- v$jobInfo
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) == 0 || is.na(x) || x <= 0) NA_real_ else x }

  closing <- v$formattedExternalCloseDate
  close_date <- if (is.null(closing) || is.na(closing)) NA_character_ else {
    d <- as.Date(sub("^([A-Za-z]{3} \\d{1,2}, \\d{4}).*$", "\\1", closing), format = "%b %d, %Y")
    if (is.na(d)) NA_character_ else format(d)
  }

  status <- unname(SCHOOLSPRING_STATUS[as.character(ji$jobStatusID)])
  if (length(status) == 0) status <- NA_character_
  if (is.na(status) && !is.null(ji$jobStatusID)) status <- paste0("status ", ji$jobStatusID)

  lo <- num(ji$payMin); hi <- num(ji$payMax)
  lo <- dplyr::coalesce(lo, hi); hi <- dplyr::coalesce(hi, lo)
  code <- if (is.null(ji$salaryCode)) "" else trimws(as.character(ji$salaryCode))
  unit <- if (!isTRUE(as.integer(ji$payDisplay) == 1L) || is.na(hi)) NA_character_ else
    switch(code,
           "Per Hour" = "hour",
           "Per Day" = "day",
           "Stipend" = "stipend",
           "Per Year" = if (hi >= 10000) "year" else "stipend",
           "Hourly and Certified Pay" = if (hi >= 10000) "year" else if (hi <= 200) "hour" else NA_character_,
           NA_character_)
  pay_text <- if (is.na(unit)) NA_character_ else
    paste0(if (lo == hi) format_pay_amount(lo) else paste0(format_pay_amount(lo), "-", format_pay_amount(hi)),
           " ", code, " (SchoolSpring pay field)")

  data.frame(job_id = as.character(ji$jobId), Close_Date = close_date, Job_Status = status,
             Pay_Min = if (is.na(unit)) NA_real_ else lo, Pay_Max = if (is.na(unit)) NA_real_ else hi,
             Pay_Unit = unit, Pay_Text = pay_text, stringsAsFactors = FALSE)
}

format_pay_amount <- function(x) paste0("$", formatC(x, format = "f", digits = if (x %% 1 == 0) 0 else 2, big.mark = ","))

SCHOOLSPRING_DETAIL_EMPTY <- data.frame(job_id = character(0), Close_Date = character(0), Job_Status = character(0),
                                         Pay_Min = numeric(0), Pay_Max = numeric(0), Pay_Unit = character(0),
                                         Pay_Text = character(0), stringsAsFactors = FALSE)

# Detail records for a set of job IDs, one request each. A failed request
# leaves only that posting's fields NA (it is skipped here and NA after the
# join), and fetching stops once budget_secs is spent, so a slow API can't
# hold up the weekly run.
fetch_schoolspring_details <- function(job_ids, delay = 0.2, budget_secs = 300) {
  deadline <- Sys.time() + budget_secs
  out <- lapply(unique(job_ids[!is.na(job_ids)]), function(id) {
    if (Sys.time() > deadline) return(NULL)
    Sys.sleep(delay)
    tryCatch({
      resp <- request(paste0("https://api.schoolspring.com/api/Jobs/", id)) %>%
        req_timeout(30) %>%
        perform_with_retry()
      parse_schoolspring_job_json(resp_body_string(resp))
    }, error = function(e) NULL)
  })
  dplyr::bind_rows(SCHOOLSPRING_DETAIL_EMPTY, dplyr::bind_rows(out))
}

# The listing plus each posting's detail fields (Close_Date, Job_Status,
# Pay_*), joined on the job ID in Link. Run before
# normalize_id_backed_k12_postings(), which passes Pay_* through.
add_schoolspring_details <- function(postings, ...) {
  if (nrow(postings) == 0) return(cbind(postings, SCHOOLSPRING_DETAIL_EMPTY[0, -1]))
  ids <- sub("^.*/jobs/(\\d+)$", "\\1", postings$Link)
  details <- fetch_schoolspring_details(ids, ...)
  details[match(ids, details$job_id), -1, drop = FALSE] %>%
    { rownames(.) <- NULL; cbind(postings, .) }
}

# ---------------------------------------------------------------------------
# How SchoolSpring postings ended (posting-dates plan, step 4)
# ---------------------------------------------------------------------------
# A SchoolSpring posting's record keeps its status after it leaves the board,
# so each week every posting that the raw archives listed before but not today
# is looked up: filled, canceled, or review (closed to applicants, outcome not
# yet entered). Filled and canceled are final; review is rechecked weekly, as
# it can still turn into filled. How a district uses the statuses varies, so
# "filled" is not comparable across districts: all 12 Libby postings that left
# Aug-Oct 2026 were still "review", and Campbell County WY closed 79 as
# "canceled" against 8 "filled" (first lookup, 2026-11-04).
# A lookup that still says "posted" means our scrape missed a live posting
# (a failed run, say), so nothing is recorded for it.
#
# schoolspring_endings.csv, one row per ended posting: job_id, District,
# Title, Posted_Date (board's own), First_Seen/Last_Seen (archive dates it was
# listed), Job_Status, Status_Since (first check that saw this status) and
# Last_Checked. Last_Seen is the last weekly sighting, so the true end lies
# between Last_Seen and the next run.
SCHOOLSPRING_FINAL_STATUS <- c("filled", "canceled")

schoolspring_job_id <- function(url) {
  url <- as.character(url)
  ifelse(!is.na(url) & grepl("schoolspring\\.com/jobs/\\d+$", url), sub("^.*/jobs/", "", url), NA_character_)
}

# One row per SchoolSpring job ID seen in the archives since `since`, with
# the dates it was first and last listed. District names go through
# canonicalize_k12_district() when it is loaded (old WY archives spell
# "Campbell County Scholl District 1").
schoolspring_sightings <- function(archive_dir = "Archivek12_Data", since = Sys.Date() - 365,
                                   canonicalize = if (exists("canonicalize_k12_district")) canonicalize_k12_district else identity) {
  files <- list.files(archive_dir, "^combined_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
  dates <- as.Date(sub("^combined_(.*)\\.csv$", "\\1", basename(files)))
  rows <- lapply(which(dates >= since), function(i) {
    d <- utils::read.csv(files[i], colClasses = "character", check.names = FALSE)
    id <- schoolspring_job_id(d$url)
    ok <- !is.na(id)
    if (!any(ok)) return(NULL)
    data.frame(job_id = id[ok], District = canonicalize(d$District[ok]), Title = d$title[ok],
               Posted_Date = d$date_posted[ok], Seen = dates[i], stringsAsFactors = FALSE)
  })
  rows <- dplyr::bind_rows(rows)
  if (nrow(rows) == 0) {
    return(data.frame(job_id = character(0), District = character(0), Title = character(0),
                      Posted_Date = character(0), First_Seen = as.Date(character(0)),
                      Last_Seen = as.Date(character(0)), stringsAsFactors = FALSE))
  }
  rows %>%
    dplyr::arrange(Seen) %>%
    dplyr::group_by(job_id) %>%
    dplyr::summarize(District = dplyr::last(District), Title = dplyr::last(Title),
                     Posted_Date = dplyr::first(Posted_Date), First_Seen = min(Seen),
                     Last_Seen = max(Seen), .groups = "drop") %>%
    as.data.frame()
}

SCHOOLSPRING_ENDINGS_EMPTY <- data.frame(
  job_id = character(0), District = character(0), Title = character(0), Posted_Date = character(0),
  First_Seen = character(0), Last_Seen = character(0), Job_Status = character(0),
  Status_Since = character(0), Last_Checked = character(0), stringsAsFactors = FALSE)

# Looks up every posting listed before but not on `today` (unless it already
# has a final status or was last listed more than recheck_days ago) and
# returns the updated endings table. `fetch` is fetch_schoolspring_details()
# in production.
update_schoolspring_endings <- function(sightings, endings = SCHOOLSPRING_ENDINGS_EMPTY, today = Sys.Date(),
                                        fetch = fetch_schoolspring_details, recheck_days = 365) {
  endings <- dplyr::bind_rows(SCHOOLSPRING_ENDINGS_EMPTY, dplyr::mutate(endings, dplyr::across(dplyr::everything(), as.character)))
  final_ids <- endings$job_id[endings$Job_Status %in% SCHOOLSPRING_FINAL_STATUS]
  gone <- sightings[sightings$Last_Seen < today & sightings$Last_Seen >= today - recheck_days &
                      !sightings$job_id %in% final_ids, , drop = FALSE]
  if (nrow(gone) == 0) return(endings)

  details <- fetch(gone$job_id)
  details <- details[!is.na(details$Job_Status) & details$Job_Status != "posted", c("job_id", "Job_Status"), drop = FALSE]
  if (nrow(details) == 0) return(endings)

  today <- format(as.Date(today))
  found <- merge(gone, details, by = "job_id")
  prev <- endings[match(found$job_id, endings$job_id), , drop = FALSE]
  same <- !is.na(prev$Job_Status) & prev$Job_Status == found$Job_Status
  updated <- data.frame(job_id = found$job_id, District = found$District, Title = found$Title,
                        Posted_Date = found$Posted_Date, First_Seen = format(found$First_Seen),
                        Last_Seen = format(found$Last_Seen), Job_Status = found$Job_Status,
                        Status_Since = ifelse(same, prev$Status_Since, today), Last_Checked = today,
                        stringsAsFactors = FALSE)
  dplyr::bind_rows(endings[!endings$job_id %in% updated$job_id, , drop = FALSE], updated) %>%
    dplyr::arrange(Last_Seen, job_id)
}

# Weekly step: reads the archives and the endings file, looks up postings
# that left the board, rewrites the file. Run after today's raw archive is
# written. Returns the table (invisibly).
refresh_schoolspring_endings <- function(archive_dir = "Archivek12_Data", path = "schoolspring_endings.csv",
                                         today = Sys.Date(), ...) {
  endings <- if (file.exists(path)) utils::read.csv(path, colClasses = "character") else SCHOOLSPRING_ENDINGS_EMPTY
  sightings <- schoolspring_sightings(archive_dir, since = as.Date(today) - 365)
  out <- update_schoolspring_endings(sightings, endings, today = as.Date(today), ...)
  utils::write.csv(out, path, row.names = FALSE)
  message("SchoolSpring endings: ", nrow(out), " postings (",
          paste(names(table(out$Job_Status)), table(out$Job_Status), collapse = ", "), ")")
  invisible(out)
}


# ---------------------------------------------------------------------------
# RedRoverK12 (Johnson County School District 1) -- unauthenticated GraphQL
# endpoint the site's own Next.js app calls
# ---------------------------------------------------------------------------

# org_id is the numeric organization ID RedRoverK12 assigns per district
# (found by tracing the real GraphQL request body with chromote --
# Johnson County SD1's is "3146" for org slug "jcsd1"). Not derivable from
# the org slug alone; a new district on this platform would need its own
# ID looked up the same way.
# Registry Job_Link -> the org_slug fetch_redrover_postings() takes (shared
# with drift_check.R, like schoolspring_domain()).
redrover_org_slug <- function(job_link) sub(".*/org/([^/?]+).*", "\\1", job_link, ignore.case = TRUE)

fetch_redrover_postings <- function(org_id, org_slug) {
  query <- 'query GetJobPostings($search: JobPostingSearchInput!) {
  jobSeekerSiteUnauthenticated {
    jobPostingSearch(search: $search) {
      results {
        id
        name
        organizationName
        location { name }
        activePublicOnDateUtc
        payTypeId
        minPay
        maxPay
      }
    }
  }
}'
  resp <- request("https://api.redroverk12.com/graphql") %>%
    req_headers(`Content-Type` = "application/json") %>%
    req_body_json(list(
      operationName = "GetJobPostings",
      variables = list(search = list(orgId = org_id, searchTerm = "", locationIds = list(), jobPostingCategoryIds = list())),
      query = query
    )) %>%
    perform_with_retry()
  parse_redrover_json(resp_body_string(resp), org_slug)
}

# RedRover's own pay types, mapped to Pay_Unit. The labels are the ones the
# job board itself prints ("per unit" is how it shows a STIPEND, e.g. a
# coaching season). The board hides pay when minPay is empty or the type is
# UNKNOWN ("per period"), so those stay NA here too.
REDROVER_PAY_UNITS <- c(HOURLY = "hour", DAILY = "day", SALARIED = "year", STIPEND = "stipend")
REDROVER_PAY_LABELS <- c(HOURLY = "per hour", DAILY = "per day", SALARIED = "per year", STIPEND = "per unit")

redrover_pay_columns <- function(n, pay_type, min_pay, max_pay) {
  lo <- suppressWarnings(as.numeric(if (is.null(min_pay)) rep(NA, n) else min_pay))
  hi <- suppressWarnings(as.numeric(if (is.null(max_pay)) rep(NA, n) else max_pay))
  type <- if (is.null(pay_type)) rep(NA_character_, n) else as.character(pay_type)
  stated <- !is.na(lo) & lo > 0 & !is.na(type) & type %in% names(REDROVER_PAY_UNITS)
  hi <- ifelse(stated & (is.na(hi) | hi < lo), lo, hi)
  money <- function(x) paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
  data.frame(
    Pay_Min = ifelse(stated, lo, NA_real_),
    Pay_Max = ifelse(stated, hi, NA_real_),
    Pay_Unit = ifelse(stated, unname(REDROVER_PAY_UNITS[type]), NA_character_),
    Pay_Text = ifelse(stated, paste(ifelse(hi > lo, paste(money(lo), "-", money(hi)), money(lo)),
                                    unname(REDROVER_PAY_LABELS[type])), NA_character_),
    stringsAsFactors = FALSE
  )
}

parse_redrover_json <- function(json_text, org_slug) {
  parsed <- jsonlite::fromJSON(json_text, simplifyVector = TRUE)
  results <- parsed$data$jobSeekerSiteUnauthenticated$jobPostingSearch$results

  if (is.null(results) || length(results) == 0 || nrow(results) == 0) {
    return(data.frame(Title = character(0), Location = character(0),
                       Posted_Date = character(0), Posting_ID = character(0),
                       Link = character(0),
                       Pay_Min = numeric(0), Pay_Max = numeric(0), Pay_Unit = character(0),
                       Pay_Text = character(0),
                       stringsAsFactors = FALSE))
  }

  cbind(
    data.frame(
      Title = results$name,
      Location = results$location$name,
      Posted_Date = substr(results$activePublicOnDateUtc, 1, 10),
      Posting_ID = paste0("redrover:", tolower(org_slug), ":", results$id),
      Link = paste0("https://jobs.redroverk12.com/org/", org_slug, "/opening/", results$id),
      stringsAsFactors = FALSE
    ),
    redrover_pay_columns(nrow(results), results$payTypeId, results$minPay, results$maxPay)
  )
}

# Converts platforms that expose a source posting ID into the K-12 pipeline's
# lowercase schema without discarding their deep links or identities.
normalize_id_backed_k12_postings <- function(postings) {
  required <- c("Title", "Location", "Posted_Date", "Posting_ID", "Link", "District")
  missing <- setdiff(required, names(postings))
  if (length(missing) > 0) {
    stop(
      "normalize_id_backed_k12_postings(): missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }

  # Pay_* passes through when the platform states it (RedRover); NA otherwise.
  postings <- ensure_posted_pay_columns(postings)
  data.frame(
    title = as.character(postings$Title),
    date_posted = as.character(postings$Posted_Date),
    position = NA_character_,
    location = as.character(postings$Location),
    url = as.character(postings$Link),
    posting_id = as.character(postings$Posting_ID),
    District = as.character(postings$District),
    Pay_Min = as.numeric(postings$Pay_Min),
    Pay_Max = as.numeric(postings$Pay_Max),
    Pay_Unit = as.character(postings$Pay_Unit),
    Pay_Text = as.character(postings$Pay_Text),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Applitrack (K-12 districts) -- script-injection endpoint whose payload is
# plain embedded HTML, not real JS execution
# ---------------------------------------------------------------------------

# tenant_path is everything between applitrack.com/ and /onlineapp/ in the
# district's own URL, e.g. "bighorn" for
# https://www.applitrack.com/bighorn/onlineapp/default.aspx. The listing
# markup itself (ul.postingsList, table.title td#wrapword, li span.label/
# span.normal) is unchanged from the original scraper's selectors -- what's
# different is fetching it directly from Output.asp (the endpoint
# default.aspx's own inline script injects via document.write()) instead of
# rendering default.aspx in a browser and waiting for that injection to
# happen.
# Registry Job_Link -> tenant_path: everything between applitrack.com/ and
# /onlineapp/ (shared with drift_check.R, like schoolspring_domain()).
applitrack_tenant_path <- function(job_link) sub(".*applitrack\\.com/([^/]+)/onlineapp.*", "\\1", job_link, ignore.case = TRUE)

# The posting's own page on the district's AppliTrack board (the link the
# board's "Email To A Friend" button shares). NA without a job ID.
applitrack_posting_url <- function(tenant_path, job_id) {
  ifelse(is.na(job_id) | !nzchar(job_id), NA_character_,
         paste0("https://www.applitrack.com/", tenant_path,
                "/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=", job_id))
}

fetch_applitrack_postings <- function(tenant_path) {
  resp <- request(paste0("https://www.applitrack.com/", tenant_path, "/onlineapp/jobpostings/Output.asp")) %>%
    req_url_query(all = "1") %>%
    perform_with_retry()
  # Applitrack serves Windows-1252 bytes with no charset in Content-Type, so
  # httr2 guesses UTF-8. Any posting containing a curly quote/en-dash/etc.
  # then fails to decode -- resp_body_string() returns NA rather than
  # erroring, and parse_applitrack_output(NA) silently yields zero rows.
  # That looks identical to a district with genuinely no openings, and it's
  # exactly what happened: confirmed live for 10 of 25 districts, one
  # (Sweetwater County SD1) hiding 69 real postings this way.
  parse_applitrack_output(resp_body_string(resp, encoding = "Windows-1252"))
}

# extract_posted_pay() lives in k12_he_classification.R; standalone scripts
# (repro_scraper.R, live_check_autofix.R) source this file on its own.
if (!exists("extract_posted_pay", mode = "function")) source("k12_he_classification.R")

parse_applitrack_output <- function(js_text) {
  # Response body is a series of document.write('<html fragment>') calls
  # with single quotes escaped as \' for JS string-literal safety; joining
  # every literal's un-escaped content reconstructs the full HTML the
  # browser would otherwise inject into the page.
  writes <- regmatches(js_text, gregexpr("document\\.write\\('(.*?)'\\);", js_text))[[1]]
  literals <- sub("^document\\.write\\('(.*)'\\);$", "\\1", writes)
  html <- paste(gsub("\\\\'", "'", literals), collapse = "")

  if (!nzchar(html)) {
    return(data.frame(title = character(0), position = character(0), position2 = character(0),
                       date_posted = character(0), location = character(0), closing_date = character(0),
                       job_id = character(0),
                       Pay_Min = numeric(0), Pay_Max = numeric(0), Pay_Unit = character(0),
                       Pay_Text = character(0), stringsAsFactors = FALSE))
  }

  soup <- rvest::read_html(html)
  postings <- rvest::html_elements(soup, "ul.postingsList")

  if (length(postings) == 0) {
    return(data.frame(title = character(0), position = character(0), position2 = character(0),
                       date_posted = character(0), location = character(0), closing_date = character(0),
                       job_id = character(0),
                       Pay_Min = numeric(0), Pay_Max = numeric(0), Pay_Unit = character(0),
                       Pay_Text = character(0), stringsAsFactors = FALSE))
  }

  rows <- lapply(postings, function(p) {
    title <- rvest::html_text2(rvest::html_element(p, "table.title td#wrapword"))
    # Each field is its own <li>: one span.label and its span.normal
    # value(s). "Position Type:" has one value or two (category +
    # subcategory, e.g. "Support Staff/" + "Para-Educator Special
    # Services"). Values are read per <li>, never by a running position
    # across the posting: guessing whether a second Position Type value
    # existed (from whether the next value had a ":") swallowed the next
    # field's value when it didn't, e.g. a "Date Posted" date, and shifted
    # every later field by one (WY Teton #1 and Carbon #1, 2026-11-03).
    # Description <li>s carry no span.label and are skipped.
    field <- setNames(as.list(rep(NA_character_, 5)), c("position", "position2", "date_posted", "location", "closing_date"))
    for (li in rvest::html_elements(p, xpath = ".//li[span[contains(concat(' ', normalize-space(@class), ' '), ' label ')]]")) {
      label <- trimws(rvest::html_text2(rvest::html_element(li, "span.label")))
      values <- rvest::html_text2(rvest::html_elements(li, "span.normal"))
      if (length(values) == 0) next
      if (label == "Position Type:") {
        field$position <- values[1]
        if (length(values) >= 2) field$position2 <- values[2]
      } else if (label == "Date Posted:") {
        field$date_posted <- values[1]
      } else if (label == "Location:") {
        field$location <- values[1]
      } else if (label == "Closing Date:") {
        field$closing_date <- values[1]
      }
    }

    # Most AppliTrack postings state their pay in the body ("SALARY:
    # $15.14 per hour", "Stipend: $3,548.00", "$46,500-$71,803"), which this
    # all=1 listing already includes -- no extra request needed.
    pay <- extract_posted_pay(rvest::html_text2(p))

    # AppliTrack's own per-posting ID: the block's id ("p2138_") and the
    # "JobID: 2138" beside its title. It makes a stable per-posting link
    # (applitrack_posting_url()), so postings sharing a title stay apart.
    job_id <- sub("^p(\\d+)_?$", "\\1", rvest::html_attr(p, "id"))
    if (is.na(job_id) || !grepl("^\\d+$", job_id)) {
      job_id <- stringr::str_match(rvest::html_text2(rvest::html_element(p, "table.title")), "JobID:\\s*(\\d+)")[, 2]
    }

    data.frame(title = title, position = field$position, position2 = field$position2,
               date_posted = field$date_posted, location = field$location,
               closing_date = field$closing_date, job_id = job_id, pay, stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Paylocity Recruiting (Laramie Montessori Charter School, and potentially
# other small orgs found later) -- job list embedded as JSON in the raw
# page, not fetched via a separate API call
# ---------------------------------------------------------------------------

# company_guid and company_slug come from the org's own recruiting URL:
# https://recruiting.paylocity.com/recruiting/jobs/All/<company_guid>/<company_slug>
fetch_paylocity_jobs <- function(company_guid, company_slug) {
  url <- paste0("https://recruiting.paylocity.com/recruiting/jobs/All/", company_guid, "/", company_slug)
  resp <- request(url) %>% perform_with_retry()
  parse_paylocity_jobs(resp_body_string(resp))
}

parse_paylocity_jobs <- function(html_text) {
  m <- regmatches(html_text, regexpr("window\\.pageData\\s*=\\s*\\{", html_text))
  if (length(m) == 0) {
    return(data.frame(Title = character(0), Location = character(0),
                       Posted_Date = character(0), Link = character(0),
                       stringsAsFactors = FALSE))
  }

  # Brace-counting to find the end of the embedded JSON object, since it's
  # not on its own line and may itself contain nested braces.
  start <- regexpr("window\\.pageData\\s*=\\s*", html_text)
  json_start <- start + attr(start, "match.length")
  remainder <- substring(html_text, json_start)
  depth <- 0
  end_pos <- NA_integer_
  for (i in seq_len(nchar(remainder))) {
    ch <- substr(remainder, i, i)
    if (ch == "{") depth <- depth + 1
    else if (ch == "}") {
      depth <- depth - 1
      if (depth == 0) { end_pos <- i; break }
    }
  }
  if (is.na(end_pos)) {
    return(data.frame(Title = character(0), Location = character(0),
                       Posted_Date = character(0), Link = character(0),
                       stringsAsFactors = FALSE))
  }

  page_data <- jsonlite::fromJSON(substr(remainder, 1, end_pos), simplifyVector = TRUE)
  jobs <- page_data$Jobs

  if (is.null(jobs) || length(jobs) == 0 || nrow(jobs) == 0) {
    return(data.frame(Title = character(0), Location = character(0),
                       Posted_Date = character(0), Link = character(0),
                       stringsAsFactors = FALSE))
  }

  data.frame(
    Title = jobs$JobTitle,
    Location = jobs$LocationName,
    Posted_Date = substr(jobs$PublishedDate, 1, 10),
    Link = paste0("https://recruiting.paylocity.com/recruiting/jobs/Details/", jobs$JobId),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# TedK12 (PowerSchool's "Hire" applicant tracking system)
# ---------------------------------------------------------------------------

# TedK12/PowerSchool Hire serves genuinely different content depending on
# the request's User-Agent, not on whether a browser executes JS -- a
# request with httr2's default UA gets a near-empty modern app shell
# (rebranded "SchoolSpring" -- same corporate family, different product)
# with zero job rows, while a request that identifies as a real browser
# gets the real, classic jQuery-based job board with real
# <tr id="JobList_N"> rows, structurally identical to what this scraper
# has always expected. Confirmed 2026-08-06 by comparing a plain curl
# fetch (empty shell, 3.7KB) against a chromote-rendered page (real
# content, 112KB, 11+ real rows) for the exact same URL, then confirming
# curl -A "<real browser UA>" alone reproduces the real content -- no
# browser automation needed in production, just a real User-Agent header.
#
# This explains scrape_log.csv's flaky "ok"/"empty" alternation for the
# same URL across different runs the same day: whichever UA a given
# request happened to negotiate as (varies by R/httr2/curl version and
# platform) determined which content came back. Same failure category as
# the 2026-08-02/03 Applitrack encoding bug -- a subtle difference between
# this scraper's request and a real browser's, not an actual site change.
# Of the 5 WY districts on TedK12, only Goshen County SD1 has no other
# source covering it (the other 4 also appear in springer_job_links.csv
# via a working SchoolSpring API scrape, which is why this went unnoticed
# in the final per-district counts).
fetch_tedk12_postings <- function(url) {
  resp <- request(url) %>%
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36") %>%
    perform_with_retry()
  parse_tedk12_postings(resp_body_string(resp), url)
}

# Same td:nth-child(1..4) = title/date/position/location structure the
# original (pre-fix) plain rvest loop always used -- that part was never
# wrong, only the missing User-Agent was. The header row's title cell
# reads literally "Job Title" (its sortable-column link text); filtered
# out the same way the original loop did.
parse_tedk12_postings <- function(html_text, url) {
  empty <- data.frame(title = character(0), date_posted = character(0), position = character(0),
                       location = character(0), url = character(0), stringsAsFactors = FALSE)

  page <- rvest::read_html(html_text)
  rows <- rvest::html_nodes(page, "tr")
  if (length(rows) == 0) return(empty)

  # map_df() over zero matched <tr> elements returns a zero-COLUMN data
  # frame (not just zero rows), since it has no iteration to infer column
  # names from -- the `length(rows) == 0` guard above exists specifically
  # to avoid falling into that case and hitting "Unknown or uninitialised
  # column" warnings on the filter below.
  result <- rows %>%
    purrr::map_df(~{
      title_link <- .x %>% rvest::html_node("td:nth-child(1) a")
      title <- title_link %>% rvest::html_text(trim = TRUE)
      date_posted <- .x %>% rvest::html_node("td:nth-child(2)") %>% rvest::html_text(trim = TRUE)
      position <- .x %>% rvest::html_node("td:nth-child(3)") %>% rvest::html_text(trim = TRUE)
      location <- .x %>% rvest::html_node("td:nth-child(4)") %>% rvest::html_text(trim = TRUE)
      # The posting's own page (ViewJob.aspx?JobID=<n>), as MT's parser keeps
      # it: a stable per-posting ID, a page that says when the posting closed,
      # and the posted pay the board's list doesn't show (2026-11-05). The
      # board URL only where a row has no link.
      href <- rvest::html_attr(title_link, "href")
      detail_url <- if (is.na(href) || !grepl("JobID=\\d+", href, ignore.case = TRUE)) url else xml2::url_absolute(href, url)
      data.frame(title = title, date_posted = date_posted, position = position,
                 location = location, url = detail_url, stringsAsFactors = FALSE)
    })

  result[!is.na(result$title) & nzchar(result$title) & result$title != "Job Title", , drop = FALSE]
}
