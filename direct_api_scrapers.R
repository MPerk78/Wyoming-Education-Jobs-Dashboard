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
# The AppliTrack, SchoolSpring detail/ending, PeopleAdmin pay/date and NEOGOV card-pay
# functions moved to the edjobs package in phase 3 (2026-11-11); attached here so every
# script that sources this file still has them. Fix those in edjobs.
suppressMessages(library(edjobs))

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
# The board a K-12 row was read from, written into Posting_Source at scrape
# time (Wy_ED_Jobs.Rmd, fetch_all_misc_district_postings()) and archived, so
# a rebuild reads it back. Same wording as Montana's, so the states' files
# stack: "<platform> (direct district board)" for a registry platform,
# "<platform> district site (direct board)" for a district's own page, and
# the statewide feed's label (state_config.R) for a WSBA row.
K12_PLATFORM_SOURCE_LABELS <- c(
  Applitrack = "AppliTrack (direct district board)", TedK12 = "TedK12 (direct district board)",
  SchoolSpring = "SchoolSpring (direct district board)", RedRoverK12 = "Red Rover (direct district board)",
  paylocity = "Paylocity (direct board)",
  wordpress = "WordPress district site (direct board)", smartsites = "SmartSites district site (direct board)",
  schoolblocks = "SchoolBlocks district site (direct board)", edlio = "Edlio district site (direct board)",
  googlesites = "Google Sites district site (direct board)",
  educational_networks = "Educational Networks district site (direct board)",
  apptegy = "Apptegy district site (direct board)", prairieview = "Prairie View district site (direct board)"
)
WSBA_POSTING_SOURCE <- "WSBA statewide vacancies"
k12_posting_source_label <- function(platform) {
  label <- unname(K12_PLATFORM_SOURCE_LABELS[as.character(platform)])
  if (anyNA(label)) stop("No Posting_Source label for platform: ", paste(unique(platform[is.na(label)]), collapse = ", "))
  label
}

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



# extract_posted_pay() is in edjobs, attached by k12_he_classification.R; standalone scripts
# (repro_scraper.R, live_check_autofix.R) source this file on its own.
if (!exists("extract_posted_pay", mode = "function")) source("k12_he_classification.R")


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
