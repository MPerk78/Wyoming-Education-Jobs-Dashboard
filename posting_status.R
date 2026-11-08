# Posting status checks (closing-date audit, claude-notes/2026-11-05_closing-date-audit.md).
#
# A posting that stops appearing on its board has only *probably* closed: a
# missed scrape, a re-titled posting or a board change looks the same. Most
# platforms will say so outright on the posting's own page, and some state an
# official closing date. Each run this checks:
#   - WSBA statewide-feed rows the pipeline would count that link a district
#     posting: closed ones are dropped (20 of 23 checked on 2026-11-05 were
#     closed, counted as live every week);
#   - every posting with a per-posting link that dropped off its board since
#     the previous run (K-12 and higher ed);
# and keeps the results in posting_status.csv (committed, like
# schoolspring_endings.csv), one row per posting page:
#   Check_URL, Platform, Segment, District, Title, Last_Listed (last run it was
#   on its board), Status (open / closed / unknown), Signal (what the page
#   said), Stated_Close_Date (the deadline the posting states, where it does),
#   First_Checked, Last_Checked, Status_Since (first check that found it
#   closed), Checks.
# A closed status is kept; open and unknown are re-checked on later runs, up
# to POSTING_STATUS_MAX_CHECKS times. A failed check is "unknown", never closed.
#
# How each platform shows a closed posting (pages captured 2026-11-06 in
# tests/testthat/fixtures/posting_status_*.txt):
#   AppliTrack  "This vacancy is not available."          (plain request)
#   TalentEd    "no longer accepting applications"        (plain request)
#   NEOGOV      "Job posting is not found or expired", or (plain request)
#               its "Closing Date" has passed (the page stays up after closing)
#   Red Rover   "ERROR 404"                               (browser; "Job Closes: <date>" when open)
#   Oracle (UW) "This job is no longer available."        (browser)
#   JazzHR      HTTP 410 Gone                             (plain request)
#   Paycom      "We Couldn't Find This Job"               (browser; "CLOSING DATE:" when open)
# SchoolSpring postings have their own record (schoolspring_endings.csv) and
# PeopleAdmin's feed is complete (a dropped posting's page stays up), so
# neither is checked here.
POSTING_STATUS_COLUMNS <- c("Check_URL", "Platform", "Segment", "District", "Title", "Last_Listed", "Status", "Signal",
                            "Stated_Close_Date", "First_Checked", "Last_Checked", "Status_Since", "Checks")
POSTING_STATUS_MAX_CHECKS <- 4
POSTING_STATUS_BROWSER_PLATFORMS <- c("redrover", "oracle", "paycom")
POSTING_STATUS_UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

posting_status_platform <- function(url) {
  u <- tolower(ifelse(is.na(url), "", url))
  dplyr::case_when(
    grepl("applitrack\\.com/[^/]+/.*applitrackjobid=\\d+", u) ~ "applitrack",
    grepl("\\.tedk12\\.com/hire/viewjob\\.aspx\\?jobid=\\d+", u) ~ "tedk12",
    grepl("redroverk12\\.com/org/[^/]+/opening/\\d+", u) ~ "redrover",
    grepl("(schooljobs|governmentjobs)\\.com/careers/[^/]+/jobs/\\d+", u) ~ "neogov",
    grepl("oraclecloud\\.com/.*/job/\\d+", u) ~ "oracle",
    grepl("\\.applytojob\\.com/apply/[a-z0-9]+", u) ~ "jazzhr",
    grepl("paycomonline\\.net/.*/jobs/\\d+", u) ~ "paycom",
    TRUE ~ NA_character_)
}

# One page per posting: the same posting can be linked several ways (WSBA
# links AppliTrack's JobPostings/View.asp, the board links default.aspx).
posting_status_check_url <- function(url) {
  p <- posting_status_platform(url)
  u <- ifelse(is.na(url), NA_character_, trimws(url))
  out <- rep(NA_character_, length(u))
  at <- !is.na(p) & p == "applitrack"
  out[at] <- sub("^.*applitrack\\.com/([^/]+)/.*[?&]applitrackjobid=(\\d+).*$",
                 "https://www.applitrack.com/\\1/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=\\2",
                 tolower(u[at]))
  te <- !is.na(p) & p == "tedk12"
  out[te] <- sub("^https?://([a-z0-9-]+)\\.tedk12\\.com/hire/viewjob\\.aspx\\?jobid=(\\d+).*$",
                 "https://\\1.tedk12.com/hire/ViewJob.aspx?JobID=\\2", tolower(u[te]))
  rr <- !is.na(p) & p == "redrover"
  out[rr] <- sub("^.*redroverk12\\.com/org/([^/]+)/opening/(\\d+).*$", "https://jobs.redroverk12.com/org/\\1/opening/\\2", u[rr])
  ng <- !is.na(p) & p == "neogov"
  out[ng] <- sub("^https?://(www\\.)?((schooljobs|governmentjobs)\\.com/careers/[^/]+/jobs/\\d+).*$", "https://www.\\2", tolower(u[ng]))
  orc <- !is.na(p) & p %in% c("oracle", "paycom")
  out[orc] <- sub("[?#].*$", "", u[orc])
  jz <- !is.na(p) & p == "jazzhr"
  out[jz] <- sub("^https?://([a-z0-9-]+)\\.applytojob\\.com/apply/([A-Za-z0-9]+).*$", "https://\\1.applytojob.com/apply/\\2", u[jz])
  # AppliTrack tenants are lower-cased above; restore nothing else.
  out
}

# Month-name or numeric dates ("November 6, 2026", "11/4/2026") as Date.
parse_status_date <- function(x) {
  x <- trimws(x)
  d <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  d[is.na(d)] <- suppressWarnings(as.Date(x[is.na(d)], format = "%B %d, %Y"))
  d
}

# What a posting's page says. text: the page's visible text. Returns a
# one-row data frame: Status, Signal, Stated_Close_Date (character or NA).
parse_posting_status <- function(platform, text, checked_on = Sys.Date()) {
  res <- function(status, signal, close = NA_character_)
    data.frame(Status = status, Signal = signal, Stated_Close_Date = close, stringsAsFactors = FALSE)
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return(res("unknown", "empty page"))
  t <- gsub("[ \t\r]+", " ", text)
  checked_on <- as.Date(checked_on)
  switch(platform,
    applitrack = {
      if (grepl("This vacancy is not available", t, fixed = TRUE)) res("closed", "vacancy is not available")
      else if (grepl("Frontline", t, fixed = TRUE)) res("open", "page lists the vacancy")
      else res("unknown", "unrecognized page")
    },
    tedk12 = {
      if (grepl("no longer accepting applications", t, fixed = TRUE)) res("closed", "no longer accepting applications")
      else if (grepl("Job Description|Apply Now", t)) res("open", "job page")
      else res("unknown", "unrecognized page")
    },
    redrover = {
      m <- regmatches(t, regexpr("Job Closes:\\s*[A-Z][a-z]+ \\d{1,2}, \\d{4}", t))
      close <- if (length(m)) format(parse_status_date(sub("^Job Closes:\\s*", "", m))) else NA_character_
      if (grepl("ERROR 404", t, fixed = TRUE)) res("closed", "404")
      else if (grepl("APPLY NOW", t, fixed = TRUE)) res("open", "apply button", close)
      else res("unknown", "unrecognized page")
    },
    neogov = {
      if (grepl("not found or expired", t, fixed = TRUE)) return(res("closed", "not found or expired"))
      m <- regmatches(t, regexpr("Closing Date\\s*\\n\\s*\\n?\\s*\\d{1,2}/\\d{1,2}/\\d{4}", t))
      if (length(m)) {
        close <- parse_status_date(sub("^.*?(\\d{1,2}/\\d{1,2}/\\d{4})$", "\\1", m))
        if (!is.na(close) && close < checked_on) return(res("closed", "closing date passed", format(close)))
        return(res("open", "closing date ahead", format(close)))
      }
      # "Continuous" or "Open until filled": the page stays up either way.
      res("unknown", "no closing date")
    },
    oracle = {
      if (grepl("This job is no longer available", t, fixed = TRUE)) res("closed", "no longer available")
      else if (grepl("APPLY NOW|Posting Date", t)) res("open", "job page")
      else res("unknown", "unrecognized page")
    },
    jazzhr = {
      if (grepl("^HTTP 410|position is no longer available", t)) res("closed", "gone (410)")
      else if (grepl("Apply", t, fixed = TRUE)) res("open", "job page")
      else res("unknown", "unrecognized page")
    },
    paycom = {
      if (grepl("Couldn.t Find This Job", t)) return(res("closed", "couldn't find this job"))
      m <- regmatches(t, regexpr("CLOSING DATE:\\s*\\n?\\s*\\n?\\s*\\d{1,2}/\\d{1,2}/\\d{4}", t))
      close <- if (length(m)) parse_status_date(sub("^.*?(\\d{1,2}/\\d{1,2}/\\d{4})$", "\\1", m)) else as.Date(NA)
      if (!is.na(close) && close < checked_on) return(res("closed", "closing date passed", format(close)))
      if (grepl("CLOSING DATE|Apply", t)) res("open", "job page", if (is.na(close)) NA_character_ else format(close))
      else res("unknown", "unrecognized page")
    },
    res("unknown", "platform not checked"))
}

# The page's visible text: a plain request (html_text2) or, for platforms that
# render in the browser, the rendered body text.
fetch_posting_page_text <- function(check_url, platform, session = NULL) {
  if (platform %in% POSTING_STATUS_BROWSER_PLATFORMS) {
    if (is.null(session)) stop("a browser session is needed for ", platform)
    session$Page$navigate(check_url)
    session$Page$loadEventFired(wait_ = TRUE, timeout_ = 40)
    Sys.sleep(6)
    return(session$Runtime$evaluate("document.body.innerText")$result$value)
  }
  resp <- httr2::request(check_url) |>
    httr2::req_user_agent(POSTING_STATUS_UA) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) == 404) return("ERROR 404")
  if (httr2::resp_status(resp) == 410) return("HTTP 410 Gone")
  rvest::html_text2(rvest::read_html(httr2::resp_body_string(resp)))
}

# Checks each distinct page once. urls: any posting links (several forms of
# the same posting check one page). session_factory: called once, only if a
# browser platform is in the list. Returns one row per check URL.
check_posting_pages <- function(urls, checked_on = Sys.Date(), session_factory = NULL, pause = 1,
                                fetch = fetch_posting_page_text) {
  check <- unique(stats::na.omit(posting_status_check_url(urls)))
  empty <- data.frame(Check_URL = character(0), Platform = character(0), Status = character(0), Signal = character(0),
                      Stated_Close_Date = character(0), stringsAsFactors = FALSE)
  if (length(check) == 0) return(empty)
  platform <- posting_status_platform(check)
  session <- NULL
  on.exit(if (!is.null(session)) tryCatch(session$close(), error = function(e) NULL), add = TRUE)
  rows <- lapply(seq_along(check), function(i) {
    if (platform[i] %in% POSTING_STATUS_BROWSER_PLATFORMS && is.null(session) && !is.null(session_factory)) {
      session <<- tryCatch(session_factory(), error = function(e) NULL)
    }
    r <- tryCatch(parse_posting_status(platform[i], fetch(check[i], platform[i], session), checked_on),
                  error = function(e) data.frame(Status = "unknown", Signal = paste("check failed:", substr(conditionMessage(e), 1, 80)),
                                                 Stated_Close_Date = NA_character_, stringsAsFactors = FALSE))
    if (pause > 0) Sys.sleep(pause)
    cbind(data.frame(Check_URL = check[i], Platform = platform[i], stringsAsFactors = FALSE), r)
  })
  do.call(rbind, rows)
}

# WSBA rows minus those whose linked posting the platform says is closed.
# checks: check_posting_pages() of these rows' urls.
drop_closed_wsba_rows <- function(wsba_rows, checks) {
  if (nrow(wsba_rows) == 0 || is.null(checks) || nrow(checks) == 0) return(wsba_rows)
  closed <- checks$Check_URL[checks$Status == "closed"]
  key <- posting_status_check_url(wsba_rows$url)
  wsba_rows[is.na(key) | !key %in% closed, , drop = FALSE]
}

# Postings with a checkable page that were on their board in the previous
# run and not in this one. prev / cur: data frames with url, title, District.
dropped_postings <- function(prev, cur) {
  pk <- posting_status_check_url(prev$url)
  ck <- posting_status_check_url(cur$url)
  d <- prev[!is.na(pk) & !pk %in% ck, , drop = FALSE]
  d$Check_URL <- pk[!is.na(pk) & !pk %in% ck]
  d[!duplicated(d$Check_URL), , drop = FALSE]
}

# Merges this run's checks into the status table. checks: check_posting_pages()
# output plus Segment, District, Title, Last_Listed.
merge_posting_status <- function(previous, checks, run_date) {
  run_date <- format(as.Date(run_date))
  if (is.null(previous) || nrow(previous) == 0) {
    previous <- as.data.frame(stats::setNames(replicate(length(POSTING_STATUS_COLUMNS), character(0), simplify = FALSE),
                                              POSTING_STATUS_COLUMNS), stringsAsFactors = FALSE)
  }
  previous[] <- lapply(previous, as.character)
  if (is.null(checks) || nrow(checks) == 0) return(previous)
  checks$Checks <- "1"
  checks$First_Checked <- run_date
  checks$Last_Checked <- run_date
  checks$Status_Since <- ifelse(checks$Status == "closed", run_date, NA_character_)
  # A closing date that passed before the check dates the closing better.
  passed <- checks$Status == "closed" & !is.na(checks$Stated_Close_Date) & checks$Stated_Close_Date < run_date
  checks$Status_Since[passed] <- checks$Stated_Close_Date[passed]
  checks <- checks[, POSTING_STATUS_COLUMNS]
  checks[] <- lapply(checks, as.character)
  hit <- match(checks$Check_URL, previous$Check_URL)
  for (i in which(!is.na(hit))) {
    j <- hit[i]
    old <- previous[j, ]
    new <- checks[i, ]
    new$First_Checked <- old$First_Checked
    new$Checks <- as.character(as.integer(old$Checks) + 1L)
    if (is.na(new$Stated_Close_Date)) new$Stated_Close_Date <- old$Stated_Close_Date
    if (identical(old$Status, "closed")) new$Status_Since <- old$Status_Since
    if (is.na(new$Last_Listed) || (!is.na(old$Last_Listed) && old$Last_Listed > new$Last_Listed)) new$Last_Listed <- old$Last_Listed
    previous[j, ] <- new
  }
  rbind(previous, checks[is.na(hit), , drop = FALSE])
}

# Postings already in the table that are still open or unknown, checked fewer
# than POSTING_STATUS_MAX_CHECKS times, and not back on their board this run.
posting_status_rechecks <- function(previous, listed_check_urls) {
  if (is.null(previous) || nrow(previous) == 0) return(previous[0, , drop = FALSE])
  previous[previous$Status != "closed" & as.integer(previous$Checks) < POSTING_STATUS_MAX_CHECKS &
             !previous$Check_URL %in% listed_check_urls, , drop = FALSE]
}

# The weekly refresh: checks what dropped off since the previous archive (K-12
# and HE), re-checks open/unknown postings, adds `extra_checks` (this run's WSBA
# checks, already done), and writes status_path. Returns the table.
refresh_posting_status <- function(k12_archive_dir = "Archivek12_Data", he_archive_dir = "Archived_HE_Data",
                                   status_path = "posting_status.csv", extra_checks = NULL,
                                   run_date = Sys.Date(), session_factory = NULL, pause = 1) {
  previous <- if (file.exists(status_path)) utils::read.csv(status_path, colClasses = "character") else NULL
  last_two <- function(dir, pattern) utils::tail(sort(list.files(dir, pattern = pattern, full.names = TRUE)), 2)
  read_k12 <- function(f) { x <- utils::read.csv(f, colClasses = "character")
    data.frame(url = x$url, title = x$title, District = x$District, stringsAsFactors = FALSE) }
  read_he <- function(f) { x <- as.data.frame(readxl::read_excel(f, col_types = "text"))
    data.frame(url = x$Link, title = x$Title, District = x$Institution, stringsAsFactors = FALSE) }
  todo <- list(); listed <- character(0)
  for (seg in c("K-12", "HE")) {
    files <- if (seg == "K-12") last_two(k12_archive_dir, "^combined_\\d{4}-\\d{2}-\\d{2}\\.csv$")
             else last_two(he_archive_dir, "^hedata_\\d{4}-\\d{2}-\\d{2}\\.xlsx$")
    if (length(files) < 2) next
    rd <- if (seg == "K-12") read_k12 else read_he
    prev <- rd(files[1]); cur <- rd(files[2])
    listed <- c(listed, stats::na.omit(posting_status_check_url(cur$url)))
    d <- dropped_postings(prev, cur)
    if (nrow(d) > 0) {
      d$Segment <- seg
      d$Last_Listed <- sub("^[a-z]+_(\\d{4}-\\d{2}-\\d{2})\\..*$", "\\1", basename(files[1]))
      todo[[seg]] <- d
    }
  }
  todo <- dplyr::bind_rows(todo)
  re <- posting_status_rechecks(previous, listed)
  if (!is.null(re) && nrow(re) > 0) {
    todo <- dplyr::bind_rows(todo, data.frame(url = re$Check_URL, title = re$Title, District = re$District,
                                              Check_URL = re$Check_URL, Segment = re$Segment,
                                              Last_Listed = re$Last_Listed, stringsAsFactors = FALSE))
  }
  if (!is.null(extra_checks) && nrow(extra_checks) > 0) todo <- todo[!todo$Check_URL %in% extra_checks$Check_URL, , drop = FALSE]
  todo <- todo[!duplicated(todo$Check_URL), , drop = FALSE]
  checks <- check_posting_pages(todo$Check_URL, run_date, session_factory, pause)
  if (nrow(checks) > 0) {
    m <- match(checks$Check_URL, todo$Check_URL)
    checks$Segment <- todo$Segment[m]; checks$District <- todo$District[m]
    checks$Title <- todo$title[m]; checks$Last_Listed <- todo$Last_Listed[m]
  }
  out <- merge_posting_status(previous, dplyr::bind_rows(checks, extra_checks), run_date)
  utils::write.csv(out, status_path, row.names = FALSE)
  message(sprintf("Posting status: %d page(s) checked (%d closed, %d open, %d unknown); %d posting(s) on file.",
                  nrow(checks) + if (is.null(extra_checks)) 0 else nrow(extra_checks),
                  sum(c(checks$Status, extra_checks$Status) == "closed"), sum(c(checks$Status, extra_checks$Status) == "open"),
                  sum(c(checks$Status, extra_checks$Status) == "unknown"), nrow(out)))
  invisible(out)
}
