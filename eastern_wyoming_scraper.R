# Eastern Wyoming Community College scraper logic, split into pure HTML
# parsing (testable against a static fixture, no browser/network needed)
# and thin RSelenium-driving wrappers around it. Previously this was one
# hardcoded-to-2-pages block with no error handling anywhere -- see
# Wy_ED_Jobs.Rmd's "Eastern" chunk for how this is actually invoked.

suppressMessages({
  library(rvest)
  library(xml2)
  library(lubridate)
})

# Parses one search-results page into a Title/Link data frame. No
# navigation, no side effects -- just HTML in, data frame out.
parse_eastern_search_page <- function(page_html) {
  soup <- if (inherits(page_html, "xml_document")) page_html else xml2::read_html(page_html)
  job_nodes <- rvest::html_nodes(soup, ".job-item.job-item-posting")
  if (length(job_nodes) == 0) {
    return(data.frame(Title = character(0), Link = character(0), stringsAsFactors = FALSE))
  }

  rows <- lapply(job_nodes, function(job) {
    title_node <- rvest::html_node(job, ".job-title a")
    title <- rvest::html_text(title_node, trim = TRUE)
    href <- rvest::html_attr(title_node, "href")
    link <- if (is.na(href)) NA_character_ else paste0("https://ewc.peopleadmin.com", href)
    data.frame(Title = title, Link = link, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Parses a job detail page for its "Open Date" field. Returns NA (not an
# error) when the row is missing entirely -- the original version used
# keep() %>% html_node("td") %>% html_text(), which returns character(0)
# (not NA) when no "Open Date" row exists, and character(0) fed into
# `if (!is.na(x) && ...)` throws "argument is of length zero", crashing the
# whole per-job lapply with nothing to catch it.
parse_eastern_open_date <- function(job_page_html) {
  soup <- if (inherits(job_page_html, "xml_document")) job_page_html else xml2::read_html(job_page_html)
  rows <- rvest::html_nodes(soup, "tr")

  is_open_date_row <- vapply(rows, function(r) {
    th_text <- rvest::html_text(rvest::html_node(r, "th"), trim = TRUE)
    isTRUE(th_text == "Open Date")
  }, logical(1))

  if (!any(is_open_date_row)) return(as.Date(NA))

  open_date_text <- rvest::html_text(rvest::html_node(rows[is_open_date_row][[1]], "td"), trim = TRUE)
  if (is.na(open_date_text) || open_date_text == "") return(as.Date(NA))
  lubridate::mdy(open_date_text)
}

# Fetches and parses one search-results page via an active RSelenium
# session.
fetch_eastern_search_page <- function(remDr, page_url, sleep_seconds = 10) {
  remDr$navigate(page_url)
  Sys.sleep(sleep_seconds)
  parse_eastern_search_page(remDr$getPageSource()[[1]])
}

# Fetches and parses one job's detail page for its Open Date.
fetch_eastern_open_date <- function(remDr, link, sleep_seconds = 3) {
  remDr$navigate(link)
  Sys.sleep(sleep_seconds)
  parse_eastern_open_date(remDr$getPageSource()[[1]])
}

# Paginates until a page returns zero postings (matching the LCCC scraper's
# pattern), instead of the previous hardcoded "scrape exactly 2 pages."
# A page-level *error* is not treated the same as a genuinely *empty* page:
# an empty page means we've reached the real end of listings and pagination
# stops; an error (timeout, transient network blip) just loses that one
# page's jobs and moves on to the next page number, so a single bad page
# can't be mistaken for "end of listings" and truncate everything after it.
# max_pages is a hard backstop against looping forever if the site is
# down entirely and every page errors.
scrape_eastern_all_pages <- function(remDr, max_pages = 50) {
  all_jobs <- list()
  page <- 1
  repeat {
    if (page > max_pages) {
      message("Eastern Wyoming: hit max_pages (", max_pages, "), stopping pagination")
      break
    }
    page_url <- paste0("https://ewc.peopleadmin.com/postings/search?page=", page)
    outcome <- tryCatch(
      list(df = fetch_eastern_search_page(remDr, page_url), error = NULL),
      error = function(e) list(df = NULL, error = conditionMessage(e))
    )

    if (!is.null(outcome$error)) {
      message("Eastern Wyoming: error scraping page ", page, ": ", outcome$error)
      page <- page + 1
      Sys.sleep(1)
      next
    }

    if (nrow(outcome$df) == 0) break  # genuine end of listings
    all_jobs[[length(all_jobs) + 1]] <- outcome$df
    page <- page + 1
    Sys.sleep(1)  # be nice to the server
  }
  if (length(all_jobs) == 0) {
    return(data.frame(Title = character(0), Link = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, all_jobs)
}

# For each scraped job, fetches its Open Date from its own detail page.
# tryCatch is per-job: one dead/changed link falls back to NA instead of
# aborting every other job's lookup.
attach_eastern_open_dates <- function(remDr, jobs) {
  if (nrow(jobs) == 0) {
    jobs$Posted_Date <- as.Date(character(0))
    return(jobs)
  }
  jobs$Posted_Date <- as.Date(vapply(seq_len(nrow(jobs)), function(i) {
    date <- tryCatch(
      as.character(fetch_eastern_open_date(remDr, jobs$Link[i])),
      error = function(e) {
        message("Eastern Wyoming: error fetching open date for '", jobs$Title[i], "': ", conditionMessage(e))
        NA_character_
      }
    )
    if (is.null(date)) NA_character_ else date
  }, character(1)))
  jobs
}

# Full end-to-end scrape (pagination + per-job open dates) via an active
# RSelenium session. Intended to be passed as the scrape_fn to safe_scrape()
# in Wy_ED_Jobs.Rmd, which handles session start/stop and error logging.
scrape_eastern_wyoming <- function(remDr) {
  jobs <- scrape_eastern_all_pages(remDr)
  attach_eastern_open_dates(remDr, jobs)
}
