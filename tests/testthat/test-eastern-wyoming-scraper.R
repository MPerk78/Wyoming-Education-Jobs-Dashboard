# Mock Sys.sleep to no-op so test pagination does not sleep 10s per page:
testthat::local_mocked_bindings(Sys.sleep = function(...) invisible(NULL), .package = "base")

# These test the pure HTML-parsing functions against static fixtures --
# real page structure as observed in the original scraper's selectors, not
# a live fetch. They exercise parsing logic without needing RSelenium,
# Firefox, or network access, which this project's CI/dev environment can't
# assume are available.

search_page_html <- "
<html><body>
  <div class='job-item job-item-posting'>
    <div class='job-title'><a href='/postings/12345'>Adjunct Instructor, Biology</a></div>
  </div>
  <div class='job-item job-item-posting'>
    <div class='job-title'><a href='/postings/12346'>Custodian</a></div>
  </div>
</body></html>
"

empty_search_page_html <- "
<html><body>
  <div class='no-results'>No postings found</div>
</body></html>
"

job_page_with_open_date_html <- "
<html><body>
<table>
  <tr><th>Position Number</th><td>1234</td></tr>
  <tr><th>Open Date</th><td>08/15/2025</td></tr>
  <tr><th>Close Date</th><td>09/15/2025</td></tr>
</table>
</body></html>
"

job_page_missing_open_date_html <- "
<html><body>
<table>
  <tr><th>Position Number</th><td>1234</td></tr>
  <tr><th>Close Date</th><td>09/15/2025</td></tr>
</table>
</body></html>
"

job_page_blank_open_date_html <- "
<html><body>
<table>
  <tr><th>Open Date</th><td></td></tr>
</table>
</body></html>
"

test_that("parse_eastern_search_page extracts title and full link for each posting", {
  result <- parse_eastern_search_page(search_page_html)

  expect_equal(nrow(result), 2)
  expect_equal(result$Title, c("Adjunct Instructor, Biology", "Custodian"))
  expect_equal(
    result$Link,
    c("https://ewc.peopleadmin.com/postings/12345", "https://ewc.peopleadmin.com/postings/12346")
  )
})

test_that("parse_eastern_search_page returns zero rows (not an error) for an empty results page", {
  result <- parse_eastern_search_page(empty_search_page_html)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Link"))
})

test_that("parse_eastern_open_date parses a present Open Date row", {
  expect_equal(parse_eastern_open_date(job_page_with_open_date_html), as.Date("2025-08-15"))
})

test_that("parse_eastern_open_date returns NA, not an error, when Open Date row is entirely absent", {
  # Regression: the original code (keep() %>% html_node('td') %>%
  # html_text()) produced character(0) here, and `if (!is.na(character(0)))`
  # throws "argument is of length zero" -- this exact page shape would have
  # crashed the whole per-job lapply with no tryCatch to catch it.
  expect_no_error(result <- parse_eastern_open_date(job_page_missing_open_date_html))
  expect_true(is.na(result))
  expect_s3_class(result, "Date")
})

test_that("parse_eastern_open_date returns NA for a blank Open Date value", {
  expect_no_error(result <- parse_eastern_open_date(job_page_blank_open_date_html))
  expect_true(is.na(result))
})

test_that("scrape_eastern_all_pages paginates until an empty page, not a hardcoded page count", {
  # Regression: the original scraper was hardcoded to "scrape first 2
  # pages" (do.call(rbind, lapply(1:2, ...))) with no dynamic termination.
  # Simulate a fake RSelenium client so this can be verified without a real
  # browser: page 1 and 2 return jobs, page 3 is empty, and pagination must
  # stop there on its own rather than needing a hardcoded page count.
  call_log <- character(0)
  fake_remDr <- list(
    navigate = function(url) call_log <<- c(call_log, url),
    getPageSource = function() {
      page_num <- as.integer(sub(".*page=(\\d+)$", "\\1", tail(call_log, 1)))
      html <- if (page_num <= 2) search_page_html else empty_search_page_html
      list(html)
    }
  )

  result <- scrape_eastern_all_pages(fake_remDr)

  expect_equal(length(call_log), 3)  # stopped after the empty 3rd page, not forced to a fixed count
  expect_equal(nrow(result), 4)      # 2 postings/page x 2 non-empty pages
})

test_that("scrape_eastern_all_pages continues past a page that errors instead of aborting entirely", {
  call_log <- character(0)
  fake_remDr <- list(
    navigate = function(url) call_log <<- c(call_log, url),
    getPageSource = function() {
      page_num <- as.integer(sub(".*page=(\\d+)$", "\\1", tail(call_log, 1)))
      if (page_num == 1) stop("simulated network error")
      if (page_num == 2) return(list(search_page_html))
      list(empty_search_page_html)
    }
  )

  expect_no_error(result <- scrape_eastern_all_pages(fake_remDr))
  # Page 1 errored (contributes nothing), page 2 has 2 postings, page 3 is empty and stops pagination.
  expect_equal(nrow(result), 2)
})
