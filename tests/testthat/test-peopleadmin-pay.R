# Pay from PeopleAdmin posting pages (parse_peopleadmin_posting_pay()).
# Identical in the MT and WY repos. Every fixture is a real posting page
# captured 2026-11-01 (only the per-session CSRF token is redacted).

posting_page <- function(name) {
  paste(readLines(test_path("fixtures", paste0("peopleadmin_posting_", name, "_2026-11-01.html")),
                  warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("fetch_peopleadmin_atom joins each posting's page pay, and a failed page blanks only that posting", {
  feed <- '<?xml version="1.0" encoding="UTF-8"?>
<feed xml:lang="en-US" xmlns="http://www.w3.org/2005/Atom">
  <entry><published>2026-10-30T09:00:00-06:00</published>
    <link rel="alternate" type="text/html" href="https://jobs.sheridan.edu/postings/4789"/><title>Financial Aid Advisor</title></entry>
  <entry><published>2026-10-30T09:00:00-06:00</published>
    <link rel="alternate" type="text/html" href="https://jobs.sheridan.edu/postings/1"/><title>Gone</title></entry>
</feed>'
  html_resp <- function(body) httr2::response(200, headers = list("Content-Type" = "text/html"), body = charToRaw(body))
  httr2::local_mocked_responses(function(req) {
    if (grepl("all_jobs$", req$url)) return(httr2::response(200, headers = list("Content-Type" = "application/atom+xml"), body = charToRaw(feed)))
    if (grepl("/4789$", req$url)) return(html_resp(posting_page("sheridan_4789")))
    httr2::response(404)
  })
  result <- fetch_peopleadmin_atom("https://jobs.sheridan.edu/postings/all_jobs", "Sheridan College Campus", detail_delay = 0)
  expect_equal(result$Title, c("Financial Aid Advisor", "Gone"))
  expect_equal(result$Pay_Min, c(18.44, NA))
  expect_equal(result$Pay_Unit, c("hour", NA))

  result <- fetch_peopleadmin_atom("https://jobs.sheridan.edu/postings/all_jobs", "Sheridan College Campus", fetch_pay = FALSE)
  expect_true(all(is.na(result$Pay_Min)))
})
