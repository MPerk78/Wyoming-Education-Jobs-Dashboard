# Open and close dates from PeopleAdmin posting pages
# (parse_peopleadmin_posting_dates()). Identical in the MT and WY repos. Same
# real posting pages as test-peopleadmin-pay.R (captured 2026-11-01).

posting_page <- function(name) {
  paste(readLines(test_path("fixtures", paste0("peopleadmin_posting_", name, "_2026-11-01.html")),
                  warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("fetch_peopleadmin_atom takes the page's Open Date over the feed's <published>", {
  feed <- '<?xml version="1.0" encoding="UTF-8"?>
<feed xml:lang="en-US" xmlns="http://www.w3.org/2005/Atom">
  <entry><published>2026-10-30T09:00:00-06:00</published>
    <link rel="alternate" type="text/html" href="https://employment.msubillings.edu/postings/7833"/><title>Billings posting</title></entry>
  <entry><published>2026-10-30T09:00:00-06:00</published>
    <link rel="alternate" type="text/html" href="https://jobs.montana.edu/postings/53470"/><title>MSU posting</title></entry>
  <entry><published>2026-10-30T09:00:00-06:00</published>
    <link rel="alternate" type="text/html" href="https://jobs.montana.edu/postings/1"/><title>Gone</title></entry>
</feed>'
  html_resp <- function(body) httr2::response(200, headers = list("Content-Type" = "text/html"), body = charToRaw(body))
  httr2::local_mocked_responses(function(req) {
    if (grepl("search.atom$", req$url)) return(httr2::response(200, headers = list("Content-Type" = "application/atom+xml"), body = charToRaw(feed)))
    if (grepl("/7833$", req$url)) return(html_resp(posting_page("msub_7833")))
    if (grepl("/53470$", req$url)) return(html_resp(posting_page("msu_53470")))
    httr2::response(404)
  })
  r <- fetch_peopleadmin_atom("https://x.edu/postings/search.atom", "X", detail_delay = 0)
  expect_equal(as.character(r$Posted_Date), c("2026-10-23", "2026-10-30", "2026-10-30"))
  expect_equal(r$Close_Date, c("2026-11-06", NA, NA))

  r <- fetch_peopleadmin_atom("https://x.edu/postings/search.atom", "X", fetch_pay = FALSE)
  expect_true("Close_Date" %in% names(r) && all(is.na(r$Close_Date)))
})
