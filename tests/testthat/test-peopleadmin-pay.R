# Pay from PeopleAdmin posting pages (parse_peopleadmin_posting_pay()).
# Identical in the MT and WY repos. Every fixture is a real posting page
# captured 2026-11-01 (only the per-session CSRF token is redacted).

posting_page <- function(name) {
  paste(readLines(test_path("fixtures", paste0("peopleadmin_posting_", name, "_2026-11-01.html")),
                  warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("MSU's Salary field gives the posted range and unit, including tenth-of-a-cent rates", {
  # jobs.montana.edu/postings/53470: "Salary range of $17.820 to $22.00 hourly"
  pay <- parse_peopleadmin_posting_pay(posting_page("msu_53470"))
  expect_equal(c(pay$Pay_Min, pay$Pay_Max), c(17.82, 22))
  expect_equal(pay$Pay_Unit, "hour")
  expect_match(pay$Pay_Text, "^Salary range of \\$17.820 to \\$22.00 hourly")

  # 52959: "$23.684/hr - $29.604/hr" -- the unit repeated after the low end
  # used to cut the range to its low end.
  pay <- parse_peopleadmin_posting_pay(posting_page("msu_52959"))
  expect_equal(c(pay$Pay_Min, pay$Pay_Max), c(23.684, 29.604))
  expect_equal(pay$Pay_Unit, "hour")
})

test_that("MSU per-credit and long-paragraph salary fields", {
  # 51221: "equivalent to $2,200 per 1 semester credit"
  pay <- parse_peopleadmin_posting_pay(posting_page("msu_51221"))
  expect_equal(pay$Pay_Min, 2200)
  expect_equal(pay$Pay_Unit, "credit hour")

  # 53243 (MSU Extension): a long paragraph keeps only the snippet around the amount.
  pay <- parse_peopleadmin_posting_pay(posting_page("msu_53243"))
  expect_equal(pay$Pay_Min, 51660)
  expect_equal(pay$Pay_Unit, "year")
  expect_lte(nchar(pay$Pay_Text), 200)
  expect_match(pay$Pay_Text, "$51,660", fixed = TRUE)
})

test_that("a pay field with no amount stays NA rather than reading the rest of the page", {
  # 53507: "Salary commensurate with experience, education, and qualifications"
  pay <- parse_peopleadmin_posting_pay(posting_page("msu_53507"))
  expect_true(is.na(pay$Pay_Min))
  expect_true(is.na(pay$Pay_Text))

  # Northwest 2703: "Competitive; Candidates to provide salary requirements"
  pay <- parse_peopleadmin_posting_pay(posting_page("northwest_2703"))
  expect_true(is.na(pay$Pay_Min))
})

test_that("MSU Billings: amount from Salary, unit stated in the text", {
  # employment.msubillings.edu/postings/7833: Salary "$ 14.264 per hour", Compensation "Hourly"
  pay <- parse_peopleadmin_posting_pay(posting_page("msub_7833"))
  expect_equal(pay$Pay_Min, 14.264)
  expect_equal(pay$Pay_Unit, "hour")
})

test_that("Western and Sheridan: unitless figures in a pay field are read as hourly", {
  # wwcwy.peopleadmin.com/postings/5719: Estimated Salary "$17.40"
  pay <- parse_peopleadmin_posting_pay(posting_page("western_5719"))
  expect_equal(pay$Pay_Min, 17.4)
  expect_equal(pay$Pay_Unit, "hour")
  expect_equal(pay$Pay_Text, "$17.40")

  # jobs.sheridan.edu/postings/4789: Hiring Range "$18.44 - $23.06" wins over
  # the Full Salary Range below it ("$18.44 - $27.67").
  pay <- parse_peopleadmin_posting_pay(posting_page("sheridan_4789"))
  expect_equal(c(pay$Pay_Min, pay$Pay_Max), c(18.44, 23.06))
  expect_equal(pay$Pay_Unit, "hour")

  # 4781: empty Hiring Range, Hourly Rate (if applicable) "$12.00"
  pay <- parse_peopleadmin_posting_pay(posting_page("sheridan_4781"))
  expect_equal(pay$Pay_Min, 12)
  expect_equal(pay$Pay_Unit, "hour")
})

test_that("Eastern has no pay field, so pay is read from the job description", {
  # ewc.peopleadmin.com/postings/1981: "College pays up to $500 for relocation
  # expenses" comes first and must not be read as pay.
  pay <- parse_peopleadmin_posting_pay(posting_page("eastern_1981"))
  expect_equal(c(pay$Pay_Min, pay$Pay_Max), c(60598, 66889))
  expect_equal(pay$Pay_Unit, "year")
  expect_match(pay$Pay_Text, "Salary Hire Range")
})

test_that("a failed page gives one all-NA pay row", {
  pay <- parse_peopleadmin_posting_pay(NA_character_)
  expect_equal(nrow(pay), 1)
  expect_true(all(is.na(pay)))
})

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
