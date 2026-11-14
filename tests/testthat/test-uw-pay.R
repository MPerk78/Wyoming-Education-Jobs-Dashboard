# Pay from University of Wyoming requisition detail records
# (parse_uw_posting_pay()). Every fixture is a real, unaltered response from
# recruitingCEJobRequisitionDetails?expand=requisitionFlexFields, captured
# 2026-11-01.

uw_detail <- function(id) {
  paste(readLines(test_path("fixtures", paste0("uw_requisition_detail_", id, "_2026-11-01.json")),
                  warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("the Posted Salary Range field gives the range, unit and verbatim text", {
  # 260119, Painter - Structural Trades: "$38,700 - $43,008"
  pay <- parse_uw_posting_pay(uw_detail("260119"))
  expect_equal(c(pay$Pay_Min, pay$Pay_Max), c(38700, 43008))
  expect_equal(pay$Pay_Unit, "year")
  expect_equal(pay$Pay_Text, "$38,700 - $43,008")
})

test_that("hand-typed values without a $ or opening with 'Up to' are still read", {
  # 262964, Hourly Pooled - Library Aide: "12.00"
  pay <- parse_uw_posting_pay(uw_detail("262964"))
  expect_equal(pay$Pay_Min, 12)
  expect_equal(pay$Pay_Unit, "hour")
  expect_equal(pay$Pay_Text, "12.00")

  # 262663, Hourly Pooled-ARCC Student Research Assistant: "Up to $25/hour"
  # (a bare "up to $X" is otherwise skipped as a benefit ceiling).
  pay <- parse_uw_posting_pay(uw_detail("262663"))
  expect_equal(pay$Pay_Min, 25)
  expect_equal(pay$Pay_Unit, "hour")
})

test_that("no amount, or no salary field, gives NA", {
  # 262650: "Salary commensurate with experience"
  pay <- parse_uw_posting_pay(uw_detail("262650"))
  expect_true(is.na(pay$Pay_Min))
  expect_true(is.na(pay$Pay_Text))

  # 262922 (Associate/Full Professor of Marketing): no Posted Salary Range field
  pay <- parse_uw_posting_pay(uw_detail("262922"))
  expect_true(is.na(pay$Pay_Min))

  expect_true(all(is.na(parse_uw_posting_pay(NA_character_))))
})

test_that("fetch_uw_postings joins each requisition's pay, and a failed detail call blanks only that posting", {
  page <- '{"items":[{"TotalJobsCount":2,"requisitionList":[
    {"Id":"260119","Title":"Painter - Structural Trades","PostedDate":"2026-10-01","PrimaryLocation":"Laramie"},
    {"Id":"999","Title":"Gone","PostedDate":"2026-10-02","PrimaryLocation":"Laramie"}
  ]}]}'
  json_resp <- function(body) httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(body))
  detail_urls <- character(0)
  mock <- function(req) {
    if (grepl("recruitingCEJobRequisitions\\?", req$url)) return(json_resp(page))
    detail_urls <<- c(detail_urls, req$url)
    if (grepl("260119", req$url)) return(json_resp(uw_detail("260119")))
    httr2::response(500)
  }
  result <- httr2::with_mocked_responses(mock, fetch_uw_postings(
    api_base = "https://example.test/hcmRestApi/resources/latest/recruitingCEJobRequisitions",
    ui_base = "https://example.test/jobs", detail_delay = 0
  ))
  expect_equal(result$Title, c("Painter - Structural Trades", "Gone"))
  expect_equal(result$Pay_Min, c(38700, NA))
  expect_true(all(grepl("recruitingCEJobRequisitionDetails", detail_urls)))
  expect_match(utils::URLdecode(detail_urls[1]), 'finder=ById;Id="260119",siteNumber=CX_1', fixed = TRUE)
})
