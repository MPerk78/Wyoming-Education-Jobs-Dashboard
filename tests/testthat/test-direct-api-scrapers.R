test_that("parse_uw_requisitions maps Oracle fields to the standard schema", {
  reqs <- data.frame(
    Id = c("111", "222"),
    Title = c("Business Manager", "Nursing Instructor"),
    PostedDate = c("2026-07-31", "2026-07-15"),
    PrimaryLocation = c("United States", "Laramie, WY, United States"),
    stringsAsFactors = FALSE
  )

  result <- parse_uw_requisitions(reqs, "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1")

  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
  expect_equal(result$Title, c("Business Manager", "Nursing Instructor"))
  expect_equal(result$Link, c(
    "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/111",
    "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/222"
  ))
})

test_that("fetch_uw_postings paginates through finder arguments and preserves each requisition", {
  page1 <- '{"items":[{"TotalJobsCount":3,"requisitionList":[
    {"Id":"101","Title":"First Job","PostedDate":"2026-08-01","PrimaryLocation":"Laramie"},
    {"Id":"102","Title":"Second Job","PostedDate":"2026-08-02","PrimaryLocation":"Laramie"}
  ]}]}'
  page2 <- '{"items":[{"TotalJobsCount":3,"requisitionList":[
    {"Id":"103","Title":"Third Job","PostedDate":"2026-08-03","PrimaryLocation":"Laramie"}
  ]}]}'
  response_bodies <- list(page1, page2)
  request_urls <- character(0)
  request_number <- 0L

  mock <- function(req) {
    request_urls <<- c(request_urls, req$url)
    request_number <<- request_number + 1L
    httr2::response(
      200,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(response_bodies[[request_number]])
    )
  }

  result <- httr2::with_mocked_responses(
    mock,
    fetch_uw_postings(
      api_base = "https://example.test/requisitions",
      ui_base = "https://example.test/jobs",
      page_size = 2,
      fetch_pay = FALSE
    )
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$Link, c(
    "https://example.test/jobs/job/101",
    "https://example.test/jobs/job/102",
    "https://example.test/jobs/job/103"
  ))
  expect_equal(length(request_urls), 2)
  finder_values <- utils::URLdecode(sub("^.*[?&]finder=([^&]+).*$", "\\1", request_urls))
  expect_equal(finder_values, c(
    "findReqs;siteNumber=CX_1,limit=2,offset=0",
    "findReqs;siteNumber=CX_1,limit=2,offset=2"
  ))
  expect_false(any(grepl("[?&](limit|offset)=", request_urls)))
})

test_that("fetch_uw_postings rejects repeated Oracle pages and overlapping IDs", {
  first_page <- '{"items":[{"TotalJobsCount":4,"requisitionList":[
    {"Id":"101","Title":"First Job","PostedDate":"2026-08-01","PrimaryLocation":"Laramie"},
    {"Id":"102","Title":"Second Job","PostedDate":"2026-08-02","PrimaryLocation":"Laramie"}
  ]}]}'
  overlapping_page <- '{"items":[{"TotalJobsCount":4,"requisitionList":[
    {"Id":"102","Title":"Second Job","PostedDate":"2026-08-02","PrimaryLocation":"Laramie"},
    {"Id":"103","Title":"Third Job","PostedDate":"2026-08-03","PrimaryLocation":"Laramie"}
  ]}]}'

  response_sequence_mock <- function(bodies) {
    request_number <- 0L
    function(req) {
      request_number <<- request_number + 1L
      httr2::response(
        200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(bodies[[request_number]])
      )
    }
  }

  expect_error(
    httr2::with_mocked_responses(
      response_sequence_mock(list(first_page, first_page)),
      fetch_uw_postings(api_base = "https://example.test/requisitions", page_size = 2, fetch_pay = FALSE)
    ),
    "repeated a page"
  )
  expect_error(
    httr2::with_mocked_responses(
      response_sequence_mock(list(first_page, overlapping_page)),
      fetch_uw_postings(api_base = "https://example.test/requisitions", page_size = 2, fetch_pay = FALSE)
    ),
    "repeated requisition Id"
  )
})

test_that("SchoolSpring source IDs and deep links survive K-12 aggregation", {
  postings <- data.frame(
    Title = c("Elementary Teacher", "Elementary Teacher"),
    Location = c("Sundance, Wyoming", "Sundance, Wyoming"),
    Posted_Date = c("2026-07-22", "2026-07-22"),
    Posting_ID = c(
      "schoolspring:crook1.schoolspring.com:5846934",
      "schoolspring:crook1.schoolspring.com:5324570"
    ),
    Link = c(
      "https://crook1.schoolspring.com/jobs/5846934",
      "https://crook1.schoolspring.com/jobs/5324570"
    ),
    District = "Crook County School District 1",
    stringsAsFactors = FALSE
  )

  normalized <- normalize_id_backed_k12_postings(postings)
  expect_equal(normalized$url, postings$Link)
  expect_equal(normalized$posting_id, postings$Posting_ID)

  counted <- normalized %>%
    dplyr::mutate(
      Archive_Date = as.Date("2026-08-11"),
      Broad_Category = "Elementary"
    )
  summary <- summarize_k12_posting_counts(counted, id_col = "posting_id")
  expect_equal(summary$sum, 2)
})

# parse_redrover_json()'s field and empty-board tests moved to edjobs with it (0.14.0); the pay test stays here
# because it also covers this state's normalize_id_backed_k12_postings().
test_that("parse_redrover_json reads the posting's own pay and keeps hidden pay as NA", {
  # Shapes from the live Johnson County SD1 response (2026-10-30): a pay
  # range, a single figure (min == max), a daily rate, a STIPEND with no
  # amount, and an UNKNOWN type the board itself doesn't display.
  fixture <- '{"data":{"jobSeekerSiteUnauthenticated":{"jobPostingSearch":{"results":[
    {"id":"142219","name":"Paraprofessional Substitute","location":{"name":"District"},"activePublicOnDateUtc":"2026-07-01T00:00:00Z","payTypeId":"HOURLY","minPay":17.66,"maxPay":18.1},
    {"id":"131082","name":"Substitute Buffalo Bus Route Driver","location":{"name":"District"},"activePublicOnDateUtc":"2026-07-01T00:00:00Z","payTypeId":"HOURLY","minPay":20.48,"maxPay":20.48},
    {"id":"132190","name":"Substitute Teacher","location":{"name":"District"},"activePublicOnDateUtc":"2026-07-01T00:00:00Z","payTypeId":"DAILY","minPay":68.0,"maxPay":136.0},
    {"id":"209197","name":"Head Indoor Track Coach","location":{"name":"Buffalo High School"},"activePublicOnDateUtc":"2026-07-01T00:00:00Z","payTypeId":"STIPEND","minPay":null,"maxPay":null},
    {"id":"1","name":"Mystery Role","location":{"name":"District"},"activePublicOnDateUtc":"2026-07-01T00:00:00Z","payTypeId":"UNKNOWN","minPay":10,"maxPay":10}
  ]}}}}'

  result <- parse_redrover_json(fixture, "jcsd1")

  expect_equal(result$Pay_Min, c(17.66, 20.48, 68, NA, NA))
  expect_equal(result$Pay_Max, c(18.1, 20.48, 136, NA, NA))
  expect_equal(result$Pay_Unit, c("hour", "hour", "day", NA, NA))
  expect_equal(result$Pay_Text, c("$17.66 - $18.10 per hour", "$20.48 per hour",
                                  "$68.00 - $136.00 per day", NA, NA))

  result$District <- "Johnson County School District #1"
  normalized <- normalize_id_backed_k12_postings(result)
  expect_equal(normalized$Pay_Unit, result$Pay_Unit)
  expect_equal(normalized$Pay_Text, result$Pay_Text)
})

test_that("normalize_id_backed_k12_postings fills Pay_* with NA for a source that states no pay", {
  postings <- data.frame(Title = "Teacher", Location = "Sundance", Posted_Date = "2026-10-01",
                         Posting_ID = "schoolspring:x:1", Link = "https://x/1", District = "Crook",
                         stringsAsFactors = FALSE)
  normalized <- normalize_id_backed_k12_postings(postings)
  expect_true(is.na(normalized$Pay_Min) && is.na(normalized$Pay_Unit))
  expect_type(normalized$Pay_Min, "double")
})

test_that("parse_paylocity_jobs extracts the embedded window.pageData JSON from a real fixture", {
  # Real fixture: Laramie Montessori Charter School's Paylocity recruiting
  # page. Confirmed the full job list is embedded as JSON directly in the
  # raw HTML (window.pageData = {...}) rather than fetched via a separate
  # API call -- a plain httr2 request reproduces this exactly, no browser
  # needed despite the page being a React app.
  html <- paste(readLines(test_path("fixtures", "paylocity_laramie_montessori.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  result <- parse_paylocity_jobs(html)

  expect_equal(nrow(result), 1)
  expect_equal(result$Title, "School Custodian-evening")
  expect_equal(result$Location, "Laramie Montessori Charter School")
  expect_equal(result$Posted_Date, "2026-06-24")
  expect_equal(result$Link, "https://recruiting.paylocity.com/recruiting/jobs/Details/4263561")
})

test_that("parse_paylocity_jobs returns zero rows (not an error) when window.pageData is absent", {
  result <- parse_paylocity_jobs("<html><body>Not a Paylocity page</body></html>")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

test_that("AppliTrack postings carry their job ID, per-posting link and closing date", {
  # Same real Teton fixture: every block is "p<JobID>_".
  path <- test_path("fixtures", "applitrack_output_wy_teton_single_position_type.bin")
  js <- iconv(readChar(path, file.size(path), useBytes = TRUE), "WINDOWS-1252", "UTF-8")
  out <- parse_applitrack_output(js)
  expect_true(all(grepl("^[0-9]+$", out$job_id)))
  expect_equal(anyDuplicated(out$job_id), 0)
  expect_true("job_id" %in% names(parse_applitrack_output("")))
  expect_equal(applitrack_posting_url("tcsd", "836"),
               "https://www.applitrack.com/tcsd/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=836")
  expect_true(is.na(applitrack_posting_url("tcsd", NA_character_)))
  # The pipeline's source_id "applitrack:<tenant>:<job id>" is the posting_id
  # as given, so two postings with the same title stay two postings.
  ids <- build_k12_posting_id(source_id = c("applitrack:tcsd:1", "applitrack:tcsd:2"),
                              title = "Paraprofessional", location = "District Wide",
                              date_posted = "2026-10-01", url = "https://www.applitrack.com/tcsd/onlineapp/default.aspx?all=1",
                              district = "Teton County School District 1")
  expect_equal(ids, c("applitrack:tcsd:1", "applitrack:tcsd:2"))
})

# SchoolSpring detail records: real ones captured 2026-11-03 from
# api.schoolspring.com/api/Jobs/<jobId> (Montana postings; the parser is
# identical in the MT and WY repos).
ss_job_fixture <- function(id) {
  paste(readLines(test_path("fixtures", paste0("schoolspring_job_", id, ".json")), warn = FALSE), collapse = "\n")
}

test_that("add_schoolspring_details joins detail fields by job ID and leaves failed ones NA", {
  postings <- data.frame(Title = c("Coach", "Teacher"), Location = "Ronan, Montana",
                         Posted_Date = "2026-06-01",
                         Link = c("https://ronan.schoolspring.com/jobs/5732320",
                                  "https://ronan.schoolspring.com/jobs/1"),
                         stringsAsFactors = FALSE)
  httr2::local_mocked_responses(function(req) {
    if (grepl("/5732320$", req$url)) {
      httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(ss_job_fixture(5732320)))
    } else {
      httr2::response(404)
    }
  })
  r <- add_schoolspring_details(postings, delay = 0)
  expect_equal(nrow(r), 2)
  expect_equal(r$Close_Date, c("2026-11-13", NA))
  expect_equal(r$Pay_Unit, c("stipend", NA))
  expect_equal(r$Title, postings$Title)
})
