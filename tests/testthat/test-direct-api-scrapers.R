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

test_that("parse_peopleadmin_atom extracts title, date, and link from a fixture feed", {
  fixture <- '<?xml version="1.0" encoding="UTF-8"?>
<feed xml:lang="en-US" xmlns="http://www.w3.org/2005/Atom">
  <title>Test College: All Jobs</title>
  <entry>
    <id>https://test.peopleadmin.com/postings/1</id>
    <published>2026-03-19T08:16:49-06:00</published>
    <updated>2026-03-31T12:41:47-06:00</updated>
    <link rel="alternate" type="text/html" href="https://test.peopleadmin.com/postings/1"/>
    <title>Math Instructor</title>
    <content>&lt;div&gt;details&lt;/div&gt;</content>
  </entry>
  <entry>
    <id>https://test.peopleadmin.com/postings/2</id>
    <published>2026-05-01T08:00:00-06:00</published>
    <updated>2026-05-01T08:00:00-06:00</updated>
    <link rel="alternate" type="text/html" href="https://test.peopleadmin.com/postings/2"/>
    <title>Adjunct Biology Instructor</title>
    <content>&lt;div&gt;more details&lt;/div&gt;</content>
  </entry>
</feed>'

  result <- parse_peopleadmin_atom(fixture, "Test College Campus")

  expect_equal(nrow(result), 2)
  expect_equal(result$Title, c("Math Instructor", "Adjunct Biology Instructor"))
  expect_equal(result$Location, rep("Test College Campus", 2))
  expect_equal(result$Posted_Date, c("2026-03-19", "2026-05-01"))
  expect_equal(result$Link, c("https://test.peopleadmin.com/postings/1", "https://test.peopleadmin.com/postings/2"))
})

test_that("parse_peopleadmin_atom returns zero rows (not an error) for an empty feed", {
  fixture <- '<?xml version="1.0" encoding="UTF-8"?>
<feed xml:lang="en-US" xmlns="http://www.w3.org/2005/Atom">
  <title>Test College: All Jobs</title>
</feed>'

  result <- parse_peopleadmin_atom(fixture, "Test College Campus")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

test_that("parse_neogov_html dedupes the card/table double-render and extracts real fields", {
  # Real fixture captured from Central Wyoming College's actual AJAX
  # response (https://www.schooljobs.com/careers/home/index?agency=cwc),
  # requested via the X-Requested-With header discovered by tracing the
  # page's own network traffic with chromote. This response renders every
  # posting TWICE -- once in a card <ul> (with location/date metadata) and
  # again in a plain <table> (title only) -- both present unconditionally,
  # not toggled by viewport/CSS. The real regression this guards: without
  # scoping to ul.job-listing-container, this fixture parses as 20 rows
  # (10 real jobs doubled) with every Location/Posted_Date as NA, since the
  # table version has no metadata to match positionally.
  fixture <- paste(readLines(test_path("fixtures", "neogov_cwc.html"), warn = FALSE), collapse = "\n")

  result <- parse_neogov_html(fixture, "https://www.schooljobs.com")

  expect_equal(nrow(result), 10)
  expect_equal(length(unique(result$Title)), 10)
  expect_true(all(!is.na(result$Location)))
  expect_true(all(!is.na(result$Posted_Date)))
  expect_true("Adjunct" %in% result$Title)
  expect_true(any(grepl("^https://www.schooljobs.com/careers/cwc/jobs/", result$Link)))
})

test_that("parse_neogov_html returns zero rows (not an error) when the fragment has no job-listing-container", {
  fixture <- '<div class="jobs-not-found-container"><h2>No jobs at this time.</h2></div>'
  result <- parse_neogov_html(fixture, "https://www.schooljobs.com")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link", POSTED_PAY_COLUMNS))
})

test_that("fetch_neogov_postings follows pagination instead of only fetching page 1", {
  # Regression: fetch_neogov_postings previously fetched only the
  # unparameterized first page and never followed NEOGOV's own pagination
  # (10 postings per page), silently truncating any agency with more than
  # 10 open postings. Confirmed live: Central Wyoming College (14 real
  # postings) and Gillette College (16) were both being cut to 10. This
  # mocks a 3-request sequence -- a full page 1 (the real 10-row fixture),
  # a partial page 2 (1 row), and an empty page 3 -- and checks the loop
  # both collects every page and stops once a page comes back empty.
  page1 <- paste(readLines(test_path("fixtures", "neogov_cwc.html"), warn = FALSE), collapse = "\n")
  page2 <- '<ul class="unstyled search-results-listing-container job-listing-container ">
    <li class="list-item" data-job-id="9999999">
      <h3 class="job-item-link-container">
        <a class="item-details-link" href="/careers/cwc/jobs/9999999/extra-adjunct">Extra Adjunct</a>
      </h3>
      <ul class="list-meta"><li>Riverton</li></ul>
      <div class="list-published"><span class="list-entry-starts"><span>Posted 1 day ago</span></span></div>
    </li>
  </ul>'
  page3 <- '<div class="jobs-not-found-container"><h2 class="not-found-text">No jobs at this time.</h2></div>'

  httr2::local_mocked_responses(list(
    httr2::response(200, body = charToRaw(page1)),
    httr2::response(200, body = charToRaw(page2)),
    httr2::response(200, body = charToRaw(page3))
  ))

  result <- fetch_neogov_postings("https://www.schooljobs.com", "cwc")

  expect_equal(nrow(result), 11)
  expect_true("Extra Adjunct" %in% result$Title)
  expect_true(any(grepl("^https://www.schooljobs.com/careers/cwc/jobs/9999999", result$Link)))
})

test_that("parse_schoolspring_json extracts fields and builds a per-job link", {
  fixture <- '{"success":true,"message":"","validationErrors":[],"value":{"page":1,"size":25,"jobsList":[
    {"jobId":5846934,"employer":"Sundance Secondary","title":"High Needs SPED Paraprofessional","location":"Sundance, Wyoming","displayDate":"2026-07-22T06:00:00"},
    {"jobId":5324570,"employer":"Sundance Secondary","title":"Substitute Custodian","location":"Sundance, Wyoming","displayDate":"2026-06-05T06:00:00"},
    {"jobId":1,"employer":"SchoolSpring","title":"Sample Certified Position","location":"Example, Wyoming","displayDate":"2010-02-16T06:00:00"}
  ]}}'

  result <- parse_schoolspring_json(fixture, "crook1.schoolspring.com")

  expect_equal(nrow(result), 2)
  expect_equal(result$Title, c("High Needs SPED Paraprofessional", "Substitute Custodian"))
  expect_equal(result$Posted_Date, c("2026-07-22", "2026-06-05"))
  expect_equal(result$Posting_ID, c(
    "schoolspring:crook1.schoolspring.com:5846934",
    "schoolspring:crook1.schoolspring.com:5324570"
  ))
  expect_equal(result$Link, c(
    "https://crook1.schoolspring.com/jobs/5846934",
    "https://crook1.schoolspring.com/jobs/5324570"
  ))
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
  summary <- summarize_k12_posting_counts(counted)
  expect_equal(summary$sum, 2)
})

test_that("parse_schoolspring_json returns zero rows (not an error) for an empty jobsList", {
  fixture <- '{"success":true,"message":"","validationErrors":[],"value":{"page":1,"size":25,"jobsList":[]}}'
  result <- parse_schoolspring_json(fixture, "crook1.schoolspring.com")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Posting_ID", "Link"))
})

test_that("parse_redrover_json extracts fields and builds a per-job link from the GraphQL response shape", {
  fixture <- '{"data":{"jobSeekerSiteUnauthenticated":{"jobPostingSearch":{"results":[
    {"id":"190114","name":"Part-time Custodian","organizationName":"Johnson County School District #1","location":{"name":"Kaycee K-12 School"},"activePublicOnDateUtc":"2026-07-27T15:02:24.7935011Z"},
    {"id":"191643","name":"Assistant Girls Basketball Coach","organizationName":"Johnson County School District #1","location":{"name":"Buffalo High School"},"activePublicOnDateUtc":"2026-07-27T15:01:36.1969706Z"}
  ]}}}}'

  result <- parse_redrover_json(fixture, "jcsd1")

  expect_equal(nrow(result), 2)
  expect_equal(result$Title, c("Part-time Custodian", "Assistant Girls Basketball Coach"))
  expect_equal(result$Location, c("Kaycee K-12 School", "Buffalo High School"))
  expect_equal(result$Posted_Date, c("2026-07-27", "2026-07-27"))
  expect_equal(result$Posting_ID, c("redrover:jcsd1:190114", "redrover:jcsd1:191643"))
  expect_equal(result$Link, c(
    "https://jobs.redroverk12.com/org/jcsd1/opening/190114",
    "https://jobs.redroverk12.com/org/jcsd1/opening/191643"
  ))
})

test_that("parse_redrover_json returns zero rows (not an error) for an empty results list", {
  fixture <- '{"data":{"jobSeekerSiteUnauthenticated":{"jobPostingSearch":{"results":[]}}}}'
  result <- parse_redrover_json(fixture, "jcsd1")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Posting_ID", "Link",
                                 "Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text"))
})

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

test_that("parse_applitrack_output un-escapes document.write literals and preserves the original position/position2 splitting logic", {
  # Real fixture captured from https://www.applitrack.com/bighorn/onlineapp/
  # jobpostings/Output.asp?all=1 -- the endpoint default.aspx's own inline
  # script injects into the page via document.write() rather than being
  # server-rendered directly.
  fixture <- paste(readLines(test_path("fixtures", "applitrack_output.txt"), warn = FALSE), collapse = "\n")

  result <- parse_applitrack_output(fixture)

  expect_equal(nrow(result), 3)
  expect_equal(result$title, c(
    "Life Skills Paraprofessional - Middle/High School",
    "Substitute Teacher",
    "Substitute Bus Driver / Activity Driver"
  ))
  # Regression: "Position Type:" consumes TWO values (category + subcategory)
  # only when the next value isn't itself another label's value -- getting
  # this wrong shifts every subsequent field (date_posted/location/
  # closing_date) by one position for every row after the first mismatch.
  expect_equal(result$position, c("Support Staff/", "Substitute/", "Transportation/"))
  expect_equal(result$position2, c("Para-Educator Special Services", "Substitute Teacher", "Substitute Bus Driver"))
  expect_equal(result$date_posted, c("8/2/2026", "7/5/2017", "12/17/2024"))
  expect_equal(result$closing_date, c("until filled", "Open During School Year", "Open During School Year"))
})

test_that("parse_applitrack_output returns zero rows (not an error) when there's no postingsList content", {
  result <- parse_applitrack_output("document.write('<div>No postings here</div>');")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("title", "position", "position2", "date_posted", "location", "closing_date", "job_id",
                                "Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text"))
})

test_that("fetch_applitrack_postings decodes Windows-1252 bytes instead of silently dropping them as NA", {
  # Regression: Applitrack serves Windows-1252 with no charset in
  # Content-Type. httr2 guesses UTF-8 by default; if a posting contains a
  # non-ASCII Windows-1252 byte (e.g. an en-dash, 0x96), UTF-8 decoding
  # fails and resp_body_string() returns NA rather than erroring -- which
  # parse_applitrack_output() then silently turns into zero rows,
  # indistinguishable from a district with genuinely no openings. Confirmed
  # live: 10 of 25 Applitrack districts were affected, one hiding 69 real
  # postings. This fixture is the real applitrack_output.txt fixture with
  # an en-dash injected into the first title and re-encoded as raw
  # Windows-1252 bytes, exactly mirroring what the live endpoint sends.
  raw_bytes <- readBin(
    test_path("fixtures", "applitrack_output_windows1252.bin"),
    "raw",
    file.info(test_path("fixtures", "applitrack_output_windows1252.bin"))$size
  )
  httr2::local_mocked_responses(
    list(httr2::response(200, headers = list("Content-Type" = "text/javascript"), body = raw_bytes))
  )

  result <- fetch_applitrack_postings("bighorn")

  expect_equal(nrow(result), 3)
  expect_equal(result$title[1], "Life Skills Para–professional - Middle/High School")
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

test_that("parse_tedk12_postings extracts real job rows from a real browser-rendered capture", {
  # Regression for the 2026-08-06 finding: TedK12/PowerSchool Hire serves
  # a near-empty modern app shell to a request with no/default User-Agent,
  # but the real classic jQuery job board (this fixture) to one that looks
  # like a browser -- see fetch_tedk12_postings()'s header comment for the
  # full story. This fixture is a real chromote-rendered capture of Goshen
  # County SD1's live TedK12 board, captured with a real browser UA.
  html <- paste(readLines(test_path("fixtures", "tedk12_goshen.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  result <- parse_tedk12_postings(html, "https://goshen.tedk12.com/hire/index.aspx")

  expect_equal(nrow(result), 12)
  expect_true(all(result$url == "https://goshen.tedk12.com/hire/index.aspx"))
  # The sortable-column header row ("Job Title"/"Posting Date"/etc., not a
  # real posting) must not survive into the result.
  expect_false("Job Title" %in% result$title)

  clerk <- result[result$title == "Office Clerk - Platte River High School", ]
  expect_equal(nrow(clerk), 1)
  expect_equal(clerk$date_posted, "08/04/2026")
  expect_equal(clerk$position, "Support")
  expect_equal(clerk$location, "Platte River High School")
})

test_that("parse_tedk12_postings returns zero rows (not an error) when there's no job table at all", {
  result <- parse_tedk12_postings("<html><body><p>Not a TedK12 page</p></body></html>", "https://example.tedk12.com/hire/index.aspx")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("title", "date_posted", "position", "location", "url"))
})

test_that("parse_applitrack_output reads each field from its own <li> (one-value Position Type)", {
  # Real fixture captured 2026-11-03 from Teton County SD1 (WY)
  # https://www.applitrack.com/tcsd/onlineapp/jobpostings/Output.asp?all=1 .
  # JobID 836 has a one-value "Position Type:" followed by "Date Posted:
  # 10/26/2026". The old running-index parser took the date as a second
  # Position Type value, so the posting got date_posted "TCSD Schools" and
  # location "10/26/2026".
  path <- test_path("fixtures", "applitrack_output_wy_teton_single_position_type.bin")
  js <- iconv(readChar(path, file.size(path), useBytes = TRUE), "WINDOWS-1252", "UTF-8")
  out <- parse_applitrack_output(js)
  expect_equal(nrow(out), 14)
  expect_true(all(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", out$date_posted)))
  p <- out[out$job_id == "836", ]
  expect_equal(p$title, "K-5 Special Education Teacher, Life Skills")
  expect_equal(p$position, "Student Support Services")
  expect_true(is.na(p$position2))
  expect_equal(p$date_posted, "10/26/2026")
  expect_equal(p$location, "TCSD Schools")
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

test_that("parse_schoolspring_job_json reads a filled posting with no deadline", {
  # Lewistown District Nurse: left the board, record still says Filled (50);
  # "Posted until filled" is stored as closeDate 2050-01-01.
  r <- parse_schoolspring_job_json(ss_job_fixture(5803798))
  expect_equal(r$job_id, "5803798")
  expect_equal(r$Job_Status, "filled")
  expect_true(is.na(r$Close_Date))
  expect_equal(c(r$Pay_Min, r$Pay_Max, r$Pay_Unit), c("25.67", "25.67", "hour"))
})

test_that("parse_schoolspring_job_json keeps the deadline applicants see, not the UTC closeDate", {
  # Ronan assistant girls' basketball: closeDate 2026-11-14T06:59 (UTC) is
  # "Nov 13, 2026 11:59 PM (Mountain Standard Time)" on the board. Its
  # "Per Year" $1,919.10 is a coaching stipend, not a salary.
  r <- parse_schoolspring_job_json(ss_job_fixture(5732320))
  expect_equal(r$Close_Date, "2026-11-13")
  expect_equal(r$Job_Status, "posted")
  expect_equal(r$Pay_Unit, "stipend")
  expect_equal(r$Pay_Min, 1919.1)
  expect_match(r$Pay_Text, "^\\$1,919\\.10 Per Year")
})

test_that("parse_schoolspring_job_json reads units from the amount when the label is ambiguous", {
  # Belgrade .5 FTE math teacher: "Hourly and Certified Pay" $22,725-$26,622 is a salary.
  r <- parse_schoolspring_job_json(ss_job_fixture(5785656))
  expect_equal(c(r$Pay_Min, r$Pay_Max), c(22725, 26622))
  expect_equal(r$Pay_Unit, "year")
  # Belgrade substitute bus driver: "Substitute" $45 says neither hour nor day.
  r <- parse_schoolspring_job_json(ss_job_fixture(5747025))
  expect_true(is.na(r$Pay_Min) && is.na(r$Pay_Unit) && is.na(r$Pay_Text))
})

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
