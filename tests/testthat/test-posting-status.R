# Posting status checks (posting_status.R; closing-date audit 2026-11-05).
# Fixtures: real posting pages captured 2026-11-06, one closed and one open
# per platform (visible text; browser-rendered for Red Rover and Oracle).
source(here::here("posting_status.R"))
fx <- function(name) paste(readLines(test_path("fixtures", paste0("posting_status_", name, ".txt")), warn = FALSE,
                                     encoding = "UTF-8"), collapse = "\n")

test_that("each platform's closed and open pages are read correctly", {
  on <- as.Date("2026-11-06")
  st <- function(p, n) parse_posting_status(p, fx(n), on)
  expect_equal(st("applitrack", "applitrack_closed")$Status, "closed")
  expect_equal(st("applitrack", "applitrack_open")$Status, "open")
  expect_equal(st("tedk12", "tedk12_closed")$Status, "closed")
  expect_equal(st("tedk12", "tedk12_open")$Status, "open")
  expect_equal(st("redrover", "redrover_closed")$Status, "closed")
  rr <- st("redrover", "redrover_open")
  expect_equal(rr$Status, "open"); expect_equal(rr$Stated_Close_Date, "2026-11-06")   # "Job Closes: November 6, 2026"
  expect_equal(st("oracle", "oracle_closed")$Status, "closed")
  expect_equal(st("oracle", "oracle_open")$Status, "open")
  # NEOGOV keeps a page up after it closes; its Closing Date says so.
  ng <- st("neogov", "neogov_closed_by_date")
  expect_equal(ng$Status, "closed"); expect_equal(ng$Stated_Close_Date, "2026-11-04"); expect_equal(ng$Signal, "closing date passed")
  ng_open <- st("neogov", "neogov_open")
  expect_equal(ng_open$Status, "open"); expect_equal(ng_open$Stated_Close_Date, "2026-11-08")
  expect_equal(st("neogov", "neogov_removed")$Status, "closed")
  expect_equal(st("jazzhr", "jazzhr_open")$Status, "open")
  expect_equal(parse_posting_status("jazzhr", "HTTP 410 Gone", on)$Status, "closed")   # what the fetch returns for a 410
  expect_equal(st("paycom", "paycom_closed")$Status, "closed")
  pc <- st("paycom", "paycom_open")
  expect_equal(pc$Status, "open"); expect_true(is.na(pc$Stated_Close_Date))   # "CLOSING DATE: Open until filled"
  # Nothing recognizable, or an empty page, is unknown, never closed.
  expect_equal(parse_posting_status("applitrack", "Service unavailable", on)$Status, "unknown")
  expect_equal(parse_posting_status("oracle", "", on)$Status, "unknown")
})

test_that("every link form of one posting checks one page", {
  u <- c("https://www.applitrack.com/park6/OnlineApp/JobPostings/View.asp?AppliTrackJobId=1026",
         "https://www.applitrack.com/park6/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=1026",
         "https://fremont2.tedk12.com/hire/ViewJob.aspx?JobID=148",
         "https://jobs.redroverk12.com/org/FCSD38/opening/159101",
         "https://www.schooljobs.com/careers/caspercollege/jobs/5516806/accountant-cc-foundation",
         "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/260119?foo=1",
         "https://laramie1.schoolspring.com/jobs/5968993", "https://www.wsba-wy.org/vacancies", NA)
  k <- posting_status_check_url(u)
  expect_equal(k[1], k[2])
  expect_equal(k[1], "https://www.applitrack.com/park6/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=1026")
  expect_equal(k[4], "https://jobs.redroverk12.com/org/FCSD38/opening/159101")
  expect_equal(k[5], "https://www.schooljobs.com/careers/caspercollege/jobs/5516806")
  expect_equal(k[6], "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/260119")
  expect_true(all(is.na(k[7:9])))
  expect_equal(posting_status_check_url("https://montanatechuniversity.applytojob.com/apply/VpDQi8ClVj/Academic-Advisor-I"),
               "https://montanatechuniversity.applytojob.com/apply/VpDQi8ClVj")
  expect_equal(posting_status_platform("https://www.paycomonline.net/v4/ats/web.php/portal/23D9610C/jobs/127049"), "paycom")   # SchoolSpring has its own record; WSBA's page and no link aren't checked
})

test_that("closed WSBA postings are dropped; open, unknown and unlinked ones stay", {
  wsba <- data.frame(title = c("K-3 Music Teachers", "Bus Driver", "Aide", "Clerk"),
                     url = c("https://www.applitrack.com/sweetwater/OnlineApp/JobPostings/view.asp?AppliTrackJobId=5652",
                             "https://fremont2.tedk12.com/hire/ViewJob.aspx?JobID=148",
                             "https://jobs.redroverk12.com/org/jcsd1/opening/1", "https://www.wsba-wy.org/vacancies"),
                     stringsAsFactors = FALSE)
  pages <- c("This vacancy is not available.", "Bus Driver Job Description Apply Now", "")
  fake <- function(check_url, platform, session) pages[match(check_url, posting_status_check_url(wsba$url))]
  checks <- check_posting_pages(wsba$url, "2026-11-06", session_factory = function() "session", pause = 0, fetch = fake)
  expect_equal(checks$Status, c("closed", "open", "unknown"))
  expect_equal(drop_closed_wsba_rows(wsba, checks)$title, c("Bus Driver", "Aide", "Clerk"))
})

test_that("a failed check is unknown, not closed", {
  boom <- function(check_url, platform, session) stop("timeout")
  x <- check_posting_pages("https://fremont2.tedk12.com/hire/ViewJob.aspx?JobID=1", "2026-11-06", pause = 0, fetch = boom)
  expect_equal(x$Status, "unknown")
  expect_match(x$Signal, "check failed")
})

test_that("the status table keeps closings, dates them by a passed closing date, and re-checks the rest", {
  ck <- function(url, status, close = NA, last = "2026-10-30")
    data.frame(Check_URL = url, Platform = "neogov", Status = status, Signal = "s", Stated_Close_Date = close,
               Segment = "HE", District = "Casper College", Title = "T", Last_Listed = last, stringsAsFactors = FALSE)
  t1 <- merge_posting_status(NULL, rbind(ck("a", "closed", "2026-11-04"), ck("b", "open", "2026-11-20"), ck("c", "unknown")),
                             "2026-11-06")
  expect_equal(t1$Status_Since, c("2026-11-04", NA, NA))   # the closing date, before the check found it
  t2 <- merge_posting_status(t1, rbind(ck("b", "closed"), ck("c", "unknown")), "2026-11-13")
  expect_equal(t2$Status, c("closed", "closed", "unknown"))
  expect_equal(t2$Status_Since[t2$Check_URL == "b"], "2026-11-13")
  expect_equal(t2$Stated_Close_Date[t2$Check_URL == "b"], "2026-11-20")   # kept from the earlier check
  expect_equal(t2$Checks, c("1", "2", "2"))
  expect_equal(t2$First_Checked, rep("2026-11-06", 3))
  # Closed postings and ones back on their board aren't re-checked.
  expect_equal(posting_status_rechecks(t2, listed_check_urls = character(0))$Check_URL, "c")
  expect_equal(nrow(posting_status_rechecks(t2, listed_check_urls = "c")), 0)
})

test_that("postings that dropped off their board are found, once per page", {
  prev <- data.frame(url = c("https://www.applitrack.com/crb1/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=1634",
                             "https://www.applitrack.com/crb1/OnlineApp/JobPostings/View.asp?AppliTrackJobId=1634",
                             "https://www.applitrack.com/crb1/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=1700",
                             "https://www.example.org/jobs"),
                     title = c("Coach", "Coach", "Teacher", "Cook"), District = "Carbon County School District 1",
                     stringsAsFactors = FALSE)
  cur <- prev[3, ]
  d <- dropped_postings(prev, cur)
  expect_equal(d$title, "Coach")
})
