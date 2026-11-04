# SchoolSpring ending reasons (refresh_schoolspring_endings(), direct_api_scrapers.R).
# The listing/detail parser tests are in test-direct-api-scrapers.R.

# How postings ended (posting-dates plan step 4). Statuses below mirror real
# lookups of 2026-11-04 (MT: 99 postings that left the board, 62 filled,
# 17 canceled, 20 review); the lookup itself is faked so no request is made.
ss_archive <- function(dir, date, ids, titles = paste("Job", ids)) {
  d <- data.frame(title = c(titles, "Para"), date_posted = "2026-09-01", url = c(
    paste0("https://example.schoolspring.com/jobs/", ids), "https://example.applitrack.com/x/default.aspx?AppliTrackJobId=9"),
    District = "Example SD", stringsAsFactors = FALSE)
  utils::write.csv(d, file.path(dir, paste0("combined_", date, ".csv")), row.names = FALSE)
}
fake_status <- function(status) function(ids) {
  s <- status[ids]
  data.frame(job_id = ids[!is.na(s)], Job_Status = unname(s[!is.na(s)]), stringsAsFactors = FALSE)
}

test_that("schoolspring_sightings keeps SchoolSpring job IDs with first and last listing dates", {
  dir <- withr::local_tempdir()
  ss_archive(dir, "2026-10-20", c("1", "2"))
  ss_archive(dir, "2026-10-27", c("2", "3"))
  s <- schoolspring_sightings(dir, since = as.Date("2026-01-01"))
  expect_equal(s$job_id, c("1", "2", "3"))
  expect_equal(format(s$First_Seen), c("2026-10-20", "2026-10-20", "2026-10-27"))
  expect_equal(format(s$Last_Seen), c("2026-10-20", "2026-10-27", "2026-10-27"))
})

test_that("update_schoolspring_endings records how postings that left the board ended", {
  dir <- withr::local_tempdir()
  ss_archive(dir, "2026-10-20", c("1", "2", "3", "4"))
  ss_archive(dir, "2026-10-27", c("4", "5"))
  s <- schoolspring_sightings(dir, since = as.Date("2026-01-01"))
  # 1 filled, 2 canceled, 3 still "posted" (our scrape missed it: not ended),
  # 4 review; 5 is listed today and is not looked up.
  asked <- NULL
  fetch <- function(ids) { asked <<- ids; fake_status(c(`1` = "filled", `2` = "canceled", `3` = "posted", `4` = "review"))(ids) }
  e <- update_schoolspring_endings(s, today = as.Date("2026-10-27"), fetch = fetch)
  expect_setequal(asked, c("1", "2", "3"))   # 4 and 5 were listed on 10-27
  e <- update_schoolspring_endings(s, today = as.Date("2026-11-03"), fetch = fetch)
  expect_setequal(e$job_id, c("1", "2", "4"))
  expect_equal(e$Job_Status[e$job_id == "1"], "filled")
  expect_equal(e$Last_Seen[e$job_id == "4"], "2026-10-27")
  expect_equal(e$Status_Since, rep("2026-11-03", 3))
})

test_that("filled and canceled are final; review is rechecked until it changes", {
  dir <- withr::local_tempdir()
  ss_archive(dir, "2026-10-20", c("1", "2"))
  s <- schoolspring_sightings(dir, since = as.Date("2026-01-01"))
  e <- update_schoolspring_endings(s, today = as.Date("2026-10-27"),
                                   fetch = fake_status(c(`1` = "filled", `2` = "review")))
  asked <- NULL
  fetch <- function(ids) { asked <<- ids; fake_status(c(`1` = "canceled", `2` = "review"))(ids) }
  e <- update_schoolspring_endings(s, e, today = as.Date("2026-11-03"), fetch = fetch)
  expect_equal(asked, "2")
  expect_equal(e$Status_Since[e$job_id == "2"], "2026-10-27")   # unchanged status keeps its date
  expect_equal(e$Last_Checked[e$job_id == "2"], "2026-11-03")
  e <- update_schoolspring_endings(s, e, today = as.Date("2026-11-10"),
                                   fetch = fake_status(c(`2` = "filled")))
  expect_equal(e$Job_Status[e$job_id == "2"], "filled")
  expect_equal(e$Status_Since[e$job_id == "2"], "2026-11-10")
  expect_equal(e$Job_Status[e$job_id == "1"], "filled")
})

test_that("a failed lookup records nothing and refresh_schoolspring_endings round-trips the file", {
  dir <- withr::local_tempdir()
  ss_archive(dir, "2026-10-20", c("1", "2"))
  ss_archive(dir, "2026-10-27", character(0), character(0))
  path <- file.path(dir, "endings.csv")
  out <- refresh_schoolspring_endings(dir, path, today = as.Date("2026-10-27"),
                                      fetch = fake_status(c(`1` = "filled")))   # 2's request failed
  expect_equal(out$job_id, "1")
  out <- refresh_schoolspring_endings(dir, path, today = as.Date("2026-11-03"),
                                      fetch = fake_status(c(`2` = "canceled")))
  expect_equal(sort(out$job_id), c("1", "2"))
  expect_equal(names(utils::read.csv(path)), names(SCHOOLSPRING_ENDINGS_EMPTY))
})
