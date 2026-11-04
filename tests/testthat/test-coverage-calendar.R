# Coverage calendar (coverage_calendar.R; posting-dates plan step 2). Log rows
# follow the real scrape_log.csv of the 2026-11-04 MT rehearsal (Gallatin
# Gateway's 404, the Apptegy prefix, UM's board carrying Missoula College).
source(here::here("coverage_calendar.R"))

cc_log <- function(...) {
  rows <- list(...)
  data.frame(timestamp = vapply(rows, `[[`, "", 1), source = vapply(rows, `[[`, "", 2),
             status = vapply(rows, `[[`, "", 3), n_rows = vapply(rows, `[[`, "", 4),
             error_message = vapply(rows, function(r) if (length(r) > 4) r[[5]] else NA_character_, ""),
             stringsAsFactors = FALSE)
}

test_that("each entity gets its run status; Apptegy prefix dropped; missing ones are not_logged", {
  log <- cc_log(
    c("2026-11-04 07:39:27", "Missoula County Public Schools", "ok", "22"),
    c("2026-11-04 07:40:01", "Gallatin Gateway School", "error", "0", "HTTP 404 Not Found."),
    c("2026-11-04 07:44:51", "Apptegy/chromote: Hamilton School District 3", "ok", "14"),
    c("2026-11-04 07:46:44", "Apptegy/chromote K-12 districts (Wolf Point, Hamilton)", "ok", "253"),
    c("2026-11-04 07:50:00", "University of Montana", "ok", "51"),
    c("2026-11-04 07:56:00", "Reference: BLS OEWS role wages", "current", "153"),
    c("2026-10-27 07:39:27", "Lone Rock School District", "ok", "2"))   # an older run
  r <- build_coverage_calendar(log, "2026-11-04",
    k12_entities = c("Missoula County Public Schools", "Gallatin Gateway School", "Hamilton School District 3",
                     "Lone Rock School District"),
    he_entities = c("University of Montana", "Missoula College"),
    feeds = c("Apptegy browser session" = "^Apptegy/chromote K-12 districts", "OPI statewide feed" = "^OPI Jobs"),
    shared_boards = c("Missoula College" = "University of Montana"))
  st <- setNames(r$Status, r$Entity)
  expect_equal(unname(st[c("Missoula County Public Schools", "Gallatin Gateway School", "Hamilton School District 3",
                           "Lone Rock School District", "Missoula College", "Apptegy browser session",
                           "OPI statewide feed")]),
               c("ok", "error", "ok", "not_logged", "ok", "ok", "not_logged"))
  expect_equal(r$Error[r$Entity == "Gallatin Gateway School"], "HTTP 404 Not Found.")
  expect_equal(r$Source[r$Entity == "Missoula College"], "University of Montana")
  expect_false(any(grepl("Reference", r$Source)))
  expect_equal(unique(r$Run_Date), "2026-11-04")
  expect_equal(names(r), COVERAGE_CALENDAR_COLUMNS)
})

test_that("the latest attempt of a rerun wins, and off-registry sources need a posting or a past row", {
  log <- cc_log(
    c("2026-11-04 07:00:00", "Niobrara County School District 1", "error", "0", "timeout"),
    c("2026-11-04 08:08:02", "Niobrara County School District 1", "ok", "5"),
    c("2026-11-04 08:09:00", "CCD district teacher FTE (WY)", "ok", "48"))
  r <- build_coverage_calendar(log, "2026-11-04", data_k12 = c("Niobrara County School District 1", "Snowy Range Academy"))
  expect_equal(r$Entity, "Niobrara County School District 1")   # Snowy Range Academy only came via WSBA
  expect_equal(r$Status, "ok")
  # Next run: Niobrara isn't logged at all -> tracked from the calendar, not_logged.
  r2 <- build_coverage_calendar(cc_log(c("2026-11-06 08:00:00", "WSBA statewide vacancies", "ok", "108")),
                                "2026-11-06", previous = r)
  expect_equal(r2$Status[r2$Entity == "Niobrara County School District 1"], "not_logged")
})

test_that("refresh_coverage_calendar replaces a same-day rerun and keeps other runs", {
  dir <- withr::local_tempdir()
  log_path <- file.path(dir, "scrape_log.csv"); path <- file.path(dir, "coverage_calendar.csv")
  utils::write.csv(cc_log(c("2026-10-27 08:00:00", "Belgrade School District 44", "ok", "20"),
                          c("2026-11-04 08:00:00", "Belgrade School District 44", "empty", "0")),
                   log_path, row.names = FALSE)
  refresh_coverage_calendar(path, log_path, run_date = as.Date("2026-10-27"), k12_entities = "Belgrade School District 44")
  refresh_coverage_calendar(path, log_path, run_date = as.Date("2026-11-04"), k12_entities = "Belgrade School District 44")
  refresh_coverage_calendar(path, log_path, run_date = as.Date("2026-11-04"), k12_entities = "Belgrade School District 44")
  cal <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(cal$Run_Date, c("2026-10-27", "2026-11-04"))
  expect_equal(cal$Status, c("ok", "empty"))
  expect_error(refresh_coverage_calendar(path, log_path, run_date = as.Date("2026-12-01")), "no scrape_log entries")
})

test_that("read_scrape_log reads logs whose error messages hold backslash-escaped quotes", {
  # Shape of the MT 2026-11-03 log: read.csv lost every row after this one.
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c('"timestamp","source","status","n_rows","error_message"',
               '"2026-11-03 13:20:01","Roberts Public School","error",0,"\' site under maintenance \' said \\"x\\""',
               '"2026-11-03 13:40:00","Montana Tech","ok",18,NA'), f)
  l <- read_scrape_log(f)
  expect_equal(l$source, c("Roberts Public School", "Montana Tech"))
  expect_equal(l$error_message[1], "' site under maintenance ' said \"x\"")
})

test_that("log_scrape_result writes quotes that read.csv reads back", {
  f <- withr::local_tempfile(fileext = ".csv")
  log_scrape_result("Roberts Public School", "error", 0L, 'said "x"', log_path = f)
  log_scrape_result("Montana Tech", "ok", 18L, log_path = f)
  l <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_equal(l$source, c("Roberts Public School", "Montana Tech"))
  expect_equal(l$error_message[1], 'said "x"')
})
