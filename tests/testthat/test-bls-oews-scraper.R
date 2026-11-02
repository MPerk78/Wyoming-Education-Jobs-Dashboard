# bls_oews_scraper.R: BLS OEWS regional pay by role, the labeled fallback for
# postings that state no pay. Fixture: a real api.bls.gov v2 response
# (elementary-teacher annual medians across Wyoming's OEWS areas, May 2025).

test_that("oews_series_id builds statewide and metro/nonmetro series IDs", {
  expect_equal(oews_series_id("3000000", "state", "252021", "13"), "OEUS300000000000025202113")
  expect_equal(oews_series_id("0013740", "area", "252021", "13"), "OEUM001374000000025202113")
  expect_equal(oews_series_id("3000004", "area", "533051", "08"), "OEUM300000400000053305108")
  expect_true(all(nchar(oews_series_plan("MT")$series_id) == 25))
})

test_that("oews_series_plan asks for annual medians everywhere and hourly ones for hourly roles", {
  plan <- oews_series_plan("MT")
  n_areas <- nrow(oews_state_areas("MT"))
  expect_equal(n_areas, 9)
  expect_equal(nrow(plan), n_areas * (nrow(OEWS_OCCUPATIONS) + sum(OEWS_OCCUPATIONS$hourly)))
  expect_false(any(duplicated(plan$series_id)))
})

test_that("parse_oews_response reads a real BLS API response", {
  json <- paste(readLines(test_path("fixtures", "bls_oews_wy_batch1_2026-10-31.json"), warn = FALSE), collapse = "\n")
  out <- parse_oews_response(json)
  expect_equal(nrow(out), 25)
  expect_equal(out$value[out$series_id == "OEUS560000000000025202113"], 61150)
  expect_equal(out$value[out$series_id == "OEUM560000700000025202113"], 59900)
  # Casper's elementary-teacher median is suppressed: no data at all.
  expect_true(is.na(out$value[out$series_id == "OEUM001622000000025202113"]))
  expect_true(all(out$year[!is.na(out$value)] == "2025"))
})

test_that("parse_oews_response turns suppressed cells into NA and stops on a refused request", {
  json <- '{"status":"REQUEST_SUCCEEDED","message":[],"Results":{"series":[
    {"seriesID":"OEUM001622000000025202113","data":[{"year":"2025","period":"A01","value":"-"}]},
    {"seriesID":"OEUM001622000000025904513","data":[]}]}}'
  out <- parse_oews_response(json)
  expect_true(all(is.na(out$value)))
  expect_error(parse_oews_response('{"status":"REQUEST_NOT_PROCESSED","message":["daily threshold reached"]}'),
               "REQUEST_NOT_PROCESSED")
})

test_that("oews_occupation_for_posting matches roles, and leaves coaches unmatched", {
  pos <- c("Teacher", "Teacher", "Teacher", "Paraprofessional", "Transportation", "Custodial/Maintenance",
           "Custodial/Maintenance", "Support Services", "Support Services", "Athletics", "Substitute")
  title <- c("3rd Grade Teacher", "High School Math Teacher", "Middle School Science", "Special Ed Para",
             "Bus Driver", "Night Custodian", "Maintenance Technician", "School Counselor", "IT Technician",
             "Head Volleyball Coach", "Substitute Teacher")
  expect_equal(oews_occupation_for_posting(pos, title),
               c("252021", "252031", "252022", "259045", "533051", "372011", "499071", "211012", NA, NA, "253031"))
})

test_that("oews_area_for_county accepts each county format and covers every county once", {
  expect_equal(oews_area_for_county(c("Yellowstone County", "Flathead", "Petroleum County", "Nowhere"), "MT"),
               c("0013740", "3000004", "3000006", NA))
  expect_equal(oews_area_for_county("Albany County, Wyoming", "WY"), "5600007")
  for (st in c("MT", "WY")) {
    counties <- unlist(lapply(OEWS_AREAS[[st]]$areas, `[[`, "counties"))
    expect_false(any(duplicated(counties)))
    expect_equal(length(counties), c(MT = 56, WY = 23)[[st]])
  }
})

test_that("attach_oews_estimates uses the local figure, the statewide one where BLS suppresses it, and none for coaches", {
  wages <- data.frame(
    Area_Code = c("3000000", "3000000", "0013740", "0013740", "3000000"),
    Area_Name = c("Montana", "Montana", "Billings, MT", "Billings, MT", "Montana"),
    SOC = c("259045", "533051", "259045", "533051", "252021"),
    Occupation = c("Teaching assistants", "School bus drivers", "Teaching assistants", "School bus drivers",
                   "Elementary school teachers"),
    Annual_Median = c(34750, 46220, 32140, NA, 57530),
    Hourly_Median = c(NA, 22.22, NA, NA, NA),
    Year = "2025", stringsAsFactors = FALSE)
  postings <- data.frame(position = c("Paraprofessional", "Transportation", "Athletics", "Teacher"),
                         title = c("Para", "Bus Driver", "Head Coach", "Grade 2 Teacher"))
  out <- attach_oews_estimates(postings, wages, c("Yellowstone County", "Yellowstone County", "Yellowstone County", NA), "MT")
  expect_equal(out$Est_Pay, c(32140, 22.22, NA, 57530))
  expect_equal(out$Est_Pay_Unit, c("year", "hour", NA, "year"))
  expect_equal(out$Est_Area, c("Billings, MT", "Montana", NA, "Montana"))
  expect_equal(out$Est_Year, c("2025", "2025", NA, "2025"))
  expect_true(all(OEWS_ESTIMATE_COLUMNS %in% names(out)))
})

test_that("refresh_oews_wages keeps the file when BLS has no newer year, and rewrites it when it does", {
  withr::local_dir(withr::local_tempdir())  # scrape_log.csv lands here, not in the repo
  path <- file.path(getwd(), "oews_role_wages.csv")
  write.csv(data.frame(Area_Code = "3000000", Area_Name = "Montana", SOC = "252021",
                       Occupation = "Elementary school teachers", Annual_Median = 57530,
                       Hourly_Median = NA, Year = "2025"), path, row.names = FALSE)
  env <- environment(refresh_oews_wages)
  real_year <- get("fetch_oews_latest_year", envir = env)
  real_fetch <- get("fetch_oews_wages", envir = env)
  withr::defer({ assign("fetch_oews_latest_year", real_year, envir = env); assign("fetch_oews_wages", real_fetch, envir = env) })
  fetched <- FALSE
  assign("fetch_oews_wages", function(...) { fetched <<- TRUE; data.frame(Area_Code = "3000000", Annual_Median = 58900, Year = "2026") }, envir = env)

  assign("fetch_oews_latest_year", function(...) "2025", envir = env)
  suppressMessages(refresh_oews_wages("MT", path, api_key = ""))
  expect_false(fetched)

  assign("fetch_oews_latest_year", function(...) "2026", envir = env)
  suppressMessages(refresh_oews_wages("MT", path, api_key = ""))
  expect_true(fetched)
  expect_equal(as.character(read.csv(path)$Year), "2026")
  expect_equal(read.csv("scrape_log.csv")$status, c("current", "ok"))
})

test_that("refresh_oews_wages keeps the file when the new year's table is implausible", {
  withr::local_dir(withr::local_tempdir())
  path <- file.path(getwd(), "oews_role_wages.csv")
  write.csv(data.frame(Area_Code = "3000000", SOC = "252021", Annual_Median = 57530, Year = "2025"), path, row.names = FALSE)
  env <- environment(refresh_oews_wages)
  real_year <- get("fetch_oews_latest_year", envir = env)
  real_fetch <- get("fetch_oews_wages", envir = env)
  withr::defer({ assign("fetch_oews_latest_year", real_year, envir = env); assign("fetch_oews_wages", real_fetch, envir = env) })
  assign("fetch_oews_latest_year", function(...) "2026", envir = env)
  # A units slip: hourly medians in the annual column.
  assign("fetch_oews_wages", function(...) data.frame(Area_Code = "3000000", Annual_Median = 28.32, Year = "2026"), envir = env)
  suppressMessages(refresh_oews_wages("MT", path, api_key = ""))
  expect_equal(read.csv(path)$Annual_Median, 57530)
  log <- read.csv("scrape_log.csv")
  expect_equal(log$status, "rejected")
  expect_match(log$error_message, "Annual_Median value")
})

test_that("the committed OEWS file covers every area and occupation", {
  st <- if (dir.exists(here::here("Mt_Ed_Jobs"))) "MT" else "WY"
  app_dir <- if (st == "MT") "Mt_Ed_Jobs" else "Wy_Ed_Jobs"
  w <- read.csv(here::here(app_dir, "oews_role_wages.csv"), colClasses = c(Area_Code = "character", SOC = "character"))
  expect_setequal(unique(w$Area_Code), oews_state_areas(st)$code)
  expect_setequal(unique(w$SOC), OEWS_OCCUPATIONS$soc)
  # Statewide figures are never suppressed, so every role has a fallback.
  state <- w[w$Area_Code == oews_state_areas(st)$code[1], ]
  expect_false(any(is.na(state$Annual_Median)))
  expect_true(all(w$Annual_Median > 15000 & w$Annual_Median < 250000, na.rm = TRUE))
})
