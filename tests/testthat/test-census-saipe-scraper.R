# Fixture is a real 5-district slice of the live SAIPE API response
# (Albany, Big Horn 1, Fremont 2, Natrona, Teton -- a deliberate mix
# covering both canonicalization cases (Big Horn, Natrona) plus the
# lowest and highest real child poverty rates in the state, so the tests
# exercise real range, not five similar rows).

read_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("canonicalize_saipe_district_name fixes Big Horn and Natrona, passes every other district through unchanged", {
  expect_equal(canonicalize_saipe_district_name("Big Horn County School District 1"), "Big Horn School District 1")
  expect_equal(canonicalize_saipe_district_name("Natrona County School District 1"), "Natrona School District 1")
  expect_equal(canonicalize_saipe_district_name("Albany County School District 1"), "Albany County School District 1")
  expect_equal(canonicalize_saipe_district_name("Fremont County School District 21"), "Fremont County School District 21")
})

test_that("canonicalize_saipe_district_name is vectorized and each element canonicalizes independently", {
  # Regression for a real bug caught during development: an earlier
  # regexec()/vapply()-based version returned the WRONG value (or errored
  # entirely) on a mixed vector because its no-match branch referenced the
  # whole input vector via lexical scoping instead of the current element.
  # A plain sub() doesn't have this failure mode -- pinning it here so a
  # future "simplify this" pass can't reintroduce it.
  result <- canonicalize_saipe_district_name(c(
    "Big Horn County School District 1", "Albany County School District 1", "Natrona County School District 1"
  ))
  expect_equal(result, c("Big Horn School District 1", "Albany County School District 1", "Natrona School District 1"))
})

test_that("parse_census_saipe_child_poverty extracts real rates for all 5 fixture districts, correctly canonicalized", {
  result <- parse_census_saipe_child_poverty(read_fixture("census_saipe_schdist_2024.json"), 2024)

  expect_equal(nrow(result), 5)
  expect_equal(names(result), c("District", "Child_Poverty_Rate", "SAIPE_Year"))
  expect_true(all(result$SAIPE_Year == 2024))
  expect_true("Big Horn School District 1" %in% result$District)
  expect_true("Natrona School District 1" %in% result$District)
  expect_false(any(grepl("Big Horn County|Natrona County", result$District)))

  # Real, meaningfully different rates -- confirms this isn't just a
  # constant or a units bug (e.g. still a raw percent instead of a 0-1
  # ratio -- Child_Poverty_Rate must be divided by 100 from SAIPE's own
  # percent-point figure).
  teton <- result$Child_Poverty_Rate[result$District == "Teton County School District 1"]
  fremont2 <- result$Child_Poverty_Rate[result$District == "Fremont County School District 2"]
  expect_equal(teton, 0.035)
  expect_equal(fremont2, 0.278)
  expect_true(fremont2 > 5 * teton)
})

test_that("parse_census_saipe_child_poverty returns an empty, correctly-shaped frame (not an error) for a header-only response", {
  result <- parse_census_saipe_child_poverty('[["SD_NAME","SAEPOVRAT5_17RV_PT","time","state","school district (unified)"]]', 2024)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("District", "Child_Poverty_Rate", "SAIPE_Year"))
})

# Real fixture: SAIPE's 2024 "school district (elementary)" response for
# Wyoming, captured 2026-11-09 -- its only row is Fremont County School
# District 38 (Arapahoe), which the unified list leaves out.
test_that("fetch_census_saipe_child_poverty adds the elementary geography (Fremont County School District 38)", {
  unified <- read_fixture("census_saipe_schdist_2024.json")
  elementary <- read_fixture("census_saipe_schdist_elementary_2024.json")
  responses <- list(
    httr2::response(status_code = 200, headers = list("Content-Type" = "application/json"), body = charToRaw(unified)),
    httr2::response(status_code = 200, headers = list("Content-Type" = "application/json"), body = charToRaw(elementary))
  )
  httr2::local_mocked_responses(responses)
  result <- fetch_census_saipe_child_poverty(year = 2024, api_key = "test")
  expect_equal(nrow(result), 6)
  expect_equal(result$Child_Poverty_Rate[result$District == "Fremont County School District 38"], 0.168)
})

test_that("an empty elementary answer (HTTP 204, no body) adds no rows", {
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, headers = list("Content-Type" = "application/json"),
                    body = charToRaw(read_fixture("census_saipe_schdist_2024.json"))),
    httr2::response(status_code = 204, body = raw(0))
  ))
  expect_equal(nrow(fetch_census_saipe_child_poverty(year = 2024, api_key = "test")), 5)
})
