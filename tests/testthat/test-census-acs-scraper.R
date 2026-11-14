# All fixtures are real captures from the live Census ACS 5-Year API
# (Albany/Campbell/Teton counties -- a deliberate mix of a low-mining
# college town, a high-mining energy county, and a high-cost resort county,
# so the tests exercise real range, not three similar rows).

read_fixture <- function(name) paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("parse_acs_json converts the Census array-of-arrays shape into a properly-named, numeric-coerced data frame", {
  result <- parse_acs_json(read_fixture("census_acs_detail_2024.json"))

  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("NAME", "B19013_001E", "B25064_001E", "B01003_001E", "state", "county"))
  expect_true(is.numeric(result$B19013_001E))
  expect_true(is.character(result$NAME))
  expect_true(is.character(result$county))  # FIPS codes stay character, not coerced

  campbell <- result[result$NAME == "Campbell County, Wyoming", ]
  expect_equal(campbell$B19013_001E, 89869)
  expect_equal(campbell$B01003_001E, 47240)
})

test_that("parse_acs_json returns an empty data frame (not an error) for a header-only response", {
  result <- parse_acs_json('[["NAME","B19013_001E","state","county"]]')
  expect_equal(nrow(result), 0)
})

test_that("clean_acs_value treats negative sentinel codes as NA, matching clean_ipeds_value()'s pattern", {
  expect_true(is.na(clean_acs_value(-666666666)))
  expect_true(is.na(clean_acs_value(NA_real_)))
  expect_equal(clean_acs_value(61917), 61917)
  expect_equal(clean_acs_value(0), 0)  # a real zero must NOT be treated as missing
})

test_that("parse_census_income_rent_population extracts real income/rent/population for all 3 fixture counties", {
  raw <- parse_acs_json(read_fixture("census_acs_detail_2024.json"))
  result <- parse_census_income_rent_population(raw, 2024)

  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("County", "Median_Household_Income", "Median_Gross_Rent", "Population", "ACS_Year"))
  expect_true(all(result$ACS_Year == 2024))

  teton <- result[result$County == "Teton County, Wyoming", ]
  expect_equal(teton$Median_Household_Income, 124172)
  expect_equal(teton$Median_Gross_Rent, 1904)
  expect_equal(teton$Population, 23396)
})

test_that("parse_census_mining_employment_share computes a real, meaningfully different rate per county", {
  raw <- parse_acs_json(read_fixture("census_acs_subject_2024.json"))
  result <- parse_census_mining_employment_share(raw)

  expect_equal(nrow(result), 3)
  albany <- result$Mining_Employment_Share[result$County == "Albany County, Wyoming"]
  campbell <- result$Mining_Employment_Share[result$County == "Campbell County, Wyoming"]
  teton <- result$Mining_Employment_Share[result$County == "Teton County, Wyoming"]

  expect_equal(round(campbell, 4), round(3626 / 23829, 4))
  # Confirms this figure actually discriminates real counties, not just
  # computes something -- Campbell (coal) is a real energy county, Teton
  # (Jackson Hole, tourism-based) genuinely has almost no mining employment.
  expect_true(campbell > 10 * teton)
  expect_true(campbell > albany)
})

test_that("parse_census_mining_employment_share returns NA (not an error or Inf) when total employment is 0", {
  raw <- data.frame(NAME = "Fake County, Wyoming", S2403_C01_001E = 0, S2403_C01_004E = 0, stringsAsFactors = FALSE)
  result <- parse_census_mining_employment_share(raw)
  expect_true(is.na(result$Mining_Employment_Share))
})

test_that("compute_population_change_pct joins current vs. prior by county and computes real, signed percent change", {
  current <- parse_census_income_rent_population(parse_acs_json(read_fixture("census_acs_detail_2024.json")), 2024)
  prior <- parse_census_income_rent_population(parse_acs_json(read_fixture("census_acs_detail_2019.json")), 2019)

  result <- compute_population_change(current, prior)
  expect_equal(nrow(result), 3)

  # Real values: Teton grew (23396 vs 23280 five years earlier), Campbell
  # and Albany both shrank slightly -- confirms this isn't just always
  # positive/always the same sign.
  teton <- result$Population_Change_Pct[result$County == "Teton County, Wyoming"]
  campbell <- result$Population_Change_Pct[result$County == "Campbell County, Wyoming"]
  expect_true(teton > 0)
  expect_true(campbell < 0)
  expect_equal(round(teton, 5), round((23396 - 23280) / 23280, 5))
})

test_that("compute_population_change_pct returns an empty frame (not an error) when either side has no rows", {
  current <- parse_census_income_rent_population(parse_acs_json(read_fixture("census_acs_detail_2024.json")), 2024)
  empty <- data.frame(County = character(0), Population = numeric(0), stringsAsFactors = FALSE)

  expect_equal(nrow(compute_population_change(current, empty)), 0)
  expect_equal(nrow(compute_population_change(empty, current)), 0)
})

test_that("census_api_key errors with a clear, actionable message when CENSUS_API_KEY isn't set", {
  withr::local_envvar(CENSUS_API_KEY = "")
  expect_error(census_api_key(), "CENSUS_API_KEY")
  expect_error(census_api_key(), "key_signup")
})
