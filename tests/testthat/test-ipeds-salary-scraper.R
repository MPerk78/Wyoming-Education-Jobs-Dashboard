# Real fixture: the actual Wyoming (fips=56) response from the Urban
# Institute's Education Data Portal salaries-instructional-staff/2024
# endpoint, captured 2026-08-04 -- all 1008 rows across 9 institutions'
# academic_rank x contract_length x sex combinations, not synthetic.

test_that("parse_ipeds_he_salaries extracts all 9 Wyoming institutions with correct headline salaries", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_salaries_2024.csv"))
  result <- parse_ipeds_he_salaries(df, 2024)

  expect_equal(nrow(result), 9)
  expect_setequal(result$Name, c(
    "Casper College", "Central Wyoming College", "Eastern Wyoming Community College",
    "Laramie County Community College", "Northwest College", "Sheridan College",
    "Gillette College", "Western Wyoming Community College", "University of Wyoming"
  ))
  expect_true(all(result$Salary_Year == "2024"))

  uw <- result[result$Name == "University of Wyoming", ]
  expect_equal(uw$Faculty_Avg_Salary, 98302.828571, tolerance = 1e-4)
  expect_equal(uw$Faculty_Avg_Salary_Professor, 127093.120879, tolerance = 1e-4)
  expect_equal(uw$Faculty_Count, 700)
  expect_true(is.na(uw$Salary_Note))
})

test_that("parse_ipeds_he_salaries surfaces the Sheridan/Gillette shared-reporting caveat instead of hiding it", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_salaries_2024.csv"))
  result <- parse_ipeds_he_salaries(df, 2024)

  sheridan <- result[result$Name == "Sheridan College", ]
  gillette <- result[result$Name == "Gillette College", ]

  # Same underlying IPEDS unitid (Northern Wyoming Community College
  # District) -- figures are identical, and both carry a note explaining why.
  expect_equal(sheridan$Faculty_Avg_Salary, gillette$Faculty_Avg_Salary)
  expect_false(is.na(sheridan$Salary_Note))
  expect_false(is.na(gillette$Salary_Note))
  expect_match(sheridan$Salary_Note, "Northern Wyoming Community College District")
})

test_that("parse_ipeds_he_salaries treats IPEDS negative sentinel codes as NA, not real values", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_salaries_2024.csv"))
  result <- parse_ipeds_he_salaries(df, 2024)

  # Casper College and Laramie County Community College are two-year
  # institutions with no full "Professor" rank reported in the source data
  # (confirmed against the raw fixture) -- this should surface as a genuine
  # NA, not a negative sentinel value or a silently dropped row.
  casper <- result[result$Name == "Casper College", ]
  lccc <- result[result$Name == "Laramie County Community College", ]
  expect_true(is.na(casper$Faculty_Avg_Salary_Professor))
  expect_true(is.na(lccc$Faculty_Avg_Salary_Professor))
  expect_false(is.na(casper$Faculty_Avg_Salary))
})

test_that("parse_ipeds_he_salaries returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_he_salaries(data.frame(), 2024)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Faculty_Avg_Salary", "Faculty_Avg_Salary_Professor",
                                 "Faculty_Count", "Salary_Year", "Salary_Note"))
})

test_that("parse_ipeds_salary_trend_year extracts one row per institution with the given year attached", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_salaries_2024.csv"))
  result <- parse_ipeds_salary_trend_year(df, 2024)

  expect_equal(nrow(result), 9)
  expect_true(all(result$Year == 2024))

  uw <- result[result$Name == "University of Wyoming", ]
  expect_equal(uw$Faculty_Avg_Salary, 98302.828571, tolerance = 1e-4)
})

test_that("parse_ipeds_salary_trend_year returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_salary_trend_year(data.frame(), 2024)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Year", "Faculty_Avg_Salary"))
})

test_that("fetch_ipeds_paginated handles JSON null fields across paginated pages without error", {
  page1 <- "{\"count\": 2, \"next\": \"https://api.example.com/page2\", \"results\": [{\"unitid\": 240727, \"year\": 2023, \"average_salary\": 98000, \"number_applied\": null}]}"
  page2 <- "{\"count\": 2, \"next\": null, \"results\": [{\"unitid\": 240693, \"year\": 2023, \"average_salary\": 65000, \"number_applied\": 100}]}"

  httr2::local_mocked_responses(list(
    httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(page1)),
    httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(page2))
  ))

  result <- fetch_ipeds_paginated("https://api.example.com/page1")
  expect_equal(nrow(result), 2)
  expect_equal(result$unitid, c(240727, 240693))
  expect_true(is.na(result$number_applied[1]))
  expect_equal(result$number_applied[2], 100)
})
