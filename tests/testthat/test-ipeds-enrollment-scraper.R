# Real fixture: the actual Wyoming (fips=56) response from the Urban
# Institute's Education Data Portal fall-enrollment/2023 endpoint,
# captured 2026-08-06 -- all 30 rows across 10 unitids' level_of_study
# breakdowns (8 of which are in IPEDS_UNITID_MAP; the other 2 -- 240709,
# 240718 -- are real WY institutions outside this project's 9-institution
# scope and correctly dropped by the left_join), not synthetic.

test_that("parse_ipeds_he_enrollment sums FTE across level_of_study into one Enrollment figure per institution", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2023.csv"))
  result <- parse_ipeds_he_enrollment(df, 2023)

  expect_equal(nrow(result), 9)
  expect_setequal(result$Name, c(
    "Casper College", "Central Wyoming College", "Eastern Wyoming Community College",
    "Laramie County Community College", "Northwest College", "Sheridan College",
    "Gillette College", "Western Wyoming Community College", "University of Wyoming"
  ))
  expect_true(all(result$Enrollment_Year == "2023"))

  # University of Wyoming has real FTE reported across all three levels in
  # the fixture (Undergraduate 7465, Graduate 1503, First professional --
  # est_fte is -1/missing but rep_fte is a real 417) -- summing them is the
  # whole point of this function, and specifically requires coalescing to
  # rep_fte for the level where est_fte is missing, not silently dropping it.
  uw <- result[result$Name == "University of Wyoming", ]
  expect_equal(uw$Enrollment, 7465 + 1503 + 417)

  # Casper College only offers undergraduate (its Graduate/First-
  # professional rows are -2 "not applicable" in the real fixture) --
  # Enrollment should be exactly that one real level's FTE, not NA.
  casper <- result[result$Name == "Casper College", ]
  expect_equal(casper$Enrollment, 2192)
})

test_that("parse_ipeds_he_enrollment gives Sheridan and Gillette the same shared Enrollment figure", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2023.csv"))
  result <- parse_ipeds_he_enrollment(df, 2023)

  sheridan <- result[result$Name == "Sheridan College", ]
  gillette <- result[result$Name == "Gillette College", ]
  expect_equal(sheridan$Enrollment, gillette$Enrollment)
  expect_equal(sheridan$Enrollment, 1852)
})

test_that("parse_ipeds_he_enrollment treats IPEDS negative sentinel codes as NA, not real values", {
  # A level_of_study an institution doesn't offer at all (both est_fte and
  # rep_fte negative) must contribute nothing to the sum, not be coerced
  # into 0 counted as a real reported figure or NA'ing out the whole total.
  df <- data.frame(
    unitid = c(240505L, 240505L, 240505L),
    level_of_study = c(1L, 2L, 3L),
    est_fte = c(2192L, -2L, -1L),
    rep_fte = c(2192L, -2L, -2L)
  )
  result <- parse_ipeds_he_enrollment(df, 2023)
  casper <- result[result$Name == "Casper College", ]
  expect_equal(casper$Enrollment, 2192)
})

test_that("parse_ipeds_he_enrollment returns NA (not 0) for an institution with no real FTE data at all", {
  df <- data.frame(
    unitid = c(240505L, 240505L),
    level_of_study = c(1L, 2L),
    est_fte = c(-1L, -2L),
    rep_fte = c(-1L, -2L)
  )
  result <- parse_ipeds_he_enrollment(df, 2023)
  casper <- result[result$Name == "Casper College", ]
  expect_true(is.na(casper$Enrollment))
})

test_that("parse_ipeds_he_enrollment returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_he_enrollment(data.frame(), 2023)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Enrollment", "Enrollment_Year"))
})

# Real fixture: the actual Wyoming (fips=56) response from the same
# endpoint for 2018, captured 2026-08-06 -- used together with the 2023
# fixture above for a real 5-year enrollment-trend comparison.

test_that("compute_enrollment_change computes a real 5-year trend matching independently-verified figures", {
  current_df <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2023.csv"))
  prior_df <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2018.csv"))
  current <- parse_ipeds_he_enrollment(current_df, 2023)
  prior <- parse_ipeds_he_enrollment(prior_df, 2018)

  result <- compute_enrollment_change(current, prior)
  expect_equal(nrow(result), 9)

  # University of Wyoming: 10,878 FTE (2018) -> 9,385 FTE (2023), a real,
  # independently-computed -13.7% decline -- cross-checked by hand against
  # the raw fixtures before this test was written, not derived from the
  # function under test.
  uw <- result[result$Name == "University of Wyoming", ]
  expect_equal(uw$Enrollment_Change_Pct, (9385 - 10878) / 10878, tolerance = 1e-6)

  # Central Wyoming College is the one WY institution that GREW over this
  # window (1,006 -> 1,030 FTE) -- confirms this isn't hardcoded to always
  # be negative.
  cwc <- result[result$Name == "Central Wyoming College", ]
  expect_true(cwc$Enrollment_Change_Pct > 0)
})

test_that("compute_enrollment_change returns an empty, correctly-shaped frame when either side has no rows", {
  some_rows <- data.frame(Name = "Casper College", Enrollment = 2192, Enrollment_Year = "2023")
  expect_equal(nrow(compute_enrollment_change(data.frame(), some_rows)), 0)
  expect_equal(nrow(compute_enrollment_change(some_rows, data.frame())), 0)
  expect_equal(names(compute_enrollment_change(data.frame(), data.frame())), c("Name", "Enrollment_Change_Pct"))
})
