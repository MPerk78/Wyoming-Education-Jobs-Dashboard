# Real fixtures: the actual Wyoming (fips=56) responses from the Urban
# Institute's Education Data Portal fsa/grants/2021 and ipeds/fall-
# enrollment/2021 endpoints, captured 2026-08-06 -- 2021 is FSA's latest
# available year (confirmed live: 2022/2023 both return zero results),
# matched to the SAME year's enrollment fixture rather than the more
# recent 2023 one used elsewhere, per this scraper's own reasoning for
# why the years must line up.

test_that("parse_ipeds_he_pell_share computes a real recipients/enrollment share matching independently-verified figures", {
  grants_df <- read.csv(test_path("fixtures", "fsa_wy_grants_2021.csv"))
  enrollment_raw <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2021.csv"))
  enrollment_df <- parse_ipeds_he_enrollment(enrollment_raw, 2021)

  result <- parse_ipeds_he_pell_share(grants_df, enrollment_df, 2021)
  expect_equal(nrow(result), 9)
  expect_true(all(result$Pell_Year == "2021"))

  # Casper College: 932 real Pell recipients / 2,277 real FTE enrollment --
  # both cross-checked by hand against the raw fixtures before this test
  # was written, not derived from the function under test.
  casper <- result[result$Name == "Casper College", ]
  expect_equal(casper$Pell_Recipient_Share, 932 / 2277, tolerance = 1e-6)

  # University of Wyoming has the lowest share of the 9 (a 4-year flagship,
  # not a community college) -- confirms this isn't a flat/hardcoded value.
  uw <- result[result$Name == "University of Wyoming", ]
  expect_true(uw$Pell_Recipient_Share < casper$Pell_Recipient_Share)
})

test_that("parse_ipeds_he_pell_share gives Sheridan and Gillette the same shared Pell share", {
  grants_df <- read.csv(test_path("fixtures", "fsa_wy_grants_2021.csv"))
  enrollment_raw <- read.csv(test_path("fixtures", "ipeds_wy_fall_enrollment_2021.csv"))
  enrollment_df <- parse_ipeds_he_enrollment(enrollment_raw, 2021)
  result <- parse_ipeds_he_pell_share(grants_df, enrollment_df, 2021)

  sheridan <- result[result$Name == "Sheridan College", ]
  gillette <- result[result$Name == "Gillette College", ]
  expect_equal(sheridan$Pell_Recipient_Share, gillette$Pell_Recipient_Share)
  expect_false(is.na(sheridan$Pell_Recipient_Share))
})

test_that("parse_ipeds_he_pell_share only counts grant_type == 4 (Pell), not other federal grant programs", {
  # A synthetic record with a non-Pell grant_type (e.g. 1 = Academic
  # Competitiveness Grant) and no real grant_type == 4 row for that
  # unitid must NOT be picked up as if it were Pell data.
  grants_df <- data.frame(
    unitid = 240505L, grant_type = 1L, grant_recipients_unitid = 999
  )
  enrollment_df <- data.frame(Name = "Casper College", Enrollment = 2277, Enrollment_Year = "2021")
  result <- parse_ipeds_he_pell_share(grants_df, enrollment_df, 2021)
  casper <- result[result$Name == "Casper College", ]
  expect_true(is.na(casper$Pell_Recipient_Share))
})

test_that("parse_ipeds_he_pell_share returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ipeds_he_pell_share(data.frame(), data.frame(), 2021)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Name", "Pell_Recipient_Share", "Pell_Year"))

  result2 <- parse_ipeds_he_pell_share(
    data.frame(unitid = 240505L, grant_type = 4L, grant_recipients_unitid = 932),
    data.frame(), 2021
  )
  expect_equal(nrow(result2), 0)
})
