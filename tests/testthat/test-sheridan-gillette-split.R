# Real fixture: the actual Wyoming (fips=56) response from the Urban
# Institute's Education Data Portal college-university/ipeds/directory/2022
# endpoint, captured 2026-08-05 -- all 10 WY-registered postsecondary
# institutions IPEDS tracks, not synthetic. No split has happened yet, so
# this fixture can only exercise the "still joint" path; the "split
# detected" tests below use a minimal synthetic frame instead, since a real
# post-split fixture doesn't exist to capture.

test_that("check_sheridan_gillette_still_joint finds nothing when they're still one shared entity", {
  df <- read.csv(test_path("fixtures", "ipeds_wy_college_directory_2022.csv"))
  result <- check_sheridan_gillette_still_joint(df)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("unitid", "inst_name"))
})

test_that("check_sheridan_gillette_still_joint detects a new unitid once IPEDS starts reporting them separately", {
  split_df <- data.frame(
    unitid = c(240666L, 240666L, 999999L),
    inst_name = c("Northern Wyoming Community College District", "Sheridan College", "Gillette College"),
    stringsAsFactors = FALSE
  )
  result <- check_sheridan_gillette_still_joint(split_df)

  expect_equal(nrow(result), 1)
  expect_equal(result$unitid, 999999L)
  expect_equal(result$inst_name, "Gillette College")
})

test_that("check_sheridan_gillette_still_joint ignores unrelated new WY institutions", {
  df <- data.frame(
    unitid = c(240666L, 555555L),
    inst_name = c("Northern Wyoming Community College District", "Some New Trade School"),
    stringsAsFactors = FALSE
  )
  result <- check_sheridan_gillette_still_joint(df)
  expect_equal(nrow(result), 0)
})

test_that("check_sheridan_gillette_still_joint returns an empty, correctly-shaped frame when given no rows", {
  result <- check_sheridan_gillette_still_joint(data.frame())
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("unitid", "inst_name"))
})
