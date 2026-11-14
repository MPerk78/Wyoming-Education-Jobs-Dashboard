# State-only tests left here when the shared classifier and posted-pay tests moved to edjobs
# (phase 2, 2026-11-11): these call this state's own code (name corrections or scrapers).


test_that("canonicalize_k12_district fixes known typos and passes through everything else", {
  expect_equal(canonicalize_k12_district("Hot Springs School District 1"), "Hot Springs County School District 1")
  expect_equal(canonicalize_k12_district("Crook County school District 1"), "Crook County School District 1")
  expect_equal(canonicalize_k12_district("Albany Count School District 1"), "Albany County School District 1")
  expect_equal(canonicalize_k12_district("Campbell County Scholl District 1"), "Campbell County School District 1")
  expect_equal(canonicalize_k12_district("Converse County School Distrcit 2"), "Converse County School District 2")
  expect_equal(canonicalize_k12_district("Sheridan County School District 1"), "Sheridan County School District 1")
})
