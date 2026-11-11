# Run expectations: the logic moved to the edjobs package (phase 0 of the R
# package migration, 2026-11-10; its tests live there). This repo keeps two
# checks: the installed edjobs is the version EDJOBS_VERSION pins, and the
# committed run_expectations.csv is well formed (a typo fails here, before the
# weekly run, rather than during it).

test_that("the installed edjobs is the version EDJOBS_VERSION pins", {
  expect_equal(as.character(utils::packageVersion("edjobs")), trimws(readLines(here::here("EDJOBS_VERSION"))))
})

test_that("the committed run_expectations.csv is well formed", {
  path <- here::here("run_expectations.csv")
  skip_if_not(file.exists(path))
  exp <- edjobs::read_run_expectations(path)
  expect_equal(edjobs::validate_run_expectations(exp, root = here::here()), character(0))
})
