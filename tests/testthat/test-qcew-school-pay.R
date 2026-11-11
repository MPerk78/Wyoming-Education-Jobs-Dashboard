# This state's committed QCEW county pay file (the QCEW code and its other tests
# moved to the edjobs package, phase 1 of the R package migration).

test_that("the committed county pay file joins to the districts' counties", {
  app_dir <- if (dir.exists(here::here("Mt_Ed_Jobs"))) "Mt_Ed_Jobs" else "Wy_Ed_Jobs"
  q <- read.csv(here::here(app_dir, "qcew_county_school_pay.csv"))
  sm <- read.csv(here::here(app_dir, "salarymap2.csv"))
  expect_gt(nrow(q), 0)
  expect_gt(sum(qcew_county_key(sm$County) %in% q$County), 0)
})
