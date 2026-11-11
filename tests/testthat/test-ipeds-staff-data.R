# This state's committed IPEDS staff salary file (the code that builds it, and
# its parsing tests, are in edjobs since phase 2, 2026-11-11).

test_that("the committed staff salary file joins to salarymap.csv", {
  app_dir <- if (dir.exists(here::here("Mt_Ed_Jobs"))) "Mt_Ed_Jobs" else "Wy_Ed_Jobs"
  staff <- read.csv(here::here(app_dir, "ipeds_staff_salaries.csv"))
  sm <- read.csv(here::here(app_dir, "salarymap.csv"))
  expect_gt(nrow(staff), 0)
  expect_true(all(staff$IPEDS_UNITID %in% sm$IPEDS_UNITID))
  expect_false(any(duplicated(paste(staff$IPEDS_UNITID, staff$Staff_Category))))
})
