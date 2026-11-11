# This state's committed OEWS file (the BLS OEWS code and its other tests
# moved to the edjobs package, phase 1 of the R package migration).

test_that("the committed OEWS file covers every area and occupation", {
  st <- if (dir.exists(here::here("Mt_Ed_Jobs"))) "MT" else "WY"
  app_dir <- if (st == "MT") "Mt_Ed_Jobs" else "Wy_Ed_Jobs"
  w <- read.csv(here::here(app_dir, "oews_role_wages.csv"), colClasses = c(Area_Code = "character", SOC = "character"))
  expect_setequal(unique(w$Area_Code), oews_state_areas(st)$code)
  expect_setequal(unique(w$SOC), OEWS_OCCUPATIONS$soc)
  # Statewide figures are never suppressed, so every role has a fallback.
  state <- w[w$Area_Code == oews_state_areas(st)$code[1], ]
  expect_false(any(is.na(state$Annual_Median)))
  expect_true(all(w$Annual_Median > 15000 & w$Annual_Median < 250000, na.rm = TRUE))
})
