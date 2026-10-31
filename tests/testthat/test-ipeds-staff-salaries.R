# ipeds_staff_salaries.R: IPEDS average pay for full-time non-faculty staff
# by category, the labeled fallback for higher-ed staff postings that state no
# pay. Fixture: a real Urban Institute API response for this state, 2024.

staff_fixture <- function() {
  f <- list.files(test_path("fixtures"), pattern = "^ipeds_staff_salaries_.*\\.json$", full.names = TRUE)[1]
  jsonlite::fromJSON(paste(readLines(f, warn = FALSE), collapse = "\n"))$results
}

test_that("parse_ipeds_staff_salaries averages outlays over staff, for the given institutions only", {
  raw <- staff_fixture()
  unitids <- unique(raw$unitid)[1:3]
  out <- parse_ipeds_staff_salaries(raw, unitids, 2024)
  expect_true(all(out$IPEDS_UNITID %in% unitids))
  expect_true(all(out$Staff_Count >= IPEDS_STAFF_MIN_COUNT))
  # The 99 "Total" row and categories with no staff are dropped.
  expect_true(all(as.character(out$Staff_Category) %in% names(IPEDS_STAFF_CATEGORIES)))
  r <- out[1, ]
  src <- raw[raw$unitid == r$IPEDS_UNITID & raw$staff_category == r$Staff_Category, ]
  expect_equal(r$Avg_Salary, round(src$salary_outlays / src$noninstruc_staff_count))
  expect_true(all(out$Avg_Salary > 15000 & out$Avg_Salary < 400000))
  expect_equal(unique(out$Year), "2024")
})

test_that("Montana State's management and office-support averages match the source by hand", {
  raw <- staff_fixture()
  skip_if_not(180461 %in% raw$unitid, "Montana fixture only")
  out <- parse_ipeds_staff_salaries(raw, 180461, 2024)
  expect_equal(out$Avg_Salary[out$Staff_Category == 5], 103808)
  expect_equal(out$Avg_Salary[out$Staff_Category == 12], 54566)
})

test_that("ipeds_staff_category_for_title maps staff titles and leaves faculty out", {
  titles <- c("Administrative Assistant to the Director", "Director of Admissions", "Staff Accountant",
              "Network Administrator", "Academic Advisor", "Athletic Trainer", "Head Volleyball Coach",
              "Custodian", "HVAC Technician", "Extension Agent - Fergus County", "Assistant Professor of Biology",
              "Adjunct Instructor, English", "Program Coordinator II")
  expect_equal(ipeds_staff_category_for_title(titles),
               c(12L, 5L, 6L, 7L, 4L, 9L, 8L, 10L, 13L, 3L, NA, NA, NA))
})

test_that("attach_ipeds_staff_estimates fills Est_* from the institution's category average", {
  staff <- data.frame(IPEDS_UNITID = c(1L, 1L), Staff_Category = c(5L, 12L),
                      Category_Label = c("management", "office and administrative support"),
                      Staff_Count = c(10, 20), Avg_Salary = c(90000, 45000), Year = "2024")
  postings <- data.frame(Title = c("Director of Housing", "Office Assistant", "Assistant Professor", "Custodian"),
                         Institution = "Test College")
  out <- attach_ipeds_staff_estimates(postings, staff, c(1L, 1L, 1L, 1L))
  # Faculty get none; the custodian's category (service) isn't in the table.
  expect_equal(out$Est_Pay, c(90000, 45000, NA, NA))
  expect_equal(out$Est_Occupation, c("management", "office and administrative support", NA, NA))
  expect_equal(out$Est_Area, c("Test College", "Test College", NA, NA))
  expect_equal(out$Est_Pay_Unit, c("year", "year", NA, NA))
})

test_that("the committed staff salary file joins to salarymap.csv", {
  app_dir <- if (dir.exists(here::here("Mt_Ed_Jobs"))) "Mt_Ed_Jobs" else "Wy_Ed_Jobs"
  staff <- read.csv(here::here(app_dir, "ipeds_staff_salaries.csv"))
  sm <- read.csv(here::here(app_dir, "salarymap.csv"))
  expect_gt(nrow(staff), 0)
  expect_true(all(staff$IPEDS_UNITID %in% sm$IPEDS_UNITID))
  expect_false(any(duplicated(paste(staff$IPEDS_UNITID, staff$Staff_Category))))
})
