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
               c(12L, NA, 6L, 7L, 4L, 9L, 8L, 10L, 13L, 3L, NA, NA, NA))
})

test_that("only senior titles get the management average; managers fall back to their function", {
  # Real titles from the 2026-10 postings. IPEDS pools every manager, so a director or
  # assistant director would get roughly VP pay (UW salary book: director median $106K,
  # assistant director $82K, manager $66K vs. the $159K management average).
  senior <- c("Executive Vice President for Academic Affairs and Provost", "Dean of Liberal Arts & Social Science",
              "Chief Information Officer (CIO)", "Vice President of Academic Affairs", "Executive Director, QCORE",
              "WPBS Foundation Executive Director", "Associate Dean of Students", "Senior Director, Research")
  expect_equal(ipeds_staff_category_for_title(senior), rep(5L, length(senior)))
  junior <- c("Director of Employee Experience", "Adult Education Director", "Facilities Director",
              "Assistant Director of Admissions", "Assistant Chief of Police", "Project Manager I",
              "IT Project Manager", "Payroll Manager", "Custodial Supervisor", "Clinic Director")
  # Campus police are "service" in IPEDS.
  expect_equal(ipeds_staff_category_for_title(junior), c(NA, NA, NA, NA, 10L, NA, 7L, 6L, 10L, 9L))
})

test_that("hourly, student and intern postings get no staff category; executive assistants are office support", {
  # Real titles that were given annual averages (UW, LCCC, Gillette, MSU-Northern).
  titles <- c("Hourly Pooled - Broadcast Studio Manager, Student Media", "Work Study - Office Aide, Accounting",
              "FWS Grounds Worker (Student)", "Student Staff, IT Pathway Technical Support",
              "Hourly Pooled - Marketing Intern, Website - Institutional Advancement",
              "Monthly Pooled -  Apartment Live-In Assistant, Housing", "Executive Assistant to the President",
              "Student Success Coordinator")
  expect_equal(ipeds_staff_category_for_title(titles), c(NA, NA, NA, NA, NA, NA, 12L, NA))
  expect_equal(is_salaried_staff_posting(titles), c(rep(FALSE, 6), TRUE, TRUE))
})

test_that("managers get their function's category where the title names one", {
  # Real titles from the 2026-11-01 postings that had no estimate.
  titles <- c("Business Manager - UW Extension", "Business Operations Manager", "Facilities Manager",
              "Lab Manager", "Senior Technology Manager", "Timekeeping and Workforce Systems Manager",
              "Assistant Fitness Center Manager")
  expect_equal(ipeds_staff_category_for_title(titles), c(6L, 6L, 13L, 7L, 7L, 6L, 8L))
  # The same words in other titles: instructional technology and writing labs are
  # education services, a research "Facility" isn't facilities maintenance, and
  # technology transfer or titles naming no function get none.
  others <- c("Instructional Technology Specialist", "Instructional Specialist, Writing Lab",
              "Technician, Master - Zoology & Physiology/Red Buttes Facility",
              "Licensing Manager - Technology Transfer Office", "Senior Project Manager", "Floor Supervisor",
              "Proto-Lab and Testbed Manager, Research Administration", "Sonography Lab Assistant")
  expect_equal(ipeds_staff_category_for_title(others), c(4L, 4L, NA, NA, NA, NA, 7L, NA))
})

test_that("part-time, temporary, seasonal and student-position jobs get no annual staff figure", {
  # Real titles that showed a full-time annual average (MSU, LCCC, Sheridan, SKC, FVCC, Western, Gillette).
  titles <- c("Cook I - Part Time", "Grounds Worker (Part-time & Student)", "Tutor, Learning Commons (Part-Time + Student)",
              "(Student Position) Peer Tutor", "Campus Activities Programmer (Student)", "Temp, Clinical Resource RN",
              "Seasonal Grounds Maintenance Technician (Part-time)", "Bus Drivers - Part Time",
              "Fitness Center Attendant")
  expect_true(all(is.na(ipeds_staff_category_for_title(titles))))
  expect_false(any(is_salaried_staff_posting(titles)))
  # "Student" as the office served, not the worker, is still salaried staff.
  expect_true(all(is_salaried_staff_posting(c("Student Activities Officer I - Wyoming Union", "Student Success Coordinator"))))
})

test_that("attach_ipeds_staff_estimates fills Est_* from the institution's category average", {
  staff <- data.frame(IPEDS_UNITID = c(1L, 1L), Staff_Category = c(5L, 12L),
                      Category_Label = c("management", "office and administrative support"),
                      Staff_Count = c(10, 20), Avg_Salary = c(90000, 45000), Year = "2024")
  postings <- data.frame(Title = c("Dean of Students", "Office Assistant", "Assistant Professor", "Custodian"),
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
