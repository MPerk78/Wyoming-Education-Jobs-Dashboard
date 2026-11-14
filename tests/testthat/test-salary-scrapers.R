# Real fixtures: the actual WSBA "2025-2026 Teacher Salary Settlements"
# and "2025-2026 Central Office Staff" PDFs, downloaded 2026-08-04. Both
# are real structured PDF tables, not synthetic -- these tests pin the
# word-position-based parsing against known-correct values checked by eye
# against the rendered PDF.

test_that("canonicalize_wsba_district_name handles the standard pattern and the two exceptions", {
  expect_equal(canonicalize_wsba_district_name("Albany #1"), "Albany County School District 1")
  expect_equal(canonicalize_wsba_district_name("Sweetwater #2"), "Sweetwater County School District 2")
  # Big Horn and Natrona don't take "County" in this project's canonical
  # naming, unlike the other 46 districts.
  expect_equal(canonicalize_wsba_district_name("Big Horn #3"), "Big Horn School District 3")
  expect_equal(canonicalize_wsba_district_name("Natrona #1"), "Natrona School District 1")
})

test_that("canonicalize_wsba_district_name is vectorized (regression: used to return only the first match, recycled)", {
  result <- canonicalize_wsba_district_name(c("Albany #1", "Big Horn #2", "Carbon #1"))
  expect_equal(result, c("Albany County School District 1", "Big Horn School District 2",
                          "Carbon County School District 1"))
})

test_that("canonicalize_wsba_district_name returns NA for text that isn't a '<name> #<n>' district line", {
  expect_true(is.na(canonicalize_wsba_district_name("District")))
  expect_true(is.na(canonicalize_wsba_district_name("2024-2025")))
})

test_that("extract_wsba_salary_year reads the year off the PDF's own title instead of guessing from today's date", {
  suppressMessages(library(pdftools))
  page <- pdf_data(test_path("fixtures", "wsba_teacher_salary_settlements.pdf"))[[1]]
  expect_equal(extract_wsba_salary_year(page), "2025-2026")
})

test_that("parse_wsba_teacher_salary extracts all 48 districts with correct base salaries", {
  suppressMessages(library(pdftools))
  page <- pdf_data(test_path("fixtures", "wsba_teacher_salary_settlements.pdf"))[[1]]
  result <- parse_wsba_teacher_salary(page)

  expect_equal(nrow(result), 48)
  expect_equal(length(unique(result$District)), 48)

  albany <- result[result$District == "Albany County School District 1", ]
  expect_equal(albany$Base_Salary_Prior_Year, 47500)
  expect_equal(albany$Base_Salary_Current_Year, 51500)

  # Regression: these three specific rows previously leaked digits from a
  # neighboring wrapped-note cell into the base salary column (a too-wide
  # column bin catching a stray token near the boundary).
  natrona <- result[result$District == "Natrona School District 1", ]
  expect_equal(natrona$Base_Salary_Prior_Year, 52000)
  sheridan1 <- result[result$District == "Sheridan County School District 1", ]
  expect_equal(sheridan1$Base_Salary_Prior_Year, 48401)
  laramie1 <- result[result$District == "Laramie County School District 1", ]
  expect_equal(laramie1$Base_Salary_Current_Year, 53250)

  # Genuinely blank cells in the source PDF (confirmed against the
  # rendered table) should stay NA, not get a value from a neighboring row.
  big_horn4 <- result[result$District == "Big Horn School District 4", ]
  expect_true(is.na(big_horn4$Base_Salary_Current_Year))
  teton <- result[result$District == "Teton County School District 1", ]
  expect_true(is.na(teton$Base_Salary_Prior_Year))
  expect_equal(teton$Base_Salary_Current_Year, 68369)
})

test_that("fetch_wsba_superintendent_salary extracts all 48 districts, deduplicated across the PDF's repeated pages", {
  result <- fetch_wsba_superintendent_salary(test_path("fixtures", "wsba_admin_salary_spreadsheet.pdf"))

  expect_equal(nrow(result), 48)
  expect_equal(length(unique(result$District)), 48)

  albany <- result[result$District == "Albany County School District 1", ]
  expect_equal(albany$Superintendent_Salary, 174000)
  expect_equal(albany$Superintendent_Contract_Days, 260)

  # Genuinely blank in the source (confirmed against the rendered table).
  campbell <- result[result$District == "Campbell County School District 1", ]
  expect_true(is.na(campbell$Superintendent_Salary))
  expect_true(is.na(campbell$Superintendent_Contract_Days))
})

test_that("needs_k12_salary_archive_update flags a genuinely new year and skips an already-recorded one", {
  expect_true(needs_k12_salary_archive_update(c("2024-2025"), "2025-2026"))
  expect_false(needs_k12_salary_archive_update(c("2024-2025", "2025-2026"), "2025-2026"))
  expect_false(needs_k12_salary_archive_update(character(0), NA_character_))
})

test_that("archive_k12_salary_snapshot creates the archive on first run and appends on a new year", {
  history_path <- withr::local_tempfile(fileext = ".csv")

  salarymap2_y1 <- data.frame(
    District = c("Albany County School District 1", "Big Horn School District 1"),
    Salary_Year = "2025-2026",
    Teacher_Base_Salary = c(51500, 56750),
    Superintendent_Salary = c(174000, 152140),
    stringsAsFactors = FALSE
  )

  expect_true(archive_k12_salary_snapshot(salarymap2_y1, history_path))
  archived <- read.csv(history_path, stringsAsFactors = FALSE)
  expect_equal(nrow(archived), 2)
  expect_equal(unique(archived$Salary_Year), "2025-2026")

  # Same year again (e.g. next week's pipeline run) -- no-op, no duplicate rows.
  expect_false(archive_k12_salary_snapshot(salarymap2_y1, history_path))
  archived_again <- read.csv(history_path, stringsAsFactors = FALSE)
  expect_equal(nrow(archived_again), 2)

  # A genuinely new year -- appended, old year's rows preserved.
  salarymap2_y2 <- salarymap2_y1
  salarymap2_y2$Salary_Year <- "2026-2027"
  salarymap2_y2$Teacher_Base_Salary <- c(53000, 58500)

  expect_true(archive_k12_salary_snapshot(salarymap2_y2, history_path))
  archived_final <- read.csv(history_path, stringsAsFactors = FALSE)
  expect_equal(nrow(archived_final), 4)
  expect_setequal(unique(archived_final$Salary_Year), c("2025-2026", "2026-2027"))
})

test_that("archive_k12_salary_snapshot skips archiving when Salary_Year isn't a single consistent value", {
  history_path <- withr::local_tempfile(fileext = ".csv")
  inconsistent <- data.frame(
    District = c("Albany County School District 1", "Big Horn School District 1"),
    Salary_Year = c("2025-2026", "2026-2027"),
    Teacher_Base_Salary = c(51500, 56750),
    Superintendent_Salary = c(174000, 152140),
    stringsAsFactors = FALSE
  )
  expect_false(archive_k12_salary_snapshot(inconsistent, history_path))
  expect_false(file.exists(history_path))
})
