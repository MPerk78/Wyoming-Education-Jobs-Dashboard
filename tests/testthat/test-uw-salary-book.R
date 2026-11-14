# uw_salary_book.R: UW's published salary book (department, title, salary)
# as the pay fallback for UW staff postings. Fixtures: pdftools::pdf_text()
# of five real pages of the FY2026 "Salary Information - Departmental
# Listing" PDF (created 2025-10-23; pages 1, 2, 10, 21 and 27, which hold a
# wrapped department name and wrapped titles), and the operating-budget page
# that links it, saved 2026-10-31.

uw_book_pages <- function() {
  txt <- paste(readLines(test_path("fixtures", "uw_salary_book_fy26_pages_2025-10-23.txt"), warn = FALSE), collapse = "\n")
  strsplit(txt, "\f")[[1]]
}

test_that("uw_salary_book_url finds the newest book on the operating-budget page", {
  html <- paste(readLines(test_path("fixtures", "uw_operating_budget_2026-10-31.html"), warn = FALSE), collapse = "\n")
  expect_equal(uw_salary_book_url(html),
               "https://www.uwyo.edu/budget-finance/_files/budget-and-institutional-planning/docs/budget/supporting-documents/dept-salary-fy26.pdf")
  expect_error(uw_salary_book_url("<html><a href='x.pdf'>Budget</a></html>"), "Salary Information")
})

test_that("parse_uw_salary_book reproduces every department total printed in the book", {
  pages <- uw_book_pages()
  book <- parse_uw_salary_book(pages)
  expect_equal(unique(book$Fiscal_Year), "FY2026")
  lines <- unlist(strsplit(pages, "\n"))
  printed <- as.numeric(gsub(",", "", sub(".*Total\\s+([0-9,]+)\\s*$", "\\1", grep("\\sTotal\\s+[0-9,]+\\s*$", lines, value = TRUE))))
  # 15 of the 19 printed totals match exactly. Three departments run across
  # the gaps between these non-consecutive pages, and the book's own
  # Intercollegiate Athletics Directors Office total (1,251,336) includes
  # $275,004 not listed as a row. In the full FY2026 book every other
  # department (245 of 246) matches.
  sums <- tapply(book$Salary, factor(book$Department, levels = unique(book$Department)), sum)
  expect_equal(length(printed), 19)
  expect_equal(sum(printed %in% sums), 15)
  expect_false(1251336 %in% sums)
  expect_false(any(is.na(book$Department)))
})

test_that("parse_uw_salary_book joins wrapped titles and wrapped department names", {
  book <- parse_uw_salary_book(uw_book_pages())
  expect_true("Assistant Director, Event Management & Game Operations" %in% book$Title)
  expect_false("Operations" %in% book$Title)
  calsnr <- book[book$Department == "College of Agriculture, Life Sciences & Natural Resources Deans Office", ]
  expect_equal(nrow(calsnr), 10)
  expect_equal(sum(calsnr$Salary), 1071607)
  expect_false(any(book$Department == "College of Agriculture, Life Sciences & Natural Resources Deans"))
})

test_that("normalize_uw_title spells out the book's abbreviations", {
  expect_equal(normalize_uw_title(c("Asst Dir, Business Ops", "Assistant Director, Business Operations")),
               rep("assistant director, business operations", 2))
  expect_equal(normalize_uw_title("Computer Support Specialist,Sr"), "computer support specialist, senior")
  expect_equal(uw_posting_role("Business Manager - UW Extension"), "Business Manager")
  expect_equal(uw_posting_role("Research Associate II (Smith Lab) - SAREC"), "Research Associate II")
})

test_that("uw_salary_book_estimate uses the same title, then the same role, with at least 3 holders", {
  book <- data.frame(
    Title = c("Business Manager", "Business Manager", "Business Manager",
              "Asst Director, Admissions", "Assistant Director, Housing", "Assistant Director, Dining",
              "Licensing Manager", "Licensing Manager"),
    Salary = c(55000, 60000, 70000, 58000, 62000, 90000, 100000, 110000))
  out <- uw_salary_book_estimate(c("Business Manager - UW Extension", "Assistant Director, American Heritage Center",
                                   "Licensing Manager - Technology Transfer Office"), book)
  expect_equal(out$Est_Pay, c(60000, 62000, NA))
  expect_equal(out$Matched_Title, c("Business Manager", "Assistant Director", NA))
  expect_equal(out$Staff_Count, c(3, 3, NA))
})

test_that("attach_uw_salary_book_estimates replaces UW figures and blanks UW's inflated IPEDS management", {
  book <- data.frame(Title = rep("Business Manager", 3), Salary = c(55000, 60000, 70000), Fiscal_Year = "FY2026")
  postings <- data.frame(
    Institution = c("University of Wyoming", "University of Wyoming", "University of Wyoming", "Casper College"),
    Title = c("Business Manager - UW Extension", "State Director - Small Business Development Center",
              "Hourly Pooled - Branding Iron Editor, Student Media", "Business Manager"),
    Est_Pay = c(159084, 159084, NA, 99000), Est_Pay_Unit = c("year", "year", NA, "year"),
    Est_Occupation = c("management", "management", NA, "management"),
    Est_Area = c("University of Wyoming", "University of Wyoming", NA, "Casper College"),
    Est_Year = c("2024", "2024", NA, "2024"))
  out <- attach_uw_salary_book_estimates(postings, book)
  expect_equal(out$Est_Pay, c(60000, NA, NA, 99000))
  expect_equal(out$Est_Source, c("UW salary book", NA, NA, "IPEDS"))
  expect_equal(out$Est_Occupation[1], "Business Manager (3 UW staff)")
  expect_equal(out$Est_Year[1], "FY2026")
})

test_that("the committed UW salary book is a full parse", {
  book <- read.csv(here::here("Wy_Ed_Jobs", "uw_salary_book.csv"), stringsAsFactors = FALSE)
  expect_gt(nrow(book), 2500)
  expect_setequal(names(book), c("Department", "Title", "Salary", "Fiscal_Year", "Source_URL"))
  expect_true(all(book$Salary > 10000 & book$Salary < 5e6))
  expect_equal(length(unique(book$Fiscal_Year)), 1)
})
