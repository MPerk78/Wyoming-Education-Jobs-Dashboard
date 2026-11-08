# Detail riding in a title (split_title_detail() / move_title_detail(),
# k12_he_classification.R): the title-shape lint's findings of 2026-11-05.

test_that("requirements, pay, status notes and file names are cut from the title", {
  s <- split_title_detail(c(
    "Bus Drivers- Must have a CDL with a school bus passenger endorsement.",   # Huntley Project
    "Secretary – $14 per hour, up to 32 hours per week",                  # Turner
    "Special Education Teachers - Open Until Filled.",                         # Fremont #21
    "Physical Education Teacher K - 5 (Open Until Filled)",
    "Northern Arapaho Language Teacher - Secondary - Open Until Filled",
    "Dish_Server_Cashier 26 - Copy",                                           # Livingston
    "Baker_Cook 2026 27"))
  expect_equal(s$title, c("Bus Drivers", "Secretary", "Special Education Teachers", "Physical Education Teacher K - 5",
                          "Northern Arapaho Language Teacher - Secondary", "Dish Server Cashier", "Baker Cook"))
  expect_equal(s$detail[1:3], c("Must have a CDL with a school bus passenger endorsement",
                                "$14 per hour, up to 32 hours per week", "Open Until Filled"))
})

test_that("ordinary titles with dashes, commas and parentheses are left alone", {
  t <- c("Teacher - K-5", "Custodian, Full-Time", "Para - Special Education", "Assistant Coach (Girls)",
         "High School Math Teacher (2026-27)", "Food Service - Cook", NA)
  s <- split_title_detail(t)
  expect_equal(s$title, t)
  expect_true(all(is.na(s$detail)))
})

test_that("pay cut from a title fills Posted Pay only where the row has none", {
  d <- data.frame(title = c("Secretary – $14 per hour, up to 32 hours per week", "Coach - $2,500 stipend"),
                  Pay_Min = c(NA, 3000), stringsAsFactors = FALSE)
  d$Pay_Max <- d$Pay_Min; d$Pay_Unit <- c(NA, "stipend"); d$Pay_Text <- c(NA, "$3,000")
  x <- move_title_detail(d)
  expect_equal(x$title, c("Secretary", "Coach"))
  expect_equal(x$Pay_Min, c(14, 3000))
  expect_equal(x$Pay_Unit, c("hour", "stipend"))
  # No pay found: no pay columns added (WY archives carry none most weeks).
  y <- move_title_detail(data.frame(title = "Special Education Teachers - Open Until Filled.", stringsAsFactors = FALSE))
  expect_false("Pay_Min" %in% names(y))
})

test_that("WSBA's 'Cabon County School District 2' is Carbon #2, in the WSBA parser too", {
  expect_equal(canonicalize_k12_district("Cabon County School District 2"), "Carbon County School District 2")
  expect_equal(canonicalize_wsba_district("Cabon County School District #2"), "Carbon County School District 2")
})
