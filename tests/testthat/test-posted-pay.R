# extract_posted_pay(): the pay a posting itself states. Unit tests use
# wording copied from real AppliTrack postings (Great Falls, Billings, MCPS,
# Helena, Sweetwater); the parser test uses a real captured Output.asp page.

test_that("extract_posted_pay reads amounts, ranges and units as written", {
  p <- extract_posted_pay(c(
    "LOCATION: Great Falls High School SALARY: $15.14 per hour (25/26 Salary Schedule)",
    "EMPLOYMENT: 2026/2027 SALARY: $48,300-$73,957 (2026/2027 salary schedule)",
    "7.5 hrs/day$21.46 to $22.81 per hour, depending on experience",
    "Starts: August 2026Stipend: $3,548.00Required Coaches Certificates",
    "Long Term Substitutes are paid at $250.00 per day and issued a contract.",
    "Salary: $160.00/Whole Day, $80.00/Half Day",
    "6:00am - 2:30pm $20.60* - $21.95*/hr depending on experience",
    "Part Time, at most 29 hours per weekSalary: P-V, $25.69",
    "BEA Salary Schedule; Full Benefits",
    NA
  ))
  expect_equal(p$Pay_Min, c(15.14, 48300, 21.46, 3548, 250, 160, 20.60, 25.69, NA, NA))
  expect_equal(p$Pay_Max, c(15.14, 73957, 22.81, 3548, 250, 160, 21.95, 25.69, NA, NA))
  expect_equal(p$Pay_Unit, c("hour", "year", "hour", "stipend", "day", "day", "hour", "hour", NA, NA))
  expect_true(all(is.na(p$Pay_Text) == is.na(p$Pay_Min)))
})

test_that("extract_posted_pay ignores bonuses, fees and differentials", {
  p <- extract_posted_pay(c(
    "*We offer a one-time $1,000 signing bonus. Starting salary will be a minimum of $46,225 to max of $86,400 DOE",
    "*Includes a $0.50 per hour differential. Pay: $20.60 per hour",
    "Application fee of $25 required."
  ))
  expect_equal(p$Pay_Min, c(46225, 20.60, NA))
  expect_equal(p$Pay_Max, c(86400, 20.60, NA))
})

test_that("parse_applitrack_output carries each posting's stated pay", {
  path <- test_path("fixtures", "applitrack_output_park1_pay.bin")
  js <- iconv(readChar(path, file.size(path), useBytes = TRUE), "WINDOWS-1252", "UTF-8")
  out <- parse_applitrack_output(js)
  expect_true(all(c("Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text") %in% names(out)))
  expect_gt(nrow(out), 0)
  expect_gte(mean(!is.na(out$Pay_Min)), 0.9)
  ok <- !is.na(out$Pay_Min)
  expect_true(all(out$Pay_Min[ok] <= out$Pay_Max[ok]))
  expect_true(all(out$Pay_Unit[ok] %in% c("hour", "day", "month", "year", "credit hour", "stipend", NA)))
  expect_true(all(mapply(function(txt, v) grepl(format(v, big.mark = ",", nsmall = 0, scientific = FALSE), gsub("\\.00\\b", "", txt), fixed = TRUE) || grepl(sprintf("%.2f", v), txt, fixed = TRUE),
                         out$Pay_Text[ok], out$Pay_Min[ok])))
  expect_equal(ncol(parse_applitrack_output("")), ncol(out))
})

test_that("extract_posted_pay skips a benefits package value and a $0.00 blank pay field", {
  # Real FVCC (Paycom) and LCCC (NEOGOV) wording, 2026-10-30.
  p <- extract_posted_pay(c(
    "Benefits eligible Unlock Exceptional Benefits at FVCC (Worth ~$25K) Our employees",
    "Adjunct - $0.00 Hourly",
    "Full-time, benefits eligible, $45,000 per year",
    "Salary: $52,000 plus benefits"
  ))
  expect_equal(p$Pay_Min, c(NA, NA, 45000, 52000))
  expect_equal(p$Pay_Unit, c(NA, NA, "year", "year"))
})

test_that("ensure_posted_pay_columns pads a zero-row frame", {
  out <- ensure_posted_pay_columns(data.frame(Title = character(0)))
  expect_equal(names(out), c("Title", "Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text"))
  expect_equal(nrow(out), 0)
  expect_type(out$Pay_Min, "double")
})

# --- Higher ed boards ---------------------------------------------------------

test_that("parse_neogov_html carries each card's pay line, or its description's pay", {
  html <- paste(readLines(test_path("fixtures", "neogov_cwc.html"), warn = FALSE), collapse = "\n")
  out <- parse_neogov_html(html, "https://www.schooljobs.com")
  expect_true(all(POSTED_PAY_COLUMNS %in% names(out)))
  expect_equal(out$Pay_Min, c(775, NA, NA, NA, 61356, 77411, 21093, 50248, 15, NA))
  expect_equal(out$Pay_Max, c(775, NA, NA, NA, 76695, 87088, 22953, 62810, 17, NA))
  expect_equal(out$Pay_Unit, c("credit hour", NA, NA, NA, "year", "year", "year", "year", "hour", NA))
  expect_equal(out$Pay_Text[5], "Full-Time - $61,356.00 - $76,695.00 Annually")
  # "Adjunct" states pay only in its description preview.
  expect_match(out$Pay_Text[1], "Current adjunct pay is $775 per credit hour", fixed = TRUE)
})

test_that("parse_neogov_html treats NEOGOV's $0.00 pay field as no pay stated", {
  # LCCC (governmentjobs.com), 2026-10-30: adjunct cards read "Adjunct - $0.00 Hourly".
  html <- paste(readLines(test_path("fixtures", "neogov_lcccwy_2026-10-30.html"), warn = FALSE), collapse = "\n")
  out <- parse_neogov_html(html, "https://www.governmentjobs.com")
  expect_equal(nrow(out), 10)
  expect_equal(out$Pay_Min, c(NA, NA, NA, NA, NA, NA, 33.39, 33.39, 33.39, NA))
  expect_equal(out$Pay_Unit, c(NA, NA, NA, NA, NA, NA, "hour", "hour", "hour", NA))
  expect_equal(out$Pay_Text[8], "Part-Time - $33.39 Hourly")
  empty <- parse_neogov_html("<html></html>", "https://www.schooljobs.com")
  expect_equal(nrow(empty), 0)
  expect_true(all(POSTED_PAY_COLUMNS %in% names(empty)))
})

test_that("parse_neogov_html keeps every posting when the card pay can't be read", {
  html <- paste(readLines(test_path("fixtures", "neogov_cwc.html"), warn = FALSE), collapse = "\n")
  env <- environment(parse_neogov_html)
  real <- get("parse_neogov_card_pay", envir = env)
  assign("parse_neogov_card_pay", function(...) stop("markup changed"), envir = env)
  withr::defer(assign("parse_neogov_card_pay", real, envir = env))
  out <- suppressMessages(parse_neogov_html(html, "https://www.schooljobs.com"))
  expect_equal(nrow(out), 10)
  expect_equal(out$Title[5], "Director of Development- Jackson Campus")
  expect_true(all(POSTED_PAY_COLUMNS %in% names(out)))
  expect_true(all(is.na(out$Pay_Min)))
})

test_that("extract_posted_pay skips district housing costs", {
  # Real OPI wording (Richey), 2026-10-31.
  p <- extract_posted_pay(c(
    "Our district owns housing which includes 3 houses and 7 apartments starting at $350 per month! Utilities included.",
    "Teacherage available for $400/month. Salary: $42,000 per year",
    "Starting salary is $45,500; current staff average $52,000"
  ))
  expect_equal(p$Pay_Min, c(NA, 42000, 45500))
})

test_that("extract_posted_pay skips amounts that go toward something else, and benefit ceilings", {
  # Real OPI wording (Turner, Judith Gap, Polson), 2026-10-31.
  p <- extract_posted_pay(c(
    "first contract year at no cost and only $200 monthly for housing/utilities every year",
    "development opportunities, and $800/month toward health, dental, and vision",
    "The program provides for up to $3,000 of state funded loan repayment",
    "Salary: up to $60,000 DOE",
    "Salary: $52,000 per year for the 2026-27 school year"
  ))
  expect_equal(p$Pay_Min, c(NA, NA, NA, 60000, 52000))
})

test_that("extract_posted_pay skips application costs, money paid into benefits, and loan-assistance lists", {
  # Real wording from the 2026-10-31 rehearsal (Billings, Rocky Boy, Billings).
  p <- extract_posted_pay(c(
    "or email lynchla@billingsschools.org. The cost is $60.00) Go to www.billingsschools.org and apply",
    "Our district also pays $11,446 into a flex benefits package (district contribution)",
    "Quality Educator Loan Assistance of up to $3,000, $4,000 & $5,000 over three years.",
    "Loan assistance up to $3,000. Salary: $45,000 per year",
    "Teaching Assistant: $15.50 per hour"
  ))
  expect_equal(p$Pay_Min, c(NA, NA, NA, 45000, 15.5))
})

test_that("is_implausible_pay flags amounts out of range for their unit", {
  bad <- is_implausible_pay(
    pay_min = c(15, 900, 55000, 145, 4771, 60, 50000, NA, 20, 15),
    pay_max = c(25, 900, 55000, 145, 4771, 60, 40000, NA, 20, 15),
    pay_unit = c("hour", "hour", "hour", "day", "stipend", NA, "year", NA, "fortnight", "hour")
  )
  expect_equal(bad, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))
})

test_that("drop_implausible_pay blanks every Pay_* column of a bad row only", {
  df <- data.frame(Pay_Min = c(18, 55000), Pay_Max = c(22, 55000), Pay_Unit = c("hour", "hour"),
                   Pay_Text = c("$18-$22/hr", "$55,000 hourly"), stringsAsFactors = FALSE)
  out <- suppressMessages(drop_implausible_pay(df))
  expect_equal(out$Pay_Min, c(18, NA))
  expect_true(all(is.na(unlist(out[2, POSTED_PAY_COLUMNS]))))
  expect_equal(out[1, ], df[1, ])
})
