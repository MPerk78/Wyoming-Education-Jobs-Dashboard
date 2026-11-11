# State-only tests left here when the shared classifier and posted-pay tests moved to edjobs
# (phase 2, 2026-11-11): these call this state's own code (name corrections or scrapers).


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

test_that("a NEOGOV pay field with an implausible unit falls back to the card's description", {
  # Real cards (Gillette College, 2026-11-10): the field says "Adjunct - $900.00
  # Hourly", the description "Grade: $900 per credit hour".
  pay <- parse_neogov_card_pay(paste(readLines(test_path("fixtures", "neogov_gillettecollege_p1_2026-11-10.html"),
                                               warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  adj <- pay[!is.na(pay$Pay_Min) & pay$Pay_Min == 900, ]
  expect_gte(nrow(adj), 2)
  expect_true(all(adj$Pay_Unit == "credit hour"))
  expect_true(all(grepl("per credit hour", adj$Pay_Text)))
  # A field with a plausible unit is still taken from the field.
  expect_false(any(is.na(pay$Pay_Unit[!is.na(pay$Pay_Min)])))
})
