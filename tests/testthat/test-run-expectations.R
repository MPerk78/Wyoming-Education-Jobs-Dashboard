# Run expectations (run_expectations.R; workflow plan 2026-11-10, item 2.1).
# The logic tests use a small hand-built file: they test the evaluator, not a
# scraper. The last test checks the committed run_expectations.csv, so a typo
# there fails here rather than during the weekly run.
source(here::here("run_expectations.R"))

exp_row <- function(...) {
  base <- list(id = "x", applies_from = "2026-11-01", expires = "2026-12-31", file = "data.csv",
               filter = "", measure = "", min = "", max = "", note = "")
  as.data.frame(modifyList(base, list(...)), stringsAsFactors = FALSE)
}

with_data_dir <- function(code) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  write.csv(data.frame(District = c("A", "A", "B", "C"), Pay_Min = c(40000, NA, 52000, NA),
                       title = c("Teacher", "Coach &#x27;Asst&#x27;", "Aide", "Bus Driver"),
                       Archive_Date = c("2026-11-06", "2026-11-13", "2026-11-13", "2026-11-13")),
            file.path(dir, "data.csv"), row.names = FALSE)
  dir
}

test_that("a count, a share and a pattern count are measured against their bounds", {
  dir <- with_data_dir()
  exp <- rbind(
    exp_row(id = "rows_A", filter = 'District == "A"', min = "2", max = "2"),
    exp_row(id = "pay_share", measure = "mean(!is.na(Pay_Min))", min = "0.6"),
    exp_row(id = "no_entities", measure = 'sum(grepl("&#x27;", title, fixed = TRUE))', max = "0"),
    exp_row(id = "latest_run", filter = "Archive_Date == max(Archive_Date)", min = "3", max = "3"))
  res <- evaluate_run_expectations(exp, run_date = "2026-11-13", root = dir)
  expect_equal(res$Value, c(2, 0.5, 1, 3))
  expect_equal(res$Status, c("pass", "fail", "fail", "pass"))
})

test_that("rows outside their dates are skipped, not failed", {
  dir <- with_data_dir()
  exp <- rbind(exp_row(id = "future", applies_from = "2026-11-20", min = "100"),
               exp_row(id = "old", expires = "2026-11-10", min = "100"))
  res <- evaluate_run_expectations(exp, run_date = "2026-11-13", root = dir)
  expect_equal(res$Status, c("not_yet", "expired"))
  # on the boundary days the row is checked
  res2 <- evaluate_run_expectations(exp, run_date = "2026-11-20", root = dir)
  expect_equal(res2$Status[1], "fail")
  res3 <- evaluate_run_expectations(exp, run_date = "2026-11-10", root = dir)
  expect_equal(res3$Status[2], "fail")
})

test_that("a missing file, a broken expression or a non-scalar measure is an error, not a pass", {
  dir <- with_data_dir()
  exp <- rbind(exp_row(id = "nofile", file = "missing.csv", min = "0"),
               exp_row(id = "badcol", filter = "NoSuchColumn == 1", min = "0"),
               exp_row(id = "vector", measure = "Pay_Min", min = "0"),
               exp_row(id = "na", measure = "mean(Pay_Min)", min = "0"),
               exp_row(id = "nonlogical_filter", filter = "District", min = "0"))
  res <- evaluate_run_expectations(exp, run_date = "2026-11-13", root = dir)
  expect_true(all(res$Status == "error"))
  expect_match(res$Detail[3], "single number")
  expect_match(res$Detail[4], "NA")
})

test_that("expressions don't see the caller's variables", {
  dir <- with_data_dir()
  threshold_from_caller <- 1
  res <- evaluate_run_expectations(exp_row(measure = "nrow(d) + threshold_from_caller", min = "0"),
                                   run_date = "2026-11-13", root = dir)
  expect_equal(res$Status, "error")
})

test_that("the report lists failures and expired ids, and is one line when all pass", {
  dir <- with_data_dir()
  exp <- rbind(exp_row(id = "ok", min = "1"), exp_row(id = "bad|pipe", min = "100", note = "needs 100"),
               exp_row(id = "gone", expires = "2026-11-01", min = "1"))
  rep <- build_run_expectations_report_markdown(evaluate_run_expectations(exp, "2026-11-13", root = dir))
  expect_match(rep[1], "2 checked, 1 passed, 1 failed, 0 errored \\(0 not yet due, 1 expired\\)")
  expect_true(any(grepl("bad\\\\|pipe", rep, fixed = FALSE)))
  expect_true(any(grepl("`gone`", rep, fixed = TRUE)))
  rep_ok <- build_run_expectations_report_markdown(evaluate_run_expectations(exp[1, ], "2026-11-13", root = dir))
  expect_length(rep_ok, 1)
})

test_that("validation catches the mistakes a hand-edited file makes", {
  dir <- with_data_dir()
  exp <- rbind(exp_row(id = "a", min = "1"), exp_row(id = "a", min = "1"),
               exp_row(id = "b", applies_from = "11/13/2026", min = "1"),
               exp_row(id = "c", file = "nope.csv", min = "1"),
               exp_row(id = "d", filter = "District ==", min = "1"),
               exp_row(id = "e"),
               exp_row(id = "f", min = "5", max = "1"))
  probs <- validate_run_expectations(exp, root = dir)
  expect_true(any(grepl("duplicate id: a", probs)))
  expect_true(any(grepl("\\(b\\): applies_from", probs)))
  expect_true(any(grepl("\\(c\\): no such file", probs)))
  expect_true(any(grepl("\\(d\\): filter does not parse", probs)))
  expect_true(any(grepl("\\(e\\): neither min nor max", probs)))
  expect_true(any(grepl("\\(f\\): min is above max", probs)))
  expect_length(validate_run_expectations(exp[1, ], root = dir), 0)
})

test_that("the committed run_expectations.csv is well formed", {
  path <- here::here("run_expectations.csv")
  skip_if_not(file.exists(path))
  exp <- read_run_expectations(path)
  expect_equal(validate_run_expectations(exp, root = here::here()), character(0))
})
