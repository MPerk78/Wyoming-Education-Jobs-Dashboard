
test_that("every dataset app.R actually reads has a REQUIRED_SCHEMAS entry", {
  # Regression against this list silently going stale as app.R changes --
  # every read.csv()/read_xlsx() target in app.R should be represented here.
  app_r_path <- here::here("Wy_Ed_Jobs", "app.R")
  skip_if_not(file.exists(app_r_path), "app.R not found -- skipping")
  app_r_text <- paste(readLines(app_r_path, warn = FALSE), collapse = "\n")

  read_targets <- regmatches(app_r_text, gregexpr('read(\\.csv|_xlsx)\\("([^"]+)"', app_r_text))[[1]]
  file_names <- unique(sub('.*"([^"]+)"$', "\\1", read_targets))

  expect_true(length(file_names) > 0)
  missing_schema_entries <- setdiff(file_names, c(names(REQUIRED_SCHEMAS), names(REQUIRED_SCHEMAS_XLSX)))
  expect_equal(missing_schema_entries, character(0))
})

test_that("the currently committed datasets pass the schema check (regression against real data)", {
  wy_ed_jobs_dir <- here::here("Wy_Ed_Jobs")
  skip_if_not(dir.exists(wy_ed_jobs_dir), "Wy_Ed_Jobs/ not found -- skipping")

  problems <- list()
  for (f in names(REQUIRED_SCHEMAS)) {
    path <- file.path(wy_ed_jobs_dir, f)
    skip_if_not(file.exists(path), paste(f, "not found -- skipping"))
    actual_cols <- names(read.csv(path, nrows = 1, check.names = FALSE))
    result <- check_file_schema(f, actual_cols, REQUIRED_SCHEMAS[[f]])
    if (!is.null(result)) problems[[length(problems) + 1]] <- result
  }

  if (length(problems) > 0) {
    print(do.call(rbind, problems))
  }
  expect_equal(length(problems), 0)
})
