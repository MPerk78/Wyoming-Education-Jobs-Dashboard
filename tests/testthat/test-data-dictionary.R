# Data dictionary and variable codebook integrity tests

test_that("data_dictionary.csv exists, has valid schema, and no missing fields", {
  dict_path <- here::here("Wy_Ed_Jobs", "data_dictionary.csv")
  skip_if_not(file.exists(dict_path), "data_dictionary.csv not found")

  dict <- utils::read.csv(dict_path, stringsAsFactors = FALSE)
  expected_cols <- c("Dataset", "File_Name", "Column_Name", "Data_Type", "Primary_Authority", "Definition")
  expect_equal(names(dict), expected_cols)
  expect_gt(nrow(dict), 0)

  # No NA or empty string values in any field
  for (col in expected_cols) {
    expect_false(any(is.na(dict[[col]])), info = paste("NA values found in column", col))
    expect_false(any(trimws(dict[[col]]) == ""), info = paste("Empty string values found in column", col))
  }
})

test_that("every column in every public research CSV is documented in data_dictionary.csv", {
  dict_path <- here::here("Wy_Ed_Jobs", "data_dictionary.csv")
  skip_if_not(file.exists(dict_path), "data_dictionary.csv not found")

  dict <- utils::read.csv(dict_path, stringsAsFactors = FALSE)
  documented_files <- unique(dict$File_Name)

  app_dir <- here::here("Wy_Ed_Jobs")

  # Columns documented ahead of the weekly run that first writes them
  # (e.g. salarymap2's FRL columns, 2026-11-09 to the 11-13 run): absent from the committed file until then, so this suite (the
  # weekly run's first, blocking step) would stop that very run. Remove each
  # entry once a weekly run has written the column.
  pending <- list()

  for (f in documented_files) {
    file_path <- file.path(app_dir, f)
    expect_true(file.exists(file_path), info = paste("Documented file does not exist:", f))

    actual_cols <- names(utils::read.csv(file_path, nrows = 1))
    actual_cols <- setdiff(actual_cols, "X") # drop row-index col if present

    dict_cols <- dict$Column_Name[dict$File_Name == f]

    missing_in_dict <- setdiff(actual_cols, dict_cols)
    expect_equal(missing_in_dict, character(0),
                 info = paste("File", f, "has columns missing in data_dictionary.csv:",
                              paste(missing_in_dict, collapse = ", ")))

    missing_in_file <- setdiff(setdiff(dict_cols, actual_cols), pending[[f]])
    expect_equal(missing_in_file, character(0),
                 info = paste("data_dictionary.csv documents columns not in file", f, ":",
                              paste(missing_in_file, collapse = ", ")))
  }
})

test_that("every public research download in app.R corresponds to a documented file in data_dictionary.csv", {
  app_path <- here::here("Wy_Ed_Jobs", "app.R")
  skip_if_not(file.exists(app_path), "app.R not found")

  dict_path <- here::here("Wy_Ed_Jobs", "data_dictionary.csv")
  skip_if_not(file.exists(dict_path), "data_dictionary.csv not found")

  dict <- utils::read.csv(dict_path, stringsAsFactors = FALSE)
  documented_files <- unique(dict$File_Name)

  app_lines <- readLines(app_path, warn = FALSE)
  copy_lines <- grep("file\\.copy\\(", app_lines, value = TRUE)
  download_files <- unique(sub('.*file\\.copy\\("([^"]+\\.csv)".*', '\\1', copy_lines))
  download_files <- grep("\\.csv$", download_files, value = TRUE)

  # Exclude data_dictionary.csv itself from being self-recursive
  download_csvs <- setdiff(download_files, "data_dictionary.csv")

  expect_true(length(download_csvs) > 0)
  undocumented_downloads <- setdiff(download_csvs, documented_files)
  expect_equal(undocumented_downloads, character(0),
               info = paste("app.R offers downloads for files not in data_dictionary.csv:",
                            paste(undocumented_downloads, collapse = ", ")))
})

test_that("DATA_DICTIONARY.md mirrors data_dictionary.csv datasets and files", {
  md_path <- here::here("DATA_DICTIONARY.md")
  skip_if_not(file.exists(md_path), "DATA_DICTIONARY.md not found")

  dict_path <- here::here("Wy_Ed_Jobs", "data_dictionary.csv")
  skip_if_not(file.exists(dict_path), "data_dictionary.csv not found")

  dict <- utils::read.csv(dict_path, stringsAsFactors = FALSE)
  md_text <- paste(readLines(md_path, warn = FALSE), collapse = "\n")

  for (f in unique(dict$File_Name)) {
    expect_true(grepl(paste0("`", f, "`"), md_text, fixed = TRUE),
                info = paste("DATA_DICTIONARY.md does not reference file:", f))
  }
})
