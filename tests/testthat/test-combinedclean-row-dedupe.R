# Unit tests for scripts/repair_combinedclean_row_duplicates.R's pure steps.
# The end-to-end repair (against the real archive) is covered indirectly by
# test-history-accumulator.R's equivalence check, which now dedupes the same
# way; these pin the row-level behaviour.

test_that("dedupe_raw_k12_rows drops only byte-identical rows", {
  df <- data.frame(
    title = c("Building Paraprofessional", "Building Paraprofessional",
              "Building Paraprofessional", "Assistant Track Coach", "Assistant Track Coach"),
    date_posted = NA_character_,
    position = c("Paraprofessional", "Paraprofessional", "Paraprofessional",
                 "AFFLERBACH ELEMENTARY", "HOBBS ELEMENTARY"),   # building distinguishes the coaches
    location = "When Filled",
    url = "https://www.applitrack.com/x/onlineapp/default.aspx?all=1",
    District = "Sweetwater County School District 1",
    stringsAsFactors = FALSE
  )
  res <- dedupe_raw_k12_rows(df)
  expect_equal(res$rows_removed, 2)                 # 3 identical paras -> 1
  expect_equal(nrow(res$data), 3)                   # 1 para + 2 distinct-building coaches
  expect_equal(sum(res$data$title == "Assistant Track Coach"), 2)
})

test_that("dedupe_raw_k12_rows is a no-op when there are no duplicates", {
  df <- data.frame(title = c("a", "b"), x = 1:2, stringsAsFactors = FALSE)
  res <- dedupe_raw_k12_rows(df)
  expect_equal(res$rows_removed, 0)
  expect_equal(nrow(res$data), 2)
})

test_that("drop_surplus_rows keeps first N of each signature in original order", {
  df <- data.frame(
    title = c("T", "T", "T", "U", "T"),
    Archive_Date = "2026-09-04",
    posting_id = c("a", "b", "c", "d", "e"),   # drifted column, must not affect matching
    District = "D",
    stringsAsFactors = FALSE
  )
  target <- data.frame(
    title = c("T", "T", "U"), Archive_Date = "2026-09-04", District = "D",
    stringsAsFactors = FALSE
  )
  res <- drop_surplus_rows(df, target, key_cols = c("title", "Archive_Date", "District"))
  expect_equal(res$rows_removed, 2)                 # 4 "T" rows -> keep 2
  expect_equal(res$data$posting_id, c("a", "b", "d"))  # first two "T"s + the "U"
})

test_that("drop_surplus_rows is a no-op when committed already matches the target multiset", {
  df <- data.frame(title = c("T", "U"), Archive_Date = "x", District = "D", stringsAsFactors = FALSE)
  res <- drop_surplus_rows(df, df, key_cols = c("title", "Archive_Date", "District"))
  expect_equal(res$rows_removed, 0)
  expect_equal(nrow(res$data), 2)
})

test_that("collapse_duplicate_posting_ids keeps the first row per posting_id", {
  # 2026-09-25: rows distinct() kept (they differ only in case/whitespace or
  # NA vs "", which build_k12_posting_id() normalizes away) shared one
  # posting_id and failed the weekly data-quality gate.
  df <- data.frame(
    title = c("Custodian", "custodian ", "Aide", "Aide"),
    location = c("Main", "Main", NA, ""),
    date_posted = "2026-09-20",
    url = "https://example.org/jobs",
    District = "Park County School District 16",
    stringsAsFactors = FALSE
  )
  df$posting_id <- build_k12_posting_id(NULL, df$title, df$location,
                                        df$date_posted, df$url, df$District)
  expect_equal(nrow(dplyr::distinct(df)), 4)
  res <- collapse_duplicate_posting_ids(df, "test")
  expect_equal(res$title, c("Custodian", "Aide"))
  expect_equal(anyDuplicated(res$posting_id), 0)

  # no-op (and silent) when every posting_id is already unique
  expect_identical(collapse_duplicate_posting_ids(res), res)
})

test_that("build_k12_posting_id gives printable fallback IDs that a rebuild reproduces", {
  # Until 2026-11 the fallback parts were joined with "\r", which landed in
  # the CSVs as a line break inside the field; and a rebuild from the raw
  # archive (which stores each week's built ID) passed that stored ID back
  # as source_id, where normalizing turned the "\r" into a space -- so the
  # weekly run and a rebuild gave the same posting different IDs.
  # Real Big Horn SD1 posting from Archivek12_Data/combined_2026-10-30.csv.
  args <- list(title = "Career and Technical Education Teacher (Grades 6-12)",
               location = "Rocky Mountain Middle/High School", date_posted = "2026-10-14",
               url = "https://www.applitrack.com/bighorn1/onlineapp/jobpostings/view.asp?all=1",
               district = "Big Horn School District 1")
  id <- do.call(build_k12_posting_id, c(list(source_id = NA), args))
  expect_false(grepl("[[:cntrl:]]", id))
  expect_match(id, "^fallback \\| big horn school district 1 \\| ")
  # Rebuild path: the stored ID comes back as source_id, old or new form.
  stored_old <- paste("fallback", "big horn school district 1", tolower(args$url),
                      "career and technical education teacher (grades 6-12)",
                      "rocky mountain middle/high school", "2026-10-14", sep = "\r")
  expect_equal(do.call(build_k12_posting_id, c(list(source_id = stored_old), args)), id)
  expect_equal(do.call(build_k12_posting_id, c(list(source_id = id), args)), id)
  # A real source ID still passes through.
  expect_equal(do.call(build_k12_posting_id, c(list(source_id = "schoolspring:laramie1.schoolspring.com:5857967"), args)),
               "schoolspring:laramie1.schoolspring.com:5857967")
})
