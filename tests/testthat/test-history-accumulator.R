# append_weekly_rows()/check_total_matches_parts() unit tests, plus an
# equivalence test proving the new incremental-append path reproduces
# exactly what the old from-scratch archive rebuild produced, using the
# REAL archived data already in this repo (Archivek12_Data/,
# Archived_HE_Data/) rather than synthetic fixtures -- this is the one
# regression that actually matters here: a subtle bug in the incremental
# logic silently drifting the growing historical database out of sync
# with what the raw archive actually says happened.

test_that("append_weekly_rows bootstraps from a missing file", {
  new_rows <- data.frame(District = c("A", "B"), Archive_Date = "2026-08-06", n = c(1, 2))
  tmp <- withr::local_tempfile()
  expect_false(file.exists(tmp))

  result <- append_weekly_rows(tmp, new_rows)
  expect_equal(nrow(result), 2)
  expect_equal(result$Archive_Date, c("2026-08-06", "2026-08-06"))
})

test_that("append_weekly_rows appends onto an existing file without disturbing prior weeks", {
  tmp <- withr::local_tempfile()
  existing <- data.frame(District = c("A", "B"), Archive_Date = "2026-07-30", n = c(5, 6))
  write.csv(existing, tmp, row.names = FALSE)

  new_rows <- data.frame(District = c("A", "B"), Archive_Date = "2026-08-06", n = c(1, 2))
  result <- append_weekly_rows(tmp, new_rows)

  expect_equal(nrow(result), 4)
  expect_setequal(unique(result$Archive_Date), c("2026-07-30", "2026-08-06"))
  expect_equal(result$n[result$Archive_Date == "2026-07-30" & result$District == "A"], 5)
})

test_that("append_weekly_rows is idempotent -- a same-day re-run replaces, not duplicates, that day", {
  tmp <- withr::local_tempfile()
  existing <- data.frame(District = c("A", "B"), Archive_Date = c("2026-07-30", "2026-08-06"), n = c(5, 1))
  write.csv(existing, tmp, row.names = FALSE)

  # Same date re-run, with a DIFFERENT count than the first pass (like a
  # workflow_dispatch re-run after fixing a bug earlier the same day).
  rerun_rows <- data.frame(District = "A", Archive_Date = "2026-08-06", n = 99)
  result <- append_weekly_rows(tmp, rerun_rows)

  expect_equal(nrow(result), 2)
  expect_equal(result$n[result$Archive_Date == "2026-08-06"], 99)
  expect_equal(result$n[result$Archive_Date == "2026-07-30"], 5)
})

test_that("append_weekly_rows refuses to append rows with more than one Archive_Date", {
  new_rows <- data.frame(District = c("A", "B"), Archive_Date = c("2026-08-06", "2026-08-07"), n = c(1, 2))
  expect_error(append_weekly_rows(withr::local_tempfile(), new_rows), "exactly one")
})

test_that("append_weekly_rows fails loudly on a column mismatch instead of writing a jagged file", {
  tmp <- withr::local_tempfile()
  write.csv(data.frame(District = "A", Archive_Date = "2026-07-30", n = 5), tmp, row.names = FALSE)

  new_rows <- data.frame(District = "A", Archive_Date = "2026-08-06", Category = "Math", n = 1)
  expect_error(append_weekly_rows(tmp, new_rows), "column mismatch")
})

test_that("append_weekly_rows handles a genuinely empty new-rows data frame without erroring", {
  tmp <- withr::local_tempfile()
  write.csv(data.frame(District = "A", Archive_Date = "2026-07-30", n = 5), tmp, row.names = FALSE)

  empty <- data.frame(District = character(0), Archive_Date = character(0), n = numeric(0))
  result <- append_weekly_rows(tmp, empty)
  expect_equal(nrow(result), 1)
})

test_that("check_total_matches_parts passes when the Total genuinely equals the sum of parts", {
  total <- data.frame(Broad_Category = "Math", Archive_Date = "2026-08-06", District = "Total", sum = 5)
  parts <- data.frame(Broad_Category = "Math", Archive_Date = "2026-08-06",
                       District = c("A", "B"), sum = c(2, 3))
  expect_true(check_total_matches_parts(total, parts, group_cols = c("Broad_Category", "Archive_Date")))
})

test_that("check_total_matches_parts stops when the Total doesn't equal the sum of parts", {
  total <- data.frame(Broad_Category = "Math", Archive_Date = "2026-08-06", District = "Total", sum = 999)
  parts <- data.frame(Broad_Category = "Math", Archive_Date = "2026-08-06",
                       District = c("A", "B"), sum = c(2, 3))
  expect_error(check_total_matches_parts(total, parts, group_cols = c("Broad_Category", "Archive_Date")),
               "don't equal the sum")
})

# ---------------------------------------------------------------------------
# Equivalence: incremental append vs. from-scratch rebuild, on real archives
# ---------------------------------------------------------------------------

k12_archive_dir <- here::here("Archivek12_Data")
he_archive_dir <- here::here("Archived_HE_Data")

sorted_archive_dates <- function(dir, pattern, date_regex = "[0-9]{4}-[0-9]{2}-[0-9]{2}") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  dates <- regmatches(files, regexpr(date_regex, files))
  data.frame(file = files, date = dates, stringsAsFactors = FALSE)[order(dates), ]
}

# Row names get reset after sorting -- otherwise two data frames with
# identical values in identical (sorted) order can still fail
# expect_equal() over a spurious row.names mismatch, since sorting
# preserves each row's original (pre-sort) row name rather than
# renumbering, and the two sides being compared were built via different
# code paths with different original row orders.
sort_df <- function(df) {
  out <- df[do.call(order, as.list(df)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

test_that("incrementally appending the latest week reproduces a full rebuild through that week (K-12)", {
  skip_if_not(dir.exists(k12_archive_dir), "Archivek12_Data not found -- skipping")
  archive <- sorted_archive_dates(k12_archive_dir, "^combined_.*\\.csv$")
  skip_if(nrow(archive) < 2, "need at least 2 archived weeks to test incremental append")

  latest <- archive[nrow(archive), ]
  prior_files <- archive$file[-nrow(archive)]

  # "Old" accumulated state: full rebuild through every week EXCEPT the
  # latest, using only the archive functions under test (not app.R/the Rmd).
  old_dir <- withr::local_tempdir()
  file.copy(prior_files, old_dir)
  old_state <- rebuild_k12_history_from_archive(old_dir)

  old_k12jobs_path <- withr::local_tempfile()
  old_allsum_path <- withr::local_tempfile()
  old_weekly_totals_path <- withr::local_tempfile()
  write.csv(old_state$k12jobs, old_k12jobs_path, row.names = FALSE)
  write.csv(old_state$allsum, old_allsum_path, row.names = FALSE)
  write.csv(old_state$k12_district_weekly_totals, old_weekly_totals_path, row.names = FALSE)

  # This week's newly classified rows, built the same way the Rmd's
  # incremental path builds them -- directly from the latest raw archive
  # file, not from re-reading everything.
  raw_latest <- read.csv(latest$file, colClasses = c("Archive_Date" = "character"))
  raw_latest <- raw_latest[, setdiff(names(raw_latest), "X"), drop = FALSE]
  # mirror Wy_ED_Jobs.Rmd / rebuild_k12_history_from_archive(): drop
  # byte-identical RAW rows (before classify_k12_position() overwrites the
  # raw `position` column) so a job listed once per building counts once
  raw_latest <- distinct(raw_latest)
  raw_latest$Archive_Date <- as.character(as.Date(raw_latest$Archive_Date))
  has_source_ids <- "posting_id" %in% names(raw_latest)
  if (!has_source_ids) raw_latest$posting_id <- NA_character_

  # Then the pipeline's own steps: posting_id, collapse_duplicate_posting_ids()
  # (snapshots that carry posting_id), and the shared builders.
  this_week_raw <- raw_latest %>%
    mutate(
      District = canonicalize_k12_district(District),
      title = fix_title_encoding(title),
      posting_id = build_k12_posting_id(
        source_id = posting_id,
        title = title,
        location = location,
        date_posted = date_posted,
        url = url,
        district = District
      )
    )
  if (has_source_ids) this_week_raw <- collapse_duplicate_posting_ids(this_week_raw, "latest")
  oews <- read.csv(here::here("Wy_Ed_Jobs", "oews_role_wages.csv"), stringsAsFactors = FALSE,
                   colClasses = c(Area_Code = "character", SOC = "character", Year = "character"))
  district_county <- read.csv(here::here("Wy_Ed_Jobs", "salarymap2.csv"), stringsAsFactors = FALSE)[, c("District", "County")]
  this_week_combinedclean <- build_k12_combinedclean(this_week_raw, oews, district_county, "WY")
  this_week_k12jobs <- build_k12_teacher_rows(this_week_combinedclean)

  this_week_weekly_totals <- this_week_combinedclean %>%
    count(District, Archive_Date, name = "n")

  this_week_k12sumdistrict <- summarize_k12_posting_counts(this_week_k12jobs)
  this_week_k12sum <- this_week_k12sumdistrict %>%
    group_by(Broad_Category, Archive_Date) %>%
    summarize(sum = sum(sum), .groups = "drop") %>%
    mutate(District = "Total") %>%
    select(Broad_Category, Archive_Date, District, sum)
  check_total_matches_parts(this_week_k12sum, this_week_k12sumdistrict, group_cols = c("Broad_Category", "Archive_Date"))
  this_week_allsum <- bind_rows(this_week_k12sum, this_week_k12sumdistrict)

  # Incremental result: old accumulated state + this week's new rows.
  incremental_k12jobs <- append_weekly_rows(old_k12jobs_path, this_week_k12jobs)
  incremental_allsum <- append_weekly_rows(old_allsum_path, this_week_allsum)
  incremental_weekly_totals <- append_weekly_rows(old_weekly_totals_path, this_week_weekly_totals)

  # Ground truth: full rebuild through the latest week, via the same
  # from-scratch logic the pipeline used before this change.
  full_dir <- withr::local_tempdir()
  file.copy(archive$file, full_dir)
  full_state <- rebuild_k12_history_from_archive(full_dir)

  # Row order legitimately differs (append puts new rows at the end;
  # full rebuild's group_by/summarize doesn't preserve chronological
  # order) -- sort both before comparing. Also round-trip BOTH sides
  # through write.csv()/read.csv() before comparing, not just sides that
  # happen to already be on disk: R's text-mode read.csv() normalizes an
  # embedded "\r\n" inside a quoted field (present in some real scraped
  # Location/Posted_Date values) down to "\n", which the incremental side
  # already goes through (it reads the existing accumulated file back)
  # but the full-rebuild side doesn't unless we do it here too. Comparing
  # one CSV-round-tripped side against one pure-in-memory side would flag
  # that normalization as a false equivalence failure -- what actually
  # matters is that both end up identical once persisted, since that's
  # the only form either one is ever read back in by the real pipeline.
  norm <- function(df) {
    df$Archive_Date <- as.character(df$Archive_Date)
    tmp <- withr::local_tempfile()
    write.csv(df, tmp, row.names = FALSE)
    sort_df(read.csv(tmp, stringsAsFactors = FALSE))
  }

  expect_equal(norm(incremental_k12jobs), norm(as.data.frame(full_state$k12jobs)), ignore_attr = TRUE)
  expect_equal(norm(incremental_allsum), norm(as.data.frame(full_state$allsum)), ignore_attr = TRUE)
  expect_equal(norm(incremental_weekly_totals), norm(as.data.frame(full_state$k12_district_weekly_totals)), ignore_attr = TRUE)
})

test_that("incrementally appending the latest week reproduces a full rebuild through that week (Higher Ed)", {
  skip_if_not(dir.exists(he_archive_dir), "Archived_HE_Data not found -- skipping")
  skip_if_not_installed("readxl")
  archive <- sorted_archive_dates(he_archive_dir, "^hedata_.*\\.xlsx$")
  skip_if(nrow(archive) < 2, "need at least 2 archived weeks to test incremental append")

  latest <- archive[nrow(archive), ]
  prior_files <- archive$file[-nrow(archive)]

  old_dir <- withr::local_tempdir()
  file.copy(prior_files, old_dir)
  old_state <- rebuild_he_history_from_archive(old_dir)

  old_faculty_path <- withr::local_tempfile()
  old_allsum_he_path <- withr::local_tempfile()
  old_weekly_totals_path <- withr::local_tempfile()
  write.csv(old_state$facultydata, old_faculty_path, row.names = FALSE)
  write.csv(old_state$allsum_he, old_allsum_he_path, row.names = FALSE)
  write.csv(old_state$he_institution_weekly_totals, old_weekly_totals_path, row.names = FALSE)

  raw_latest <- readxl::read_xlsx(latest$file)
  raw_latest$Archive_Date <- as.character(as.Date(raw_latest$Archive_Date))
  raw_latest <- raw_latest %>% mutate(Institution = canonicalize_he_institution(Institution))

  this_week_he_cat <- raw_latest %>% mutate(Job_Type = classify_he_job_type(Title))
  # Archives from 2026-10-30 on carry Pay_*, which the pipeline keeps out of
  # facultydata.csv.
  this_week_facultydata <- this_week_he_cat %>%
    filter(Job_Type %in% c("Instructor/Teacher/Faculty", "Adjunct/Part-Time Faculty")) %>%
    mutate(Category = classify_he_faculty_category(Title)) %>%
    select(-any_of(c(POSTED_PAY_COLUMNS, "Close_Date")))

  this_week_weekly_totals <- raw_latest %>% count(Institution, Archive_Date, name = "n")

  this_week_hesum <- this_week_facultydata %>%
    group_by(Category, Archive_Date, Job_Type) %>%
    summarize(sum = n(), .groups = "drop") %>%
    mutate(Institution = "Total") %>%
    select(Category, Archive_Date, Institution, Job_Type, sum)
  this_week_hesuminst <- this_week_facultydata %>%
    group_by(Category, Archive_Date, Institution, Job_Type) %>%
    summarise(sum = n(), .groups = "drop")
  check_total_matches_parts(this_week_hesum, this_week_hesuminst, group_cols = c("Category", "Archive_Date", "Job_Type"))
  this_week_allsum_he <- bind_rows(this_week_hesum, this_week_hesuminst)

  incremental_facultydata <- append_weekly_rows(old_faculty_path, as.data.frame(this_week_facultydata))
  incremental_allsum_he <- append_weekly_rows(old_allsum_he_path, this_week_allsum_he)
  incremental_weekly_totals <- append_weekly_rows(old_weekly_totals_path, this_week_weekly_totals)

  full_dir <- withr::local_tempdir()
  file.copy(archive$file, full_dir)
  full_state <- rebuild_he_history_from_archive(full_dir)

  # See the K-12 test's matching comment above for why both sides get
  # round-tripped through write.csv()/read.csv() before comparing.
  norm <- function(df) {
    df$Archive_Date <- as.character(df$Archive_Date)
    tmp <- withr::local_tempfile()
    write.csv(df, tmp, row.names = FALSE)
    sort_df(read.csv(tmp, stringsAsFactors = FALSE))
  }

  expect_equal(norm(incremental_facultydata), norm(as.data.frame(full_state$facultydata)), ignore_attr = TRUE)
  expect_equal(norm(incremental_allsum_he), norm(as.data.frame(full_state$allsum_he)), ignore_attr = TRUE)
  expect_equal(norm(incremental_weekly_totals), norm(as.data.frame(full_state$he_institution_weekly_totals)), ignore_attr = TRUE)
})

test_that("rebuild_parity_row ignores row order, NA vs blank and \\r\\n, and counts real differences", {
  committed <- data.frame(a = c("x", "y", "y"), b = c("1", "", "2\n3"), c = "k", stringsAsFactors = FALSE)
  same <- committed[c(3, 1, 2), c("a", "b", "c")]
  same$b[same$b == ""] <- NA
  same$b <- sub("\n", "\r\n", same$b, fixed = TRUE)
  r <- rebuild_parity_row("f.csv", committed, same)
  expect_equal(c(r$Only_Committed, r$Only_Rebuilt), c(0, 0))
  expect_equal(r$Missing_Cols, "")
  # A dropped column and a duplicated / missing row both show.
  r2 <- rebuild_parity_row("f.csv", committed, rbind(committed[c(1, 1, 3), c("a", "b")]))
  expect_equal(r2$Missing_Cols, "c")
  expect_equal(c(r2$Only_Committed, r2$Only_Rebuilt), c(1, 1))
  expect_match(r2$Example, "committed: y \\| <NA>")
})
