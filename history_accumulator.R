# Turns the weekly pipeline's derived-dataset writes from "reprocess every
# archived raw snapshot from scratch" into "append this week's newly
# computed rows onto what's already on disk."
#
# Before this, Wy_ED_Jobs.Rmd re-read and re-classified EVERY historical
# archive file (Archivek12_Data/*.csv, Archived_HE_Data/*.xlsx -- 100+ files
# and growing by one every week since August 2024) on every single weekly
# run, just to derive datasets that only ever gain one new week's worth of
# rows. That's O(all history) work on a job with a hard 30-minute CI
# timeout, for a result that's 99%+ identical to last week's. It works fine
# today; it does not fail gracefully as the archive keeps growing -- it
# just eventually blows the timeout with no warning beforehand.
#
# append_weekly_rows() instead takes only this week's freshly classified
# rows and appends them onto the existing accumulated CSV, so the pipeline
# now does O(this week) work regardless of how many weeks of history exist.
# The archived raw snapshots keep being written every week exactly as
# before (that's the durable source of truth / disaster-recovery path,
# untouched by this change) -- they're just no longer read back every run.
# scripts/rebuild_k12_history_from_archive.R and
# scripts/rebuild_he_history_from_archive.R preserve the old from-scratch
# logic for exactly that recovery case, and are what
# tests/testthat/test-history-accumulator.R checks this incremental path
# against on real archived data to prove the two stay equivalent.

# check_total_matches_parts() and rebuild_parity_row() moved to the edjobs package in phase 2 (2026-11-11); attached here so
# every script that sources this file still has them.
suppressMessages(library(edjobs))

suppressMessages(library(dplyr))

# Appends new_rows (expected to be exactly one Archive_Date's worth of
# freshly computed rows) onto whatever's already at existing_path.
#
# Idempotent: if new_rows' date is already present in the existing file
# (a same-day re-run -- workflow_dispatch, a CI retry), that date's old
# rows are replaced rather than duplicated, so running the pipeline twice
# in one day can't double-count that day.
#
# Fails loudly on a column mismatch between the existing file and new_rows
# rather than silently writing a jagged file via bind_rows()'s NA-padding --
# a classifier renaming/adding a column is a real schema change that needs
# a human decision (update the accumulated file's schema, or rebuild from
# archive), not something that should get baked into the growing history
# without anyone noticing.
#
# read_fn is injectable so this is testable against synthetic existing
# data without real files on disk.
append_weekly_rows <- function(existing_path, new_rows, date_col = "Archive_Date",
                                read_fn = utils::read.csv) {
  if (nrow(new_rows) == 0) {
    if (!file.exists(existing_path)) return(new_rows)
    existing <- read_fn(existing_path, stringsAsFactors = FALSE)
    existing[[date_col]] <- as.character(existing[[date_col]])
    return(existing)
  }

  new_rows[[date_col]] <- as.character(new_rows[[date_col]])
  new_dates <- unique(new_rows[[date_col]])
  if (length(new_dates) != 1) {
    stop("append_weekly_rows(): new_rows must contain exactly one ", date_col,
         ", got ", length(new_dates), ": ", paste(new_dates, collapse = ", "))
  }

  if (!file.exists(existing_path)) {
    return(new_rows)
  }

  existing <- read_fn(existing_path, stringsAsFactors = FALSE)
  existing[[date_col]] <- as.character(existing[[date_col]])

  if (!setequal(names(existing), names(new_rows))) {
    stop(
      "append_weekly_rows(): column mismatch between existing ", existing_path,
      " (", paste(sort(names(existing)), collapse = ", "), ") and this week's new rows (",
      paste(sort(names(new_rows)), collapse = ", "), "). A classifier or schema likely ",
      "changed -- fix the mismatch, or rebuild the file from the raw archive (see ",
      "scripts/rebuild_k12_history_from_archive.R / rebuild_he_history_from_archive.R), ",
      "rather than letting bind_rows() silently pad the difference with NA."
    )
  }

  # Same-day re-run guard -- drop this date's rows from the existing file
  # before appending, so a re-run replaces rather than duplicates.
  existing <- existing[existing[[date_col]] != new_dates, , drop = FALSE]

  dplyr::bind_rows(existing, new_rows[names(existing)])
}


# Counts K-12 teacher postings by their durable source identity, rather than
# title/location text. Two vacancies can share that display text while having
# distinct source posting IDs (notably on SchoolSpring).
summarize_k12_posting_counts <- function(k12jobs) {
  required <- c("Broad_Category", "Archive_Date", "District", "posting_id")
  missing <- setdiff(required, names(k12jobs))
  if (length(missing) > 0) {
    stop(
      "summarize_k12_posting_counts(): missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }

  k12jobs %>%
    dplyr::group_by(Broad_Category, Archive_Date, District) %>%
    dplyr::summarize(sum = dplyr::n_distinct(posting_id), .groups = "drop")
}

# ---------------------------------------------------------------------------
# The K-12 derived rows, built one way for the weekly pipeline
# (Wy_ED_Jobs.Rmd) and for the from-archive rebuild
# (scripts/rebuild_k12_history_from_archive.R), so the two can't drift apart
# (classification safeguards proposal, item 12: the rebuild had dropped
# Close_Date and Est_*). Callers dedupe first (distinct(), then
# collapse_duplicate_posting_ids() where the rows carry posting_id).
# ---------------------------------------------------------------------------

# combinedclean.csv's rows: placeholders out, current classifier, posted pay
# guarded (drop_implausible_pay()), and the OEWS pay estimate for the
# posting's role in its district's county (statewide where the county is
# unknown). Needs bls_oews_scraper.R sourced.
build_k12_combinedclean <- function(rows, oews_wages, district_county, state = "WY") {
  if (!"Close_Date" %in% names(rows)) rows$Close_Date <- NA_character_
  rows$Close_Date <- as.character(rows$Close_Date)
  out <- rows %>%
    filter(!is_placeholder_posting(title)) %>%
    mutate(position = classify_k12_position(title),
           District = canonicalize_k12_district(District)) %>%
    ensure_posted_pay_columns() %>%
    drop_implausible_pay()
  out <- attach_oews_estimates(out, oews_wages, district_county$County[match(out$District, district_county$District)], state)
  out %>%
    dplyr::select(title, Archive_Date, date_posted, position, location, url, posting_id, District, Close_Date,
                  dplyr::all_of(POSTED_PAY_COLUMNS), dplyr::all_of(OEWS_ESTIMATE_COLUMNS))
}

# k12jobanalysis.csv's rows: the Teacher postings, with subject and broad
# category.
build_k12_teacher_rows <- function(combinedclean) {
  combinedclean %>%
    filter(position == "Teacher") %>%
    mutate(Category = classify_k12_subject(title),
           Broad_Category = classify_k12_broad_category(Category)) %>%
    dplyr::select(title, Archive_Date, position, location, url, posting_id, District, Category, Broad_Category)
}

