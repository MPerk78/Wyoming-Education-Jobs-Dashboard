# Repairs the within-run duplicate-row inflation of
# k12_district_weekly_totals.csv (and, to a lesser extent, k12jobanalysis.csv).
#
# Some direct-platform employment pages render the same opening once per
# building it applies to. The modern registry-driven Applitrack scraper
# (e.g. Sweetwater County SD1's "?all=1" listing) captures each line as a
# byte-identical row. build_k12_posting_id() collapses them to one
# posting_id, so allsum.csv (a n_distinct(posting_id)) was never affected.
# But k12_district_weekly_totals.csv is a raw count() of rows, and app.R's
# teacher vacancy-rate numerator (k12_teacher_current_counts) is a raw
# count() of k12jobanalysis.csv rows -- so a single vacancy listed under 9
# buildings counted as 9 (Sweetwater SD1: 96 rows for 80 distinct openings
# on 2026-09-04, and similar most weeks back through the archive).
#
# Fixed at the source: Wy_ED_Jobs.Rmd now does `combined %>% distinct()`
# before classify_k12_position() overwrites the raw `position` column, and
# rebuild_k12_history_from_archive() dedupes each raw snapshot the same way.
# This script repairs the data those paths already shipped.
#
#   Rscript scripts/repair_combinedclean_row_duplicates.R --dry-run
#   Rscript scripts/repair_combinedclean_row_duplicates.R --apply
#
# --apply makes the SMALLEST edit that brings the committed files in line
# with the fixed pipeline, row order preserved:
#   * combinedclean.csv           -- current week rebuilt from the latest raw
#                                    Archivek12_Data snapshot through the
#                                    fixed Rmd logic (raw distinct -> classify)
#   * k12_district_weekly_totals.csv -- only the per-(District, week) `n`
#                                    values that change are overwritten
#   * k12jobanalysis.csv          -- only the surplus byte-identical rows are
#                                    dropped (matched against the fixed
#                                    rebuild on every column except the
#                                    historically-drifted posting_id)
# allsum.csv is unchanged (it is already a n_distinct and matches the fixed
# rebuild exactly). The raw Archivek12_Data/ snapshots are left untouched.
# Git retains the before-state; inspect git diff before committing.
#
# KNOWN LIMITATION, tracked for a later pass: older SchoolSpring snapshots
# stored the building name in `position`, which legitimately distinguished
# ~24 "one assistant track/football coach per elementary school" postings
# that share a title. Some snapshots (old and, occasionally, current) drop
# that field, so per-file dedup collapses that cluster and UNDER-counts a
# handful of Laramie County SD1 / Sweetwater SD1 weekly totals (coaches are
# not teachers, so k12jobanalysis.csv / the vacancy rate are unaffected).
# The raw archive genuinely cannot tell "one posting listed 24 times" from
# "24 separate openings" there; a well-defined de-duplicated count is
# preferred to a raw count that tracks how often the source repeated a line.

suppressMessages(library(dplyr))

# Pure repair step, used for the current-week combinedclean snapshot: drop
# byte-identical rows BEFORE classify_k12_position() would overwrite the raw
# `position` column (older SchoolSpring rows carry the building there and
# must stay distinct). Mirrors Wy_ED_Jobs.Rmd exactly.
dedupe_raw_k12_rows <- function(df) {
  is_dup <- duplicated(df)
  list(data = df[!is_dup, , drop = FALSE], rows_removed = sum(is_dup))
}

# Drop only the surplus copies of each signature: keep, in original order,
# as many rows of each signature as `target` has, drop the rest. Used to
# bring k12jobanalysis.csv down to the fixed rebuild's row multiset without
# reordering or touching the drifted posting_id column.
drop_surplus_rows <- function(df, target, key_cols) {
  sig <- do.call(paste, c(lapply(df[, key_cols, drop = FALSE], as.character), sep = "\r"))
  tgt <- do.call(paste, c(lapply(target[, key_cols, drop = FALSE], as.character), sep = "\r"))
  allowed <- table(tgt)
  seen <- integer(0)
  keep <- logical(nrow(df))
  for (i in seq_len(nrow(df))) {
    s <- sig[i]
    n_seen <- if (is.na(seen[s])) 0L else seen[s]
    cap <- if (is.na(allowed[s])) 0L else allowed[s]
    if (n_seen < cap) {
      keep[i] <- TRUE
      seen[s] <- n_seen + 1L
    }
  }
  list(data = df[keep, , drop = FALSE], rows_removed = sum(!keep))
}

latest_k12_archive <- function(archive_dir) {
  files <- list.files(archive_dir, pattern = "^combined_.*\\.csv$", full.names = TRUE)
  dates <- as.Date(sub(".*combined_(.*)\\.csv$", "\\1", files))
  files[which.max(dates)]
}

repair_combinedclean_row_duplicates <- function(
    archive_dir = "Archivek12_Data",
    output_dir = "Wy_Ed_Jobs",
    write = FALSE) {
  report <- data.frame(
    file = character(0), metric = character(0),
    before = numeric(0), after = numeric(0), stringsAsFactors = FALSE
  )
  add <- function(file, metric, before, after) {
    report <<- rbind(report, data.frame(
      file = file, metric = metric, before = before, after = after,
      stringsAsFactors = FALSE
    ))
  }

  # --- combinedclean.csv: rebuild the current week from its raw snapshot ---
  cc_path <- file.path(output_dir, "combinedclean.csv")
  if (file.exists(cc_path)) {
    cc_old <- read.csv(cc_path, stringsAsFactors = FALSE)
    raw <- read.csv(latest_k12_archive(archive_dir),
                    colClasses = c("Archive_Date" = "character"), stringsAsFactors = FALSE)
    raw <- raw[, setdiff(names(raw), "X"), drop = FALSE]
    cc_new <- dedupe_raw_k12_rows(raw)$data %>%
      mutate(
        Archive_Date = as.character(as.Date(Archive_Date)),
        position = classify_k12_position(title),
        District = canonicalize_k12_district(District)
      ) %>%
      dplyr::select(title, Archive_Date, date_posted, position, location, url, posting_id, District)
    if (nrow(cc_new) != nrow(cc_old)) {
      add(cc_path, "rows", nrow(cc_old), nrow(cc_new))
      if (write) write.csv(cc_new, cc_path, row.names = FALSE)
    }
  }

  history <- rebuild_k12_history_from_archive(archive_dir)

  # --- k12_district_weekly_totals.csv: overwrite only changed n values ---
  wt_path <- file.path(output_dir, "k12_district_weekly_totals.csv")
  if (file.exists(wt_path)) {
    wt_old <- read.csv(wt_path, stringsAsFactors = FALSE)
    fixed <- history$k12_district_weekly_totals %>%
      mutate(Archive_Date = as.character(as.Date(Archive_Date)))
    wt_new <- wt_old %>%
      mutate(.row = row_number(),
             .key_date = as.character(as.Date(Archive_Date))) %>%
      dplyr::select(-n) %>%
      left_join(fixed %>% dplyr::select(District, .key_date = Archive_Date, n),
                by = c("District", ".key_date")) %>%
      arrange(.row) %>%
      dplyr::select(District, Archive_Date, n)
    changed <- sum(wt_old$n != wt_new$n | is.na(wt_new$n))
    if (changed > 0 || anyNA(wt_new$n)) {
      add(wt_path, "total n", sum(wt_old$n), sum(wt_new$n))
      add(wt_path, "rows with changed n", changed, changed)
      if (write) write.csv(wt_new, wt_path, row.names = FALSE)
    }
  }

  # --- k12jobanalysis.csv: drop only surplus byte-identical rows ---
  kj_path <- file.path(output_dir, "k12jobanalysis.csv")
  if (file.exists(kj_path)) {
    kj_old <- read.csv(kj_path, stringsAsFactors = FALSE)
    target <- history$k12jobs %>%
      mutate(Archive_Date = as.character(as.Date(Archive_Date)))
    key_cols <- c("title", "Archive_Date", "position", "location", "url",
                  "District", "Category", "Broad_Category")
    res <- drop_surplus_rows(kj_old, target, key_cols)
    if (res$rows_removed > 0) {
      add(kj_path, "rows", nrow(kj_old), nrow(res$data))
      if (write) write.csv(res$data, kj_path, row.names = FALSE)
    }
  }

  report
}

if (sys.nframe() == 0) {
  source("k12_he_classification.R")
  library(edjobs)
  source("history_accumulator.R")
  source(file.path("scripts", "rebuild_k12_history_from_archive.R"))

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1 || !all(args %in% c("--dry-run", "--apply"))) {
    stop("Usage: Rscript scripts/repair_combinedclean_row_duplicates.R --dry-run|--apply")
  }

  result <- repair_combinedclean_row_duplicates(write = identical(args, "--apply"))
  if (nrow(result) == 0) {
    cat("No byte-identical duplicate rows found; no files changed.\n")
  } else {
    print(result, row.names = FALSE)
    if (identical(args, "--apply")) {
      cat("\nApplied. allsum.csv unchanged (already a n_distinct).\n")
    } else {
      cat("\nDry run -- rerun with --apply to write the repaired files.\n")
    }
  }
}
