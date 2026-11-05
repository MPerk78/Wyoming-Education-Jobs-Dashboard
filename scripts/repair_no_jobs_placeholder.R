# Repairs the historical "No jobs" placeholder fabrication: the pre-registry
# Applitrack loop in Wy_ED_Jobs.Rmd inserted a literal title = "No jobs" row
# whenever a district's scrape returned zero real postings, so a genuinely
# empty week would still produce a row -- unlike every other platform in the
# same pipeline (TedK12/SchoolSpring/RedRoverK12), which simply drop an empty
# result and let safe_scrape()'s own "empty" log entry be the record of it.
# That placeholder was never real scraped data; it was pipeline code
# fabricating a row. Fixed at the source in the registry-driven loop (see
# git history for "Fix the 'No jobs' Applitrack placeholder" and the
# k12_district_registry.csv migration immediately before it) -- this script
# repairs the damage the old behavior already did to committed data before
# that fix: confirmed present in Archivek12_Data snapshots back to
# 2025-10-03 (Fremont County SD24, Lincoln County SD1, Sublette County SD9,
# and Fremont County SD24's own earlier-dated duplicate), and it wasn't just
# cosmetic -- k12_district_weekly_totals.csv counts these rows the same as a
# real posting (position == "Teacher"-filtered k12jobanalysis.csv is
# unaffected, since classify_k12_position("No jobs") is "Other", not
# "Teacher"), so it currently reports these three districts as having a
# steady 1 open position for months where the true count was 0.
#
# Run from the repository root:
#   Rscript scripts/repair_no_jobs_placeholder.R --dry-run
#   Rscript scripts/repair_no_jobs_placeholder.R --apply
#
# --apply removes the fabricated rows from the affected raw archive
# snapshots and the current combinedclean.csv, then rebuilds
# k12jobanalysis.csv, allsum.csv, and k12_district_weekly_totals.csv from
# the now-clean archive via rebuild_k12_history_from_archive() (the same
# disaster-recovery rebuild tests/testthat/test-history-accumulator.R checks
# for equivalence against the incremental accumulator). Git retains the
# before-state for review/reversion; inspect git diff before committing.

suppressMessages(library(dplyr))

NO_JOBS_PLACEHOLDER_TITLE <- "No jobs"

empty_no_jobs_report <- function() {
  data.frame(file = character(0), rows_removed = integer(0), stringsAsFactors = FALSE)
}

# Pure repair step: strips any row whose title is exactly the placeholder
# string, leaving every other row (including a district's genuine real
# postings that same week, if any -- the placeholder was only ever inserted
# when nrow() == 0, so in practice it never coexists with real rows for the
# same district/week, but this doesn't assume that) untouched.
repair_no_jobs_rows <- function(postings) {
  if (!"title" %in% names(postings)) {
    stop("repair_no_jobs_rows(): data is missing required column 'title'")
  }
  is_placeholder <- !is.na(postings$title) & postings$title == NO_JOBS_PLACEHOLDER_TITLE
  list(data = postings[!is_placeholder, , drop = FALSE], rows_removed = sum(is_placeholder))
}

repair_no_jobs_placeholder <- function(
    archive_dir = "Archivek12_Data",
    current_combinedclean_path = file.path("Wy_Ed_Jobs", "combinedclean.csv"),
    output_dir = "Wy_Ed_Jobs",
    write = FALSE) {
  archive_files <- sort(list.files(archive_dir, pattern = "\\.csv$", full.names = TRUE))
  if (length(archive_files) == 0) {
    stop("repair_no_jobs_placeholder(): no archive files found in ", archive_dir)
  }

  reports <- lapply(archive_files, function(f) {
    df <- read.csv(f, colClasses = c("Archive_Date" = "character"), stringsAsFactors = FALSE)
    repaired <- repair_no_jobs_rows(df)
    if (write && repaired$rows_removed > 0) {
      write.csv(repaired$data, f, row.names = FALSE)
    }
    data.frame(file = f, rows_removed = repaired$rows_removed, stringsAsFactors = FALSE)
  })
  report <- do.call(rbind, reports)
  report <- report[report$rows_removed > 0, , drop = FALSE]

  current_rows_removed <- 0L
  if (file.exists(current_combinedclean_path)) {
    current <- read.csv(current_combinedclean_path, stringsAsFactors = FALSE)
    current_repaired <- repair_no_jobs_rows(current)
    current_rows_removed <- current_repaired$rows_removed
    if (write && current_rows_removed > 0) {
      write.csv(current_repaired$data, current_combinedclean_path, row.names = FALSE)
    }
  }
  if (current_rows_removed > 0) {
    report <- rbind(report, data.frame(
      file = current_combinedclean_path, rows_removed = current_rows_removed, stringsAsFactors = FALSE
    ))
  }

  if (write && nrow(report) > 0) {
    history <- rebuild_k12_history_from_archive(archive_dir)
    write.csv(history$k12jobs, file.path(output_dir, "k12jobanalysis.csv"), row.names = FALSE)
    write.csv(history$allsum, file.path(output_dir, "allsum.csv"), row.names = FALSE)
    write.csv(history$k12_district_weekly_totals,
              file.path(output_dir, "k12_district_weekly_totals.csv"), row.names = FALSE)
  }

  report
}

if (sys.nframe() == 0) {
  source("k12_he_classification.R")
  source("bls_oews_scraper.R")
  source("history_accumulator.R")
  source(file.path("scripts", "rebuild_k12_history_from_archive.R"))

  args <- commandArgs(trailingOnly = TRUE)
  if (!all(args %in% c("--dry-run", "--apply")) || length(args) != 1) {
    stop("Usage: Rscript scripts/repair_no_jobs_placeholder.R --dry-run|--apply")
  }

  result <- repair_no_jobs_placeholder(write = identical(args, "--apply"))
  if (nrow(result) == 0) {
    cat("No '", NO_JOBS_PLACEHOLDER_TITLE, "' placeholder rows found; no files changed.\n", sep = "")
  } else {
    cat(
      if (identical(args, "--apply")) "Repaired " else "Would repair ",
      sum(result$rows_removed), " fabricated row(s) across ", nrow(result), " file(s).\n", sep = ""
    )
    print(result, row.names = FALSE)
    if (identical(args, "--apply")) {
      cat("Rebuilt k12jobanalysis.csv, allsum.csv, and k12_district_weekly_totals.csv from the cleaned archive.\n")
    } else {
      cat("Run again with --apply to write the repaired snapshots and rebuild the derived history.\n")
    }
  }
}
