# The accumulated history files (combinedclean.csv, k12jobanalysis.csv, the
# weekly totals). Before this, Wy_ED_Jobs.Rmd re-read and re-classified every
# archive file on every weekly run (100+ files, one more each week since
# August 2024) to derive datasets that only gain one week of rows: O(all
# history) work against a hard CI timeout. The pipeline now classifies only
# this run's rows and appends them. The raw archives are still written every
# week as the durable source of truth, and
# scripts/rebuild_k12_history_from_archive.R /
# rebuild_he_history_from_archive.R rebuild the accumulated files from them;
# tests/testthat/test-history-accumulator.R checks the two stay equivalent.
#
# append_weekly_rows(), summarize_k12_posting_counts(),
# build_k12_combinedclean() and build_k12_teacher_rows() are in the edjobs
# package since 0.10.0 (phase 4, 2026-11-12), with one K-12 output schema
# for every state (edjobs::K12_POSTING_COLUMNS). check_total_matches_parts()
# and rebuild_parity_row() moved in phase 2. Attached here so every script
# that sources this file still has them.
suppressMessages(library(edjobs))

suppressMessages(library(dplyr))

# Wyoming's raw rows and archives call the posting id posting_id; the
# accumulated files use the shared name, Posting_ID. The values are the same.
WY_K12_COLUMNS <- c(Posting_ID = "posting_id")

# How a posting_id was made (build_k12_posting_id(), k12_he_classification.R):
# the board's own id for the posting, or a fingerprint of its fields.
WY_ID_FROM_SOURCE <- "Source posting ID"
WY_ID_FROM_FIELDS <- "Source fallback: district + link + title + location + posted date (no source posting ID)"
k12_posting_identity_method <- function(posting_id) {
  ifelse(is.na(posting_id), NA_character_, ifelse(startsWith(as.character(posting_id), "fallback"), WY_ID_FROM_FIELDS, WY_ID_FROM_SOURCE))
}

# combinedclean.csv's rows from one run's K-12 postings, for the weekly
# pipeline (Wy_ED_Jobs.Rmd) and the from-archive rebuild alike. Callers
# dedupe first (distinct(), then collapse_duplicate_posting_ids() where the
# rows carry posting_id). Wyoming does not yet record Posting_Source,
# Feed_District, Also_On_Feed or Role_Hint; those columns are NA.
wy_k12_combinedclean <- function(rows, oews_wages, district_county) {
  rows$Posting_Identity_Method <- k12_posting_identity_method(rows$posting_id)
  build_k12_combinedclean(rows, oews_wages, district_county, "WY", columns = WY_K12_COLUMNS,
                          canonicalize_district = canonicalize_k12_district)
}
