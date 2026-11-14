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

# The board a row came from, for runs archived before Posting_Source was
# recorded (2026-11-12): filled only where the archived row settles it, NA
# otherwise.
#   - A source posting id names its platform.
#   - A row whose link is WSBA's vacancies page itself is a WSBA listing
#     with no link of its own.
#   - Before 2026-08-02 the pipeline read no WSBA rows (the WSBA scraper was
#     wired in that day), so every archived row came from a district's own
#     board and its link's host tells the platform; a link anywhere else is
#     the district's own site (hand-collected then, from otherjobs.xlsx).
#   - From 2026-08-02 on, a fingerprint id with any other link stays NA: a
#     WSBA row linking a district's posting and a direct row look the same.
K12_ID_PREFIX_SOURCES <- c(applitrack = "AppliTrack (direct district board)", schoolspring = "SchoolSpring (direct district board)",
                           redrover = "Red Rover (direct district board)")
K12_LINK_HOST_SOURCES <- c("applitrack[.]com" = "AppliTrack (direct district board)", "schoolspring[.]com" = "SchoolSpring (direct district board)",
                           "tedk12[.]com" = "TedK12 (direct district board)", "redroverk12[.]com" = "Red Rover (direct district board)")
K12_DISTRICT_SITE_SOURCE <- "District site (direct board)"
WSBA_FIRST_READ <- as.Date("2026-08-02")
WSBA_FEED_LABEL <- "WSBA statewide vacancies"   # state_config.R's feed label; test-history-accumulator.R holds the two together
WSBA_FEED_URL <- "https://www.wsba-wy.org/vacancies"
backfill_k12_posting_source <- function(posting_id, url, archive_date) {
  prefix <- sub(":.*$", "", tolower(as.character(posting_id)))
  out <- unname(K12_ID_PREFIX_SOURCES[prefix])
  out[is.na(out) & !is.na(url) & url == WSBA_FEED_URL] <- WSBA_FEED_LABEL
  before_wsba <- !is.na(archive_date) & as.Date(as.character(archive_date)) < WSBA_FIRST_READ
  host <- sub("^https?://([^/?#]+).*$", "\\1", tolower(as.character(url)))
  linked <- !is.na(url) & grepl("^https?://", url, ignore.case = TRUE)
  by_host <- rep(NA_character_, length(out))
  by_host[linked] <- K12_DISTRICT_SITE_SOURCE
  for (pattern in names(K12_LINK_HOST_SOURCES)) by_host[linked & grepl(pattern, host)] <- K12_LINK_HOST_SOURCES[[pattern]]
  fill <- is.na(out) & before_wsba
  out[fill] <- by_host[fill]
  out
}

# combinedclean.csv's rows from one run's K-12 postings, for the weekly
# pipeline (Wy_ED_Jobs.Rmd) and the from-archive rebuild alike. Callers
# dedupe first (distinct(), then collapse_duplicate_posting_ids() where the
# rows carry posting_id). Posting_Source and Also_On_Feed are recorded at
# scrape time from 2026-11-12; earlier runs get Posting_Source where
# backfill_k12_posting_source() can settle it and NA for Also_On_Feed.
# Feed_District is the organization WSBA names, for WSBA rows. Role_Hint is
# NA: WSBA gives no description to read one from.
wy_k12_combinedclean <- function(rows, oews_wages, district_county) {
  rows$Posting_Identity_Method <- k12_posting_identity_method(rows$posting_id)
  if (!"Posting_Source" %in% names(rows)) rows$Posting_Source <- NA_character_
  rows$Posting_Source <- dplyr::coalesce(as.character(rows$Posting_Source), backfill_k12_posting_source(rows$posting_id, rows$url, rows$Archive_Date))
  rows$Feed_District <- ifelse(rows$Posting_Source %in% WSBA_FEED_LABEL, as.character(rows$District), NA_character_)
  if ("Also_On_Feed" %in% names(rows)) rows$Also_On_Feed <- as.logical(rows$Also_On_Feed)
  build_k12_combinedclean(rows, oews_wages, district_county, "WY", columns = WY_K12_COLUMNS,
                          canonicalize_district = canonicalize_k12_district)
}
