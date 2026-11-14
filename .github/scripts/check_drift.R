# Tier 1 of the drift-alert system (see drift_check.R for the pure logic
# and its tests). Non-blocking and informational only -- unlike
# sanity_check.R, this never aborts the workflow. Writes
# /tmp/drift_flagged.csv, empty (header-only) if nothing looks off.

source("drift_check.R")
source("k12_he_classification.R")

# The raw archive snapshots (Archivek12_Data/combined_*.csv) keep whatever
# district spelling each scraper emitted -- e.g. Applitrack's "Converse
# County School Distrcit 2" -- while Wy_Ed_Jobs/combinedclean.csv (this
# week's current data, below) has already had canonicalize_k12_district()
# applied. Comparing the two directly makes every misspelled-then-corrected
# district look like a source that went from N postings/week to zero: its
# raw-name history has a real baseline, but its current count under that
# same raw name is 0 because every current row now carries the corrected
# name. Canonicalizing the archive names on read here is the same
# on-read normalization rebuild_k12_history_from_archive.R already does.
read_k12_archive <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  canonicalize_k12_district(df$District)
}

read_he_archive <- function(path) {
  df <- readxl::read_xlsx(path)
  df$Institution
}

load_snapshots <- function(dir, pattern, date_regex, reader, exclude_date) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  dates <- regmatches(files, regexpr(date_regex, files))
  keep <- dates != exclude_date
  files <- files[keep]
  dates <- dates[keep]

  snapshots <- lapply(files, function(f) data.frame(name = reader(f)))
  names(snapshots) <- dates
  snapshots
}

today <- format(Sys.Date(), "%Y-%m-%d")
date_regex <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"

k12_snapshots <- load_snapshots("Archivek12_Data", "^combined_.*\\.csv$", date_regex, read_k12_archive, today)
he_snapshots <- load_snapshots("Archived_HE_Data", "^hedata_.*\\.xlsx$", date_regex, read_he_archive, today)

# Weeks a source's scrape errored or wasn't logged (coverage calendar) say
# nothing about its posting count: left out of its baseline, not counted as 0
# (edjobs 0.12.0; Mike, 2026-11-13).
failed_weeks <- failed_source_weeks(if (file.exists("coverage_calendar.csv")) read.csv("coverage_calendar.csv", stringsAsFactors = FALSE))
k12_baseline <- build_historical_counts(k12_snapshots, "name", failed = failed_weeks)
he_baseline <- build_historical_counts(he_snapshots, "name", failed = failed_weeks)

# combinedclean.csv is already canonicalized; canonicalize again anyway so
# the two sides of the comparison can never fall out of sync if that ever
# changes (canonicalize_k12_district() is idempotent).
combined_raw <- read.csv("Wy_Ed_Jobs/combinedclean.csv", stringsAsFactors = FALSE)
registry <- if (file.exists("k12_district_registry.csv")) read.csv("k12_district_registry.csv", stringsAsFactors = FALSE) else NULL

current_k12 <- as.data.frame(table(canonicalize_k12_district(combined_raw$District)))
names(current_k12) <- c("name", "count")
current_he <- as.data.frame(table(readxl::read_xlsx("Wy_Ed_Jobs/hedata.xlsx")$Institution))
names(current_he) <- c("name", "count")

tag_type <- function(df, label) {
  if (nrow(df) > 0) df$type <- label
  df
}

flagged_k12 <- tag_type(flag_drift(current_k12, k12_baseline), "K-12")
flagged_he <- tag_type(flag_drift(current_he, he_baseline), "Higher Ed")

flagged <- if (nrow(flagged_k12) == 0 && nrow(flagged_he) == 0) {
  data.frame(name = character(0), mean_count = numeric(0), n_weeks = integer(0), count = numeric(0), type = character(0))
} else {
  rbind(flagged_k12, flagged_he)
}

# scrape_log.csv is this same run's own record of what actually errored vs.
# came back genuinely empty (see attach_scrape_log_errors()'s comment in
# drift_check.R) -- read it if the pipeline step before this one produced
# one. Its absence (a fresh checkout with nothing scraped yet) just means no
# source gets a scrape_error attached, same as before this existed.
scrape_log <- if (file.exists("scrape_log.csv")) {
  read.csv("scrape_log.csv", stringsAsFactors = FALSE)
} else {
  data.frame(timestamp = character(0), source = character(0), status = character(0),
             n_rows = integer(0), error_message = character(0))
}
flagged <- attach_scrape_log_errors(flagged, scrape_log)

# Central Safety Net status summary
safety_net_summary <- summarize_central_safety_net(
  combined_df = combined_raw,
  registry = registry,
  scrape_log = scrape_log,
  feed_label = WSBA_STATEWIDE_SOURCE,
  feed_short_name = "WSBA",
  discrepancy_path = "wy_discrepancy_report.csv"
)

flagged$safety_net_vacancies <- if (nrow(flagged) == 0) {
  integer(0)
} else {
  vapply(seq_len(nrow(flagged)), function(i) {
    if (flagged$type[i] == "K-12") {
      val <- safety_net_summary$fallback_districts[[flagged$name[i]]]
      if (!is.null(val)) as.integer(val) else 0L
    } else {
      NA_integer_
    }
  }, integer(1))
}

cat(sprintf(
  "Drift check: %d K-12 baseline sources, %d HE baseline sources, %d flagged (%d confirmed by scrape_log as errored).\n",
  nrow(k12_baseline), nrow(he_baseline), nrow(flagged), sum(!is.na(flagged$scrape_error))
))
cat(sprintf(
  "Statewide feed: %d postings; %d district(s) we scrape directly have %d posting(s) counted from it that their own board doesn't list.\n",
  safety_net_summary$total_feed_vacancies, length(safety_net_summary$fallback_districts), safety_net_summary$total_fallback_vacancies
))
if (nrow(flagged) > 0) print(flagged)

write.csv(flagged, "/tmp/drift_flagged.csv", row.names = FALSE)
