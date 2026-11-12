# Disaster-recovery / verification tool: rebuilds the K-12 accumulated
# datasets (k12jobanalysis.csv, allsum.csv, k12_district_weekly_totals.csv)
# from scratch by reprocessing every raw snapshot in Archivek12_Data/, the
# same way the weekly pipeline used to do on every single run before
# history_accumulator.R's incremental-append path replaced that.
#
# Two real uses:
#   1. Disaster recovery -- if a shipped accumulated CSV is ever lost or
#      believed corrupted, this rebuilds it from the durable raw archive
#      (which the weekly pipeline still writes every run, unchanged).
#   2. Ground truth for tests/testthat/test-history-accumulator.R, which
#      checks that incrementally appending one new week onto an existing
#      accumulated file reproduces exactly what a full from-scratch
#      rebuild through that same week would produce.
#
# Run directly (Rscript scripts/rebuild_k12_history_from_archive.R) to
# actually rebuild and overwrite the files on disk; source() it from
# elsewhere (as the tests do) to get rebuild_k12_history_from_archive()
# without triggering that.

suppressMessages(library(dplyr))

rebuild_k12_history_from_archive <- function(archive_dir = "Archivek12_Data",
                                             reference_dir = here::here("Wy_Ed_Jobs")) {
  # The pay-estimate inputs the pipeline uses (Wy_ED_Jobs.Rmd): OEWS wages and
  # each district's county from salarymap2.csv.
  oews_wages <- read.csv(file.path(reference_dir, "oews_role_wages.csv"), stringsAsFactors = FALSE,
                         colClasses = c(Area_Code = "character", SOC = "character", Year = "character"))
  district_county <- read.csv(file.path(reference_dir, "salarymap2.csv"), stringsAsFactors = FALSE)[, c("District", "County")]
  csv_files <- list.files(archive_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) {
    stop("rebuild_k12_history_from_archive(): no archive files found in ", archive_dir)
  }

  # Build each weekly snapshot's rows independently and dedupe byte-identical
  # RAW rows within that snapshot -- exactly what Wy_ED_Jobs.Rmd does to one
  # run's `combined` before classify_k12_position() overwrites the raw
  # `position` column. A modern direct-platform page that lists the same
  # opening once per building it applies to (e.g. Sweetwater County SD1's
  # Applitrack "?all=1" listing) arrives as N identical rows; build_k12_
  # posting_id() collapses them to one posting_id, but k12_district_weekly_
  # totals below is a raw count() of rows, so the duplicates inflated a
  # handful of districts' weekly totals. This has to run on the raw row:
  # older SchoolSpring snapshots put the building name in `position`, which
  # legitimately distinguishes ~26 "one coach per elementary school"
  # postings that share a title -- dedupe after classify_k12_position()
  # buckets that away and they collapse wrongly. Deliberately per-file:
  # byte-identical rows across two DIFFERENT snapshots carrying the same
  # Archive_Date (a few of the earliest archive files overlap that way) are
  # a separate, still-open issue, left alone here so this path stays
  # equivalent to the incremental one (test-history-accumulator.R), which
  # only ever sees a single run's rows at a time.
  combinedclean <- csv_files %>%
    lapply(function(file) {
      df <- read.csv(file, colClasses = c("Archive_Date" = "character")) %>%
        select(-any_of("X")) %>%
        distinct()
      has_source_ids <- "posting_id" %in% names(df)
      if (!has_source_ids) {
        df$posting_id <- NA_character_
      }
      if (!"date_posted" %in% names(df)) {
        df$date_posted <- NA_character_
      }
      df <- df %>%
        mutate(
          title = fix_title_encoding(title),
          Archive_Date = case_when(
            grepl("/", Archive_Date) ~ as.Date(Archive_Date, format = "%m/%d/%Y"),
            grepl("-", Archive_Date) ~ as.Date(Archive_Date, format = "%Y-%m-%d"),
            TRUE ~ as.Date(NA)
          ),
          District = canonicalize_k12_district(District),
          posting_id = build_k12_posting_id(
            source_id = posting_id,
            title = title,
            location = location,
            date_posted = date_posted,
            url = url,
            district = District
          )
        )
      # Mirrors Wy_ED_Jobs.Rmd's collapse_duplicate_posting_ids() -- only for
      # snapshots the modern pipeline wrote (they carry posting_id), since
      # that step never ran on the older ones, whose building-in-`position`
      # rows would otherwise collapse wrongly (see above). Then the same
      # builder the pipeline uses (history_accumulator.R).
      if (has_source_ids) {
        df <- collapse_duplicate_posting_ids(df, basename(file))
      }
      for (col in intersect(c("Pay_Min", "Pay_Max"), names(df))) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      for (col in intersect(c("Pay_Unit", "Pay_Text", "Close_Date"), names(df))) df[[col]] <- as.character(df[[col]])
      wy_k12_combinedclean(df, oews_wages, district_county)
    }) %>%
    bind_rows() %>%
    filter(!is_placeholder_posting(title))

  k12jobs <- build_k12_teacher_rows(combinedclean)

  k12_district_weekly_totals <- combinedclean %>%
    count(District, Archive_Date, name = "n")

  k12sumdistrict <- summarize_k12_posting_counts(k12jobs)

  k12sum <- k12sumdistrict %>%
    group_by(Broad_Category, Archive_Date) %>%
    summarize(sum = sum(sum), .groups = "drop") %>%
    mutate(District = "Total") %>%
    select(Broad_Category, Archive_Date, District, sum)

  allsum <- bind_rows(k12sum, k12sumdistrict)
  latest_k12sumdistrict <- k12sumdistrict %>%
    filter(Archive_Date == max(Archive_Date))
  allnow <- bind_rows(
    latest_k12sumdistrict %>%
      group_by(Broad_Category) %>%
      summarize(Sum = sum(sum), .groups = "drop") %>%
      mutate(District = "Total"),
    latest_k12sumdistrict %>%
      transmute(Broad_Category, Sum = sum, District)
  )

  list(
    combinedclean = combinedclean %>% filter(Archive_Date == max(Archive_Date)),
    k12jobs = k12jobs,
    k12_district_weekly_totals = k12_district_weekly_totals,
    allsum = allsum,
    allnow = allnow
  )
}

# Only runs when this file is executed directly (Rscript ...), not when
# source()'d -- source() adds a stack frame, so sys.nframe() > 0 while its
# top-level statements evaluate, whereas a directly Rscript'd file's
# top-level statements run at sys.nframe() == 0.
if (sys.nframe() == 0) {
  source("k12_he_classification.R")
  library(edjobs)
  source("history_accumulator.R")
  result <- rebuild_k12_history_from_archive()

  write.csv(result$combinedclean, file.path("Wy_Ed_Jobs", "combinedclean.csv"), row.names = FALSE)
  write.csv(result$k12jobs, file.path("Wy_Ed_Jobs", "k12jobanalysis.csv"), row.names = FALSE)
  write.csv(result$allsum, file.path("Wy_Ed_Jobs", "allsum.csv"), row.names = FALSE)
  write.csv(result$allnow, file.path("Wy_Ed_Jobs", "allnow.csv"), row.names = FALSE)
  write.csv(result$k12_district_weekly_totals, file.path("Wy_Ed_Jobs", "k12_district_weekly_totals.csv"), row.names = FALSE)

  cat("Rebuilt combinedclean.csv (", nrow(result$combinedclean), " rows), k12jobanalysis.csv (",
      nrow(result$k12jobs), " rows), allsum.csv/allnow.csv (",
      nrow(result$allsum), "/", nrow(result$allnow), " rows), k12_district_weekly_totals.csv (",
      nrow(result$k12_district_weekly_totals), " rows) from ",
      length(list.files("Archivek12_Data", pattern = "\\.csv$")), " archive snapshots.\n", sep = "")
}
