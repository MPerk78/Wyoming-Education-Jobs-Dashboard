# Disaster-recovery / verification tool: rebuilds the Higher Ed accumulated
# datasets (facultydata.csv, allsum_he.csv, he_institution_weekly_totals.csv)
# from scratch by reprocessing every raw snapshot in Archived_HE_Data/, the
# same way the weekly pipeline used to do on every single run before
# history_accumulator.R's incremental-append path replaced that.
#
# See rebuild_k12_history_from_archive.R's header for the two real uses
# (disaster recovery, and ground truth for
# tests/testthat/test-history-accumulator.R) -- same reasoning applies here
# on the HE side.

suppressMessages(library(dplyr))

rebuild_he_history_from_archive <- function(archive_dir = "Archived_HE_Data") {
  xlsx_files <- list.files(archive_dir, pattern = "\\.xlsx$", full.names = TRUE)
  if (length(xlsx_files) == 0) {
    stop("rebuild_he_history_from_archive(): no archive files found in ", archive_dir)
  }

  # Archives from 2026-10-30 on also carry Pay_*, which the weekly path
  # keeps out of facultydata.csv; dropped here so the two stay equivalent.
  combined_HE_data <- xlsx_files %>%
    lapply(readxl::read_xlsx) %>%
    bind_rows() %>%
    select(-any_of(c(POSTED_PAY_COLUMNS, "Close_Date")))

  combined_HE_data$Archive_Date <- as.Date(combined_HE_data$Archive_Date)
  combined_HE_data <- combined_HE_data %>%
    mutate(Institution = canonicalize_he_institution(Institution)) %>%
    filter(!is_placeholder_posting(Title))

  combined_cat <- combined_HE_data %>%
    mutate(Job_Type = classify_he_job_type(Title))

  facultydata <- combined_cat %>%
    filter(Job_Type %in% c("Instructor/Teacher/Faculty", "Adjunct/Part-Time Faculty")) %>%
    mutate(Category = classify_he_faculty_category(Title))

  he_institution_weekly_totals <- combined_HE_data %>%
    count(Institution, Archive_Date, name = "n")

  hesum <- facultydata %>%
    group_by(Category, Archive_Date, Job_Type) %>%
    summarize(sum = n(), .groups = "drop") %>%
    mutate(Institution = "Total") %>%
    select(Category, Archive_Date, Institution, Job_Type, sum)

  hesuminst <- facultydata %>%
    group_by(Category, Archive_Date, Institution, Job_Type) %>%
    summarise(sum = n(), .groups = "drop")

  allsum_he <- bind_rows(hesum, hesuminst)

  latest_date <- max(facultydata$Archive_Date)
  latest_facultydata <- facultydata %>% filter(Archive_Date == latest_date)

  henowsum <- latest_facultydata %>%
    group_by(Category, Job_Type) %>%
    summarize(Sum = n(), .groups = "drop") %>%
    mutate(Institution = "Total") %>%
    select(Category, Job_Type, Sum, Institution)

  henowinstsum <- latest_facultydata %>%
    group_by(Category, Institution, Job_Type) %>%
    summarize(Sum = n(), .groups = "drop") %>%
    select(Category, Job_Type, Sum, Institution)

  allnow_he <- bind_rows(henowsum, henowinstsum)

  list(
    facultydata = facultydata,
    allsum_he = allsum_he,
    allnow_he = allnow_he,
    he_institution_weekly_totals = he_institution_weekly_totals
  )
}

# Only runs when this file is executed directly -- see
# rebuild_k12_history_from_archive.R's matching comment for why.
if (sys.nframe() == 0) {
  source("k12_he_classification.R")
  result <- rebuild_he_history_from_archive()

  write.csv(result$facultydata, file.path("Wy_Ed_Jobs", "facultydata.csv"), row.names = FALSE)
  write.csv(result$allsum_he, file.path("Wy_Ed_Jobs", "allsum_he.csv"), row.names = FALSE)
  write.csv(result$allnow_he, file.path("Wy_Ed_Jobs", "allnow_he.csv"), row.names = FALSE)
  write.csv(result$he_institution_weekly_totals, file.path("Wy_Ed_Jobs", "he_institution_weekly_totals.csv"), row.names = FALSE)

  cat("Rebuilt facultydata.csv (", nrow(result$facultydata), " rows), allsum_he.csv/allnow_he.csv (",
      nrow(result$allsum_he), "/", nrow(result$allnow_he), " rows), he_institution_weekly_totals.csv (",
      nrow(result$he_institution_weekly_totals), " rows) from ",
      length(list.files("Archived_HE_Data", pattern = "\\.xlsx$")), " archive snapshots.\n", sep = "")
}
