# Repairs the historical University of Wyoming duplication caused by Oracle
# ignoring top-level limit/offset query parameters. This is deliberately a
# narrow migration: it only removes repeated direct Oracle requisition IDs
# after proving every copy of each repeated ID is otherwise identical.
#
# Run from the repository root:
#   Rscript scripts/repair_uw_pagination_duplicates.R --dry-run
#   Rscript scripts/repair_uw_pagination_duplicates.R --apply
#
# --apply updates the affected raw HE snapshots and current hedata.xlsx, then
# deterministically rebuilds facultydata.csv, allsum_he.csv,
# he_institution_weekly_totals.csv, and allnow_he.csv. Git retains the
# before-state for review/reversion; inspect git diff before committing.

suppressMessages(library(dplyr))

UW_INSTITUTION <- "University of Wyoming"
UW_ORACLE_JOB_URL <- paste0(
  "^https://eeik\\.fa\\.us2\\.oraclecloud\\.com/hcmUI/",
  "CandidateExperience/en/sites/[^/]+/job/([^/?#]+)(?:[?#].*)?$"
)

uw_oracle_posting_id <- function(links) {
  links <- as.character(links)
  is_oracle_job <- !is.na(links) & grepl(UW_ORACLE_JOB_URL, links)
  ids <- rep(NA_character_, length(links))
  ids[is_oracle_job] <- sub(UW_ORACLE_JOB_URL, "\\1", links[is_oracle_job])
  ids
}

empty_uw_repair_report <- function() {
  data.frame(
    posting_id = character(0),
    duplicate_rows_removed = integer(0),
    stringsAsFactors = FALSE
  )
}

# Pure, fail-closed repair step. A repeated Oracle ID with any conflicting
# value is not automatically resolved because that would hide a source-side
# change rather than repair a known repeated page.
repair_uw_oracle_duplicates <- function(postings, source_name = "<data>") {
  required <- c("Institution", "Link")
  missing <- setdiff(required, names(postings))
  if (length(missing) > 0) {
    stop(
      "repair_uw_oracle_duplicates(): ", source_name,
      " is missing required column(s): ", paste(missing, collapse = ", ")
    )
  }

  posting_ids <- uw_oracle_posting_id(postings$Link)
  eligible <- postings$Institution == UW_INSTITUTION & !is.na(posting_ids)
  eligible_ids <- posting_ids[eligible]
  duplicate_ids <- unique(eligible_ids[duplicated(eligible_ids)])

  if (length(duplicate_ids) == 0) {
    return(list(data = postings, report = empty_uw_repair_report()))
  }

  report <- lapply(duplicate_ids, function(id) {
    rows <- postings[eligible & posting_ids == id, , drop = FALSE]
    varies <- vapply(
      rows,
      function(column) length(unique(as.character(column))) > 1,
      logical(1)
    )
    if (any(varies)) {
      stop(
        "repair_uw_oracle_duplicates(): ", source_name,
        " has conflicting copies of Oracle requisition ", id, " in column(s): ",
        paste(names(rows)[varies], collapse = ", "),
        ". Refusing to guess which record is correct."
      )
    }
    data.frame(
      posting_id = id,
      duplicate_rows_removed = nrow(rows) - 1L,
      stringsAsFactors = FALSE
    )
  })
  report <- do.call(rbind, report)

  keep <- rep(TRUE, nrow(postings))
  keep[which(eligible)] <- !duplicated(eligible_ids)
  list(data = postings[keep, , drop = FALSE], report = report)
}

repair_uw_oracle_workbook <- function(path, write = FALSE) {
  postings <- as.data.frame(readxl::read_xlsx(path), stringsAsFactors = FALSE)
  result <- repair_uw_oracle_duplicates(postings, path)
  if (write && nrow(result$report) > 0) {
    writexl::write_xlsx(result$data, path)
  }
  result$report <- data.frame(
    file = rep(path, nrow(result$report)),
    result$report,
    stringsAsFactors = FALSE
  )
  result
}

# Recreates allnow_he.csv from the repaired current snapshot. The accumulated
# HE history is rebuilt separately from the archive snapshots.
rebuild_current_he_aggregates <- function(hedata) {
  required <- c("Title", "Location", "Posted_Date", "Institution", "Link", "Archive_Date")
  missing <- setdiff(required, names(hedata))
  if (length(missing) > 0) {
    stop(
      "rebuild_current_he_aggregates(): missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }

  hedata <- hedata %>%
    dplyr::mutate(Institution = canonicalize_he_institution(Institution))
  facultydata <- hedata %>%
    dplyr::mutate(Job_Type = classify_he_job_type(Title)) %>%
    dplyr::filter(Job_Type %in% c("Instructor/Teacher/Faculty", "Adjunct/Part-Time Faculty")) %>%
    dplyr::mutate(Category = classify_he_faculty_category(Title))

  total <- facultydata %>%
    dplyr::group_by(Category, Job_Type) %>%
    dplyr::summarize(Sum = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(Institution = "Total") %>%
    dplyr::select(Category, Job_Type, Sum, Institution)
  by_institution <- facultydata %>%
    dplyr::group_by(Category, Institution, Job_Type) %>%
    dplyr::summarize(Sum = dplyr::n(), .groups = "drop") %>%
    dplyr::select(Category, Job_Type, Sum, Institution)

  list(allnow_he = dplyr::bind_rows(total, by_institution))
}

repair_uw_pagination_artifacts <- function(
    archive_dir = "Archived_HE_Data",
    current_hedata_path = file.path("Wy_Ed_Jobs", "hedata.xlsx"),
    output_dir = "Wy_Ed_Jobs",
    write = FALSE) {
  archive_files <- sort(list.files(
    archive_dir, pattern = "^hedata_.*\\.xlsx$", full.names = TRUE
  ))
  if (length(archive_files) == 0) {
    stop("repair_uw_pagination_artifacts(): no HE archive snapshots found in ", archive_dir)
  }
  if (!file.exists(current_hedata_path)) {
    stop("repair_uw_pagination_artifacts(): current snapshot not found at ", current_hedata_path)
  }

  archive_results <- lapply(archive_files, repair_uw_oracle_workbook, write = write)
  current_result <- repair_uw_oracle_workbook(current_hedata_path, write = write)
  reports <- c(lapply(archive_results, `[[`, "report"), list(current_result$report))
  reports <- Filter(function(report) nrow(report) > 0, reports)
  report <- if (length(reports) == 0) {
    data.frame(
      file = character(0),
      posting_id = character(0),
      duplicate_rows_removed = integer(0),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, reports)
  }

  current_aggregates <- rebuild_current_he_aggregates(current_result$data)
  if (write) {
    history <- rebuild_he_history_from_archive(archive_dir)
    utils::write.csv(history$facultydata,
                     file.path(output_dir, "facultydata.csv"), row.names = FALSE)
    utils::write.csv(history$allsum_he,
                     file.path(output_dir, "allsum_he.csv"), row.names = FALSE)
    utils::write.csv(history$he_institution_weekly_totals,
                     file.path(output_dir, "he_institution_weekly_totals.csv"), row.names = FALSE)
    utils::write.csv(current_aggregates$allnow_he,
                     file.path(output_dir, "allnow_he.csv"), row.names = FALSE)
  }

  list(report = report, current_aggregates = current_aggregates)
}

if (sys.nframe() == 0) {
  source("k12_he_classification.R")
  source(file.path("scripts", "rebuild_he_history_from_archive.R"))

  args <- commandArgs(trailingOnly = TRUE)
  if (!all(args %in% c("--dry-run", "--apply")) || length(args) != 1) {
    stop("Usage: Rscript scripts/repair_uw_pagination_duplicates.R --dry-run|--apply")
  }

  result <- repair_uw_pagination_artifacts(write = identical(args, "--apply"))
  if (nrow(result$report) == 0) {
    cat("No repeated direct Oracle requisition IDs found; no files changed.\n")
  } else {
    cat(
      if (identical(args, "--apply")) "Repaired " else "Would repair ",
      sum(result$report$duplicate_rows_removed), " duplicate UW rows across ",
      length(unique(result$report$file)), " workbook(s).\n", sep = ""
    )
    print(result$report, row.names = FALSE)
    if (identical(args, "--dry-run")) {
      cat("Run again with --apply to write the repaired snapshots and derived datasets.\n")
    }
  }
}
