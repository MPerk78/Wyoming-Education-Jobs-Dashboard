#!/usr/bin/env Rscript
# .github/scripts/check_rebuild_parity.R
#
# Classification safeguards proposal, item 12: rebuilds the K-12 and HE
# derived datasets from the raw archives (scripts/rebuild_*_history_from_archive.R,
# in memory; nothing is written to Wy_Ed_Jobs/) and checks they reproduce the
# committed files, ignoring row order. A difference means the rebuild and the
# weekly pipeline have drifted apart, so the rebuild can no longer be trusted
# for disaster recovery or a reclassification (scripts/reclassify_committed_data.R).
#
# Non-blocking in the weekly workflow: differences go to /tmp/drift_report.md
# (the "Scraper drift check" issue) and /tmp/rebuild_parity.csv. Old weeks
# without a recorded OPI decision are re-decided against the current
# crosswalk and matcher, so a crosswalk change can move them; that is worth a
# look but never a reason to stop the data commit.
#
# Usage (repo root): Rscript .github/scripts/check_rebuild_parity.R

suppressPackageStartupMessages({ library(dplyr); library(readxl) })
for (f in c("k12_he_classification.R", "bls_oews_scraper.R", "history_accumulator.R",
            "scripts/rebuild_k12_history_from_archive.R", "scripts/rebuild_he_history_from_archive.R")) source(f)

app <- "Wy_Ed_Jobs"
k12 <- rebuild_k12_history_from_archive()
he <- rebuild_he_history_from_archive()
pairs <- list("combinedclean.csv" = k12$combinedclean, "k12jobanalysis.csv" = k12$k12jobs,
              "allsum.csv" = k12$allsum, "allnow.csv" = k12$allnow,
              "k12_district_weekly_totals.csv" = k12$k12_district_weekly_totals,
              "facultydata.csv" = he$facultydata, "allsum_he.csv" = he$allsum_he, "allnow_he.csv" = he$allnow_he,
              "he_institution_weekly_totals.csv" = he$he_institution_weekly_totals)
parity <- bind_rows(lapply(names(pairs), function(f) {
  rebuild_parity_row(f, read.csv(file.path(app, f), colClasses = "character", check.names = FALSE), pairs[[f]])
}))
write.csv(parity, "/tmp/rebuild_parity.csv", row.names = FALSE)
bad <- parity[nzchar(parity$Missing_Cols) | nzchar(parity$Extra_Cols) | parity$Only_Committed > 0 | parity$Only_Rebuilt > 0, ]
print(parity[, c("File", "Committed_Rows", "Rebuilt_Rows", "Only_Committed", "Only_Rebuilt")], row.names = FALSE)
if (nrow(bad) == 0) {
  cat("Rebuild parity: all", nrow(parity), "files reproduced from the archives.\n")
} else {
  cat("Rebuild parity: ", nrow(bad), " file(s) differ.\n", sep = "")
  lines <- c("### Rebuild from the archives no longer reproduces the committed data", "",
             "`scripts/rebuild_*_history_from_archive.R` and the weekly pipeline have drifted apart. Don't run a rebuild or `scripts/reclassify_committed_data.R` into the repo until this is understood (details in this run's log).", "",
             "| File | Committed rows | Rebuilt rows | Rows only committed | Rows only rebuilt | Columns missing / extra |",
             "|---|---|---|---|---|---|",
             sprintf("| `%s` | %d | %d | %d | %d | %s |", bad$File, bad$Committed_Rows, bad$Rebuilt_Rows, bad$Only_Committed,
                     bad$Only_Rebuilt, gsub("\\|", "/", paste(bad$Missing_Cols, "/", bad$Extra_Cols))))
  for (i in seq_len(nrow(bad))) if (nzchar(bad$Example[i])) cat(bad$File[i], "e.g.", substr(bad$Example[i], 1, 400), "\n")
  if (file.exists("/tmp/drift_report.md")) write(c("", "---", "", lines), file = "/tmp/drift_report.md", append = TRUE) else
    writeLines(lines, "/tmp/drift_report.md")
}
