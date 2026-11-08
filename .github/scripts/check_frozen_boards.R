# Frozen-board check (WY spells trust audit 2026-11-05, finding 2; functions in
# posting_spells.R and drift_check.R). Non-blocking and informational only,
# like check_drift.R: a board (K-12 district + its board host, or an HE
# institution) whose whole posting set has been identical for
# FROZEN_MIN_RUNS+ runs, still frozen this run. check_drift.R can't see it:
# the count is steady, which is exactly what it looks for.
# Appends to /tmp/drift_report.md; writes /tmp/frozen_boards.csv either way.

suppressPackageStartupMessages({ library(dplyr); library(readxl) })
source("k12_he_classification.R")
source("drift_check.R")
source("posting_spells.R")

lookback <- FROZEN_MIN_RUNS + 8
recent <- function(dir, pattern) utils::tail(sort(list.files(dir, pattern = pattern, full.names = TRUE)), lookback)
run_of <- function(f) as.Date(sub("^[a-z]+_(\\d{4}-\\d{2}-\\d{2})\\..*$", "\\1", basename(f)))

k12 <- bind_rows(lapply(recent("Archivek12_Data", "^combined_\\d{4}-\\d{2}-\\d{2}\\.csv$"), function(f) {
  x <- read.csv(f, colClasses = "character")
  data.frame(Run_Date = run_of(f), District = canonicalize_k12_district(spell_text(x$District)),
             Posting_Source = k12_posting_source(x$url), title = spell_text(x$title), stringsAsFactors = FALSE)
}))
k12 <- k12[!is.na(k12$title) & !is_placeholder_posting(k12$title), , drop = FALSE]
he <- bind_rows(lapply(recent("Archived_HE_Data", "^hedata_\\d{4}-\\d{2}-\\d{2}\\.xlsx$"), function(f) {
  x <- as.data.frame(read_excel(f, col_types = "text"))
  inst <- canonicalize_he_institution(spell_text(x$Institution))
  data.frame(Run_Date = run_of(f), District = inst, Posting_Source = inst, title = spell_text(x$Title),
             stringsAsFactors = FALSE)
}))
he <- he[!is.na(he$title) & !is_placeholder_posting(he$title), , drop = FALSE]

frozen <- bind_rows(mutate(frozen_board_runs(k12), Segment = "K-12"), mutate(frozen_board_runs(he), Segment = "HE"))
ongoing <- frozen[frozen$Ongoing, , drop = FALSE]
cat(sprintf("Frozen boards: %d board(s) unchanged for %d+ runs with %d+ postings and still frozen (over the last %d K-12 / %d HE runs).\n",
            nrow(ongoing), FROZEN_MIN_RUNS, FROZEN_MIN_POSTINGS, length(unique(k12$Run_Date)), length(unique(he$Run_Date))))
write.csv(frozen, "/tmp/frozen_boards.csv", row.names = FALSE)

report_lines <- build_frozen_board_report_markdown(frozen)
if (length(report_lines) > 0) {
  print(ongoing, row.names = FALSE)
  if (file.exists("/tmp/drift_report.md")) {
    write(c("", "---", "", report_lines), file = "/tmp/drift_report.md", append = TRUE)
  } else {
    writeLines(report_lines, "/tmp/drift_report.md")
  }
}
