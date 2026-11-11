# Run expectations (run_expectations.R; workflow plan 2026-11-10, item 2.1).
# Non-blocking and informational, like check_drift.R: evaluates every row of
# run_expectations.csv against this run's freshly written files. Always writes
# the summary to the run's step summary page and /tmp/run_expectations.csv.
# Appends to /tmp/drift_report.md (which files or updates the drift issue)
# when a row failed or errored, or when the report already exists for another
# reason, so an all-pass week with nothing else to say files no issue.

source("run_expectations.R")

if (!file.exists("run_expectations.csv")) {
  cat("No run_expectations.csv, nothing to check.\n")
  quit(save = "no", status = 0)
}

run_date <- as.Date(Sys.getenv("RUN_DATE", unset = format(Sys.Date())))
exp <- read_run_expectations("run_expectations.csv")
probs <- validate_run_expectations(exp)
if (length(probs) > 0) cat("Problems in run_expectations.csv (rows still evaluated):\n", paste0("  ", probs, "\n"), sep = "")

res <- evaluate_run_expectations(exp, run_date = run_date)
write.csv(res, "/tmp/run_expectations.csv", row.names = FALSE)
for (i in seq_len(nrow(res))) {
  cat(sprintf("%-8s %-40s %s%s\n", res$Status[i], res$id[i],
              format_expectation_value(res$Value[i]),
              if (res$Status[i] %in% c("pass", "fail", "error"))
                paste0("  (expected ", format_expectation_range(res$min[i], res$max[i]), ")",
                       if (nzchar(res$Detail[i])) paste0(" ", res$Detail[i]) else "") else ""))
}

report_lines <- build_run_expectations_report_markdown(res)
cat(report_lines[1], "\n")

summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_path)) write(c(report_lines, ""), file = summary_path, append = TRUE)

needs_look <- any(res$Status %in% c("fail", "error"))
if (needs_look || file.exists("/tmp/drift_report.md")) {
  if (file.exists("/tmp/drift_report.md")) {
    write(c("", "---", "", report_lines), file = "/tmp/drift_report.md", append = TRUE)
  } else {
    writeLines(report_lines, "/tmp/drift_report.md")
  }
}
