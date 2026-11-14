# Posted-pay coverage check (see flag_pay_coverage_drop() in drift_check.R).
# Non-blocking and informational only, like check_drift.R: a source whose
# postings still arrive but have lost the pay they used to carry is added
# to /tmp/drift_report.md. Writes /tmp/pay_coverage_flagged.csv either way.

source("drift_check.R")
source("k12_he_classification.R")

# Same source naming as check_drift.R: archive district names are
# canonicalized on read, since combinedclean.csv already is.
k12_pay_counts <- function(df) {
  if (!"Pay_Min" %in% names(df)) return(NULL)
  pay_coverage_counts(canonicalize_k12_district(df$District), !is.na(df$Pay_Min))
}

he_pay_counts <- function(df) {
  if (!"Pay_Min" %in% names(df)) return(NULL)
  pay_coverage_counts(df$Institution, !is.na(df$Pay_Min))
}

archive_history <- function(dir, pattern, reader, counter, exclude_date) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  files <- files[!grepl(exclude_date, basename(files), fixed = TRUE)]
  Filter(Negate(is.null), lapply(files, function(f) counter(reader(f))))
}

today <- format(Sys.Date(), "%Y-%m-%d")
read_csv <- function(f) read.csv(f, stringsAsFactors = FALSE)
read_xlsx <- function(f) as.data.frame(readxl::read_xlsx(f))

k12_history <- archive_history("Archivek12_Data", "^combined_.*\\.csv$", read_csv, k12_pay_counts, today)
he_history <- archive_history("Archived_HE_Data", "^hedata_.*\\.xlsx$", read_xlsx, he_pay_counts, today)

k12_current <- k12_pay_counts(read_csv("Wy_Ed_Jobs/combinedclean.csv"))
he_current <- he_pay_counts(read_xlsx("Wy_Ed_Jobs/hedata.xlsx"))

flag <- function(current, history, label) {
  if (is.null(current)) return(NULL)
  out <- flag_pay_coverage_drop(current, history)
  out$type <- rep(label, nrow(out))
  out
}
flagged <- rbind(flag(k12_current, k12_history, "K-12"), flag(he_current, he_history, "Higher Ed"))
if (is.null(flagged)) {
  flagged <- data.frame(name = character(0), mean_share = numeric(0), n_weeks = integer(0),
                        n = integer(0), with_pay = integer(0), type = character(0))
}

cat(sprintf("Pay coverage: %d K-12 and %d HE archived weeks with pay, %d source(s) flagged.\n",
            length(k12_history), length(he_history), nrow(flagged)))
write.csv(flagged, "/tmp/pay_coverage_flagged.csv", row.names = FALSE)

report_lines <- build_pay_coverage_report_markdown(flagged)
if (length(report_lines) > 0) {
  print(flagged)
  if (file.exists("/tmp/drift_report.md")) {
    write(c("", "---", "", report_lines), file = "/tmp/drift_report.md", append = TRUE)
  } else {
    writeLines(report_lines, "/tmp/drift_report.md")
  }
}

# Posted amounts drop_implausible_pay() blanked this week, from today's raw
# archives (which keep pay as scraped).
implausible <- function(df, source_col, title_col, label) {
  if (is.null(df) || !"Pay_Min" %in% names(df)) return(NULL)
  bad <- is_implausible_pay(df$Pay_Min, df$Pay_Max, df$Pay_Unit)
  if (!any(bad)) return(NULL)
  data.frame(source = df[[source_col]][bad], title = df[[title_col]][bad], df[bad, POSTED_PAY_COLUMNS],
             type = label, stringsAsFactors = FALSE, row.names = NULL)
}
todays <- function(dir, pattern, reader) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  f <- f[grepl(today, basename(f), fixed = TRUE)]
  if (length(f) == 0) NULL else reader(f[1])
}
bad_pay <- rbind(implausible(todays("Archivek12_Data", "^combined_.*\\.csv$", read_csv), "District", "title", "K-12"),
                 implausible(todays("Archived_HE_Data", "^hedata_.*\\.xlsx$", read_xlsx), "Institution", "Title", "Higher Ed"))
cat(sprintf("Implausible posted pay blanked this run: %d\n", if (is.null(bad_pay)) 0L else nrow(bad_pay)))
if (!is.null(bad_pay)) {
  print(bad_pay[, c("source", "title", "Pay_Min", "Pay_Max", "Pay_Unit")])
  bad_lines <- build_implausible_pay_report_markdown(bad_pay)
  if (file.exists("/tmp/drift_report.md")) {
    write(c("", "---", "", bad_lines), file = "/tmp/drift_report.md", append = TRUE)
  } else {
    writeLines(bad_lines, "/tmp/drift_report.md")
  }
}
