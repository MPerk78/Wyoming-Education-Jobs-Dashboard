# Title-quality check (classification safeguards items 5 and 6; functions in
# drift_check.R). Non-blocking and informational only, like check_drift.R:
#   - title-shape lint: this run's postings whose title isn't a job title
#     (page text, a summary, a level without its role, markup, pay the
#     Posted Pay column doesn't carry), listed per source;
#   - per-source Other share: a source whose share of postings the
#     classifier puts in Other jumped against its own recent weeks;
#   - keyword conflicts (item 9): titles whose words name a different bucket
#     from the one the classifier gave them.
# All go into /tmp/drift_report.md. Writes /tmp/title_lint.csv,
# /tmp/other_share_flagged.csv and /tmp/keyword_conflicts.csv either way.

suppressPackageStartupMessages(library(dplyr))
source("k12_he_classification.R")
source("drift_check.R")

app <- if (dir.exists("Mt_Ed_Jobs")) "Mt_Ed_Jobs" else "Wy_Ed_Jobs"
montana <- app == "Mt_Ed_Jobs"
lookback_weeks <- 8
today <- format(Sys.Date(), "%Y-%m-%d")
read_csv <- function(f) read.csv(f, stringsAsFactors = FALSE)
read_xlsx <- function(f) as.data.frame(readxl::read_xlsx(f))

# Same source naming as check_drift.R / check_pay_coverage.R: in Montana the
# OPI statewide rows collapse into one source (archives from before
# Posting_Source existed are skipped); in Wyoming archive district names are
# canonicalized, as combinedclean.csv already is.
k12_sources <- function(df) {
  if (montana) {
    if (!"Posting_Source" %in% names(df)) return(NULL)
    collapse_statewide_feed_names(df)
  } else {
    canonicalize_k12_district(df$District)
  }
}

k12 <- read_csv(file.path(app, "combinedclean.csv"))
he <- read_xlsx(file.path(app, "hedata.xlsx"))
k12$source <- k12_sources(k12)
he$source <- he$Institution

# ---- 5. title-shape lint ----
lint <- function(df, title_col, label) {
  reason <- lint_title_shape(df[[title_col]])
  pay_in_title <- if ("Pay_Min" %in% names(df)) title_pay_unrecorded(df[[title_col]], df$Pay_Min) else FALSE
  reason <- ifelse(pay_in_title, ifelse(is.na(reason), "pay in the title, Posted Pay blank",
                                        paste0(reason, "; pay in the title, Posted Pay blank")), reason)
  hit <- !is.na(reason)
  data.frame(source = df$source[hit], type = rep(label, sum(hit)), title = df[[title_col]][hit],
             reason = reason[hit], stringsAsFactors = FALSE)
}
hits <- rbind(lint(k12, "title", "K-12"), lint(he, "Title", "Higher Ed"))
hits <- hits[order(hits$type != "K-12", hits$source), , drop = FALSE]
cat(sprintf("Title lint: %d K-12 and %d HE posting(s) with a title that doesn't look like a job title, at %d source(s).\n",
            sum(hits$type == "K-12"), sum(hits$type == "Higher Ed"), length(unique(paste(hits$type, hits$source)))))
write.csv(hits, "/tmp/title_lint.csv", row.names = FALSE)

# ---- 6. per-source Other share against its own recent weeks ----
recent_archives <- function(dir, pattern) {
  files <- sort(list.files(dir, pattern = pattern, full.names = TRUE))
  files <- files[!grepl(today, basename(files), fixed = TRUE)]
  utils::tail(files, lookback_weeks)
}
k12_other <- function(df) {
  src <- k12_sources(df)
  if (is.null(src)) return(NULL)
  other_share_counts(src, classify_k12_position(df$title) == "Other")
}
he_other <- function(df) other_share_counts(df$Institution, classify_he_job_type(df$Title) == "Other")
k12_history <- Filter(Negate(is.null), lapply(recent_archives("Archivek12_Data", "^combined_.*\\.csv$"),
                                              function(f) k12_other(read_csv(f))))
he_history <- Filter(Negate(is.null), lapply(recent_archives("Archived_HE_Data", "^hedata_.*\\.xlsx$"),
                                             function(f) he_other(read_xlsx(f))))

k12$is_other <- classify_k12_position(k12$title) == "Other"
he$is_other <- classify_he_job_type(he$Title) == "Other"
flag <- function(df, history, title_col, label) {
  out <- flag_other_share_jump(other_share_counts(df$source, df$is_other), history)
  out$type <- rep(label, nrow(out))
  out$examples <- vapply(out$name, function(s) {
    t <- unique(df[[title_col]][df$source == s & df$is_other])
    paste(sprintf("\"%s\"", substr(utils::head(t, 3), 1, 60)), collapse = ", ")
  }, character(1))
  out
}
other_flagged <- rbind(flag(k12, k12_history, "title", "K-12"), flag(he, he_history, "Title", "Higher Ed"))
cat(sprintf("Other share: %.1f%% of K-12 and %.1f%% of HE postings in Other; %d source(s) flagged against their last %d K-12 / %d HE archived weeks.\n",
            100 * mean(k12$is_other), 100 * mean(he$is_other), nrow(other_flagged),
            length(k12_history), length(he_history)))
write.csv(other_flagged, "/tmp/other_share_flagged.csv", row.names = FALSE)

# ---- 9. keyword conflicts ----
conflicts <- function(df, title_col, bucket, rules, label) {
  rule <- find_keyword_conflicts(df[[title_col]], bucket, rules)
  hit <- !is.na(rule)
  data.frame(source = df$source[hit], type = rep(label, sum(hit)), title = df[[title_col]][hit],
             bucket = bucket[hit], rule = rule[hit], stringsAsFactors = FALSE)
}
conflict_hits <- rbind(conflicts(k12, "title", classify_k12_position(k12$title), k12_keyword_conflict_rules, "K-12"),
                       conflicts(he, "Title", classify_he_job_type(he$Title), he_keyword_conflict_rules, "Higher Ed"))
cat(sprintf("Keyword conflicts: %d K-12 and %d HE posting(s) whose title names a different bucket.\n",
            sum(conflict_hits$type == "K-12"), sum(conflict_hits$type == "Higher Ed")))
write.csv(conflict_hits, "/tmp/keyword_conflicts.csv", row.names = FALSE)

report_lines <- c(build_other_share_report_markdown(other_flagged), build_title_lint_report_markdown(hits),
                  build_keyword_conflict_report_markdown(conflict_hits))
if (length(report_lines) > 0) {
  if (nrow(other_flagged) > 0) print(other_flagged, row.names = FALSE)
  if (nrow(hits) > 0) print(hits[, c("source", "type", "reason")], row.names = FALSE)
  if (nrow(conflict_hits) > 0) print(conflict_hits[, c("source", "type", "bucket", "rule")], row.names = FALSE)
  if (file.exists("/tmp/drift_report.md")) {
    write(c("", "---", "", report_lines), file = "/tmp/drift_report.md", append = TRUE)
  } else {
    writeLines(report_lines, "/tmp/drift_report.md")
  }
}
