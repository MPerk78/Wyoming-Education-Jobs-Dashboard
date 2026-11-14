# Compares one of our archive snapshots against one of Mark's, per Mark's
# proposed 4-week audit: same-day archives, diffed record-by-record rather
# than by whole-table totals alone -- a raw count match can still hide one
# side missing postings another side over-counted by the same amount.
#
# Usage:
#   Rscript scripts/compare_with_mark.R <our_k12.csv> <mark_k12.csv> \
#     <our_he.xlsx> <mark_he.xlsx> [out_report.md]
#
# our_*/mark_* are single dated archive snapshots (Archivek12_Data/
# combined_<date>.csv and Archived_HE_Data/hedata_<date>.xlsx format,
# or Mark's equivalents with the same columns). Any pair of dates can be
# passed -- the report always states the two source dates and, if they
# differ, flags that the comparison isn't a clean same-day read.

suppressMessages({
  library(dplyr)
  library(readxl)
  library(stringr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0) sub("^--file=", "", script_arg) else "scripts/compare_with_mark.R"
repo_root <- dirname(dirname(normalizePath(script_path)))
setwd(repo_root)

source("k12_he_classification.R")
source("misc_district_scrapers.R")  # for normalize_title()

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
  stop("Usage: Rscript scripts/compare_with_mark.R <our_k12.csv> <mark_k12.csv> <our_he.xlsx> <mark_he.xlsx> [out_report.md]")
}
our_k12_path  <- args[[1]]
mark_k12_path <- args[[2]]
our_he_path   <- args[[3]]
mark_he_path  <- args[[4]]
out_path      <- if (length(args) >= 5) args[[5]] else "compare_with_mark_report.md"

read_archive_date <- function(path) {
  m <- regmatches(path, regexpr("\\d{4}-\\d{2}-\\d{2}", path))
  if (length(m) == 0) NA_character_ else m
}

# ---------------------------------------------------------------------------
# K-12: load + canonicalize district names so raw source typos don't
# masquerade as coverage differences between the two systems.
# ---------------------------------------------------------------------------
load_k12 <- function(path) {
  df <- read.csv(path, colClasses = "character")
  # Both our own raw archive and Mark's raw archive use the same X/title/
  # date_posted/position/location/url/District/Archive_Date shape.
  df %>%
    mutate(
      District = canonicalize_k12_district(District),
      key = paste(normalize_title(title), str_squish(tolower(location %||% "")), District, sep = " | ")
    )
}

# ---------------------------------------------------------------------------
# Higher Ed: load + canonicalize institution names.
# ---------------------------------------------------------------------------
load_he <- function(path) {
  df <- read_excel(path) %>% mutate(across(everything(), as.character))
  df %>%
    mutate(
      Institution = canonicalize_he_institution(Institution),
      key = paste(normalize_title(Title), str_squish(tolower(Location %||% "")), Institution, sep = " | ")
    )
}

count_diff_table <- function(our_df, mark_df, group_col) {
  our_counts  <- our_df  %>% count(.data[[group_col]], name = "ours")
  mark_counts <- mark_df %>% count(.data[[group_col]], name = "marks")
  full_join(our_counts, mark_counts, by = group_col) %>%
    mutate(ours = coalesce(ours, 0L), marks = coalesce(marks, 0L), diff = ours - marks) %>%
    arrange(desc(abs(diff)))
}

record_diffs <- function(our_df, mark_df) {
  list(
    ours_only  = our_df  %>% filter(!key %in% mark_df$key),
    marks_only = mark_df %>% filter(!key %in% our_df$key)
  )
}

md <- character(0)
add <- function(...) md[[length(md) + 1]] <<- paste0(...)

our_k12_date  <- read_archive_date(our_k12_path)
mark_k12_date <- read_archive_date(mark_k12_path)
our_he_date   <- read_archive_date(our_he_path)
mark_he_date  <- read_archive_date(mark_he_path)

add("# compare_with_mark report")
add("")
add("K-12: ours = `", our_k12_path, "` (", our_k12_date, ") vs. Mark's = `", mark_k12_path, "` (", mark_k12_date, ")")
if (!identical(our_k12_date, mark_k12_date)) {
  add("")
  add("**NOTE:** these are different dates (", our_k12_date, " vs. ", mark_k12_date,
      ") -- this is NOT a clean same-day comparison. Differences below may reflect ",
      "normal week-to-week posting/removal churn, not a real coverage gap. Treat this ",
      "as a first pass, not a verdict.")
}
add("")

k12_our  <- load_k12(our_k12_path)
k12_mark <- load_k12(mark_k12_path)

add("## K-12: totals")
add("")
add("- Ours: ", nrow(k12_our), " rows")
add("- Mark's: ", nrow(k12_mark), " rows")
add("")
add("## K-12: per-district counts (sorted by |diff|)")
add("")
k12_district_diff <- count_diff_table(k12_our, k12_mark, "District")
add("| District | Ours | Mark's | Diff |")
add("|---|---:|---:|---:|")
for (i in seq_len(nrow(k12_district_diff))) {
  r <- k12_district_diff[i, ]
  add("| ", r$District, " | ", r$ours, " | ", r$marks, " | ", r$diff, " |")
}
add("")

k12_rd <- record_diffs(k12_our, k12_mark)
add("## K-12: postings we have that Mark doesn't (", nrow(k12_rd$ours_only), ")")
add("")
if (nrow(k12_rd$ours_only) > 0) {
  for (i in seq_len(nrow(k12_rd$ours_only))) {
    r <- k12_rd$ours_only[i, ]
    add("- [", r$District, "] ", r$title, " -- ", r$location)
  }
}
add("")
add("## K-12: postings Mark has that we don't (", nrow(k12_rd$marks_only), ")")
add("")
if (nrow(k12_rd$marks_only) > 0) {
  for (i in seq_len(nrow(k12_rd$marks_only))) {
    r <- k12_rd$marks_only[i, ]
    add("- [", r$District, "] ", r$title, " -- ", r$location)
  }
}
add("")

he_our  <- load_he(our_he_path)
he_mark <- load_he(mark_he_path)

add("## Higher Ed: ours = `", our_he_path, "` (", our_he_date, ") vs. Mark's = `", mark_he_path, "` (", mark_he_date, ")")
if (!identical(our_he_date, mark_he_date)) {
  add("")
  add("**NOTE:** different dates (", our_he_date, " vs. ", mark_he_date, ") -- same caveat as above.")
}
add("")
add("## Higher Ed: totals")
add("")
add("- Ours: ", nrow(he_our), " rows")
add("- Mark's: ", nrow(he_mark), " rows")
add("")
add("## Higher Ed: per-institution counts (sorted by |diff|)")
add("")
he_inst_diff <- count_diff_table(he_our, he_mark, "Institution")
add("| Institution | Ours | Mark's | Diff |")
add("|---|---:|---:|---:|")
for (i in seq_len(nrow(he_inst_diff))) {
  r <- he_inst_diff[i, ]
  add("| ", r$Institution, " | ", r$ours, " | ", r$marks, " | ", r$diff, " |")
}
add("")

he_rd <- record_diffs(he_our, he_mark)
add("## Higher Ed: postings we have that Mark doesn't (", nrow(he_rd$ours_only), ")")
add("")
if (nrow(he_rd$ours_only) > 0) {
  for (i in seq_len(nrow(he_rd$ours_only))) {
    r <- he_rd$ours_only[i, ]
    add("- [", r$Institution, "] ", r$Title, " -- ", r$Location)
  }
}
add("")
add("## Higher Ed: postings Mark has that we don't (", nrow(he_rd$marks_only), ")")
add("")
if (nrow(he_rd$marks_only) > 0) {
  for (i in seq_len(nrow(he_rd$marks_only))) {
    r <- he_rd$marks_only[i, ]
    add("- [", r$Institution, "] ", r$Title, " -- ", r$Location)
  }
}
add("")

writeLines(md, out_path)
cat("Report written to", out_path, "\n")
cat("K-12 district diff summary:\n")
print(k12_district_diff)
cat("\nHE institution diff summary:\n")
print(he_inst_diff)
