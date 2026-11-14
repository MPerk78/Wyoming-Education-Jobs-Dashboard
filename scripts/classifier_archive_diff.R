#!/usr/bin/env Rscript
# scripts/classifier_archive_diff.R
#
# The review for any change to the classifier, which lives in the edjobs
# package since phase 2 of the package migration (2026-11-11): runs the
# classifier as pinned (edjobs tag v<EDJOBS_VERSION>, or --base <ref>) and the
# classifier in your edjobs working tree (--edjobs, default ../edjobs) over
# every distinct title in the raw archives, and lists every title whose bucket
# moves. Unit tests and the golden set only cover the titles someone thought
# of; this covers every title the pipeline has ever seen. On 2026-11-08 it was
# the only check that caught a "Bus Paraprofessional" rule also taking dual
# roles such as "Bus Driver/Classroom Paraprofessional". Read every line of
# the output: each move should be one you meant.
#
# Usage (state repo root):
#   Rscript scripts/classifier_archive_diff.R
#   Rscript scripts/classifier_archive_diff.R --edjobs ~/repos/Jobs_Dash_repos/edjobs --base v0.2.0
#   Rscript scripts/classifier_archive_diff.R --also ../Wyoming-Education-Jobs-Dashboard   # sibling's archives too
#   Rscript scripts/classifier_archive_diff.R --out moves.csv
# Identical in the MT and WY repos. reclassify_committed_data.R runs it first.

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(readxl) })

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
edjobs_dir <- arg_value("--edjobs", "../edjobs")
base_ref <- arg_value("--base", paste0("v", trimws(readLines("EDJOBS_VERSION", warn = FALSE)[1])))
CLASSIFIER_PATH <- "R/k12_he_classification.R"

# The classifier file at an edjobs ref, written to a temp file (stops if unreadable).
edjobs_classifier_at <- function(edjobs_dir, ref) {
  f <- tempfile(fileext = ".R")
  if (system2("git", c("-C", edjobs_dir, "show", paste0(ref, ":", CLASSIFIER_PATH)), stdout = f) != 0) {
    stop("Couldn't read ", CLASSIFIER_PATH, " at ", ref, " in ", edjobs_dir, " (pass --edjobs <edjobs checkout>).")
  }
  f
}
also <- arg_value("--also")
out_path <- arg_value("--out")

read_titles <- function(dirs, pattern, column) {
  files <- list.files(dirs, pattern = pattern, full.names = TRUE)
  titles <- unlist(lapply(files, function(f) {
    x <- if (grepl("\\.csv$", f)) utils::read.csv(f, colClasses = "character", check.names = FALSE)
         else as.data.frame(readxl::read_excel(f))
    col <- grep(paste0("^", column, "$"), names(x), ignore.case = TRUE)[1]
    if (is.na(col)) character(0) else as.character(x[[col]])
  }))
  t <- unique(titles[!is.na(titles) & nzchar(trimws(titles))])
  list(titles = t, files = length(files))
}

classifier_moves <- function(old_env, new_env, k12_titles, he_titles) {
  one <- function(kind, titles, fn, only = NULL) {
    if (length(titles) == 0) return(NULL)
    old <- get(fn, envir = old_env)(titles)
    new <- get(fn, envir = new_env)(titles)
    keep <- old != new
    if (!is.null(only)) keep <- keep & only(old_env, new_env, titles)
    if (!any(keep)) return(NULL)
    data.frame(kind = kind, title = titles[keep], old = old[keep], new = new[keep], stringsAsFactors = FALSE)
  }
  # Subject only matters where the title is (or becomes) a Teacher posting.
  teacher <- function(o, n, t) get("classify_k12_position", envir = o)(t) == "Teacher" |
    get("classify_k12_position", envir = n)(t) == "Teacher"
  dplyr::bind_rows(
    one("K-12 position", k12_titles, "classify_k12_position"),
    one("K-12 subject (Teacher rows)", k12_titles, "classify_k12_subject", only = teacher),
    one("HE job type", he_titles, "classify_he_job_type"),
    one("HE faculty category", he_titles, "classify_he_faculty_category")
  )
}

if (sys.nframe() == 0) {
  old_env <- new.env(); sys.source(edjobs_classifier_at(edjobs_dir, base_ref), envir = old_env)
  new_env <- new.env(); sys.source(file.path(edjobs_dir, CLASSIFIER_PATH), envir = new_env)

  roots <- c(".", also)
  k12 <- read_titles(file.path(roots, "Archivek12_Data"), "\\.(csv|xlsx)$", "title")
  he <- read_titles(file.path(roots, "Archived_HE_Data"), "\\.xlsx$", "title")
  moves <- classifier_moves(old_env, new_env, k12$titles, he$titles)

  cat(sprintf("Classifier edjobs %s -> edjobs working tree, over %d K-12 titles (%d archives) and %d HE titles (%d archives)%s.\n",
              base_ref, length(k12$titles), k12$files, length(he$titles), he$files,
              if (is.null(also)) "" else paste0(", including ", also)))
  if (is.null(moves) || nrow(moves) == 0) {
    cat("No title changes bucket.\n")
  } else {
    for (k in unique(moves$kind)) {
      m <- moves[moves$kind == k, ]
      cat(sprintf("\n%s: %d title(s) move\n", k, nrow(m)))
      print(dplyr::count(m, old, new, name = "titles"), row.names = FALSE)
      m <- m[order(m$old, m$new, m$title), ]
      cat(sprintf("  %s  [%s -> %s]\n", substr(gsub("\\s+", " ", m$title), 1, 100), m$old, m$new), sep = "")
    }
  }
  if (!is.null(out_path)) {
    utils::write.csv(if (is.null(moves)) data.frame(kind = character(), title = character(), old = character(),
                                                    new = character()) else moves,
                     out_path, row.names = FALSE)
    cat("\nWritten:", out_path, "\n")
  }
}
