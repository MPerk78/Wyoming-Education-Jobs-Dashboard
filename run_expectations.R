# Run expectations: what a person expects to see after a change lands, written
# down as rows the weekly run checks itself (workflow plan 2026-11-10, item 2.1).
# Each row of run_expectations.csv names a committed data file, an optional row
# filter, a measure that reduces the filtered rows to one number, and the range
# that number should fall in. .github/scripts/check_run_expectations.R
# evaluates every row after the pipeline and the data checks, and reports
# failures in the drift issue; the person reads a pass/fail list, not a log.
# Byte-identical in both state repos.
#
# Columns of run_expectations.csv:
#   id            short unique name
#   applies_from  first run date the row is checked (YYYY-MM-DD; the run after
#                 the change it checks lands)
#   expires       last run date the row is checked; later runs list it as expired
#   file          path from the repo root (.csv, or .xlsx read as text)
#   filter        R expression over the file's columns selecting rows; blank = all
#   measure       R expression giving one number; the selected rows are `d`, and
#                 columns are also visible by name. Blank = nrow(d)
#   min, max      inclusive bounds; blank = unbounded on that side
#   note          what the row checks and why (shown on failure)
# Expressions are evaluated with base R only (no packages), as written in this
# repo's own file; they never see scraped text as code.

RUN_EXPECTATION_COLUMNS <- c("id", "applies_from", "expires", "file", "filter", "measure", "min", "max", "note")

read_run_expectations <- function(path = "run_expectations.csv") {
  x <- utils::read.csv(path, colClasses = "character", na.strings = character(0), check.names = FALSE,
                       strip.white = TRUE, encoding = "UTF-8")
  missing <- setdiff(RUN_EXPECTATION_COLUMNS, names(x))
  if (length(missing) > 0) stop("run_expectations.csv is missing column(s): ", paste(missing, collapse = ", "))
  x[RUN_EXPECTATION_COLUMNS]
}

# Problems with the file itself (the test suite runs this on the committed file,
# so a typo fails before the weekly run rather than during it). Returns a
# character vector, empty when the file is fine. check_files = TRUE also
# requires each named file to exist; the committed-file test leaves it off,
# because a row may name a file only the coming run writes (posting_status.csv
# before WY's 2026-11-13 run): failing on that would stop the pre-run suite,
# and the run, that writes it. At run time a missing file is an `error` row.
validate_run_expectations <- function(exp, root = ".", check_files = FALSE) {
  probs <- character(0)
  add <- function(i, msg) probs <<- c(probs, sprintf("row %d (%s): %s", i, exp$id[i], msg))
  dup <- unique(exp$id[duplicated(exp$id)])
  if (length(dup) > 0) probs <- c(probs, paste("duplicate id:", paste(dup, collapse = ", ")))
  for (i in seq_len(nrow(exp))) {
    if (!nzchar(exp$id[i])) add(i, "blank id")
    af <- as.Date(exp$applies_from[i], optional = TRUE); ex <- as.Date(exp$expires[i], optional = TRUE)
    if (is.na(af)) add(i, "applies_from is not a YYYY-MM-DD date")
    if (is.na(ex)) add(i, "expires is not a YYYY-MM-DD date")
    if (!is.na(af) && !is.na(ex) && ex < af) add(i, "expires is before applies_from")
    if (!nzchar(exp$file[i])) add(i, "blank file")
    else if (check_files && !file.exists(file.path(root, exp$file[i]))) add(i, paste("no such file:", exp$file[i]))
    for (col in c("filter", "measure")) {
      if (nzchar(exp[[col]][i])) {
        ok <- tryCatch({ parse(text = exp[[col]][i]); TRUE }, error = function(e) FALSE)
        if (!ok) add(i, paste(col, "does not parse as R"))
      }
    }
    lo <- suppressWarnings(as.numeric(exp$min[i])); hi <- suppressWarnings(as.numeric(exp$max[i]))
    if (nzchar(exp$min[i]) && is.na(lo)) add(i, "min is not a number")
    if (nzchar(exp$max[i]) && is.na(hi)) add(i, "max is not a number")
    if (!nzchar(exp$min[i]) && !nzchar(exp$max[i])) add(i, "neither min nor max is set")
    if (!is.na(lo) && !is.na(hi) && lo > hi) add(i, "min is above max")
  }
  probs
}

read_expectation_file <- function(path) {
  if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    # read as text, then typed the way read.csv types a .csv, so a numeric
    # comparison in a filter is numeric for both formats
    utils::type.convert(as.data.frame(readxl::read_excel(path, col_types = "text"), stringsAsFactors = FALSE), as.is = TRUE)
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
  }
}

# One expectation against an already-read data frame. Returns list(value, error).
measure_expectation <- function(df, filter, measure) {
  tryCatch({
    d <- df
    if (nzchar(filter)) {
      keep <- eval(parse(text = filter), envir = df, enclos = baseenv())
      if (!is.logical(keep) || !(length(keep) %in% c(1L, nrow(df)))) stop("filter must give TRUE/FALSE per row")
      keep[is.na(keep)] <- FALSE
      d <- df[rep_len(keep, nrow(df)), , drop = FALSE]
    }
    value <- if (nzchar(measure)) eval(parse(text = measure), envir = c(as.list(d), list(d = d)), enclos = baseenv())
             else nrow(d)
    if (is.logical(value)) value <- as.numeric(value)
    if (!is.numeric(value) || length(value) != 1) stop("measure must give a single number")
    list(value = as.numeric(value), error = NA_character_)
  }, error = function(e) list(value = NA_real_, error = conditionMessage(e)))
}

# Every row against the files under `root` as of `run_date`. Status is one of
# pass, fail, error (file missing, expression failed, NA value), not_yet
# (applies_from is later), expired (expires is earlier).
evaluate_run_expectations <- function(exp, run_date = Sys.Date(), root = ".") {
  run_date <- as.Date(run_date)
  cache <- list()
  out <- exp
  out$Value <- NA_real_; out$Status <- NA_character_; out$Detail <- ""
  for (i in seq_len(nrow(exp))) {
    af <- as.Date(exp$applies_from[i], optional = TRUE); ex <- as.Date(exp$expires[i], optional = TRUE)
    if (!is.na(af) && run_date < af) { out$Status[i] <- "not_yet"; next }
    if (!is.na(ex) && run_date > ex) { out$Status[i] <- "expired"; next }
    f <- exp$file[i]
    if (is.null(cache[[f]])) {
      cache[[f]] <- tryCatch(suppressWarnings(read_expectation_file(file.path(root, f))),
                             error = function(e) structure(list(), class = "read_error", msg = conditionMessage(e)))
    }
    df <- cache[[f]]
    if (inherits(df, "read_error")) { out$Status[i] <- "error"; out$Detail[i] <- paste("could not read", f); next }
    m <- measure_expectation(df, exp$filter[i], exp$measure[i])
    out$Value[i] <- m$value
    if (!is.na(m$error)) { out$Status[i] <- "error"; out$Detail[i] <- m$error; next }
    if (is.na(m$value)) { out$Status[i] <- "error"; out$Detail[i] <- "measure gave NA"; next }
    lo <- suppressWarnings(as.numeric(exp$min[i])); hi <- suppressWarnings(as.numeric(exp$max[i]))
    ok <- (is.na(lo) || m$value >= lo) && (is.na(hi) || m$value <= hi)
    out$Status[i] <- if (ok) "pass" else "fail"
  }
  out
}

format_expectation_range <- function(lo, hi) {
  ifelse(nzchar(lo) & nzchar(hi), ifelse(lo == hi, lo, paste0(lo, " to ", hi)),
         ifelse(nzchar(lo), paste0(">= ", lo), paste0("<= ", hi)))
}

format_expectation_value <- function(v) ifelse(is.na(v), "", format(signif(v, 6), big.mark = ",", trim = TRUE))

# Markdown for the drift issue and the run summary. A one-line summary always;
# a table of failures and errors when there are any, and the expired ids so
# they can be deleted from the file.
build_run_expectations_report_markdown <- function(res) {
  n <- function(s) sum(res$Status == s)
  checked <- n("pass") + n("fail") + n("error")
  head_line <- sprintf("**Run expectations:** %d checked, %d passed, %d failed, %d errored (%d not yet due, %d expired).",
                       checked, n("pass"), n("fail"), n("error"), n("not_yet"), n("expired"))
  lines <- head_line
  bad <- res[res$Status %in% c("fail", "error"), , drop = FALSE]
  if (nrow(bad) > 0) {
    cell <- function(x) gsub("|", "\\|", gsub("\n", " ", x), fixed = TRUE)
    lines <- c(lines, "", "| id | status | value | expected | file | note |", "|---|---|---|---|---|---|",
               sprintf("| %s | %s | %s | %s | %s | %s |", cell(bad$id), bad$Status,
                       cell(ifelse(bad$Status == "error", bad$Detail, format_expectation_value(bad$Value))),
                       cell(format_expectation_range(bad$min, bad$max)), cell(bad$file), cell(bad$note)))
  }
  if (n("expired") > 0) {
    lines <- c(lines, "", paste0("Expired (delete from `run_expectations.csv`): ",
                                 paste(sprintf("`%s`", res$id[res$Status == "expired"]), collapse = ", "), "."))
  }
  lines
}
