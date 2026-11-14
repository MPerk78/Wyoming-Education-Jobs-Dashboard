# Blocking schema-drift check, run right after sanity_check.R (before
# anything gets committed) -- see schema_check.R's header for why this has
# to be a hard gate rather than a diagnostic-only check like drift_check.R.

source("schema_check.R")

problems <- list()

for (f in names(REQUIRED_SCHEMAS)) {
  path <- file.path("Wy_Ed_Jobs", f)
  if (!file.exists(path)) {
    problems[[length(problems) + 1]] <- data.frame(file = f, missing_column = "<entire file missing>", stringsAsFactors = FALSE)
    next
  }
  actual_cols <- names(read.csv(path, nrows = 1, check.names = FALSE))
  result <- check_file_schema(f, actual_cols, REQUIRED_SCHEMAS[[f]])
  if (!is.null(result)) problems[[length(problems) + 1]] <- result
}

for (f in names(REQUIRED_SCHEMAS_XLSX)) {
  path <- file.path("Wy_Ed_Jobs", f)
  if (!file.exists(path)) {
    problems[[length(problems) + 1]] <- data.frame(file = f, missing_column = "<entire file missing>", stringsAsFactors = FALSE)
    next
  }
  actual_cols <- names(readxl::read_xlsx(path, n_max = 1))
  result <- check_file_schema(f, actual_cols, REQUIRED_SCHEMAS_XLSX[[f]])
  if (!is.null(result)) problems[[length(problems) + 1]] <- result
}

if (length(problems) > 0) {
  all_problems <- do.call(rbind, problems)
  cat("Schema check FAILED:\n")
  print(all_problems)
  stop(
    "Schema check failed: ", nrow(all_problems), " missing column(s) across ",
    length(unique(all_problems$file)), " file(s) -- see above. app.R references these ",
    "columns by name, so this would crash the live dashboard for every user on the next ",
    "deploy. Refusing to commit. Fix whichever pipeline chunk produces the affected ",
    "file(s), or if this is an intentional schema change, update REQUIRED_SCHEMAS in ",
    "schema_check.R to match."
  )
}

cat("Schema check passed -- every shipped dataset has all the columns app.R expects.\n")
