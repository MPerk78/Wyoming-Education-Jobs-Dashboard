# Salary-source coverage check, run after the Rmd render against the
# just-regenerated salarymap2.csv (K-12/WSBA) and salarymap.csv (HE/IPEDS).
# Unlike check_drift.R (job postings, trailing statistical baseline), this
# checks against a known-fixed universe size -- see drift_check.R's
# check_salary_coverage() for why that's the right shape of check here.
# Diagnostic-only: never blocks the pipeline (see weekly-scrape.yml's
# continue-on-error on this step).

source("drift_check.R")
source("ipeds_salary_scraper.R")

k12 <- read.csv(file.path("Wy_Ed_Jobs", "salarymap2.csv"), stringsAsFactors = FALSE)
he <- read.csv(file.path("Wy_Ed_Jobs", "salarymap.csv"), stringsAsFactors = FALSE)

# Wyoming has 48 school districts -- WSBA's teacher salary PDF and
# superintendent salary spreadsheet each report one row per district
# regardless of whether an individual cell is blank (see
# tests/testthat/test-salary-scrapers.R), so nrow() itself is the right
# structural signal: a parser that silently extracts fewer district rows
# than that means the PDF layout changed, not that a district's salary
# happens to be unreported this year.
#
# IPEDS's 9 WY institutions are hardcoded in ipeds_salary_scraper.R's
# IPEDS_UNITID_MAP, so salarymap.csv always has 9 rows regardless of
# whether the API actually matched real salary data -- the meaningful
# signal there is coverage of the headline Faculty_Avg_Salary field, which
# has historically been 9/9 (unlike Professor-rank salary, which is
# legitimately sparse at two-year colleges and isn't checked here).
flags <- rbind(
  check_salary_coverage("K-12 teacher base salary (WSBA)", nrow(k12), expected = 48L),
  check_salary_coverage("HE avg faculty salary (IPEDS)", sum(!is.na(he$Faculty_Avg_Salary)), expected = 9L, min_ok = 8L),
  # Added 2026-08-06 alongside ipeds_enrollment_scraper.R -- same known-
  # fixed-universe reasoning as the salary check above, just for IPEDS's
  # fall-enrollment (FTE) survey instead of its instructional-staff survey.
  check_salary_coverage("HE fall enrollment FTE (IPEDS)", sum(!is.na(he$Enrollment)), expected = 9L, min_ok = 8L),
  check_salary_coverage("HE 5-year enrollment trend (IPEDS)", sum(!is.na(he$Enrollment_Change_Pct)), expected = 9L, min_ok = 8L),
  # Added 2026-08-06 alongside fsa_pell_scraper.R -- same known-fixed-
  # universe reasoning, for the FSA Pell Grant recipient data instead of
  # IPEDS's own surveys.
  check_salary_coverage("HE Pell Grant recipient share (FSA)", sum(!is.na(he$Pell_Recipient_Share)), expected = 9L, min_ok = 8L)
)

if (!is.null(flags) && nrow(flags) > 0) print(flags)

coverage_lines <- if (is.null(flags) || nrow(flags) == 0) {
  character(0)
} else {
  lines <- c(
    paste0("Automated salary-source coverage check flagged ", nrow(flags), " source(s) as of ", Sys.Date(), "."),
    "",
    "Unlike the job-posting drift check above, these are hard assertions against a known, essentially-fixed universe (48 WY school districts, 9 WY public HE institutions) rather than a trailing statistical baseline -- salary data updates far less often (once a year, not weekly), so a genuine week-to-week dip isn't expected. A source landing below its expected count usually means the source changed its page/PDF/API layout and the parser is silently extracting less real data, not that Wyoming lost school districts or colleges.",
    ""
  )
  for (i in seq_len(nrow(flags))) {
    r <- flags[i, ]
    lines <- c(lines, sprintf("- **%s**: expected >= %d, got %d", r$name, r$expected, r$actual))
  }
  c(lines, "")
}

# --------------------------------------------------------------------------
# Value plausibility -- catches a source that returns the RIGHT ROW COUNT
# but the WRONG VALUES (a WSBA PDF column nudging just enough to misalign
# salary_scrapers.R's hardcoded pixel windows is the concrete risk this
# guards against; see drift_check.R::check_salary_yoy_plausibility()'s
# header for the full reasoning). Neither check_salary_coverage() above
# nor flag_drift() in check_drift.R can catch this shape of failure --
# both only ever look at how many rows came back.
# --------------------------------------------------------------------------
k12_current <- setNames(k12$Teacher_Base_Salary, k12$District)
k12_prior <- setNames(k12$Teacher_Base_Salary_Prior_Year, k12$District)
k12_supt <- setNames(k12$Superintendent_Salary, k12$District)
he_current <- setNames(he$Faculty_Avg_Salary, he$Name)
he_y1ago <- setNames(he$Faculty_Avg_Salary_Y1Ago, he$Name)

# Bounds are deliberately generous -- real WY 2025-2026 data sits at
# $45k-$57k (teacher base) and $127k-$175k (superintendent); these bounds
# exist to catch a parser reading a location code or a stray digit into a
# dollar column, not to flag genuine, if unusual, salary growth over time.
plausibility_flags <- list(
  check_salary_value_bounds("K-12 teacher base salary (WSBA)", k12_current, min_ok = 25000, max_ok = 150000),
  check_salary_value_bounds("K-12 superintendent salary (WSBA)", k12_supt, min_ok = 60000, max_ok = 350000),
  check_salary_value_bounds("HE avg faculty salary (IPEDS)", he_current, min_ok = 30000, max_ok = 150000),
  check_salary_yoy_plausibility(k12_current, k12_prior),
  check_salary_yoy_plausibility(he_current, he_y1ago)
)
plausibility_flags <- plausibility_flags[!vapply(plausibility_flags, is.null, logical(1))]

if (length(plausibility_flags) > 0) for (f in plausibility_flags) print(f)

plausibility_lines <- if (length(plausibility_flags) == 0) {
  character(0)
} else {
  lines <- c(
    "Automated salary VALUE plausibility check flagged something worth a human look.",
    "",
    "Unlike the coverage check above (which only checks row counts), this checks whether the dollar figures themselves look sane -- either outside a generous real-world range, or a year-over-year change that's a statistical outlier against every other district's/institution's change this same run. Usually means a source's page/PDF/API layout shifted just enough to misparse values while still returning a plausible row count, not that the underlying number is really this different.",
    ""
  )
  for (f in plausibility_flags) {
    if ("min_ok" %in% names(f)) {
      for (i in seq_len(nrow(f))) {
        r <- f[i, ]
        lines <- c(lines, sprintf("- **%s** (%s): $%s is outside the expected $%s-$%s range", r$name, r$entity,
                                   format(r$value, big.mark = ","), format(r$min_ok, big.mark = ","), format(r$max_ok, big.mark = ",")))
      }
    } else {
      for (i in seq_len(nrow(f))) {
        r <- f[i, ]
        lines <- c(lines, sprintf("- **%s**: $%s -> $%s (%+.1f%%), a real outlier vs. every other entity's change this run",
                                   r$name, format(r$prior, big.mark = ","), format(r$current, big.mark = ","), r$pct_change * 100))
      }
    }
  }
  c(lines, "")
}

# Sheridan College and Gillette College are in the process of splitting
# from their shared parent entity -- app.R shows both a shared JOINT
# Vacancy_Rate (combined postings / the one shared IPEDS faculty count,
# flagged via Vacancy_Rate_Shared) until IPEDS starts reporting them
# separately (see ipeds_salary_scraper.R's "Sheridan/Gillette split watch"
# section). This is a real live network check (unlike the coverage check
# above, which only reads the already-rendered CSVs), so a transient API
# failure here should surface as "couldn't check" rather than silently
# doing nothing.
split_result <- tryCatch(
  fetch_sheridan_gillette_split_check(),
  error = function(e) NULL
)

split_lines <- if (is.null(split_result)) {
  c(
    "Could not check whether IPEDS has started reporting Sheridan College and Gillette College separately -- the directory API call failed. Not necessarily a problem (could be a transient network issue), but worth a manual check if it keeps happening.",
    ""
  )
} else if (nrow(split_result) > 0) {
  c(
    paste0("IPEDS now reports a NEW unitid for Sheridan/Gillette as of ", Sys.Date(), " -- they may have finished splitting into two separate colleges:"),
    "",
    paste0("- unitid ", split_result$unitid, ": ", split_result$inst_name),
    "",
    "If this is real, update IPEDS_UNITID_MAP in ipeds_salary_scraper.R to the new unitid(s). That's the only change needed: parse_ipeds_he_salaries()'s Salary_Note only fires for unitid == 240666, so it (and everything downstream keyed off it -- Wy_Ed_Jobs/app.R's Vacancy_Rate_Shared in map_he) will automatically go back to each institution having its own real, independent Faculty_Count and vacancy rate once neither institution maps to 240666 anymore.",
    ""
  )
} else {
  character(0)
}

# --------------------------------------------------------------------------
# Reference tables (BLS OEWS/QCEW, IPEDS staff pay, the UW salary book):
# the pipeline keeps last run's file when a refresh fails or is rejected
# (write_reference_file()), so without this a broken download would only
# show up as an ever-older file. See reference_table_status().
# --------------------------------------------------------------------------
library(edjobs)
scrape_log <- if (file.exists("scrape_log.csv")) read.csv("scrape_log.csv", stringsAsFactors = FALSE) else
  data.frame(timestamp = character(0), source = character(0), status = character(0), error_message = character(0))
reference_flags <- reference_table_status("Wy_Ed_Jobs", scrape_log)
if (nrow(reference_flags) > 0) print(reference_flags)
reference_lines <- if (nrow(reference_flags) == 0) {
  character(0)
} else {
  c(
    "Reference tables behind the pay estimates need a look.",
    "",
    "The dashboard keeps showing the previous file for each of these, so nothing is blank -- but its figures stop updating, and a rejected refresh means the source changed shape.",
    "",
    sprintf("- **%s** (`%s`): %s", reference_flags$name, reference_flags$file, reference_flags$problem),
    ""
  )
}

if (length(coverage_lines) == 0 && length(plausibility_lines) == 0 && length(reference_lines) == 0 && length(split_lines) == 0) {
  cat("Salary source coverage and value plausibility look healthy, and Sheridan/Gillette are still reported jointly as expected.\n")
  quit(status = 0, save = "no")
}

report_lines <- c(coverage_lines, plausibility_lines, reference_lines, split_lines)

# Append to the same report the job-posting drift check may have already
# started, so both land in one GitHub Issue/comment instead of two separate
# threads.
if (file.exists("/tmp/drift_report.md")) {
  write(c("", "---", "", report_lines), file = "/tmp/drift_report.md", append = TRUE)
} else {
  writeLines(report_lines, "/tmp/drift_report.md")
}
cat("Flagged items written to /tmp/drift_report.md\n")
