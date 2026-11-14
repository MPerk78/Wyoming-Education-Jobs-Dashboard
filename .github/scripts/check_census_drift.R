# Census ACS/SAIPE coverage + value plausibility check, run after the Rmd
# render against the just-regenerated salarymap2.csv (K-12 county context +
# district child poverty) and salarymap.csv (HE county context, added
# 2026-08-06). Same shape and same reasoning as check_salary_drift.R --
# check_salary_coverage()/check_salary_value_bounds() in drift_check.R
# are already fully generic (name/actual/expected, or name/actual/min_ok/
# max_ok) despite their "salary" name, so this reuses them directly rather
# than duplicating the same logic under a new name. Diagnostic-only: never
# blocks the pipeline (see weekly-scrape.yml's continue-on-error on this
# step).

source("drift_check.R")

k12 <- read.csv(file.path("Wy_Ed_Jobs", "salarymap2.csv"), stringsAsFactors = FALSE)
he <- read.csv(file.path("Wy_Ed_Jobs", "salarymap.csv"), stringsAsFactors = FALSE)

# --------------------------------------------------------------------------
# Coverage
# --------------------------------------------------------------------------
#
# ACS county context is COUNTY-level, not row-level -- salarymap2.csv has
# 48 K-12 district rows across 23 distinct WY counties, salarymap.csv has
# 9 HE institution rows across 9 distinct counties (no two HE institutions
# share a county). Counting distinct counties WITH a non-NA figure (not
# just nrow()) is the structural signal that matches how many counties
# Wyoming actually has -- 23 for K-12, 9 for HE.
k12_counties_covered <- length(unique(k12$County[!is.na(k12$Median_Household_Income)]))
he_counties_covered <- length(unique(he$County[!is.na(he$Median_Household_Income)]))

# SAIPE is K-12-district-level with no HE equivalent (child poverty rate
# doesn't map onto a mostly-adult college student body) -- Fremont County
# School District 38 (Arapahoe Charter High School) is a confirmed real
# gap in SAIPE's own coverage (see census_saipe_scraper.R's header), so
# 47/48 is the expected full-health count here, not 48/48.
k12_saipe_covered <- sum(!is.na(k12$Child_Poverty_Rate))

flags <- rbind(
  check_salary_coverage("K-12 county context (Census ACS)", k12_counties_covered, expected = 23L),
  check_salary_coverage("HE county context (Census ACS)", he_counties_covered, expected = 9L),
  check_salary_coverage("K-12 district child poverty (Census SAIPE)", k12_saipe_covered, expected = 48L, min_ok = 47L)
)

if (!is.null(flags) && nrow(flags) > 0) print(flags)

coverage_lines <- if (is.null(flags) || nrow(flags) == 0) {
  character(0)
} else {
  lines <- c(
    paste0("Automated Census source coverage check flagged ", nrow(flags), " source(s) as of ", Sys.Date(), "."),
    "",
    "Like the salary coverage check, this is a hard assertion against a known, essentially-fixed universe (23 WY counties, 48 school districts, 9 HE institutions) rather than a trailing statistical baseline -- Census data updates at most once a year. A source landing below its expected count usually means the Census API changed its response shape (a variable code retired, a geography type renamed) and the parser is silently extracting less real data, not that Wyoming lost counties or districts.",
    ""
  )
  for (i in seq_len(nrow(flags))) {
    r <- flags[i, ]
    lines <- c(lines, sprintf("- **%s**: expected >= %d, got %d", r$name, r$expected, r$actual))
  }
  c(lines, "")
}

# --------------------------------------------------------------------------
# Value plausibility -- same reasoning as check_salary_drift.R's own
# section: a coverage check alone can't catch a source that returns the
# right ROW count but wrong VALUES (a Census variable code silently
# starting to return a different table's data, still numeric and still
# present, would pass every coverage check above).
# --------------------------------------------------------------------------
#
# Bounds are deliberately generous -- real WY 2024 ACS data sits at
# $59k-$90k (county median income) and $780-$1,160/mo (county median
# rent); Mining_Employment_Share and Population_Change_Pct are ratios
# (already divided, not percentages), so their bounds are on that same
# -1 to 1 scale, not -100 to 100. These bounds exist to catch a parser
# reading the wrong Census variable/column into a field, not to flag
# genuine, if unusual, economic conditions.
k12_income <- setNames(k12$Median_Household_Income, k12$District)
k12_rent <- setNames(k12$Median_Gross_Rent, k12$District)
k12_mining <- setNames(k12$Mining_Employment_Share, k12$District)
k12_pop_change <- setNames(k12$Population_Change_Pct, k12$District)
k12_child_poverty <- setNames(k12$Child_Poverty_Rate, k12$District)
he_income <- setNames(he$Median_Household_Income, he$Name)
he_rent <- setNames(he$Median_Gross_Rent, he$Name)
he_mining <- setNames(he$Mining_Employment_Share, he$Name)
he_pop_change <- setNames(he$Population_Change_Pct, he$Name)

plausibility_flags <- list(
  check_salary_value_bounds("K-12 county median income (Census ACS)", k12_income, min_ok = 20000, max_ok = 200000),
  check_salary_value_bounds("K-12 county median rent (Census ACS)", k12_rent, min_ok = 200, max_ok = 3000),
  check_salary_value_bounds("K-12 county mining/energy employment share (Census ACS)", k12_mining, min_ok = 0, max_ok = 1),
  check_salary_value_bounds("K-12 county population change, 5yr (Census ACS)", k12_pop_change, min_ok = -0.5, max_ok = 0.5),
  check_salary_value_bounds("K-12 district child poverty rate (Census SAIPE)", k12_child_poverty, min_ok = 0, max_ok = 1),
  check_salary_value_bounds("HE county median income (Census ACS)", he_income, min_ok = 20000, max_ok = 200000),
  check_salary_value_bounds("HE county median rent (Census ACS)", he_rent, min_ok = 200, max_ok = 3000),
  check_salary_value_bounds("HE county mining/energy employment share (Census ACS)", he_mining, min_ok = 0, max_ok = 1),
  check_salary_value_bounds("HE county population change, 5yr (Census ACS)", he_pop_change, min_ok = -0.5, max_ok = 0.5)
)
plausibility_flags <- plausibility_flags[!vapply(plausibility_flags, is.null, logical(1))]

if (length(plausibility_flags) > 0) for (f in plausibility_flags) print(f)

plausibility_lines <- if (length(plausibility_flags) == 0) {
  character(0)
} else {
  lines <- c(
    "Automated Census VALUE plausibility check flagged something worth a human look.",
    "",
    "Unlike the coverage check above (which only checks how many counties/districts came back), this checks whether the figures themselves look sane -- outside a generous real-world range. Usually means the Census API's response shape or a variable code changed just enough to misparse values while still returning a plausible count, not that the underlying number is really this different.",
    ""
  )
  for (f in plausibility_flags) {
    for (i in seq_len(nrow(f))) {
      r <- f[i, ]
      lines <- c(lines, sprintf("- **%s** (%s): %s is outside the expected %s-%s range", r$name, r$entity,
                                 format(r$value, big.mark = ","), format(r$min_ok, big.mark = ","), format(r$max_ok, big.mark = ",")))
    }
  }
  c(lines, "")
}

if (length(coverage_lines) == 0 && length(plausibility_lines) == 0) {
  cat("Census source coverage and value plausibility look healthy.\n")
  quit(status = 0, save = "no")
}

report_lines <- c(coverage_lines, plausibility_lines)

# Appends to the same report the job-posting/salary drift checks may have
# already started, so all three land in one GitHub Issue/comment instead
# of separate threads -- same pattern check_salary_drift.R itself uses.
if (file.exists("/tmp/drift_report.md")) {
  write(c("", "---", "", report_lines), file = "/tmp/drift_report.md", append = TRUE)
} else {
  writeLines(report_lines, "/tmp/drift_report.md")
}
cat("Flagged items written to /tmp/drift_report.md\n")
