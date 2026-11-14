# Wyoming HE institution fall enrollment from IPEDS, via the Urban
# Institute's Education Data Portal -- a different IPEDS survey component
# from the instructional-staff salary data in ipeds_salary_scraper.R
# (kept in its own file, matching this project's one-file-per-source
# convention -- census_saipe_scraper.R is the same story: a separate
# federal survey from census_acs_scraper.R despite sharing an agency).
# https://educationdata.urban.org/documentation/colleges.html
#
# Depends on ipeds_salary_scraper.R being sourced first -- reuses its
# fetch_ipeds_paginated() (a generic pagination helper, nothing salary-
# specific in it) and IPEDS_UNITID_MAP.
#
# Endpoint used: college-university/ipeds/fall-enrollment/{year}/?fips=56
# -- despite the URL segment name, live records confirm this is actually
# IPEDS's FTE ("full-time equivalent") enrollment survey, not a headcount
# one: each row is one (unitid, level_of_study) combination with
# credit_hours/contact_hours/est_fte/rep_fte fields, not the age/sex/race
# breakdown a literal "fall enrollment" headcount endpoint would have.
# Confirmed against Urban's own rendered documentation page 2026-08-06,
# which describes this exact field set as "enrollment-full-time-
# equivalent" -- this endpoint path is a working alias for the same data.
#
# level_of_study: 1=Undergraduate, 2=Graduate, 3=First professional,
# 4=Postbaccalaureate. A level an institution doesn't offer reports -2
# ("not applicable") for that row rather than omitting the row entirely.
#
# est_fte ("estimated" -- IPEDS calculates it from credit/contact hours)
# and rep_fte ("reported" -- the institution directly reports its own FTE
# figure) are two different collection methods for the same underlying
# concept, and which one is actually populated for a given level/
# institution/year varies -- confirmed live: University of Wyoming's
# First-professional-level row has est_fte = -1 (missing) but a real
# rep_fte = 417. Coalescing to whichever is real (rather than preferring
# one and dropping the other) is the only way to not silently lose real
# enrollment; parse_ipeds_he_enrollment() then sums across every
# level_of_study to get one total Enrollment figure per institution -- the
# same shape Students_Per_Teacher already uses on the K-12 side (CCD
# Enrollment / CCD Teachers_Total_FTE), giving HE its own parallel
# Students_Per_Teacher via Enrollment / Faculty_Count (IPEDS instructional
# staff, already fetched in ipeds_salary_scraper.R).
#
# Sheridan College and Gillette College share one unitid (240666) in
# IPEDS_UNITID_MAP the same way they share Faculty_Count/Faculty_Avg_
# Salary -- both institutions naturally get the same combined Enrollment
# figure here too, with no special-casing needed (the left_join below
# just produces two rows from the two IPEDS_UNITID_MAP rows that already
# point at the one shared unitid).

suppressMessages({
  library(httr2)
  library(dplyr)
})

IPEDS_ENROLLMENT_ENDPOINT <- "https://educationdata.urban.org/api/v1/college-university/ipeds/fall-enrollment"

clean_ipeds_enrollment_value <- function(x) ifelse(is.na(x) | x < 0, NA_real_, x)

fetch_ipeds_wy_enrollment_for_year <- function(year) {
  url <- paste0(IPEDS_ENROLLMENT_ENDPOINT, "/", year, "/?fips=56")
  fetch_ipeds_paginated(url)
}

# IPEDS publishes fall enrollment with roughly a 1-2 year lag (confirmed
# live 2026-08-06: 2023 has real WY data, 2024/2025 both return zero
# results) -- walks backward the same way find_latest_ipeds_salary_year()
# does, rather than hardcoding a year that will eventually go stale.
find_latest_ipeds_enrollment_year <- function(start_year = as.integer(format(Sys.Date(), "%Y")),
                                               years_back = 5) {
  for (year in seq(start_year, start_year - years_back)) {
    df <- fetch_ipeds_wy_enrollment_for_year(year)
    if (nrow(df) > 0) return(list(year = year, data = df))
  }
  list(year = NA_integer_, data = data.frame())
}

# Pure transformation, kept separate from the network fetch so it's
# testable against a captured fixture.
parse_ipeds_he_enrollment <- function(df, year) {
  if (nrow(df) == 0) {
    return(data.frame(Name = character(0), Enrollment = numeric(0), Enrollment_Year = character(0), stringsAsFactors = FALSE))
  }

  totals <- df %>%
    transmute(
      unitid,
      fte = coalesce(clean_ipeds_enrollment_value(est_fte), clean_ipeds_enrollment_value(rep_fte))
    ) %>%
    group_by(unitid) %>%
    summarize(Enrollment = if (all(is.na(fte))) NA_real_ else sum(fte, na.rm = TRUE), .groups = "drop")

  IPEDS_UNITID_MAP %>%
    left_join(totals, by = "unitid") %>%
    mutate(Enrollment_Year = as.character(year)) %>%
    select(Name, Enrollment, Enrollment_Year)
}

fetch_ipeds_he_enrollment <- function() {
  latest <- find_latest_ipeds_enrollment_year()
  parse_ipeds_he_enrollment(latest$data, latest$year)
}

# Institution enrollment trend vs. n_years_back -- HE's analogue of the
# Census county context chunk's Population_Change_Pct, just computed from
# IPEDS's own multi-year history at the institution level instead of two
# ACS vintages at the county level. Arguably a more direct job-security
# signal for a prospective faculty hire than county population is -- a
# shrinking institution is a more specific risk than a shrinking county.
# Kept as a pure join+compute function, separate from the two live
# fetches, so it's testable without network access -- same shape as
# census_acs_scraper.R's compute_population_change().
compute_enrollment_change <- function(current, prior) {
  if (nrow(current) == 0 || nrow(prior) == 0) {
    return(data.frame(Name = character(0), Enrollment_Change_Pct = numeric(0), stringsAsFactors = FALSE))
  }
  current %>%
    select(Name, Enrollment) %>%
    inner_join(prior %>% select(Name, Enrollment_Prior = Enrollment), by = "Name") %>%
    mutate(Enrollment_Change_Pct = ifelse(!is.na(Enrollment) & !is.na(Enrollment_Prior) & Enrollment_Prior > 0,
                                           (Enrollment - Enrollment_Prior) / Enrollment_Prior, NA_real_)) %>%
    select(Name, Enrollment_Change_Pct)
}

# trend_years_back = 5 matches the K-12 side's Population_Change_Pct
# window -- Wyoming HE enrollment genuinely doesn't move meaningfully
# week to week or even year to year, so this is refreshed at the same
# "once a year is plenty" cadence as the rest of this pipeline's salary/
# staffing data despite running weekly.
fetch_ipeds_he_enrollment_trend <- function(trend_years_back = 5) {
  latest <- find_latest_ipeds_enrollment_year()
  if (is.na(latest$year)) {
    return(data.frame(Name = character(0), Enrollment_Change_Pct = numeric(0), stringsAsFactors = FALSE))
  }
  current <- parse_ipeds_he_enrollment(latest$data, latest$year)
  prior_year <- latest$year - trend_years_back
  prior <- parse_ipeds_he_enrollment(fetch_ipeds_wy_enrollment_for_year(prior_year), prior_year)
  compute_enrollment_change(current, prior)
}
