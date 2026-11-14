# Wyoming Higher Ed faculty salary data from IPEDS (the federal Integrated
# Postsecondary Education Data System), via the Urban Institute's Education
# Data Portal -- a real public JSON REST API wrapping IPEDS's own Human
# Resources/Salaries (SAL) survey component, so no PDF/HTML scraping is
# needed here (unlike the WSBA K-12 salary data in salary_scrapers.R).
# https://educationdata.urban.org/documentation/
#
# Endpoint used: college-university/ipeds/salaries-instructional-staff/{year}
# filtered to fips=56 (Wyoming). Per NCES, HR/SAL data is reported "as of
# November 1" of the same calendar year as the survey year -- e.g. year 2024
# means salaries as of Nov 1, 2024, not the "2023-24" academic year.
#
# Each institution reports one row per (academic_rank x contract_length x
# sex) combination; academic_rank/contract_length/sex == 99 means "all
# ranks/lengths/sexes combined" -- that combined row is what's extracted as
# the headline "Faculty_Avg_Salary" figure, plus the academic_rank == 1
# ("Professor") row as a second reference point, mirroring how the WSBA
# scraper surfaces both a base teacher salary and a superintendent salary.
# Negative values (-1, -2, -3) are IPEDS/Urban sentinel codes for
# not-available/not-applicable/suppressed, not real data, and are treated
# as NA.
#
# Wyoming has 9 public HE institutions in this project's salarymap.csv, but
# only 8 distinct IPEDS unitids report salary data for them -- Sheridan
# College and Gillette College are both part of the same accredited entity,
# Northern Wyoming Community College District, so IPEDS has no way to tell
# their faculty salaries apart. Both get the same figures here, flagged via
# Salary_Note rather than silently presented as institution-specific.

suppressMessages({
  library(httr2)
  library(dplyr)
})

IPEDS_SALARY_ENDPOINT <- "https://educationdata.urban.org/api/v1/college-university/ipeds/salaries-instructional-staff"

IPEDS_UNITID_MAP <- tibble::tribble(
  ~unitid, ~Name,
  240505L, "Casper College",
  240514L, "Central Wyoming College",
  240596L, "Eastern Wyoming Community College",
  240620L, "Laramie County Community College",
  240657L, "Northwest College",
  240666L, "Sheridan College",
  240666L, "Gillette College",
  240693L, "Western Wyoming Community College",
  240727L, "University of Wyoming"
)

# Follows the API's `next` pagination link until exhausted, returning every
# result row as one data frame. Urban Institute API endpoints (salaries,
# enrollment, directory) can include records with JSON nulls (e.g. 2023 salary
# data contains null admissions fields), which breaks a per-record
# as.data.frame(). simplifyVector = TRUE hands conversion off to jsonlite,
# turning nulls into NAs across rows consistently.
fetch_ipeds_paginated <- function(url) {
  fetch_json_paginated(url)
}

fetch_ipeds_wy_salaries_for_year <- function(year) {
  url <- paste0(IPEDS_SALARY_ENDPOINT, "/", year, "/?fips=56")
  fetch_ipeds_paginated(url)
}

# IPEDS publishes this survey with roughly a 1-2 year lag -- rather than
# hardcode a year that will eventually go stale, walk backward from the
# current year until a non-empty response is found.
find_latest_ipeds_salary_year <- function(start_year = as.integer(format(Sys.Date(), "%Y")),
                                           years_back = 5) {
  for (year in seq(start_year, start_year - years_back)) {
    df <- tryCatch(fetch_ipeds_wy_salaries_for_year(year), error = function(e) data.frame())
    if (nrow(df) > 0) return(list(year = year, data = df))
  }
  list(year = NA_integer_, data = data.frame())
}

clean_ipeds_value <- function(x) ifelse(is.na(x) | x < 0, NA_real_, x)

# Pure transformation, kept separate from the network fetch above so it can
# be tested against a captured fixture instead of hitting the live API.
parse_ipeds_he_salaries <- function(df, year) {
  if (nrow(df) == 0) {
    return(data.frame(
      Name = character(0), Faculty_Avg_Salary = numeric(0),
      Faculty_Avg_Salary_Professor = numeric(0), Faculty_Count = numeric(0),
      Salary_Year = character(0), Salary_Note = character(0), stringsAsFactors = FALSE
    ))
  }

  overall <- df %>%
    filter(academic_rank == 99, contract_length == 99, sex == 99) %>%
    transmute(unitid,
              Faculty_Avg_Salary = clean_ipeds_value(average_salary),
              Faculty_Count = clean_ipeds_value(instruc_staff_count))

  professor <- df %>%
    filter(academic_rank == 1, contract_length == 99, sex == 99) %>%
    transmute(unitid, Faculty_Avg_Salary_Professor = clean_ipeds_value(average_salary))

  IPEDS_UNITID_MAP %>%
    left_join(overall, by = "unitid") %>%
    left_join(professor, by = "unitid") %>%
    mutate(
      Salary_Year = as.character(year),
      Salary_Note = ifelse(
        unitid == 240666,
        "Reported by Northern Wyoming Community College District, which includes both Sheridan College and Gillette College -- IPEDS does not break this figure out by campus.",
        NA_character_
      )
    ) %>%
    select(Name, Faculty_Avg_Salary, Faculty_Avg_Salary_Professor, Faculty_Count, Salary_Year, Salary_Note)
}

fetch_ipeds_he_salaries <- function() {
  latest <- find_latest_ipeds_salary_year()
  parse_ipeds_he_salaries(latest$data, latest$year)
}

# Pure transformation for one year of trend data -- kept separate from the
# network fetch below so it's testable against a captured fixture. Only
# the headline "all ranks combined" figure is extracted -- Professor-rank/
# Faculty_Count/Salary_Note aren't needed for a multi-year trend, just the
# one number worth comparing year over year.
parse_ipeds_salary_trend_year <- function(df, year) {
  if (nrow(df) == 0) {
    return(data.frame(Name = character(0), Year = integer(0), Faculty_Avg_Salary = numeric(0)))
  }
  overall <- df %>%
    filter(academic_rank == 99, contract_length == 99, sex == 99) %>%
    transmute(unitid, Faculty_Avg_Salary = clean_ipeds_value(average_salary))
  IPEDS_UNITID_MAP %>%
    left_join(overall, by = "unitid") %>%
    transmute(Name, Year = year, Faculty_Avg_Salary)
}

# Faculty_Avg_Salary going back n_years, for a multi-year salary trend
# alongside fetch_ipeds_he_salaries()'s single latest-year figure. Unlike
# WSBA (which only ever exposes the current + one prior year with no
# public historical archive -- checked directly 2026-08-05, confirmed no
# archive exists), IPEDS is queryable by year going back indefinitely, so
# this is straightforward: just re-run parse_ipeds_salary_trend_year() for
# each of the last few years. Returns long format (one row per Name x
# Year) since that's the more composable shape; callers pivot wide if
# they need it.
fetch_ipeds_he_salary_trend <- function(n_years = 3) {
  latest <- find_latest_ipeds_salary_year()
  if (is.na(latest$year)) {
    return(data.frame(Name = character(0), Year = integer(0), Faculty_Avg_Salary = numeric(0)))
  }

  years <- seq(latest$year, latest$year - n_years + 1)
  rows <- lapply(years, function(y) {
    df <- if (y == latest$year) latest$data else tryCatch(fetch_ipeds_wy_salaries_for_year(y), error = function(e) data.frame())
    parse_ipeds_salary_trend_year(df, y)
  })
  bind_rows(rows)
}

# ---------------------------------------------------------------------------
# Sheridan/Gillette split watch
# ---------------------------------------------------------------------------
#
# Sheridan College and Gillette College are historically one accredited
# entity (Northern Wyoming Community College District, unitid 240666) but
# are in the process of splitting into two separate colleges -- so
# IPEDS_UNITID_MAP's hardcoded shared unitid above, and app.R's suppression
# of Vacancy_Rate for both (see combined_map_data's Salary_Note check),
# will eventually go stale once IPEDS starts reporting them separately.
# When that happens, a NEW unitid will show up in the WY institution
# directory with an inst_name containing "Gillette" or "Sheridan" that
# ISN'T 240666 -- this checks for exactly that, so the split is a one-line
# assertion someone can act on instead of something to remember to
# recheck by hand.
IPEDS_COLLEGE_DIRECTORY_ENDPOINT <- "https://educationdata.urban.org/api/v1/college-university/ipeds/directory"

# Unlike salaries-instructional-staff, the directory endpoint's records
# have real JSON nulls (e.g. a college with no CBSA assignment) mixed in
# with populated fields, which breaks fetch_ipeds_paginated()'s per-record
# as.data.frame() -- same issue ccd_staff_scraper.R hit for the same
# reason. simplifyVector = TRUE hands the conversion off to jsonlite,
# which turns nulls into NA correctly. Kept separate from
# fetch_ipeds_paginated() rather than changing it, since that function is
# already tested working for the salary endpoint and doesn't need this.
fetch_ipeds_directory_paginated <- function(url) {
  fetch_json_paginated(url)
}

fetch_ipeds_wy_college_directory_for_year <- function(year) {
  url <- paste0(IPEDS_COLLEGE_DIRECTORY_ENDPOINT, "/", year, "/?fips=56")
  fetch_ipeds_directory_paginated(url)
}

find_latest_ipeds_directory_year <- function(start_year = as.integer(format(Sys.Date(), "%Y")),
                                              years_back = 5) {
  for (year in seq(start_year, start_year - years_back)) {
    df <- tryCatch(
      fetch_ipeds_wy_college_directory_for_year(year),
      error = function(e) data.frame()
    )
    if (nrow(df) > 0) return(list(year = year, data = df))
  }
  list(year = NA_integer_, data = data.frame())
}

# Pure transformation, kept separate from the network fetch above so it can
# be tested against a captured fixture. Returns a data frame of newly-found
# unitids if the split has happened, or a 0-row frame (not NULL, so callers
# don't need a special case) if Sheridan/Gillette are still reported jointly.
check_sheridan_gillette_still_joint <- function(directory_df) {
  empty <- data.frame(unitid = integer(0), inst_name = character(0), stringsAsFactors = FALSE)
  if (nrow(directory_df) == 0) return(empty)

  split_candidates <- directory_df[
    grepl("Gillette|Sheridan", directory_df$inst_name, ignore.case = TRUE) &
      directory_df$unitid != 240666,
  ]
  if (nrow(split_candidates) == 0) return(empty)
  split_candidates[, c("unitid", "inst_name")]
}

fetch_sheridan_gillette_split_check <- function() {
  latest <- find_latest_ipeds_directory_year()
  check_sheridan_gillette_still_joint(latest$data)
}
