# Wyoming county-level socioeconomic context from the US Census Bureau's
# American Community Survey (ACS) 5-Year Estimates, via the Census Data API
# directly (not the Urban Institute Education Data Portal this project uses
# elsewhere for CCD/IPEDS -- Census's own API doesn't wrap this data).
# https://www.census.gov/data/developers/data-sets/acs-5year.html
#
# Requires a free Census API key (https://api.census.gov/data/key_signup.html)
# -- every endpoint here 302-redirects to an error page without one, even
# for a single low-volume request (confirmed 2026-08-06). Read from the
# CENSUS_API_KEY environment variable -- a GitHub Actions secret for the
# weekly pipeline, .Renviron locally -- never hardcoded or committed.
#
# One row per Wyoming county (23 total), joined onto salarymap2.csv's
# existing County column (already formatted "<Name> County, Wyoming",
# matching the API's own NAME field exactly -- no separate FIPS lookup
# needed for the join, though FIPS is kept in the output for reference).
#
# Four figures, all from the ACS 5-Year Estimates (the only ACS product
# reliable enough at Wyoming's small-county population sizes -- 1-year
# estimates have margins of error too wide to be meaningful for most WY
# counties):
#   - Median household income (B19013_001E)
#   - Median gross rent (B25064_001E)
#   - Mining/oil & gas share of civilian employment (S2403_C01_004E /
#     S2403_C01_001E, the ACS Subject Tables endpoint) -- Wyoming's
#     energy-producing counties (Campbell, Sublette, Sweetwater, Converse)
#     pay teachers measurably more specifically to compete with these
#     wages for the same local labor pool; this is the one figure here
#     that helps explain WHY salaries vary across WY districts, not just
#     what the cost of living looks like. Confirmed live 2026-08-06:
#     Sublette (12.6%) and Campbell (17.6%) vs. Teton (0.3%) -- a huge,
#     real spread, not noise.
#   - Population change vs. 5 years earlier (B01003_001E, current vintage
#     vs. one 5 years back) -- growing vs. shrinking community, a longer-
#     term signal current job openings alone can't show. Built from two
#     ACS 5-Year vintages rather than the Population Estimates Program
#     (PEP): PEP's API restructured at some point after the 2020 Census,
#     and the modern endpoint/variable names couldn't be found working
#     during a live check (2026-08-06) across several attempted
#     year/variable combinations (pep/population/variables.json itself
#     404s), while ACS5's population total is stable, already the same
#     API family as everything else here, and accurate enough for a
#     multi-year trend at this scale.

suppressMessages({
  library(httr2)
  library(dplyr)
})

ACS_DETAIL_ENDPOINT <- "https://api.census.gov/data/%d/acs/acs5"
ACS_SUBJECT_ENDPOINT <- "https://api.census.gov/data/%d/acs/acs5/subject"
WY_STATE_FIPS <- "56"

`%||%` <- function(x, y) if (is.null(x)) y else x

census_api_key <- function() {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key)) {
    stop(
      "CENSUS_API_KEY environment variable is not set -- get a free key at ",
      "https://api.census.gov/data/key_signup.html and set it as an environment ",
      "variable (a GitHub Actions secret for the weekly pipeline, .Renviron locally)."
    )
  }
  key
}

# Fetches one ACS table's variables for every Wyoming county in one call.
# endpoint_template: ACS_DETAIL_ENDPOINT or ACS_SUBJECT_ENDPOINT.
fetch_acs_wy_counties <- function(endpoint_template, get_vars, year, api_key) {
  url <- sprintf(endpoint_template, year)
  resp <- request(url) %>%
    req_url_query(
      get = paste(get_vars, collapse = ","),
      `for` = "county:*",
      `in` = paste0("state:", WY_STATE_FIPS),
      key = api_key
    ) %>%
    perform_with_retry()
  parse_acs_json(resp_body_string(resp))
}

# Pure transformation: the Census API returns a JSON array-of-arrays (first
# row is column names, e.g. [["NAME","B19013_001E","state","county"],
# ["Albany County, Wyoming","59881","56","001"], ...]) rather than an array
# of objects -- fromJSON() on that shape gives a character matrix, not a
# data frame with real column names, so this does that conversion and
# coerces every column except NAME/state/county to numeric. Kept separate
# from the network fetch so it's testable against a captured fixture.
parse_acs_json <- function(json_text) {
  raw <- jsonlite::fromJSON(json_text)
  if (is.null(raw) || nrow(raw) < 2) {
    return(data.frame())
  }
  header <- raw[1, ]
  body <- as.data.frame(raw[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(body) <- header

  non_numeric_cols <- c("NAME", "state", "county")
  for (col in setdiff(names(body), non_numeric_cols)) {
    body[[col]] <- suppressWarnings(as.numeric(body[[col]]))
  }
  body
}

# Census ACS uses negative sentinel codes (e.g. -666666666) for "not
# computed"/suppressed cells in some tables, the same shape of problem
# ipeds_salary_scraper.R's clean_ipeds_value() already handles for IPEDS --
# not currently observed in any of the 23 WY counties for the specific
# variables this file uses, but a real risk for future years/counties
# (a small county's rent or income can legitimately be suppressed for
# reliability), so guarded the same way rather than assumed away.
clean_acs_value <- function(x) ifelse(is.na(x) | x < 0, NA_real_, x)

fetch_census_income_rent_population <- function(year = NULL, api_key = census_api_key()) {
  year <- year %||% latest_acs5_year()
  raw <- fetch_acs_wy_counties(ACS_DETAIL_ENDPOINT, c("NAME", "B19013_001E", "B25064_001E", "B01003_001E"), year, api_key)
  parse_census_income_rent_population(raw, year)
}

parse_census_income_rent_population <- function(raw, year) {
  if (nrow(raw) == 0) {
    return(data.frame(County = character(0), Median_Household_Income = numeric(0),
                       Median_Gross_Rent = numeric(0), Population = numeric(0),
                       ACS_Year = integer(0), stringsAsFactors = FALSE))
  }
  raw %>%
    transmute(
      County = NAME,
      Median_Household_Income = clean_acs_value(B19013_001E),
      Median_Gross_Rent = clean_acs_value(B25064_001E),
      Population = clean_acs_value(B01003_001E),
      ACS_Year = year
    )
}

fetch_census_mining_employment_share <- function(year = NULL, api_key = census_api_key()) {
  year <- year %||% latest_acs5_year()
  raw <- fetch_acs_wy_counties(ACS_SUBJECT_ENDPOINT, c("NAME", "S2403_C01_001E", "S2403_C01_004E"), year, api_key)
  parse_census_mining_employment_share(raw)
}

parse_census_mining_employment_share <- function(raw) {
  if (nrow(raw) == 0) {
    return(data.frame(County = character(0), Mining_Employment_Share = numeric(0), stringsAsFactors = FALSE))
  }
  raw %>%
    transmute(
      County = NAME,
      total = clean_acs_value(S2403_C01_001E),
      mining = clean_acs_value(S2403_C01_004E),
      Mining_Employment_Share = ifelse(!is.na(total) & total > 0, mining / total, NA_real_)
    ) %>%
    select(County, Mining_Employment_Share)
}

# Population change vs. n_years_back, both from ACS 5-Year vintages (see
# this file's header for why PEP wasn't used instead). current/prior are
# each the output of fetch_census_income_rent_population() (or a subset
# with a Population column) for two different years -- kept as a pure
# join+compute function, separate from the two live fetches, so it's
# testable without network access.
compute_population_change <- function(current, prior) {
  if (nrow(current) == 0 || nrow(prior) == 0) {
    return(data.frame(County = character(0), Population_Change_Pct = numeric(0), stringsAsFactors = FALSE))
  }
  current %>%
    select(County, Population) %>%
    inner_join(prior %>% select(County, Population_Prior = Population), by = "County") %>%
    mutate(Population_Change_Pct = ifelse(!is.na(Population) & !is.na(Population_Prior) & Population_Prior > 0,
                                           (Population - Population_Prior) / Population_Prior, NA_real_)) %>%
    select(County, Population_Change_Pct)
}

# ACS 5-Year estimates publish roughly a year behind the current calendar
# year -- walks backward the same way find_latest_ipeds_salary_year() and
# find_latest_ccd_directory_year() already do elsewhere in this project,
# rather than hardcoding a year that will eventually go stale. Checks a
# cheap single-variable request rather than the full multi-variable one.
latest_acs5_year <- function(start_year = as.integer(format(Sys.Date(), "%Y")),
                              years_back = 3, api_key = census_api_key()) {
  for (year in seq(start_year, start_year - years_back)) {
    result <- tryCatch(
      fetch_acs_wy_counties(ACS_DETAIL_ENDPOINT, c("NAME", "B01003_001E"), year, api_key),
      error = function(e) data.frame()
    )
    if (nrow(result) > 0) return(year)
  }
  stop("latest_acs5_year(): no working ACS 5-Year vintage found in the last ", years_back, " years.")
}

# Full pipeline: income, rent, mining-employment share, and population
# change vs. 5 years earlier, one row per WY county. trend_years_back
# controls how far back the population-change comparison looks -- 5 years
# is deliberately more than one weekly-cadence-scale gap, since county
# population genuinely does not move meaningfully week to week or even
# year to year at Wyoming's scale; this is a slow-moving structural signal,
# refreshed at the same "once a year is plenty" cadence as the WSBA/IPEDS
# salary data elsewhere in this pipeline.
fetch_census_county_context <- function(api_key = census_api_key(), trend_years_back = 5) {
  year <- latest_acs5_year(api_key = api_key)

  current <- fetch_census_income_rent_population(year, api_key)
  prior <- fetch_census_income_rent_population(year - trend_years_back, api_key)
  mining <- fetch_census_mining_employment_share(year, api_key)
  pop_change <- compute_population_change(current, prior)

  current %>%
    left_join(mining, by = "County") %>%
    left_join(pop_change, by = "County")
}
