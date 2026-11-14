# Wyoming school-district-level child poverty rate from the US Census
# Bureau's Small Area Income and Poverty Estimates (SAIPE) program, via the
# Census Data API's SAIPE School District time series.
# https://www.census.gov/programs-surveys/saipe/data/datasets.html
#
# Requires the same free CENSUS_API_KEY census_acs_scraper.R uses (see that
# file's header for why every Census Data API endpoint needs one) -- read
# from the same environment variable, not a separate one, since it's the
# same Census API account either way.
#
# Unlike census_acs_scraper.R's county-level join, this is real DISTRICT-
# level data (SAIPE's own SD_NAME field, not a county name) -- the closest
# free proxy to free/reduced-lunch eligibility at the actual district a
# posting is in, not just its county. No separate NCES LEAID crosswalk was
# needed: SAIPE's own SD_NAME field already matches this project's
# canonical district naming almost exactly (confirmed live 2026-08-06, 47
# of 48 real WY districts matched directly on name alone) -- the one
# exception is the same Big Horn/Natrona "<County> County School District"
# vs. "<County> School District" quirk canonicalize_ccd_district_name()
# (ccd_staff_scraper.R) and canonicalize_wsba_district_name()
# (salary_scrapers.R) already handle for their own sources, handled the
# same way here rather than shared, matching how those two don't share one
# either -- each source's own name format is close-but-not-identical in
# its own way.
#
# "school district (unified)" (GEOCAT 970) is the right geography type for
# Wyoming -- WY districts are K-12 unified districts, not the separate
# elementary/secondary districts SAIPE also tracks in some other states.
# Fremont County School District 38 (Arapahoe) is the exception: SAIPE files
# it under "school district (elementary)", so fetch_census_saipe_child_poverty()
# asks for that geography too (an 2026-08-06 note called it absent from SAIPE;
# it was only absent from the unified list).

suppressMessages({
  library(httr2)
  library(dplyr)
})

SAIPE_SCHDIST_ENDPOINT <- "https://api.census.gov/data/timeseries/poverty/saipe/schdist"

# Only Big Horn and Natrona drop "County" in this project's canonical
# naming -- the exact same exception three other sources in this codebase
# already carry their own copy of (WSBA, CCD, and now SAIPE), each in its
# own source-specific format. Unlike those two (which use regexec()/
# regmatches() to reformat a "#N"-style suffix), SAIPE's SD_NAME already
# matches this project's canonical format for every OTHER district, so a
# plain sub() -- which leaves a non-matching string untouched, unlike
# regexec()/vapply() returning NA for anything that doesn't match a full
# pattern -- is the right tool here, not just a shorter one.
canonicalize_saipe_district_name <- function(name) {
  name <- trimws(name)
  sub("^(Big Horn|Natrona) County School District", "\\1 School District", name)
}

# Pure transformation: SAIPE's array-of-arrays JSON shape has different
# column names than census_acs_scraper.R's ACS responses (SD_NAME instead
# of NAME, plus a district-code geography column), so this doesn't reuse
# parse_acs_json() -- reusing it would have silently tried to coerce
# SD_NAME (real district name text) to numeric via that function's
# NAME/state/county exclude-list, which doesn't know about SD_NAME.
parse_saipe_json <- function(json_text) {
  raw <- jsonlite::fromJSON(json_text)
  if (is.null(raw) || nrow(raw) < 2) {
    return(data.frame())
  }
  header <- raw[1, ]
  body <- as.data.frame(raw[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(body) <- header
  body
}

# Unified districts, plus SAIPE's elementary geography: the Census school-
# district map files Fremont County School District 38 (Arapahoe) as an
# elementary district, so a unified-only request missed it (2024: 16.8%;
# the earlier "absent from SAIPE" note was this, found 2026-11-09). A year
# with no elementary districts answers 204 with no body: no rows.
fetch_census_saipe_child_poverty <- function(year = NULL, api_key = census_api_key()) {
  year <- year %||% latest_saipe_year(api_key = api_key)
  fetch_geo <- function(geocat) {
    resp <- request(SAIPE_SCHDIST_ENDPOINT) %>%
      req_url_query(
        get = paste(c("SD_NAME", "SAEPOVRAT5_17RV_PT"), collapse = ","),
        `for` = paste0("school district (", geocat, ")"),
        `in` = paste0("state:", WY_STATE_FIPS),
        time = year,
        key = api_key
      ) %>%
      perform_with_retry()
    if (!resp_has_body(resp)) return(NULL)
    body <- resp_body_string(resp)
    if (!nzchar(trimws(body))) return(NULL)
    parse_census_saipe_child_poverty(body, year)
  }
  dplyr::bind_rows(fetch_geo("unified"), fetch_geo("elementary"))
}

parse_census_saipe_child_poverty <- function(json_text, year) {
  raw <- parse_saipe_json(json_text)
  if (nrow(raw) == 0) {
    return(data.frame(District = character(0), Child_Poverty_Rate = numeric(0),
                       SAIPE_Year = integer(0), stringsAsFactors = FALSE))
  }
  raw %>%
    transmute(
      District = canonicalize_saipe_district_name(SD_NAME),
      Child_Poverty_Rate = clean_acs_value(suppressWarnings(as.numeric(SAEPOVRAT5_17RV_PT))) / 100,
      SAIPE_Year = year
    )
}

# SAIPE publishes with a longer lag than ACS 5-Year -- walks backward the
# same way latest_acs5_year() and the other find_latest_*_year() functions
# elsewhere in this project do, rather than hardcoding a year that will
# eventually go stale.
latest_saipe_year <- function(start_year = as.integer(format(Sys.Date(), "%Y")),
                               years_back = 4, api_key = census_api_key()) {
  for (year in seq(start_year, start_year - years_back)) {
    result <- tryCatch({
      resp <- request(SAIPE_SCHDIST_ENDPOINT) %>%
        req_url_query(get = "SD_NAME", `for` = "school district (unified)",
                      `in` = paste0("state:", WY_STATE_FIPS), time = year, key = api_key) %>%
        perform_with_retry()
      parse_saipe_json(resp_body_string(resp))
    }, error = function(e) data.frame())
    if (nrow(result) > 0) return(year)
  }
  stop("latest_saipe_year(): no working SAIPE vintage found in the last ", years_back, " years.")
}
