# Wyoming K-12 teacher FTE (staff headcount, not job postings) from NCES's
# Common Core of Data (CCD), via the same Urban Institute Education Data
# Portal API used for HE salaries in ipeds_salary_scraper.R -- the district
# directory endpoint already reports teachers_total_fte directly, so no
# separate staff survey/endpoint is needed.
# https://educationdata.urban.org/documentation/
#
# Endpoint: school-districts/ccd/directory/{year} filtered to fips=56
# (Wyoming). agency_type == 1 ("regular local school district") with at
# least one school is what isolates real school districts from the handful
# of other WY-registered LEAs CCD tracks (BOCES-style regional service
# agencies, children's homes, a state health department entry with 0
# schools) -- confirmed live that this filter yields exactly 48 rows, a 1:1
# name match (after canonicalization) against the same 48 districts WSBA's
# salary PDFs cover.
#
# CCD's own district names ("Albany County School District #1") use "#N"
# (inconsistently spaced -- "#1" in most rows, "# 1" in a few) and, unlike
# this project's canonical form, always include "County" -- except Big Horn
# and Natrona don't take "County" in this project's canonical naming (the
# same exception canonicalize_wsba_district_name() in salary_scrapers.R
# handles), so that's done explicitly here too rather than assumed away.
#
# This exists to support a "vacancy rate" figure (current teacher-category
# postings / teachers_total_fte) alongside raw posting counts -- see
# app.R's map popup logic.

suppressMessages({
  library(httr2)
  library(dplyr)
})

CCD_DISTRICT_DIRECTORY_ENDPOINT <- "https://educationdata.urban.org/api/v1/school-districts/ccd/directory"

fetch_ccd_paginated <- function(url) {
  fetch_json_paginated(url)
}

fetch_ccd_wy_directory_for_year <- function(year) {
  url <- paste0(CCD_DISTRICT_DIRECTORY_ENDPOINT, "/", year, "/?fips=56")
  fetch_ccd_paginated(url)
}

# CCD lags similarly to IPEDS -- walk backward from the current year until
# a non-empty response is found rather than hardcoding a year that will
# eventually go stale (see find_latest_ipeds_salary_year() for the same
# pattern).
find_latest_ccd_directory_year <- function(start_year = as.integer(format(Sys.Date(), "%Y")),
                                            years_back = 5) {
  for (year in seq(start_year, start_year - years_back)) {
    df <- fetch_ccd_wy_directory_for_year(year)
    if (nrow(df) > 0) return(list(year = year, data = df))
  }
  list(year = NA_integer_, data = data.frame())
}

canonicalize_ccd_district_name <- function(name) {
  name <- trimws(name)
  pattern <- "^(.+?)\\s*#\\s*(\\d+)$"
  matches <- regmatches(name, regexec(pattern, name))
  vapply(matches, function(m) {
    if (length(m) != 3) return(NA_character_)
    prefix <- m[2]
    num <- m[3]
    if (grepl("^Big Horn County", prefix)) {
      paste0("Big Horn School District ", num)
    } else if (grepl("^Natrona County", prefix)) {
      paste0("Natrona School District ", num)
    } else {
      paste0(prefix, " ", num)
    }
  }, character(1))
}

# Pure transformation, kept separate from the network fetch above so it can
# be tested against a captured fixture instead of hitting the live API.
parse_ccd_teacher_fte <- function(df) {
  if (nrow(df) == 0) {
    return(data.frame(District = character(0), Teachers_Total_FTE = numeric(0),
                       Enrollment = numeric(0), stringsAsFactors = FALSE))
  }

  df %>%
    filter(agency_type == 1, !is.na(number_of_schools), number_of_schools >= 1) %>%
    transmute(
      District = canonicalize_ccd_district_name(lea_name),
      Teachers_Total_FTE = ifelse(is.na(teachers_total_fte) | teachers_total_fte < 0, NA_real_, teachers_total_fte),
      Enrollment = ifelse(is.na(enrollment) | enrollment < 0, NA_real_, enrollment)
    ) %>%
    filter(!is.na(District))
}

fetch_ccd_teacher_fte <- function() {
  latest <- find_latest_ccd_directory_year()
  result <- parse_ccd_teacher_fte(latest$data)
  if (nrow(result) > 0) result$CCD_Year <- as.character(latest$year)
  result
}

# ---------------------------------------------------------------------------
# Free/reduced-price lunch share (2026-11-09)
# ---------------------------------------------------------------------------
# Share of students eligible for free or reduced-price lunch, from CCD's
# school-level directory (free_or_reduced_price_lunch, enrollment), summed to
# the district. Shown beside SAIPE's child poverty rate: a school-based count
# rather than a Census model estimate. Two caveats, both stated on the
# dashboard: a school using community eligibility (free meals for all) may
# count every student, and a district only gets a share when its reporting
# schools hold at least FRL_MIN_REPORTED_SHARE of its enrollment (no figure
# from a few small schools). CCD marks a missing count as -1.
CCD_SCHOOL_DIRECTORY_ENDPOINT <- "https://educationdata.urban.org/api/v1/schools/ccd/directory"
FRL_MIN_REPORTED_SHARE <- 0.8

fetch_ccd_school_directory <- function(fips, year) {
  fetch_json_paginated(paste0(CCD_SCHOOL_DIRECTORY_ENDPOINT, "/", year, "/?fips=", fips))
}

# `id_map`: one entry per map entity (district or school) -> NCES ids. A
# 7-digit id is a district (all its schools); a 12-digit id is one school
# (a charter inside a host district).
parse_ccd_frl_share <- function(schools, id_map) {
  empty <- data.frame(District = character(0), Free_Reduced_Lunch_Share = numeric(0),
                       FRL_Schools_Reporting = integer(0), stringsAsFactors = FALSE)
  if (nrow(schools) == 0 || length(id_map) == 0) return(empty)
  s <- data.frame(ncessch = as.character(schools$ncessch), leaid = as.character(schools$leaid),
                  frl = suppressWarnings(as.numeric(schools$free_or_reduced_price_lunch)),
                  enrollment = suppressWarnings(as.numeric(schools$enrollment)), stringsAsFactors = FALSE)
  s <- s[!is.na(s$enrollment) & s$enrollment > 0, , drop = FALSE]
  rows <- lapply(names(id_map), function(entity) {
    ids <- as.character(id_map[[entity]])
    mine <- s[s$leaid %in% ids[nchar(ids) == 7] | s$ncessch %in% ids[nchar(ids) == 12], , drop = FALSE]
    rep <- mine[!is.na(mine$frl) & mine$frl >= 0, , drop = FALSE]
    share <- if (nrow(rep) == 0 || sum(rep$enrollment) < FRL_MIN_REPORTED_SHARE * sum(mine$enrollment)) {
      NA_real_
    } else {
      min(1, sum(rep$frl) / sum(rep$enrollment))
    }
    data.frame(District = entity, Free_Reduced_Lunch_Share = share,
               FRL_Schools_Reporting = nrow(rep), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# Each entity takes the newest year in which it has a share (FRL_Year says
# which): some districts skip a year (WY 2024: 8 of 48 reported no counts).
# When no year's directory answers at all (CCD down), it stops instead of
# returning blank rows, so safe_scrape() logs an error and the pipeline keeps
# last run's values (review, 2026-11-09: an outage had logged "ok" and blanked
# every district's share).
fetch_ccd_frl_share <- function(fips, id_map, start_year = as.integer(format(Sys.Date(), "%Y")), years_back = 4) {
  out <- NULL
  answered <- 0L
  errors <- character(0)
  for (year in seq(start_year, start_year - years_back)) {
    todo <- setdiff(names(id_map), out$District)
    if (length(todo) == 0) break
    schools <- tryCatch(fetch_ccd_school_directory(fips, year),
                        error = function(e) { errors <<- c(errors, conditionMessage(e)); data.frame() })
    if (nrow(schools) == 0 || !"free_or_reduced_price_lunch" %in% names(schools)) next
    answered <- answered + 1L
    r <- parse_ccd_frl_share(schools, id_map[todo])
    r <- r[!is.na(r$Free_Reduced_Lunch_Share), , drop = FALSE]
    if (nrow(r) > 0) out <- dplyr::bind_rows(out, transform(r, FRL_Year = as.character(year)))
  }
  if (answered == 0L) {
    stop("fetch_ccd_frl_share(): no CCD school-directory year answered",
         if (length(errors)) paste0(" (", errors[1], ")") else "")
  }
  missing <- setdiff(names(id_map), out$District)
  dplyr::bind_rows(out, data.frame(District = missing, Free_Reduced_Lunch_Share = rep(NA_real_, length(missing)),
                                   FRL_Schools_Reporting = rep(0L, length(missing)),
                                   FRL_Year = rep(NA_character_, length(missing)), stringsAsFactors = FALSE))
}

# Staffing for an entity that isn't its own district in CCD's district
# directory (a charter inside a host district): teachers and students summed
# over its schools in the school directory. Same `id_map` as parse_ccd_frl_share().
parse_ccd_school_staffing <- function(schools, id_map) {
  if (nrow(schools) == 0 || length(id_map) == 0) {
    return(data.frame(District = character(0), Teachers_Total_FTE = numeric(0), Enrollment = numeric(0)))
  }
  fte <- suppressWarnings(as.numeric(schools$teachers_fte)); enr <- suppressWarnings(as.numeric(schools$enrollment))
  fte[!is.na(fte) & fte < 0] <- NA; enr[!is.na(enr) & enr < 0] <- NA
  dplyr::bind_rows(lapply(names(id_map), function(entity) {
    ids <- as.character(id_map[[entity]])
    i <- as.character(schools$leaid) %in% ids[nchar(ids) == 7] | as.character(schools$ncessch) %in% ids[nchar(ids) == 12]
    data.frame(District = entity,
               Teachers_Total_FTE = if (any(i) && any(!is.na(fte[i]))) sum(fte[i], na.rm = TRUE) else NA_real_,
               # 0 students means none enrolled here (a BOCES, a state school whose
               # students are counted elsewhere), not a ratio of 0: NA.
               Enrollment = if (any(i) && sum(enr[i], na.rm = TRUE) > 0) sum(enr[i], na.rm = TRUE) else NA_real_,
               stringsAsFactors = FALSE)
  }))
}

# The geographic school districts (NCES LEAIDs) a point lies in, from the
# Census Bureau's public geocoder (no key): one unified district, or an
# elementary and a secondary district where a town is split (Butte Elem +
# Butte H S). For a school with no poverty estimate of its own (a charter, a
# private or tribal school), the surrounding district's SAIPE rate stands in
# (a split's two rates averaged, as for registry districts), labeled as such.
# NA when the geocoder finds none.
geocode_school_district <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) return(NA_character_)
  resp <- httr2::request("https://geocoding.geo.census.gov/geocoder/geographies/coordinates") %>%
    httr2::req_url_query(x = lon, y = lat, benchmark = "Public_AR_Current", vintage = "Current_Current",
                         layers = "Unified School Districts,Elementary School Districts,Secondary School Districts",
                         format = "json") %>%
    perform_with_retry()
  geo <- jsonlite::fromJSON(httr2::resp_body_string(resp))$result$geographies
  ids <- unlist(lapply(c("Unified School Districts", "Elementary School Districts", "Secondary School Districts"),
                       function(layer) { g <- geo[[layer]]; if (is.data.frame(g) && nrow(g) > 0 && !is.null(g$GEOID)) as.character(g$GEOID) }))
  if (length(ids) == 0) NA_character_ else unique(ids)
}

# A school's surrounding-district child poverty rate (charter, private, tribal
# or early-childhood school with no SAIPE geography of its own): the SAIPE
# rates of the districts geocode() finds at its point, averaged where a town is
# split into elementary and secondary districts. `saipe`: LEAID,
# Child_Poverty_Rate (and SAIPE_Year). `district_names`: LEAID -> display name.
# Returns status "ok", "none" (no district found / no rate) or "failed" (the
# geocoder errored: the caller keeps last run's value and logs it).
surrounding_district_poverty <- function(lat, lon, saipe, district_names = NULL, geocode = geocode_school_district) {
  res <- list(status = "none", rate = NA_real_, year = NA, basis = NA_character_)
  host <- tryCatch(geocode(lat, lon), error = function(e) structure(NA_character_, failed = TRUE))
  if (isTRUE(attr(host, "failed"))) { res$status <- "failed"; return(res) }
  h <- match(host, as.character(saipe$LEAID))
  h <- h[!is.na(h) & !is.na(saipe$Child_Poverty_Rate[h])]
  if (length(h) == 0) return(res)
  ids <- as.character(saipe$LEAID[h])
  nm <- if (!is.null(district_names)) unique(ifelse(is.na(district_names[ids]), ids, district_names[ids])) else ids
  res$status <- "ok"
  res$rate <- mean(saipe$Child_Poverty_Rate[h])
  if ("SAIPE_Year" %in% names(saipe)) res$year <- saipe$SAIPE_Year[h[1]]
  res$basis <- paste("Surrounding district:", paste(nm, collapse = " / "))
  res
}
