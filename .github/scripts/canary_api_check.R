# Pre-flight API Canary Check: tests core external APIs for reachability and
# basic schema integrity in ~5-10 seconds before running the full scrape pipeline.
#
# Prevents wasting 20+ runner minutes on a run doomed by an upstream outage,
# a retired endpoint, or unexpected API payload restructuring.

suppressMessages({
  library(httr2)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
strict_mode <- "--strict" %in% args

check_endpoint <- function(name, url, validator, timeout_secs = 10) {
  t0 <- proc.time()
  res <- tryCatch({
    req <- request(url) %>% req_timeout(timeout_secs)
    resp <- req_perform(req)
    status <- resp_status(resp)
    if (status != 200) {
      list(ok = FALSE, msg = sprintf("HTTP %d", status), elapsed = (proc.time() - t0)[["elapsed"]])
    } else {
      val_res <- validator(resp)
      if (isTRUE(val_res$ok)) {
        list(ok = TRUE, msg = "OK", elapsed = (proc.time() - t0)[["elapsed"]])
      } else {
        list(ok = FALSE, msg = val_res$error, elapsed = (proc.time() - t0)[["elapsed"]])
      }
    }
  }, error = function(e) {
    list(ok = FALSE, msg = conditionMessage(e), elapsed = (proc.time() - t0)[["elapsed"]])
  })
  res$name <- name
  res
}

validate_urban_ipeds <- function(resp) {
  body <- resp_body_json(resp, simplifyVector = TRUE)
  if (!"results" %in% names(body) || length(body$results) == 0) {
    return(list(ok = FALSE, error = "missing 'results' in payload"))
  }
  df <- as.data.frame(body$results)
  if (!all(c("unitid", "year") %in% names(df))) {
    return(list(ok = FALSE, error = "expected keys 'unitid' and 'year' missing"))
  }
  list(ok = TRUE)
}

validate_urban_ccd <- function(resp) {
  body <- resp_body_json(resp, simplifyVector = TRUE)
  if (!"results" %in% names(body) || length(body$results) == 0) {
    return(list(ok = FALSE, error = "missing 'results' in payload"))
  }
  df <- as.data.frame(body$results)
  if (!all(c("leaid", "year") %in% names(df))) {
    return(list(ok = FALSE, error = "expected keys 'leaid' and 'year' missing"))
  }
  list(ok = TRUE)
}

validate_wsba <- function(resp) {
  html <- resp_body_string(resp)
  if (!grepl("wsba", html, ignore.case = TRUE) && !grepl("wyoming", html, ignore.case = TRUE)) {
    return(list(ok = FALSE, error = "response did not match WSBA job board signature"))
  }
  list(ok = TRUE)
}

validate_census <- function(resp) {
  json <- resp_body_json(resp)
  if (!is.list(json) || length(json) < 2) {
    return(list(ok = FALSE, error = "Census response not tabular list matrix"))
  }
  list(ok = TRUE)
}

run_canaries <- function(fips = "56") {
  checks <- list()

  # 1. Urban Institute IPEDS Salaries
  ipeds_url <- sprintf("https://educationdata.urban.org/api/v1/college-university/ipeds/salaries-instructional-staff/2024/?fips=%s&limit=1", fips)
  checks[[1]] <- check_endpoint("Urban Institute IPEDS Salary", ipeds_url, validate_urban_ipeds)

  # 2. Urban Institute CCD Directory
  ccd_url <- sprintf("https://educationdata.urban.org/api/v1/school-districts/ccd/directory/2023/?fips=%s&limit=1", fips)
  checks[[2]] <- check_endpoint("Urban Institute CCD Directory", ccd_url, validate_urban_ccd)

  # 3. WSBA Statewide Feed
  wsba_url <- "https://www.wsba-wy.org/vacancies"
  checks[[3]] <- check_endpoint("WSBA Statewide Feed", wsba_url, validate_wsba)

  # 4. Census Bureau (if key present)
  census_key <- Sys.getenv("CENSUS_API_KEY")
  if (nzchar(census_key)) {
    census_url <- sprintf("https://api.census.gov/data/2022/acs/acs5?get=NAME,B19013_001E&for=county:001&in=state:%s&key=%s", fips, census_key)
    checks[[4]] <- check_endpoint("Census Bureau ACS", census_url, validate_census)
  }

  checks
}

if (sys.nframe() == 0) {
  cat("Running Pre-Flight API Canary Checks...\n")
  results <- run_canaries(fips = "56")
  all_ok <- TRUE

  for (r in results) {
    status_tag <- if (r$ok) "[OK]  " else "[FAIL]"
    cat(sprintf("  %s %-32s (%.2fs): %s\n", status_tag, r$name, r$elapsed, r$msg))
    if (!r$ok) all_ok <- FALSE
  }

  if (!all_ok && strict_mode) {
    stop("API Canary checks failed in strict mode.")
  } else if (!all_ok) {
    cat("Warning: One or more canary checks failed (non-strict mode; continuing).\n")
  } else {
    cat("All pre-flight API canary checks passed.\n")
  }
}
