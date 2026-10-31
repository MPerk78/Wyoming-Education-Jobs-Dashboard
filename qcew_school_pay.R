# ---------------------------------------------------------------------------
# BLS QCEW: average annual pay of public school employees, by county
# ---------------------------------------------------------------------------
#
# County context for the map: what local-government elementary and
# secondary schools (NAICS 6111, ownership 3 -- public school districts)
# paid their employees on average, across every role, from BLS's Quarterly
# Census of Employment and Wages open-data files (data.bls.gov/cew/data/api,
# which serves plain CSV to scripts). Not a figure for any one job. BLS
# withholds counties with too few employers to protect confidentiality
# (most of Wyoming's), which stay out of the file.
#
# Shared verbatim by the Montana and Wyoming dashboards.

QCEW_API <- "https://data.bls.gov/cew/data/api"

# County rows for one state from QCEW's annual NAICS 6111 file:
# County ("Yellowstone County"), County_FIPS, School_Employment,
# Avg_Annual_Pay, Year. titles is QCEW's area_titles table.
parse_qcew_school_pay <- function(industry, titles, state_fips) {
  industry$area_fips <- sprintf("%05s", as.character(industry$area_fips))
  for (col in c("own_code", "annual_avg_emplvl", "avg_annual_pay")) industry[[col]] <- suppressWarnings(as.numeric(industry[[col]]))
  x <- industry[substr(industry$area_fips, 1, 2) == state_fips & industry$own_code == 3 &
                  !endsWith(industry$area_fips, "000") & !endsWith(industry$area_fips, "999") &
                  industry$disclosure_code != "N" & industry$avg_annual_pay > 0, , drop = FALSE]
  titles$area_fips <- sprintf("%05s", as.character(titles$area_fips))
  name <- titles$area_title[match(x$area_fips, titles$area_fips)]
  data.frame(
    County = sub(",.*$", "", name),
    County_FIPS = x$area_fips,
    School_Employment = x$annual_avg_emplvl,
    Avg_Annual_Pay = x$avg_annual_pay,
    Year = as.character(x$year),
    stringsAsFactors = FALSE
  )
}

read_qcew_csv <- function(url) {
  resp <- httr2::request(url) %>% perform_with_retry()
  # All text, so FIPS codes keep their leading zeros; numbers are converted
  # where they're used.
  utils::read.csv(text = httr2::resp_body_string(resp), stringsAsFactors = FALSE, colClasses = "character")
}

# Latest year QCEW has published annual averages for, searched back from
# this calendar year.
fetch_qcew_school_pay <- function(state_fips, start_year = as.integer(format(Sys.Date(), "%Y")), lookback = 3) {
  titles <- read_qcew_csv("https://data.bls.gov/cew/doc/titles/area/area_titles.csv")
  for (year in seq(start_year, start_year - lookback)) {
    industry <- tryCatch(read_qcew_csv(paste0(QCEW_API, "/", year, "/a/industry/6111.csv")),
                         error = function(e) NULL)
    if (is.null(industry)) next
    out <- parse_qcew_school_pay(industry, titles, state_fips)
    if (nrow(out) > 0) return(out)
  }
  stop("No QCEW annual school pay for state ", state_fips, " since ", start_year - lookback)
}

# Joins on a county however it's written: "Yellowstone County" or
# "Albany County, Wyoming".
qcew_county_key <- function(county) sub(",.*$", "", trimws(as.character(county)))
