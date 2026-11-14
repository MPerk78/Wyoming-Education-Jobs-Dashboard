# Wyoming HE institution Pell Grant recipient share, from the US
# Department of Education's Federal Student Aid (FSA) office, via the
# Urban Institute's Education Data Portal -- a different federal agency
# than IPEDS (which the rest of this project's HE scrapers use), though
# bundled under the same "college-university" API namespace. Kept in its
# own file matching this project's one-file-per-source convention (same
# story as census_saipe_scraper.R being separate from census_acs_
# scraper.R despite both being Census Bureau programs).
# https://educationdata.urban.org/documentation/colleges.html
#
# Depends on ipeds_salary_scraper.R (fetch_ipeds_directory_paginated(),
# IPEDS_UNITID_MAP) and ipeds_enrollment_scraper.R (fetch_ipeds_wy_
# enrollment_for_year(), parse_ipeds_he_enrollment()) both being sourced
# first. Uses fetch_ipeds_directory_paginated() rather than fetch_ipeds_
# paginated() -- like the IPEDS directory endpoint, FSA's grants records
# mix real JSON nulls (grant_type rows an institution has no recipients
# for) with populated fields, which breaks fetch_ipeds_paginated()'s
# per-record as.data.frame() the same way it broke on the directory
# endpoint (confirmed live 2026-08-06: identical error). simplifyVector =
# TRUE hands the conversion off to jsonlite, which turns nulls into NA
# correctly.
#
# Endpoint used: college-university/fsa/grants/{year}/?fips=56, filtered
# to grant_type == 4 ("Pell Grant" -- the other grant_type codes cover
# other federal grant programs, like Academic Competitiveness/SMART/Iraq-
# Afghanistan Service grants, this project has no use for).
# grant_recipients_unitid is the real, unduplicated count of students who
# received a Pell grant at that institution that year -- deliberately not
# grant_recipients_opeid, which can double-count when one OPEID's award
# volume is allocated across multiple unit IDs (see this endpoint's own
# description, confirmed live 2026-08-06).
#
# Pell recipients are a real, defensible proxy for "share of students at
# this institution who are low-income" -- the closest HE analogue to
# K-12's Child_Poverty_Rate (SAIPE has no HE equivalent; see
# census_saipe_scraper.R's header for why). Divided by IPEDS FTE
# enrollment from the SAME YEAR as the Pell data, not the latest available
# Enrollment figure salarymap.csv otherwise carries -- FSA's grants data
# lags IPEDS's own enrollment data by a couple of years (confirmed live
# 2026-08-06: FSA grants tops out at 2021, IPEDS fall-enrollment goes
# through 2023), and pairing mismatched years would quietly overstate or
# understate the share. This is a headcount-of-recipients over FTE-
# enrollment ratio, not two directly comparable headcounts -- an honest
# limitation, not a silent one, the same spirit as every other proxy
# figure in this project.

suppressMessages({
  library(httr2)
  library(dplyr)
})

FSA_GRANTS_ENDPOINT <- "https://educationdata.urban.org/api/v1/college-university/fsa/grants"
FSA_PELL_GRANT_TYPE <- 4L

clean_fsa_value <- function(x) ifelse(is.na(x) | x < 0, NA_real_, x)

fetch_fsa_wy_grants_for_year <- function(year) {
  url <- paste0(FSA_GRANTS_ENDPOINT, "/", year, "/?fips=56")
  fetch_ipeds_directory_paginated(url)
}

# FSA publishes this survey with a longer lag than IPEDS's own surveys
# (confirmed live 2026-08-06: 2021 is the latest year with real WY data,
# 2022/2023 both return zero results) -- walks backward the same way
# find_latest_ipeds_salary_year()/find_latest_ipeds_enrollment_year() do.
find_latest_fsa_grants_year <- function(start_year = as.integer(format(Sys.Date(), "%Y")),
                                        years_back = 6) {
  for (year in seq(start_year, start_year - years_back)) {
    df <- fetch_fsa_wy_grants_for_year(year)
    if (nrow(df) > 0) return(list(year = year, data = df))
  }
  list(year = NA_integer_, data = data.frame())
}

# Pure transformation, kept separate from the network fetches so it's
# testable against captured fixtures. enrollment_df is the output of
# parse_ipeds_he_enrollment() for the SAME year as grants_df (see this
# file's header for why the years must match).
parse_ipeds_he_pell_share <- function(grants_df, enrollment_df, year) {
  if (nrow(grants_df) == 0 || nrow(enrollment_df) == 0) {
    return(data.frame(Name = character(0), Pell_Recipient_Share = numeric(0), Pell_Year = character(0), stringsAsFactors = FALSE))
  }

  pell <- grants_df %>%
    filter(grant_type == FSA_PELL_GRANT_TYPE) %>%
    transmute(unitid, Pell_Recipients = clean_fsa_value(grant_recipients_unitid))

  IPEDS_UNITID_MAP %>%
    left_join(pell, by = "unitid") %>%
    left_join(enrollment_df %>% select(Name, Enrollment), by = "Name") %>%
    mutate(
      Pell_Recipient_Share = ifelse(!is.na(Pell_Recipients) & !is.na(Enrollment) & Enrollment > 0,
                                     Pell_Recipients / Enrollment, NA_real_),
      Pell_Year = as.character(year)
    ) %>%
    select(Name, Pell_Recipient_Share, Pell_Year)
}

fetch_ipeds_he_pell_share <- function() {
  latest_grants <- find_latest_fsa_grants_year()
  if (is.na(latest_grants$year)) {
    return(data.frame(Name = character(0), Pell_Recipient_Share = numeric(0), Pell_Year = character(0), stringsAsFactors = FALSE))
  }
  enrollment_df <- parse_ipeds_he_enrollment(
    fetch_ipeds_wy_enrollment_for_year(latest_grants$year), latest_grants$year
  )
  parse_ipeds_he_pell_share(latest_grants$data, enrollment_df, latest_grants$year)
}
