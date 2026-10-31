# ---------------------------------------------------------------------------
# IPEDS non-instructional staff salaries
# ---------------------------------------------------------------------------
#
# Each college's average pay for its full-time non-faculty staff, by IPEDS
# occupational category (Urban Institute Education Data Portal endpoint
# college-university/ipeds/salaries-noninstructional-staff/{year}: staff
# count and total salary outlays per category; average = outlays / count).
# Used only as a labeled fallback for higher-ed staff postings that state no
# pay -- an institution-wide category average, not that job's offer. Annual
# data; the weekly pipeline refreshes ipeds_staff_salaries.csv and keeps the
# existing file if the API fails.
#
# Shared verbatim by the Montana and Wyoming dashboards.

IPEDS_STAFF_ENDPOINT <- "https://educationdata.urban.org/api/v1/college-university/ipeds/salaries-noninstructional-staff"

# Urban's staff_category codes (api/v1/api-values/?format_name=staff_category),
# shortened for display.
IPEDS_STAFF_CATEGORIES <- c(
  "2" = "research", "3" = "public service", "4" = "education services (library, advising, academic affairs)",
  "5" = "management", "6" = "business and financial operations", "7" = "computer, engineering and science",
  "8" = "community service, legal, arts, media and sports", "9" = "healthcare", "10" = "service",
  "11" = "sales", "12" = "office and administrative support",
  "13" = "construction and maintenance", "14" = "production and transportation"
)

# Averages from fewer full-time staff than this are too noisy to show.
IPEDS_STAFF_MIN_COUNT <- 3

# One row per institution x category: IPEDS_UNITID, Staff_Category,
# Category_Label, Staff_Count, Avg_Salary, Year. unitids limits the result to
# this dashboard's institutions.
parse_ipeds_staff_salaries <- function(results, unitids, year) {
  df <- as.data.frame(results, stringsAsFactors = FALSE)
  keep <- df$unitid %in% unitids & as.character(df$staff_category) %in% names(IPEDS_STAFF_CATEGORIES) &
    !is.na(df$noninstruc_staff_count) & df$noninstruc_staff_count >= IPEDS_STAFF_MIN_COUNT &
    !is.na(df$salary_outlays) & df$salary_outlays > 0
  df <- df[keep, , drop = FALSE]
  data.frame(
    IPEDS_UNITID = as.integer(df$unitid),
    Staff_Category = as.integer(df$staff_category),
    Category_Label = unname(IPEDS_STAFF_CATEGORIES[as.character(df$staff_category)]),
    Staff_Count = df$noninstruc_staff_count,
    Avg_Salary = round(df$salary_outlays / df$noninstruc_staff_count),
    Year = as.character(year),
    stringsAsFactors = FALSE
  )
}

fetch_ipeds_staff_year <- function(year, fips) {
  url <- paste0(IPEDS_STAFF_ENDPOINT, "/", year, "/?fips=", fips)
  pages <- list()
  while (!is.null(url)) {
    body <- jsonlite::fromJSON(httr2::resp_body_string(httr2::request(url) %>% perform_with_retry()))
    if (length(body$results) > 0) pages[[length(pages) + 1]] <- body$results
    url <- body[["next"]]
  }
  dplyr::bind_rows(pages)
}

# The most recent year with data for this state, searched back from this
# calendar year.
fetch_ipeds_staff_salaries <- function(fips, unitids, start_year = as.integer(format(Sys.Date(), "%Y")), lookback = 4) {
  for (year in seq(start_year, start_year - lookback)) {
    results <- tryCatch(fetch_ipeds_staff_year(year, fips), error = function(e) data.frame())
    if (nrow(results) > 0) return(parse_ipeds_staff_salaries(results, unitids, year))
  }
  stop("No IPEDS non-instructional staff salary data for fips ", fips, " since ", start_year - lookback)
}

# The IPEDS staff category for a higher-ed posting title. NA for faculty
# postings (classify_he_job_type()) and for titles no category fits.
# Checked in order, so "Administrative Assistant to the Director" is office
# support and "Athletic Trainer" is healthcare, not sports.
ipeds_staff_category_for_title <- function(title) {
  t <- tolower(as.character(title))
  faculty <- classify_he_job_type(title) %in% c("Instructor/Teacher/Faculty", "Adjunct/Part-Time Faculty")
  code <- dplyr::case_when(
    grepl("nurse|\\brn\\b|physician|clinic|pharmac|athletic trainer|therapist|medical|dental", t) ~ 9L,
    grepl("research (assistant|associate|scientist|technician|technologist|specialist|coordinator)|postdoc", t) ~ 2L,
    grepl("administrative (assistant|associate|support|specialist)|secretar|receptionist|\\bclerk|office (assistant|associate|specialist)|program assistant|data entry", t) ~ 12L,
    grepl("director|\\bdean\\b|vice president|provost|president|chancellor|\\bchief\\b|manager|superintendent", t) ~ 5L,
    grepl("account|financ|budget|payroll|purchas|procure|auditor|bursar|human resources|\\bhr\\b|benefits", t) ~ 6L,
    grepl("\\bit\\b|information technology|developer|programmer|software|network|systems? admin|database|engineer|\\bweb|cyber|scientist|laboratory|lab tech", t) ~ 7L,
    grepl("librar|curator|archiv|advis|tutor|instructional design|learning specialist|success coach", t) ~ 4L,
    grepl("counsel|social work|attorney|legal|paralegal|marketing|communications|graphic|designer|media|photograph|video|\\bcoach|athletic|sports|recreation|outreach", t) ~ 8L,
    grepl("extension agent", t) ~ 3L,
    grepl("cashier", t) ~ 11L,
    grepl("mainten|hvac|electric|plumb|carpent|grounds|mechanic|locksmith|painter|facilities tech|laborer|\\boperator", t) ~ 13L,
    grepl("custod|janitor|food|cook|dining|chef|barista|security|police|child ?care|housekeep", t) ~ 10L,
    grepl("driver|delivery|warehouse|shipping|\\bmail", t) ~ 14L,
    TRUE ~ NA_integer_
  )
  ifelse(faculty, NA_integer_, code)
}

# Adds Est_* to higher-ed postings: the institution's IPEDS average for the
# posting's staff category (Est_Pay, per year), Est_Occupation (category),
# Est_Area (institution), Est_Year. unitid_of_institution maps each
# posting's Institution to its IPEDS_UNITID.
attach_ipeds_staff_estimates <- function(postings, staff, unitid_of_institution) {
  category <- ipeds_staff_category_for_title(postings$Title)
  key <- paste(unitid_of_institution, category)
  idx <- match(key, paste(staff$IPEDS_UNITID, staff$Staff_Category))
  postings$Est_Pay <- staff$Avg_Salary[idx]
  postings$Est_Pay_Unit <- ifelse(is.na(idx), NA_character_, "year")
  postings$Est_Occupation <- staff$Category_Label[idx]
  postings$Est_Area <- ifelse(is.na(idx), NA_character_, as.character(postings$Institution))
  postings$Est_Year <- staff$Year[idx]
  postings
}
