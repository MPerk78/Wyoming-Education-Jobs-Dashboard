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

# Hourly-pool, work-study, student and intern postings. The figures here are
# for full-time salaried staff, so these get none ("Hourly Pooled - Broadcast
# Studio Manager, Student Media" is a student job, not management).
# Part-time, temporary and seasonal jobs and attendants too: a full-time
# annual average overstates them ("Cook I - Part Time" showed MSU's $57K
# service average, a $11/hr "Fitness Center Attendant" Gillette's $59K).
HOURLY_OR_STUDENT_TITLE <- paste0("pooled|work[- ]?study|\\bintern(ship)?s?\\b|student (employee|worker|technician|assistant|aide|staff|position)|\\bfws\\b|",
                                  "\\(student|(&|\\+|\\bor) student|part[- ]?time|\\btemp(orary)?\\b|seasonal|attendant")

# A higher-ed posting for salaried staff: not faculty, not hourly or student.
is_salaried_staff_posting <- function(title) {
  !classify_he_job_type(title) %in% c("Instructor/Teacher/Faculty", "Adjunct/Part-Time Faculty") &
    !grepl(HOURLY_OR_STUDENT_TITLE, tolower(as.character(title)))
}

# IPEDS's management average is pooled over every manager at a college, so it
# only fits senior titles. In UW's FY26 salary book the median VP/provost/dean/
# chief earns $177K, AVP/associate dean $162K and executive director $148K
# (UW's IPEDS management average: $159K), but a director $106K, an assistant
# director $82K and a manager $66K.
SENIOR_MANAGEMENT_TITLE <- "president|provost|chancellor|\\bdean\\b|\\bchief\\b|(executive|senior|sr\\.?) director|director, senior"
NOT_SENIOR_TITLE <- "(assistant|associate|deputy) chief|assistant to\\b"

# The IPEDS staff category for a higher-ed posting title. NA for faculty,
# hourly and student postings (is_salaried_staff_posting()) and for titles
# no category fits. Checked in order, so "Administrative Assistant to the
# Director" is office support and "Athletic Trainer" is healthcare, not sports.
# Only senior titles get the management average. Other directors get none
# (no category tracks their pay); managers and supervisors get their
# function's category ("IT Manager" -> computer, "Business Manager" ->
# business, "Facilities Manager" -> maintenance) when one fits; "Project
# Manager" and other titles naming no function get none.
ipeds_staff_category_for_title <- function(title) {
  t <- tolower(as.character(title))
  # A lab manager, coordinator or technician is lab staff; a lab assistant
  # ("Sonography Lab Assistant", $23.76/hr) isn't paid like one.
  lab_staff <- grepl("\\blab\\b.*(manager|coordinator|supervisor|tech)|(manager|coordinator|supervisor)\\b.*\\blab\\b", t)
  senior <- grepl(SENIOR_MANAGEMENT_TITLE, t) & !grepl(NOT_SENIOR_TITLE, t)
  t <- gsub("\\b(manager|supervisor|superintendent)s?\\b", " ", t)
  code <- dplyr::case_when(
    grepl("nurse|\\brn\\b|physician|clinic|pharmac|athletic trainer|therapist|medical|dental", t) ~ 9L,
    grepl("research (assistant|associate|scientist|technician|technologist|specialist|coordinator)|postdoc", t) ~ 2L,
    grepl("(administrative|executive) (assistant|associate|support|specialist)|secretar|receptionist|\\bclerk|office (assistant|associate|specialist)|program assistant|data entry", t) ~ 12L,
    senior ~ 5L,
    grepl("director", t) ~ NA_integer_,
    grepl("account|financ|budget|payroll|timekeep|purchas|procure|auditor|bursar|human resources|\\bhr\\b|benefits|\\bbusiness\\b", t) ~ 6L,
    grepl("\\bit\\b|information technology|developer|programmer|software|network|systems? admin|database|engineer|\\bweb|cyber|scientist|laboratory", t) |
      ((lab_staff | grepl("\\btechnology\\b", t)) & !grepl("technology transfer|instructional|writing", t)) ~ 7L,
    grepl("librar|curator|archiv|advis|tutor|instructional (design|technolog|specialist)|writing (lab|center)|learning specialist|success coach", t) ~ 4L,
    grepl("counsel|social work|attorney|legal|paralegal|marketing|communications|graphic|designer|media|photograph|video|\\bcoach|athletic|sports|recreation|fitness|outreach", t) ~ 8L,
    grepl("extension agent", t) ~ 3L,
    grepl("cashier", t) ~ 11L,
    grepl("mainten|hvac|electric|plumb|carpent|grounds|mechanic|locksmith|painter|facilities|laborer|\\boperator", t) ~ 13L,
    grepl("custod|janitor|food|cook|dining|chef|barista|security|police|child ?care|housekeep", t) ~ 10L,
    grepl("driver|delivery|warehouse|shipping|\\bmail", t) ~ 14L,
    TRUE ~ NA_integer_
  )
  ifelse(is_salaried_staff_posting(title), code, NA_integer_)
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
