# Schema-drift guard for every dataset app.R reads by name.
#
# Every existing check in this repo (sanity_check.R, drift_check.R,
# check_salary_drift.R's coverage/plausibility checks) validates the DATA:
# how many rows, whether the numbers look sane. None of them check that the
# COLUMNS a downstream pipeline chunk or app.R itself references by name
# actually exist -- a chunk silently dropping or renaming a column (a
# refactor typo, a classifier change that forgot to update a select()) would
# still pass every one of those checks (they don't look at column names at
# all) and then crash app.R with a cryptic "object 'Teachers_Total_FTE' not
# found" for the first user who opens the dashboard after the bad deploy,
# with nothing in the error identifying which upstream FILE is actually
# responsible.
#
# check_file_schema() is the pure, testable half. .github/scripts/
# verify_schema.R is the blocking runner (like sanity_check.R, not
# diagnostic-only like drift_check.R) -- a missing column WILL break the
# live app for every user, so this has to stop the commit, not just alert
# about it after the fact.
#
# REQUIRED_SCHEMAS reflects the columns app.R actually reads by name from
# each file (verified against app.R directly, not just "whatever columns
# happen to be in the file today") -- k12_salary_history.csv is
# deliberately absent since nothing in app.R reads it yet.
# check_file_schema() moved to the edjobs package in phase 2 (2026-11-11); attached here so
# every script that sources this file still has them.
suppressMessages(library(edjobs))

REQUIRED_SCHEMAS <- list(
  # "Posted / listed" fallback (scripts/build_posting_spells.R); optional in
  # app.R, which shows the column as before without it.
  "posting_first_seen.csv" = c("Segment", "Key", "First_Seen", "Left_Censored"),
  "combinedclean.csv" = c("title", "date_posted", "position", "location", "url", "posting_id", "District", "Close_Date",
                          "Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text",
                          "Est_Pay", "Est_Pay_Unit", "Est_Occupation", "Est_Area", "Est_Year"),
  "k12jobanalysis.csv" = c("title", "Archive_Date", "location", "posting_id", "District"),
  "allsum.csv" = c("Broad_Category", "Archive_Date", "District", "sum"),
  "allnow.csv" = c("Broad_Category", "Sum", "District"),
  "k12_district_weekly_totals.csv" = c("District", "Archive_Date", "n"),
  "salarymap2.csv" = c("District", "County", "Latitude", "Longitude", "Job_Link",
                        "Teacher_Base_Salary", "Teacher_Base_Salary_Prior_Year", "Salary_Year",
                        "Superintendent_Salary", "Superintendent_Contract_Days", "Salary_Source",
                        "Teachers_Total_FTE", "Data_Coverage", "Enrollment",
                        "Median_Household_Income", "Median_Gross_Rent", "Mining_Employment_Share",
                        "Population_Change_Pct", "ACS_Year", "Child_Poverty_Rate", "SAIPE_Year",
                        "Teacher_Avg_Salary"),
  "facultydata.csv" = c("Title", "Location", "Institution", "Link", "Archive_Date", "Job_Type", "Category"),
  "allsum_he.csv" = c("Category", "Archive_Date", "Institution", "Job_Type", "sum"),
  "allnow_he.csv" = c("Category", "Job_Type", "Sum", "Institution"),
  "he_institution_weekly_totals.csv" = c("Institution", "Archive_Date", "n"),
  "salarymap.csv" = c("Name", "Longitude", "Latitude", "Link", "Faculty_Avg_Salary",
                       "Faculty_Avg_Salary_Professor", "Faculty_Count", "Salary_Year",
                       "Salary_Note", "Salary_Source", "Faculty_Avg_Salary_Y1Ago", "Faculty_Avg_Salary_Y2Ago",
                       "County", "Median_Household_Income", "Median_Gross_Rent",
                       "Mining_Employment_Share", "Population_Change_Pct", "ACS_Year", "Enrollment",
                       "Enrollment_Change_Pct", "Pell_Recipient_Share", "Pell_Year"),
  "data_dictionary.csv" = c("Dataset", "File_Name", "Column_Name", "Data_Type", "Primary_Authority", "Definition"),
  "job_duration_spells.csv" = c("District", "Title", "Category", "Broad_Category", "Position", "First_Seen_Date", "Last_Seen_Date", "Weeks_Observed", "Estimated_Days_Open", "Status"),
  "he_job_duration_spells.csv" = c("Institution", "Title", "Category", "Job_Type", "First_Seen_Date", "Last_Seen_Date", "Weeks_Observed", "Estimated_Days_Open", "Status"),
  "qcew_county_school_pay.csv" = c("County", "Avg_Annual_Pay", "Year"),
  # Reference tables the pipeline reads to build the Est_* columns (kept
  # between runs, refreshed through write_reference_file()).
  "oews_role_wages.csv" = c("Area_Code", "Area_Name", "SOC", "Occupation", "Annual_Median", "Hourly_Median", "Year"),
  "ipeds_staff_salaries.csv" = c("IPEDS_UNITID", "Staff_Category", "Category_Label", "Avg_Salary", "Year"),
  "uw_salary_book.csv" = c("Title", "Salary", "Fiscal_Year", "Source_URL")
)

REQUIRED_SCHEMAS_XLSX <- list(
  "hedata.xlsx" = c("Title", "Location", "Posted_Date", "Institution", "Link", "Archive_Date",
                  "Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text",
                  "Est_Pay", "Est_Pay_Unit", "Est_Occupation", "Est_Area", "Est_Year", "Est_Source")
)

