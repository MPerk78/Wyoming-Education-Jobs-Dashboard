# Wyoming's state configuration for the edjobs package (state_config(), since
# edjobs 0.7.0, phase 4). What package code needs to know about this state, as
# data. Sourced by the pipeline's setup chunk, tests/testthat/helper-setup.R
# and .github/scripts/check_salary_drift.R; set before anything refreshes or
# checks one of WY's own reference tables.
library(edjobs)

set_state_config(state_config(
  state = "Wyoming", fips = "56", app_dir = "Wy_Ed_Jobs",
  # WY's own reference tables (the shared ones are edjobs::REFERENCE_TABLE_RULES).
  reference_tables = list(
    uw_salary_book = list(name = "UW salary book", required = "Salary",
                          bounds = list(Salary = c(1000, 5000000)),
                          year_col = "Fiscal_Year", max_age = 2)
  )
))
