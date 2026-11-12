# Wyoming's state configuration for the edjobs package (state_config(), since
# edjobs 0.7.0, phase 4). What package code needs to know about this state, as
# data. Sourced by the pipeline's setup chunk, tests/testthat/helper-setup.R
# and .github/scripts/check_salary_drift.R; set before anything refreshes or
# checks one of WY's own reference tables.
library(edjobs)

# Where a Wyoming source name comes from, and how each is scraped
# (edjobs::resolve_scraper_call(), for the PR live check,
# scripts/repro_scraper.R and the auto-fix issue). Three groups:
#   - k12_district_registry.csv: one fetch_* per Platform, with arguments
#     derived from Job_Link/Org_ID by the same helpers Wy_ED_Jobs.Rmd uses
#     (applitrack_tenant_path() etc. in direct_api_scrapers.R);
#   - misc_district_registry (misc_district_scrapers.R): platform -> the
#     fetch_* that fetch_misc_district_postings() dispatches to;
#   - higher ed and the one charter school: hardcoded calls in the Rmd,
#     mirrored in EXTRA_SCRAPER_CALLS. test-drift-check.R asserts each one
#     still appears verbatim in Wy_ED_Jobs.Rmd, so the two can't drift.
# WSBA-only orgs (WSBA_ONLY_ORGS) have no scraper of their own and resolve
# to NULL, as does anything else not listed.
WY_K12_SCRAPER_SPECS <- list(
  Applitrack   = scraper_spec("fetch_applitrack_postings", list(function(row) applitrack_tenant_path(row$Job_Link))),
  TedK12       = scraper_spec("fetch_tedk12_postings", "Job_Link"),
  SchoolSpring = scraper_spec("fetch_schoolspring_postings", list(function(row) schoolspring_domain(row$Job_Link))),
  RedRoverK12  = scraper_spec("fetch_redrover_postings",
                              list(function(row) as.character(row$Org_ID), function(row) redrover_org_slug(row$Job_Link)))
)
# `session = TRUE`: the fetch_* takes a chromote session first.
WY_MISC_SCRAPER_SPECS <- list(
  wordpress = scraper_spec("fetch_wordpress_postings", "url"),
  smartsites = scraper_spec("fetch_smartsites_postings", "url"),
  schoolblocks = scraper_spec("fetch_schoolblocks_postings", "url"),
  edlio = scraper_spec("fetch_edlio_postings", "url"),
  googlesites = scraper_spec("fetch_googlesites_postings", "url", session = TRUE),
  educational_networks = scraper_spec("fetch_educational_networks_postings", "url"),
  apptegy = scraper_spec("fetch_apptegy_postings", "url", session = TRUE),
  prairieview = scraper_spec("fetch_prairieview_postings", "url", session = TRUE)
)
# `url`: the page a person would visit (the drift check's corroboration
# step), not necessarily the endpoint the scraper reads. Western was once
# listed at schooljobs.com/careers/westernwyoming, a real but wrong NEOGOV
# agency that returned a plausible "0 jobs found" page; it is on PeopleAdmin
# (see Wy_ED_Jobs.Rmd's "western wyoming" chunk).
EXTRA_SCRAPER_CALLS <- list(
  "Laramie County Community College"  = list(fn = "fetch_neogov_postings", args = list("https://www.governmentjobs.com", "lcccwy"),
                                             url = "https://www.governmentjobs.com/careers/lcccwy"),
  "Casper College"                    = list(fn = "fetch_neogov_postings", args = list("https://www.schooljobs.com", "caspercollege"),
                                             url = "https://www.schooljobs.com/careers/caspercollege"),
  "Western Wyoming Community College" = list(fn = "fetch_peopleadmin_atom", args = list("https://wwcwy.peopleadmin.com/postings/all_jobs", "Western Wyoming Campus"),
                                             url = "https://wwcwy.peopleadmin.com/postings/all_jobs"),
  "Central Wyoming College"           = list(fn = "fetch_neogov_postings", args = list("https://www.schooljobs.com", "cwc"),
                                             url = "https://www.schooljobs.com/careers/cwc"),
  "Eastern Wyoming Community College" = list(fn = "fetch_peopleadmin_atom", args = list("https://ewc.peopleadmin.com/postings/all_jobs", "Eastern Wyoming Campus"),
                                             url = "https://ewc.peopleadmin.com/postings/all_jobs"),
  "Gillette College"                  = list(fn = "fetch_neogov_postings", args = list("https://www.schooljobs.com", "gillettecollege"),
                                             url = "https://www.schooljobs.com/careers/gillettecollege"),
  "Sheridan College"                  = list(fn = "fetch_peopleadmin_atom", args = list("https://jobs.sheridan.edu/postings/all_jobs", "Sheridan College Campus"),
                                             url = "https://jobs.sheridan.edu/postings/all_jobs"),
  "Northwest College"                 = list(fn = "fetch_peopleadmin_atom", args = list("https://northwestcollege.simplehire.com/postings/all_jobs", "Northwest College Campus"),
                                             url = "https://northwestcollege.simplehire.com/postings/all_jobs"),
  "University of Wyoming"             = list(fn = "fetch_uw_postings", args = list(),
                                             url = "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1"),
  "Laramie Montessori Charter School" = list(fn = "fetch_paylocity_jobs", args = list("70f7d03b-6c9f-4106-8e70-9d781a8bbcba", "Laramie-Montessori-School-Inc"))
)
# The colleges' pages by name (the entries above that carry a url).
he_institution_urls <- unlist(lapply(EXTRA_SCRAPER_CALLS, `[[`, "url"))

set_state_config(state_config(
  state = "Wyoming", fips = "56", app_dir = "Wy_Ed_Jobs",
  feed = list(short_name = "WSBA", label = "WSBA statewide vacancies", url = "https://www.wsba-wy.org/vacancies",
              discrepancy_file = "wy_discrepancy_report.csv"),
  # The first archive that counts toward the drift check's per-source
  # baselines: earlier runs predate fixes that changed the counts.
  drift_baseline_from = "2026-08-03",
  scraper_sources = list(
    # The registry sits at the repository root; tests run from tests/testthat.
    scraper_registry(function() utils::read.csv(if (file.exists("k12_district_registry.csv")) "k12_district_registry.csv"
                                                else here::here("k12_district_registry.csv"), stringsAsFactors = FALSE),
                     "District", "k12_district_registry.csv", specs = WY_K12_SCRAPER_SPECS),
    scraper_registry(function() misc_district_registry, "District", "misc_district_registry (misc_district_scrapers.R)",
                     specs = WY_MISC_SCRAPER_SPECS, platform_col = "platform", url_col = "url"),
    scraper_calls(EXTRA_SCRAPER_CALLS, where = "a hardcoded call in Wy_ED_Jobs.Rmd")
  ),
  # The auto-fix issue: scraper files read as text; fixtures are named
  # <platform>_<district>..., and several districts share one parser.
  autofix = list(scraper_files = c("direct_api_scrapers.R", "misc_district_scrapers.R"), fixtures_by = "name",
                 fixture_example = "tests/testthat/fixtures/misc_districts/{platform}_<district>_rendered_{date}.txt"),
  # WY's own reference tables (the shared ones are edjobs::REFERENCE_TABLE_RULES).
  reference_tables = list(
    uw_salary_book = list(name = "UW salary book", required = "Salary",
                          bounds = list(Salary = c(1000, 5000000)),
                          year_col = "Fiscal_Year", max_age = 2)
  )
))
