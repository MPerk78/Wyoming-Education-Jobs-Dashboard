# Wyoming Education Jobs Dashboard — Project Progress Ledger

**Maintainers & Authors:** Mike Bostick & Mark Perkins  
**Target Environment:** Posit Connect Cloud (Shiny for R)  
**CI/CD Orchestration:** GitHub Actions (`.github/workflows/weekly-scrape.yml`)  
**Production Cadence:** Automated weekly scrape & build every Friday 05:00 UTC (and manual `workflow_dispatch`)  
**Repository Branch:** `main`

---

## 1. Executive Summary & Purpose

This ledger serves as the authoritative, version-controlled record of feature development, pipeline hardening, data schema evolution, and cross-state feature synchronization between the **Wyoming Education Jobs Dashboard** and its sister project, the **Montana Education Jobs Dashboard**.

Unlike ephemeral conversation logs or local scratch handoffs, this document lives within the repository root so any human contributor, collaborator, or autonomous coding agent can instantly understand the exact state of the system, what has been implemented, active guardrails, and upcoming priorities.

---

## 2. Feature & Architecture Parity Matrix (WY vs. MT)

| Capability / Module | Wyoming (`Wy_Ed_Jobs`) | Montana (`Mt_Ed_Jobs`) | Parity Status | Implementation Notes |
| :--- | :--- | :--- | :--- | :--- |
| **K-12 Active Jobs Table** | Implemented | Implemented | **Full Parity** | Filterable by district, position, subject, broad category; exportable. |
| **K-12 District Summary** | Implemented | Implemented | **Full Parity** | District profiles, student-teacher ratios, salary benchmarks. |
| **K-12 Longitudinal Trends** | Implemented | Implemented | **Full Parity** | Multi-year vacancy volume time series with rolling averages. |
| **K-12 Current Trends** | Implemented | Implemented | **Full Parity** | In-season bar/donut distributions by category & subject. |
| **K-12 New This Week** | Implemented | Implemented | **Full Parity** | Delta detection vs. prior week's scrape snapshot. |
| **K-12 Vacancy Spells & Time-to-Fill** | Implemented | Implemented | **Full Parity** | Interactive Shiny tab (`k12_spells`) with KPIs, histograms, and spell tables. |
| **Higher Ed Vacancy Spells & Time-to-Fill** | Implemented | Implemented | **Full Parity** | Interactive Shiny tab (`he_spells`) with KPIs, duration histograms, discipline breakdown, and spell table. |
| **Higher Ed Jobs Table** | Implemented | Implemented | **Full Parity** | Faculty, adjunct, and institutional staff postings. |
| **Higher Ed Summary & Trends** | Implemented | Implemented | **Full Parity** | IPEDS instructional salary benchmarks & enrollment. |
| **Geographic Leaflet Map** | Implemented | Implemented | **Full Parity** | Interactive position/subject keyword search ("special ed", "mathematics"), suggestion chips, dynamic radius scaling, layer controls (Positron, OSM, Esri Topo), and cluster pins. |
| **Data Sources & Codebook Tab** | Implemented | Implemented | **Full Parity** | Direct CSV download buttons and interactive schemas. |
| **Federal NCES LEA IDs** | Embedded (48/48) | Embedded (200/200) | **Full Parity** | 7-digit `NCES_LEAID` in registry, `salarymap2.csv`, and `job_duration_spells.csv`. |
| **Federal IPEDS UNITIDs** | Embedded (8/8) | Embedded (23/23) | **Full Parity** | 6-digit `IPEDS_UNITID` in higher ed institutional registry and `salarymap.csv`. |
| **Pipeline Dry-Run Test Harness** | Implemented | Implemented | **Full Parity** | Evaluates scrape combination chunks against mock data frames in CI. |
| **Pipeline AST Defensive Linting**| Implemented | Implemented | **Full Parity** | Blocks syntax typos and enforces date coercions. |
| **Connect Cloud Manifest Engine** | Implemented | Implemented | **Full Parity** | Enforces literal `null` metadata and users blocks for Connect Cloud. |

---

## 3. Implementation Chronology & Milestones

### Milestone 1: Core Architecture & Vacancy Spells Model (Wyoming Origin)
* **K-12 Vacancy Spells & Duration Panel:**
  * Pioneered the empirical duration spell panel (`job_duration_spells.csv`) tracking time-to-fill, right-censored vacancies, and subject-area shortages across Wyoming school districts.
  * Designed the interactive `k12_spells` dashboard module featuring:
    - 4 reactive summary KPIs (Median Days Open, Long-Duration Vacancies, Active vs. Closed).
    - Density/histogram distributions and role-level duration box plots.
    - Exportable tabular panel for labor economists and empirical education policy analysts.
* **Federal Universal Identifiers Integration:**
  * Embedded official 7-digit `NCES_LEAID` Local Education Agency identifiers from NCES CCD into `k12_district_registry.csv`, `salarymap2.csv`, and `job_duration_spells.csv`.
  * Embedded 6-digit `IPEDS_UNITID` for all Wyoming community colleges and the University of Wyoming in `salarymap.csv`.
* **State Staffing Matrix Integration:**
  * Integrated Wyoming Department of Education (WDE602) staffing salary reports, providing role-specific district average total compensation benchmarks (`Role_Avg_Salary`) alongside WSBA minimum base teacher salaries.

### Milestone 2: Cross-State Scraper Defensive Engineering (October 2026)
* **`safe_scrape()` Enhancements:**
  * Hardened `safe_scrape()` in `scrape_helpers.R` to enforce character coercion on `date_posted` and `Posted_Date` fields upon return. This guarantees compatibility with `empty_df()` and blocks `bind_rows()` type collisions across heterogeneous scrapers.
* **Pipeline Dry-Run Test Harness:**
  * Implemented `tests/testthat/test-pipeline-orchestration-dryrun.R` executing mock chunk evaluations of data combination logic in ~0.3s.
* **Attribution Harmonization:**
  * Standardized project authorship across dashboard headers, data dictionaries, and codebooks to:  
    *"Developed by Mike Bostick and Mark Perkins"*.

### Milestone 3: Higher Education Vacancy Duration Spells & Script Parity
* **Collegiate Vacancy Spells & Time-to-Fill (`he_job_duration_spells.csv` & `he_spells`):**
  * Derived longitudinal collegiate duration spell panels from `facultydata.csv` tracking time-to-fill, right-censored vacancies, and academic discipline indicators across all Wyoming colleges and UW (711 spells).
  * Implemented interactive `he_spells` module in `Wy_Ed_Jobs/app.R`:
    - 4 reactive summary KPIs: Median Vacancy Duration (Days), Currently Open Searches, Persistent Searches (≥90d), Longest Duration Discipline.
    - 4 reactive user filters: Vacancy Status, Appointment Type (Regular vs Adjunct), Academic Focus (STEM, Health Sciences, CTE), Institution Picker.
    - 2 Plotly interactive visualizations: Median Days Open by Discipline horizontal bar chart and binned duration distribution histogram.
    - Searchable DT data table with CSV and Excel export buttons.
    - Direct CSV download button in the *Data Sources & Open Research Downloads* tab (`he_job_duration_spells.csv`).
  * Created standalone automated generator scripts `scripts/generate_he_job_duration_spells.R` and `scripts/generate_job_duration_spells.R`.
  * Added `he_job_duration_spells.csv` to `REQUIRED_SCHEMAS` in `schema_check.R`, verified with `test-schema-check.R`.
  * Documented all schema variables in `DATA_DICTIONARY.md`, `data_dictionary.csv`, and `DATA_COOKBOOK.md`.
  * Integrated automated spell generation step into `.github/workflows/weekly-scrape.yml`.

### Milestone 4: Interactive Spells Hardening, Research Downloads Parity & UX Polish
* **Interactive Vacancy Spells Resiliency:**
  * Added zero-row / empty filter state handling across both K-12 and Higher Ed vacancy spells modules (`k12_spells`, `he_spells`):
    - Reactive KPIs return clean zero counts (`0 Days`, `0 Open Spells`, `0 Persistent Spells`, `None`) instead of NaN or errors.
    - Plotly duration and distribution charts render clean, informative placeholder notifications (`"No vacancy spells match the selected filter criteria"`) instead of leaving an empty white canvas.
    - Duration distribution histograms regularized with `tidyr::complete()` across all 5 standard duration bins (`1–14 Days`, `15–30 Days`, `31–60 Days`, `61–90 Days`, `90+ Days` for K-12; `1–30 Days`, `31–60 Days`, `61–90 Days`, `91–180 Days`, `180+ Days` for Higher Ed), guaranteeing consistent axes and comparable geometries under dynamic filtering.
    - Searchable DT spell tables render clean empty states without `req()` blocking.
* **Homepage Interactive Drill-Down:**
  * Enhanced "Top K-12 Hiring Districts This Week" and "Top Higher Ed Hiring Institutions This Week" tables with clickable employer action links.
  * Clicking any top employer automatically selects that employer, switches to the filtered `k12_table` or `he_table`, and displays all active positions.
  * Hardened `observeEvent(input$popup_view_jobs)` to seamlessly handle mapped institutions, direct scrape LEAs, and statewide locations.
* **Open Research Downloads Completeness:**
  * Added direct CSV downloads for both **K-12 Weekly Vacancies** (`download_weekly_totals`, serving `k12_district_weekly_totals.csv`) and **Higher Ed Weekly Vacancies** (`download_he_weekly_totals`, serving `he_institution_weekly_totals.csv`).
  * Documented both series in `Wy_Ed_Jobs/data_dictionary.csv` and `DATA_DICTIONARY.md`.
* **Data Quality Test Suite Expansion:**
  * Registered `job_duration_spells.csv` and `he_job_duration_spells.csv` into `ALL_OUTPUT_CSVS` in `test-weekly-data-quality.R`, enforcing automated verification against NaN, Inf, and literal string leakage across 1,296 passing tests.
* **Attribution Harmonization:**
  * Harmonized home tab footer attribution to `"Developed by Mike Bostick and Mark Perkins"`.

### Milestone 5: Tab Navigation Consolidation & Head-to-Head Comparator Modules
* **Sidebar Navigation Consolidation:**
  * Streamlined sidebar navigation from 16 tabs down to 10 tabs, removing sidebar clutter while preserving 100% of underlying analytical modules.
  * Replaced flat tab list with organized `tabBox` subtab navigation:
    - **Profiles & Compare** (`k12_summary` / `he_summary`): Subtab 1: Directory Table, Subtab 2: Head-to-Head Comparator.
    - **Current Trends & New** (`k12_current` / `he_current`): Subtab 1: Category & Discipline Breakdown, Subtab 2: New This Week Postings.
    - **Trends & Duration Spells** (`k12_trends` / `he_trends`): Subtab 1: Longitudinal Hiring Trends, Subtab 2: Vacancy Spells & Time-to-Fill.
* **Side-by-Side District & College Comparator:**
  * Built interactive Head-to-Head Comparators allowing users to select and benchmark up to 4 districts or colleges simultaneously.
  * Dynamic comparative cards display active openings, vacancy rates, compensation (starting base, average total pay, IPEDS faculty compensation), student enrollment, student-to-staff ratios, and community socioeconomic metrics.
  * Comparative Plotly visualizations for salary benchmarks, staffing scale, and appointment distributions.
  * Real-time filtered openings table showing active postings across the selected employers.
  * One-touch quick presets for "Top Hiring" and "Largest Enrollment" employers.
* **Test Suite Expansion & Zero-Warning Cleanliness:**
  * Added automated reactive tests in `test-app-reactives.R` verifying comparator card rendering, Plotly graphics, tabular filters, and preset button event handlers.
  * Updated `LEAF_TABS` contract in `test-app-boot-smoke.R` to reflect the 10 consolidated destination tabs.
  * Verified 1,312 passing tests (0 fails, 0 warnings, 1 skip on CRAN).

### Milestone 6: Map Multi-Dimensional Filtering & Responsive Navigation (2026-10-28)
* **Multi-Dimensional Map Filter Panel:**
  * Built an integrated responsive control panel directly above the interactive Leaflet map featuring:
    - **Employer Type:** Toggle K-12 Districts, Higher Ed Institutions, or both.
    - **County Filter:** Dropdown populated from all 23 Wyoming counties with clean user-facing labels (e.g., "Albany County", "Laramie County").
    - **Salary Benchmark Tier:** Filter by average educator compensation ($70,000+ Average Pay, $60,000 - $70,000, Under $60,000) harmonizing district teacher average pay (with starting base fallback) and IPEDS faculty average pay.
    - **Posting Volume & Urgency:** Filter by employer hiring intensity (All Active Locations, High Volume 5+ openings, Multiple Openings 2+, New Postings This Week).
    - **1-Click Reset:** Single-click "Reset Filters" action button restoring all controls to default statewide view.
* **Dynamic Filter Result Badge:**
  * Real-time indicator displaying matching location counts and cumulative active openings across the filtered locations (`Showing X of Y active locations (Z current openings across filtered locations)`).
  * Informative warning state with guidance to broaden filters when zero locations match the selected criteria.
* **Responsive Geo-Spatial Viewport Centering:**
  * Automatically pans and bounds-zooms the Leaflet map viewport to the selected county when a user chooses a county filter, and smoothly returns to the statewide Wyoming view when reset.
* **Defensive Marker Lifecycle & Zero-Row Guarding:**
  * Ensured `leafletProxy` reliably clears markers without generating orphaned popups or JavaScript errors on 0-row filter states.
* **Automated Reactive Test Coverage:**
  * Added comprehensive tests in `test-app-reactives.R` verifying default filtering, single and combined filter attributes, county zoom coordinates, empty state handling, and reset triggers (20 new tests, 1,332 total tests passing with 0 fails and 0 warnings).

### Milestone 7: Research Data Dictionary Completeness & Automated Codebook Invariants (2026-10-28)
* **100% Data Dictionary & Codebook Column Coverage:**
  * Expanded `Wy_Ed_Jobs/data_dictionary.csv` to 131 variables across all 10 public research download CSVs on the Data Sources tab.
  * Resolved documentation drift for `k12jobanalysis.csv` (76,000+ historical records), Census ACS variables (`Median_Household_Income`, `Median_Gross_Rent`, `Population`, `Mining_Employment_Share`), WDE staffing subgroups, and IPEDS historical faculty compensation.
  * Synchronized [`DATA_DICTIONARY.md`](file:///Users/mikebostick/repos/Jobs_Dash_repos/Wyoming-Education-Jobs-Dashboard/DATA_DICTIONARY.md) markdown codebook with complete markdown tables and universal identifier definitions (`NCES_LEAID`, `IPEDS_UNITID`).
* **Automated Codebook Invariant Test Harness:**
  * Created [`tests/testthat/test-data-dictionary.R`](file:///Users/mikebostick/repos/Jobs_Dash_repos/Wyoming-Education-Jobs-Dashboard/tests/testthat/test-data-dictionary.R) enforcing:
    - 100% column documentation: every column in every public download CSV must exist in `data_dictionary.csv`.
    - Zero phantom columns: every documented column must exist in the target dataset.
    - Non-empty descriptions and valid data types.
    - Synchronized dataset inventories between Shiny UI download handlers and `data_dictionary.csv`.
* **Data Quality Test Suite Registration:**
  * Added `data_dictionary.csv` to `ALL_OUTPUT_CSVS` in `tests/testthat/test-weekly-data-quality.R`.
  * Verified 1,390 passing tests across the entire test suite (0 fails, 0 warnings, 1 skip on CRAN).

### Milestone 8: Categorization Engine Overhaul & Modernized UX Navigation Sync (2026-10-28)
* **Categorization Engine & Precedence Overhaul:**
  * Synchronized classification engine in `k12_he_classification.R` with comprehensive precedence overhaul:
    - Prioritized Clerical (`Secretary`, `Clerk`, `Registrar`) before `Teacher`.
    - Prioritized paraprofessionals (`Teacher Assistant`, `Teaching Assistant`, `Instructional Assistant`) into Paraprofessional rather than inflating Teacher counts.
    - Prioritized Higher Ed administrative/Provost leadership roles (`Faculty Affairs`, `Faculty Development`, `Faculty Director`) before faculty classifications.
    - Fixed misclassifications: `Zoology` now routed to `Science` instead of `Specialist`, `Political Science` and `Social Sciences` routed to `Social Science` instead of `Science`, and `Health Science`/`CNA` routed to CTE/Health instead of general Science.
    - Expanded subject definitions: `Elementary Education` (`K-[1-6]`), `English Language Arts` (`Intervention Teacher`), `Language Education` (`Culture Teacher`), `Substitute Teaching` (`Guest Teacher`), `Health Science` (`CNA`, `Certified Nursing Assistant`), and Higher Ed faculty fields (`leadership fellows`, `financial planning`, `family financial`, `iv&v`, `verification & validation`).
  * Rebuilt historical and current datasets from durable raw archives (`Archivek12_Data/` and `Archived_HE_Data/`):
    - **Wyoming Higher Ed:** Uncategorized postings dropped from 3,522 (18.6%) down to **2,576 (13.6%)** (86.4% categorized).
    - **Wyoming K-12:** Uncategorized dropped from 1,068 (8.1%) down to **518 (3.9%)** (96.1% categorized).
    - Recovered and accurately categorized **1,496 postings** in Wyoming previously lost in generic or misclassified buckets.
* **Shiny Dashboard UI/UX Overhaul:**
  * **Simplified Information Architecture:** Streamlined sidebar navigation to eliminate ambiguous labels ("Profiles & Compare") in favor of clear, intuitive domain entities: `Home`, `Map`, `All Postings`, `Districts` / `Colleges & Universities`, `New Postings`, `Current Trends`, `Longitudinal Trends`, `Data`, `Report Mistake`.
  * **1-Click Quick Filter Toolbar:** Added an instant filter button bar (`radioGroupButtons`: `[ 📋 All Active Postings ]` | `[ 🟢 New This Week ]`) directly above the primary DataTables on both K-12 and Higher Ed postings pages.
  * **Elimination of Nested Sub-tabs:** Removed inner `tabBox` views inside the Current Trends pages in favor of clean single-card layouts.
  * **Responsive Table Containment:** Applied `.trends-table-container` CSS bounding table widths to `980px`, adding clean borders, auto margins, styled headers, and fixed-width sparkline columns, eliminating horizontal table sprawling and whitespace overflow.
  * **Dedicated New Postings Tabs:** Implemented dedicated `k12_new` and `he_new` sidebar views.
* **Test Suite Verification & Parity:**
  * Full test suite passing with 0 failures and 0 warnings.
  * Connect Cloud manifest regenerated via `rsconnect::writeManifest(appDir = "Wy_Ed_Jobs")`.

---

## 4. Key Architectural Contracts & Invariants

1. **Pipeline / Dashboard Decoupling:**
   * `Wy_ED_Jobs.Rmd` is the sole pipeline entry point; it scrapes, munges, and saves derived CSVs to `Wy_Ed_Jobs/`.
   * `Wy_Ed_Jobs/app.R` is the Shiny dashboard reading committed CSVs. It **never** scrapes live pages.
2. **Posting Identity Contract (`Posting_ID`):**
   * Assigned once prior to deduplication, longitudinal accumulation, or spell calculations.
   * Uses real per-posting URLs when available; falls back to `title + location + date_posted` with an occurrence suffix.
3. **Schema Integrity:**
   * Every CSV consumed by `app.R` must be registered in `REQUIRED_SCHEMAS` inside `schema_check.R` and tested in `test-schema-check.R`.
4. **Posit Connect Cloud Manifest Safety:**
   * Empty lists in R become `{}` in JSON via `jsonlite::toJSON()`, which Connect Cloud rejects. Post-processing forces `metadata` and `users` to literal `null`.

---

## 5. Active & Upcoming Workstreams

- [x] Initial design and release of Vacancy Spells panel.
- [x] Universal `NCES_LEAID` and `IPEDS_UNITID` integration across all data layers.
- [x] Harmonize attribution to Mike Bostick & Mark Perkins.
- [x] Harden `safe_scrape()` against heterogeneous date type collisions.
- [x] Add pipeline dry-run and AST linting tests to CI test suite.
- [x] Create reproducible generator scripts `scripts/generate_job_duration_spells.R` and `scripts/generate_he_job_duration_spells.R`.
- [x] Expand Higher Ed analytics to include vacancy duration metrics (`he_job_duration_spells.csv` and `he_spells` tab).
- [x] Optimize test suite runtime for heuristic scrapers (mock `Sys.sleep()` in `test-eastern-wyoming-scraper.R`, cutting run time from 64s to <1s).
- [x] Harden interactive Vacancy Spells modules against empty filter states and regularize duration distribution bins.
- [x] Enable 1-click drill-down navigation from Homepage top hiring employers tables to filtered jobs tables.
- [x] Add K-12 and Higher Ed weekly vacancy series downloads and codebook definitions.
- [x] Add spell panels to automated data-quality invariant suite.
- [x] Harmonize attribution across all dashboard views.
- [x] Streamline sidebar navigation into consolidated tabBoxes (10 main tabs).
- [x] Implement Head-to-Head District and College Comparator modules.
- [x] Multi-dimensional map filtering (salary tiers, county cluster, posting volume/urgency) with dynamic count badge and auto-zoom.
- [x] 100% data dictionary column coverage and automated codebook invariant test suite.
- [x] Overhaul K-12 and Higher Ed categorization engine: eliminate misclassifications (Zoology, Pol Sci, Health Sci), audit precedence (clerical before teacher, paraprofessional before teacher, provost/admin before faculty), and expand subject coverage recovering 1,496 previously uncategorized jobs.
- [x] Modernize dashboard UX navigation: clear tab titles (`Districts`, `Colleges & Universities`), flat Current Trends layout without nested tabs, 1-click `🟢 New This Week` filter button, and responsive table container styling.
- [x] Comprehensive 8-dimension data quality audit & historical archive reprocessing across MT & WY:
  * Full reprocessing from raw historical archives back to first scrape date (`rebuild_k12_history_from_archive.R` and higher ed rebuilds) propagating classification fixes across all historical time-series datasets (`allsum.csv`, `k12jobanalysis.csv`, `facultydata.csv`, `job_duration_spells.csv`).
  * Repaired text encoding / HTML entity leakage (`&amp;`, `&quot;`, `&#39;`, `&lt;`, `&gt;`, `&nbsp;`) in job titles.
  * Resolved non-instructional role precedence leaks: bare `Clerk` (Attendance, Records, Library Clerk) routed to `Staff` ahead of `Teacher`; `Library/Media Assistant` to `Paraprofessional`.
  * Expanded discipline coverage: Arapaho/Arapahoe language, Drama/Theatre in Art, Choir/Chorus/Vocal in Music, Trade & Technical, Autism/ASD in SPED, Grade 1-7 primary grades.
  * 100% test pass rate across both WY (1,424 tests) and MT (3,051 tests).
- [x] Streamline dashboard navigation architecture across K-12 and Higher Ed:
  * Promoted **Vacancy Spells** to a dedicated top-level sidebar tab for both K-12 (`k12_spells`) and Higher Education (`he_spells`) featuring 4 KPI ValueBoxes, duration distributions, and searchable spell listings.
  * Unified **Current Trends** and **Longitudinal Trends** into a single cohesive **Trends** tab (`k12_trends`, `he_trends`) using a clean top-level `tabBox` with two panels: Current Snapshot (12-week sparklines and discipline breakdown) and Longitudinal Trends (multi-year timeline slider and interactive Plotly time series).
  * Retired redundant standalone "New Postings" sidebar tab, relying on the 1-click `🟢 New This Week` quick-filter toggle in the main postings tables. Preserved reactive server endpoints (`output$k12_new_table`, `output$he_new_table`) for backwards compatibility.
  * Standardized sidebar navigation into exactly 4 symmetrical items per education tier:
    - K-12 CAREERS: `All Postings`, `Districts`, `Trends`, `Vacancy Spells`
    - HIGHER EDUCATION: `All Postings`, `Colleges & Universities`, `Trends`, `Vacancy Spells`
  * 100% test pass rate across both WY (1,424 tests) and MT (3,051 tests).
- [x] Refine public-facing terminology and sidebar proportions:
  * Replaced technical jargon like "scraped" with clean phrasing ("compiled", "weekly updates") across data provenance and Vacancy Spells modules.
  * Single-word navigation clarity: Renamed tabs to concise, intuitive terms (`Jobs` for postings, `Colleges` for institutions, `Time-to-Fill` for duration metrics, `Feedback` for bug reporting).
  * Narrowed sidebar container: Reduced sidebar width from default 230px to 180px (`width = "180px"`, `titleWidth = "180px"`), eliminating empty void space after tab labels and giving 50px more width to data tables and maps. Streamlined category headers to `K-12`, `HIGHER ED`, and `COMMUNITY`.
- [x] Full Wyoming K-12 and Higher Education Statewide Infrastructure & Platform Drift Audit:
  * **Fremont County School District 24** (Shoshoni, WY): Recovered from silent platform drift (AppliTrack throwing `HTTP 500 Internal Server Error`); migrated registry to RedRoverK12 (org `4335`, slug `fremont24`), restoring 2 active vacancies (`Elementary Special Education Paraprofessional`, `Substitute Teacher`) to direct pipeline.
  * **Comprehensive K-12 Structured Registry Audit (38/38 - 100% Active)**:
    - 21 AppliTrack districts: 100% operational, 313 live vacancies (Big Horn, Carbon, Converse, Fremont, Park, Sheridan, Sweetwater, Teton, Uinta, Washakie).
    - 8 SchoolSpring districts: 100% operational, 220 live vacancies (Crook, Laramie 1 & 2, Natrona, Fremont 25, Albany, Campbell, Lincoln 2).
    - 2 TedK12 districts: 100% operational, 18 live vacancies (Fremont 2, Goshen 1).
    - 7 RedRoverK12 districts: 100% operational, 51 live vacancies (Johnson 1, Hot Springs 1, Fremont 38, Lincoln 1, Sublette 9, Platte 2, Fremont 24).
    - Total structured pipeline: 568 live postings extracted across all 38 districts with 0 errors.
  * **Heuristic District & Charter Audit (13/13 - 100% Active, 78 live postings)**:
    - 10 public districts: Niobrara 1 (5), Park 16 (2), Platte 1 (13), Sheridan 3 (0), Sublette 1 (2), Uinta 4 (4), Uinta 6 (5), Washakie 2 (1), Weston 1 (14), Weston 7 (5).
    - 3 charter schools: Cheyenne Classical (1), Wyoming Classical (21), Prairie View (5).
  * **Higher Education Institutions (9/9 - 100% Active, 489 live postings)**:
    - University of Wyoming (231), LCCC (84), Sheridan College (71), Western Wyoming CC (41), Gillette College (27), Central Wyoming College (13), Eastern Wyoming College (10), Casper College (8), Northwest College (4).
  * **Charter Schools & WSBA (100% Active)**:
    - Laramie Montessori Charter School (Paylocity): 2 live postings.
    - WSBA Statewide Vacancy Portal: 110 live postings.
  * Total suite: 1,424 tests PASS (0 failures, 0 warnings, 1 skip on CRAN).
- [x] Interactive Leaflet Map Position & Keyword Search with Suggestion Chips and Multi-Basemap Controls:
  * **Interactive Keyword / Subject Search**: Added instant full-text query matching (`match_job_query`) supporting compound terms, educational abbreviations ("special ed", "sped", "math", "pe", "cte"), and multi-word token matching across both K-12 and Higher Education postings.
  * **Dynamic Employer Filtering & Marker Scaling**: Map marker locations automatically filter to only employers with active matching openings, dynamically scaling circle marker radius proportional to matching vacancy counts.
  * **Rich Contextual Tooltips & Popups**: Hover tooltips and click popups highlight matching position counts, listing sample matching job titles alongside total vacancy counts and median salaries.
  * **Dynamic Feedback Badging**: Added real-time count badge above the map displaying exact matching openings, matching locations, and active query terms (with red guidance badge when 0 matches found).
  * **Instant Suggestion Chips**: Integrated 1-click filter chips for popular subject areas (`Special Ed`, `Mathematics`, `Elementary`, `Science`, `Music / Art`, `Bus Driver`, and `Clear`).
  * **Multi-Basemap Layer Control**: Added interactive tile layer switcher offering CartoDB Positron (clean default), OpenStreetMap, and Esri World Topo Map.
  * Total suite: 1,468 tests PASS (0 failures, 0 warnings, 1 skip on CRAN).
- [x] Post-review fixes (2026-10-29):
  * **Position classifier regressions from the precedence overhaul** (`classify_k12_position()`, identical in MT and WY): bare `Intern` matched inside "Internal Applicants Only" (teacher and bus-driver postings landed in Support Services); `Technician` was checked ahead of Custodial/Transportation/Food Services (cafeteria, maintenance, HVAC, facility technicians); `Director`/`Driver` ahead of Teacher sent "Band/Choir Director" to Administration and "Drivers Education Teacher" to Transportation. Fixed with a word-bounded `\bIntern\b`, a separate late Technician rule, and an early Teacher rule for ensemble directors and Drivers Ed; regression tests added in `test-k12-classification.R`. K-12 history and `job_duration_spells.csv` rebuilt from archive.

- [x] Categorization & research-download data-quality audit (2026-10-30):
  * **One shared classifier for both states.** The Higher Ed job-type and faculty-category functions had drifted between MT and WY (different rule order, Legal checked before Criminal Justice in WY); they are now identical in both repos, with each state's extra rules folded in.
  * **K-12 position:** OPI Jobs for Teachers publishes its subject-area field as the title ("Elementary Curriculum", "Special Education", "Music"), so those teaching postings landed in Other. Titles that name only a subject or grade now classify as Teacher unless they name an assistant/advisor-type role. Also: `\bPara`/`\bSub` word boundaries, nurses and counseling after Teacher/Staff/Admin, and new keywords for guards, noon duty, grounds, superintendents, stipend sports, and kitchen staff. Other fell from 10.9% to 1.8% of archived K-12 rows; uncategorized teacher subjects are 1.5%.
  * **K-12 subject:** bare "CTE" moved to the last-checked CTE bucket so "CTE – Agricultural/Business/FCS" reach their subjects; "Physical Ed", "Health Enhancement", "Reading", "World Languages", "Industrial Technology" and the "Special Educaiton" typo now match.
  * **HE:** "latin" matched inside "Gal**latin** College" (every Gallatin pool landed in Language) and "ems" inside "Syst**ems**" (Information Systems faculty in Health Sciences); bare "Instructional" counted technologists, designers and campus child-center aides as faculty. HE job-type Other fell from 17.2% to 1.6% (MT) and 5.9% to ~0% (WY); WY uncategorized faculty fell from 13.6% to 3.7%.
  * **Placeholder postings dropped** via shared `is_placeholder_posting()` in the pipeline and rebuild scripts: "Sample/Future Certified/Classified Position", account-creation templates, "Student Teaching Opportunities", "No open positions", a scraped "Job Title" header (EWC). Standing applicant pools are kept. Raw archives are unchanged.
  * **Vacancy spells:** a same-title posting absent for more than 21 days now starts a new spell (WY had 244 merged re-postings with a median "duration" of 382 days); `Weeks_Observed` counts calendar weeks rather than scrape dates.
  * **MT `salarymap2.csv`:** the read-back + registry join emitted `NCES_LEAID.x`/`.y` (a third copy would follow next run); collapsed to one `NCES_LEAID` and fixed at the source.
  * **District name variants:** MT OPI location labels normalized ("BILLINGS" / "Billings, MT" → "Billings"); WY "#21" / "No.1" spellings canonicalized.
  * Regression tests added for every fix; histories and spells rebuilt from archive.
- [x] WY weekly run fix (2026-10-30): the weekly salary chunks now keep `IPEDS_UNITID` and no longer duplicate the CCD
  columns (`.x`/`.y`). Both had been introduced by Oct 27 hand-edits to the committed CSVs that the pipeline didn't
  reproduce. Verified with full local rehearsals of both weekly workflows.
- [ ] Posted pay from the job posting itself (in progress):
  * [x] Increment 1: shared `extract_posted_pay()` (amount/range, unit, source snippet; never converted between units).
    AppliTrack postings carry `Pay_Min`/`Pay_Max`/`Pay_Unit`/`Pay_Text` into `combinedclean.csv`, with a "Posted Pay"
    column in the Jobs table. About 91% of sampled live AppliTrack postings state an amount.
  * [x] Increment 2 (2026-10-30): RedRover boards carry Pay_* from the GraphQL `payTypeId`/`minPay`/`maxPay` fields.
  * [x] Increment 3 (2026-10-30): higher ed. The NEOGOV colleges (LCCC, Casper, Central Wyoming, Gillette) carry Pay_*
    from each listing card's pay line, or its description preview when that line is blank, into `hedata.xlsx` and the
    raw HE archive, with a "Posted Pay" column in the Higher Ed Jobs table. Live 2026-10-30: LCCC 57/84 (NEOGOV prints
    "$0.00" for adjuncts with no stated pay, now read as none), Casper 7/8, Central 8/13, Gillette 21/22.
    `facultydata.csv` keeps its schema (no Pay_*). PeopleAdmin colleges and UW: not yet.
  * [x] Pay-coverage monitor (2026-10-31): `check_pay_coverage.R` (non-blocking weekly step, in the monitoring
    self-check) flags a source whose postings still arrive but whose share with stated pay fell to <=20% of its own
    average (needs 2+ archived weeks with pay, avg share >=40%, 3+ postings), into the weekly drift issue. Pure logic
    in `drift_check.R` (`flag_pay_coverage_drop()`), tested.
  * [x] Map pay filter rebuilt on posted pay (2026-10-31): "Pay" bands in each posting's own unit (hourly: under $18 /
    $18-25 / $25+; annual: MT under $50k / $50-60k / $60k+, WY under $60k / $60-70k / $70k+), nothing converted. A posting
    counts by its posted pay (range midpoint), else its district/institution role figure, else the estimate below; day
    rates and stipends only under "All". The band narrows each dot to its matching postings, and "View N matching jobs"
    carries it to the Jobs tab.
  * [x] BLS OEWS fallback (2026-10-31): `bls_oews_scraper.R` (shared) pulls May-2025 regional medians for 17 school roles
    via the BLS API (bls.gov downloads refuse scripts), refreshing `oews_role_wages.csv` only when BLS publishes a new
    year. Each K-12 posting gets `Est_*` from its district's county area (statewide where unknown or suppressed); coaches
    get none. Jobs table: "Typical pay (BLS est.)" with role/area on hover, shown only where no pay is posted.
  * [x] IPEDS staff fallback (2026-10-31): `ipeds_staff_salaries.R` (shared) -- each college's average pay for full-time
    non-faculty staff by IPEDS category; HE staff postings get `Est_*` by title -> category. HE Jobs table: "Typical pay
    (IPEDS est.)".
  * [x] QCEW county context (2026-10-31): `qcew_school_pay.R` (shared) -- average pay of public school employees by
    county (all roles), shown in the district map popup.
  * [x] UW salary book (2026-10-31): IPEDS lumps each college's managers into one average, and UW reports only its top
    111 there ($159K), so UW assistant directors and business managers showed $159K. `uw_salary_book.R` (WY only) parses
    UW's published "Salary Information - Departmental Listing" (FY2026: 3,084 salaried staff, department/title/salary,
    no names); UW staff postings get the median salary of UW staff with the same title, else the same role before the
    first comma, with 3+ holders (Business Manager $60,894 from 40; Assistant Director $60,756 from 44). Unmatched UW
    management titles show nothing rather than $159K. BLS OEWS was checked and rejected for this: postsecondary
    administrators' 25th percentile is $81K (MT) / $108K (WY). HE Jobs column renamed "Typical pay (est.)", source on hover.
  * [x] Hourly-pool, work-study, student and intern postings no longer get an annual IPEDS figure (shared
    `is_salaried_staff_posting()`); e.g. UW Student Media hourly jobs had shown $68,490. "Executive Assistant" is office
    support, not management.
  * [ ] Declined: ACS county earnings (less specific than QCEW for the same question).
  * [x] Pay data-quality safeguards (2026-11-01, both states). Posted amounts out of range for their unit
    (`PAY_PLAUSIBLE_RANGE`) are blanked before they reach the dashboard (`drop_implausible_pay()`); the raw archives
    keep them, and the weekly drift issue lists them. Extractor no longer reads application costs, money paid into a
    benefits package, or loan-assistance lists as pay (real misreads from the 10-31 rehearsal). Reference tables behind
    the estimates (OEWS, QCEW, IPEDS staff pay, the UW salary book) refresh through `write_reference_file()`, which keeps
    the old file when a download is partial, mostly blank or out of range, and logs each refresh to `scrape_log.csv`;
    `check_salary_drift.R` reports failed, rejected, missing or stale refreshes. Schema check and the blocking
    data-quality sweep cover the new files and the Pay_*/Est_* columns.
  * [x] Pay extractor fixes (2026-11-01, both states): a unit run into the next label ("$145/dayBENEFITS",
    "$21.69/hrThe District") is read as day/hour, and an earlier amount's bonus wording no longer vetoes a salary
    after it ("Signing bonus $1,000, $42,000 annual salary").
  * [x] `posting_id` (2026-11-01): fallback IDs are joined with " | " instead of a carriage return, which put line
    breaks inside the field; a stored fallback ID passed back in by the archive rebuild is re-derived from the fields,
    so weekly runs and rebuilds give one posting the same ID (rebuilds had been writing spaces). `k12jobanalysis.csv`
    and `combinedclean.csv` rewritten with the new IDs (no other column changed; weekly distinct-ID counts identical).
  * [x] Repeated rows in `k12jobanalysis.csv` (129) and `facultydata.csv` (170) documented in DATA_DICTIONARY.md and
    kept: older snapshots lost the field that told them apart, so they can be separate openings.
  * [x] Classification audit (2026-11-01, both states; shared classifier). Every archived title since Oct 2025 checked
    against its bucket. K-12: new "Activities/Extracurricular" bucket (FFA/FBLA/DECA/HOSA advisors, drama, the musical,
    speech & debate, robotics, color guard, student council; cheer stays Athletics); every substitute posting is
    Substitute (substitute paras were Paraprofessional); coach/kitchen/bus aides go to Athletics/Food Services/
    Transportation instead of Support Services; "Teacher Aide" is Paraprofessional; bare "Aid" no longer matches inside
    "Paid" and "Para" no longer matches "Paradise Valley"; janitors, LPNs, accountants, district IT staff and
    grade-band-only titles ("Junior High 7-8") leave Other; "None at this time" is a placeholder. HE: faculty
    applicant pools are Adjunct/Part-Time; postdocs and research scientists are Research & Science; custodians are
    Maintenance & Service; UW "Hourly Pooled - <job>" is classified by the job (student/intern jobs are Student
    Positions). Spells `is_support_staff` now includes Transportation and Food Services, as documented. Committed
    combinedclean (position, and Est_* on changed rows) and the teacher/faculty histories, aggregates and spells
    reclassified; raw archives unchanged. Tests: test-classification-misroutes.R (identical in both repos).
  * [ ] Add `Pay_*` to `hedata.xlsx` in `REQUIRED_SCHEMAS_XLSX` once the first weekly run writes it.
