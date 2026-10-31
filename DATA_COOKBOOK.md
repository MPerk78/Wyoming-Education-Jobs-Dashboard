# Data Cookbook [SUPERSEDED]

> [!NOTE]
> **This internal document has been superseded by the official [DATA_DICTIONARY.md](DATA_DICTIONARY.md) and `data_dictionary.csv`.**
> For standardized variable definitions, federal identifiers (`NCES_LEAID`, `IPEDS_UNITID`), and empirical research documentation, please consult `DATA_DICTIONARY.md`.

Every dataset the pipeline (`Wy_ED_Jobs.Rmd`) produces and ships to `Wy_Ed_Jobs/`, where `Wy_Ed_Jobs/app.R` reads it. All of these except `combinedclean.csv`, `hedata.xlsx`, `allnow.csv`, and `allnow_he.csv` are **accumulated, growing datasets** — each weekly run appends that week's newly classified rows onto what's already there (see `history_accumulator.R`), rather than rebuilding from scratch. `schema_check.R`'s `REQUIRED_SCHEMAS` is the enforced, tested version of the column lists below — if this document and that list ever disagree, trust the code and open an issue.

---

## K-12

### `combinedclean.csv` — current K-12 postings (this week only)

One row per open K-12 posting, all position types (not just teachers). Powers the K-12 Jobs Table and the map's current-openings counts. Rebuilt fresh every run — **not** an accumulated file.

| Column | Type | Notes |
|---|---|---|
| `title` | text | Job title as posted. Passed through `fix_title_encoding()` at ingestion, so a Windows-1252 byte from a non-Applitrack source won't display as mojibake. |
| `Archive_Date` | date | The date this snapshot was scraped. |
| `date_posted` | text | Original posted-date text, format varies by source platform. |
| `position` | text | Coarse bucket from `classify_k12_position()` (Teacher, Support Services, Administration, etc. — see `k12_he_classification.R` for the full list). |
| `location` | text | School/site name or district office, as scraped — format varies a lot by source platform. |
| `url` | text | Direct posting URL where the source exposes one (including SchoolSpring and RedRover); otherwise the district job-board URL. |
| `posting_id` | text | Durable source posting ID when available. Sources without one use a stable fallback fingerprint of their URL, district, title, location, and posted date; `Archive_Date` is intentionally excluded so the same posting retains its identity across weeks. |
| `District` | text | Canonical district name (`canonicalize_k12_district()` applied). |

**Sources combined into this file**: AppliTrack, TedK12, SchoolSpring, RedRoverK12, direct district pages for the miscellaneous-district registry, and the complete WSBA statewide vacancies feed. WSBA rows are removed only when their normalized district, title, and posted date exactly match a direct-platform row; this preserves WSBA listings that direct boards do not expose.

**Byte-identical rows are dropped** (`combined %>% distinct()`, before `classify_k12_position()` overwrites the raw `position` column) — some direct-platform pages list one opening once per building it applies to, and the modern scraper emits an identical row for each. Rows distinguished by any field, including a per-building `position` value in older SchoolSpring snapshots, are kept. This is why the per-district row count here equals that week's `k12_district_weekly_totals.csv` `n` exactly.

**Pay columns**: `Pay_Min`/`Pay_Max`/`Pay_Unit`/`Pay_Text` are the pay the posting itself states (`extract_posted_pay()`), NA where it states none. `Est_Pay`/`Est_Pay_Unit`/`Est_Occupation`/`Est_Area`/`Est_Year` are the BLS OEWS regional median for the posting's role (`attach_oews_estimates()` in `bls_oews_scraper.R`, from `oews_role_wages.csv`), the app's labeled fallback ("Typical pay (BLS est.)") where no pay is posted. The area comes from the district's county; rows with no county use the statewide figure.

### `oews_role_wages.csv` — BLS OEWS regional pay by role (annual)

One row per OEWS area x occupation: the state, each metro area and each nonmetropolitan area (Casper, Cheyenne; Western and Eastern Wyoming), for the 17 occupations K-12 postings are matched to (`OEWS_OCCUPATIONS` in `bls_oews_scraper.R`). Fetched from the BLS Public Data API, which serves only the latest year; the pipeline checks for a newer year each run (one API query) and rewrites the file only when BLS publishes one. A failed check keeps the existing file. `BLS_API_KEY` raises the API limits but isn't required.

| Column | Type | Notes |
|---|---|---|
| `Area_Code` | text | OEWS area code (keep as text: metro codes have leading zeros, e.g. `0013740`). |
| `Area_Name` | text | |
| `SOC` | text | 2018 SOC code without the dash, e.g. `259045`. |
| `Occupation` | text | |
| `Annual_Median` | number | Annual median wage. `NA` where BLS suppresses the figure for that area. |
| `Hourly_Median` | number | Hourly median, fetched only for roles usually paid hourly; `NA` where BLS publishes none (school-year roles such as teaching assistants). |
| `Year` | text | Reference year (May). |

### `qcew_county_school_pay.csv` — average pay of public school employees by county (annual)

BLS Quarterly Census of Employment and Wages, NAICS 6111 (elementary and secondary schools), local-government ownership (public school districts): one row per county with a published figure -- only the counties BLS releases -- 3 of Wyoming's 23 for 2025 (Campbell, Fremont, Sweetwater); the rest are withheld for confidentiality. All roles together, so it's county context in the map popup ("... public school employees averaged $X in 2025"), not a figure for any posting. Fetched each run from QCEW's open-data CSV files (latest year with annual averages); a failed fetch keeps the existing file.

| Column | Type | Notes |
|---|---|---|
| `County` | text | "Yellowstone County" (joined to `salarymap2.csv`'s `County` with any ", State" suffix dropped). |
| `County_FIPS` | text | Five-digit county FIPS. |
| `School_Employment` | number | Annual average employment. |
| `Avg_Annual_Pay` | number | Annual average pay per employee. |
| `Year` | text | |

### `k12jobanalysis.csv` — full history, Teacher postings only

Row-level history of every Teacher-position posting ever scraped, one row per posting per week it appeared. Powers "New This Week," the teacher-vacancy-rate numerator, and every teacher trend chart. **Accumulated** — grows by one week's rows per run.

| Column | Type | Notes |
|---|---|---|
| `title` | text | |
| `Archive_Date` | date | |
| `position` | text | Always `"Teacher"` in this file (pre-filtered). |
| `location` | text | |
| `url` | text | |
| `posting_id` | text | Durable posting identity, used for category counts so distinct same-title/location openings are not collapsed. |
| `District` | text | Canonical district name. |
| `Category` | text | Fine-grained subject from `classify_k12_subject()` (e.g. "Elementary Education", "Special Education - General"). |
| `Broad_Category` | text | Coarser grouping from `classify_k12_broad_category()`; this is what the app's category color palettes and pickers use. |

### `allsum.csv` — longitudinal category counts, by district and statewide

One row per (`Broad_Category`, `Archive_Date`, `District`) combination, **plus** a `District == "Total"` row per (`Broad_Category`, `Archive_Date`) that's the sum of that week's district rows — computed as an explicit sum of the parts, not an independent count, so `Total` can never silently drift out of sync with what it's supposed to add up to (`check_total_matches_parts()` asserts this before every weekly append). Powers the Longitudinal Teacher Trends chart. **Accumulated.**

| Column | Type | Notes |
|---|---|---|
| `Broad_Category` | text | |
| `Archive_Date` | date | |
| `District` | text | A real district name, or `"Total"`. |
| `sum` | integer | Distinct `posting_id` values that week. This preserves separate openings with identical title/location text when the source exposes distinct IDs. |

### `allnow.csv` — current-week category counts

Same shape as `allsum.csv` minus the `Archive_Date` column (it's always just the latest week). Not accumulated — recomputed fresh every run since there's no history to preserve here.

| Column | Type | Notes |
|---|---|---|
| `Broad_Category` | text | |
| `Sum` | integer | Note the capital S — inconsistent with `allsum.csv`'s lowercase `sum`, a pre-existing quirk kept as-is rather than a breaking rename. |
| `District` | text | A real district name, or `"Total"`. |

### `k12_district_weekly_totals.csv` — all-category weekly totals per district

One row per (`District`, `Archive_Date`), counting **every** position type, not just Teacher — this is what powers the sparkline next to each district's raw count on the Home tab's "Top K-12 Hiring Districts" table, so it needs to match that same all-category number. **Accumulated.**

| Column | Type | Notes |
|---|---|---|
| `District` | text | |
| `Archive_Date` | date | |
| `n` | integer | All postings that district had that week, any position type. A raw row count of that week's de-duplicated `combinedclean` rows (byte-identical duplicates already dropped — see `combinedclean.csv` above), so it equals that week's distinct `combinedclean` rows for the district exactly. `scripts/repair_combinedclean_row_duplicates.R` retro-fixed the pre-fix inflation; its header notes a residual pre-2026 SchoolSpring under-count still open for a later pass. |

### `salarymap2.csv` — one row per district, static reference + refreshed figures

Not an archive — one current row per of Wyoming's 48 school districts. `District`/`County`/`Latitude`/`Longitude`/`Link`/`Job_Link` are hand-maintained; everything else is refreshed from its live source every run (and left untouched if that run's fetch fails, rather than being blanked out).

| Column | Type | Source | Notes |
|---|---|---|---|
| `District` | text | hand-maintained | Canonical name; the join key everything else in the app keys off. |
| `County` | text | hand-maintained | |
| `Latitude` / `Longitude` | numeric | hand-maintained | Used for the map marker. |
| `Link` | text | hand-maintained | District's general website (not currently surfaced in the app). |
| `Job_Link` | text | hand-maintained | District's job-board URL, shown in the map popup as "Careers page." |
| `Teacher_Base_Salary` / `Teacher_Base_Salary_Prior_Year` | numeric | WSBA (weekly refresh) | Current + prior year base salary from the same PDF scrape. |
| `Teacher_Avg_Salary` / `Teacher_Avg_Base_Salary` | numeric | Wyoming Dept of Education (WDE602) | Average total compensation / average base salary for instructional teachers from the WDE602 School District Staff Member Collection report (`dist_k12_staff_fte_and_avg_salary_by_assign_cat`). |
| `WDE_Salary_Year` / `WDE_Salary_Source` | text | Wyoming Dept of Education | e.g. `"2025-26"` and `"WDE602 Staff Member Collection (edu.wyoming.gov)"`. |
| `Salary_Year` | text | WSBA | e.g. `"2025-2026"` — the real freshness signal (there's no separate "last updated" timestamp, since the dollar figures only change once a year regardless of scrape cadence). |
| `Superintendent_Salary` / `Superintendent_Contract_Days` | numeric | WSBA | |
| `Salary_Source` | text | WSBA | Literal string `"WSBA (wsba-wy.org)"`, for the summary table's footnote link. |
| `Teachers_Total_FTE` | numeric | NCES CCD via Urban Institute (weekly refresh) | Vacancy-rate denominator. |
| `Enrollment` | numeric | NCES CCD via Urban Institute (weekly refresh) | Total student enrollment. Powers `Students_Per_Teacher` (`Enrollment ÷ Teachers_Total_FTE`), computed in `app.R`, not stored here. |
| `CCD_Year` / `CCD_Source` | text | NCES CCD | |
| `Data_Coverage` | text | `misc_district_coverage_tiers()` (weekly refresh) | `"Full"` for districts on a real job-board platform; `"Partial (WSBA + own page)"` for the 10 `misc_district_registry` districts, whose postings come from WSBA's statewide feed (confirmed ~25-40% complete on its own) plus a heuristically-scraped district page; `"Partial (WSBA only)"` for standalone orgs in `WSBA_ONLY_ORGS` (Snowy Range Academy, Northeast Wyoming BOCES, STRIDE Learning Center, Sweetwater County CDC, Wyoming Boys School) whose postings originate exclusively from WSBA. Shown as a badge on the map and a column on the K-12 Summary table — see `misc_district_scrapers.R`'s header for the full reasoning. |
| `Median_Household_Income` / `Median_Gross_Rent` | numeric | Census ACS 5-Year (`census_acs_scraper.R`, weekly refresh) | County-level, not district-level — joined on `County`, so sibling districts in the same county share identical values. Annual dollar figures / monthly rent respectively. |
| `Mining_Employment_Share` | numeric | Census ACS 5-Year Subject Tables | County's civilian workforce share (0–1) employed in mining/oil & gas — the one figure here that helps explain *why* teacher pay varies across WY districts (energy counties pay more to compete with these wages), not just what things cost. |
| `Population_Change_Pct` | numeric | Census ACS 5-Year | County population change vs. the same ACS product 5 years earlier — a slow-moving growth/decline signal, refreshed at the same "once a year is plenty" cadence as salary data despite the pipeline running weekly. |
| `ACS_Year` | integer | Census ACS 5-Year | The ACS 5-Year vintage's end year (e.g. `2024` means the 2020–2024 5-year estimate) — the real freshness signal for the four columns above, same convention as `Salary_Year`/`CCD_Year`. |
| `Child_Poverty_Rate` | numeric | Census SAIPE (`census_saipe_scraper.R`, weekly refresh) | **District-level**, unlike the five ACS columns above — the closest free proxy to free/reduced-lunch eligibility at the actual district a posting is in (SAIPE doesn't publish free/reduced-lunch counts directly). Two districts in the same county can and do differ meaningfully here (confirmed with real data: Fremont County SD2 and SD21 sit at 27–28% while Fremont County SD1 sits at 6%, despite sharing the exact same county-level income figure). Fremont County School District 38 (Arapahoe Charter High School) is a confirmed real gap in SAIPE's own coverage. |
| `SAIPE_Year` | integer | Census SAIPE | The real freshness signal for `Child_Poverty_Rate`, same convention as `ACS_Year`. |

### `k12_salary_history.csv` — multi-year K-12 salary archive

WSBA publishes no historical archive of its own (checked directly, confirmed no public archive exists) — this is this project's own accumulating record, one snapshot appended per new `Salary_Year` WSBA publishes (not one row per weekly run; `needs_k12_salary_archive_update()` guards against duplicate-year appends). Not currently read by `app.R` — it exists to grow into a real multi-year trend as more `Salary_Year`s accumulate, the same way `salarymap.csv`'s `Faculty_Avg_Salary_Y1Ago`/`Y2Ago` already work on the HE side via IPEDS's deeper history.

| Column | Type | Notes |
|---|---|---|
| `District` | text | |
| `Salary_Year` | text | |
| `Teacher_Base_Salary` | numeric | |
| `Superintendent_Salary` | numeric | |

---

## Higher Ed

### `hedata.xlsx` — current HE postings (this week only)

One row per open posting across all 9 public HE institutions, every job type (not just faculty). Rebuilt fresh every run — **not** accumulated (the accumulated raw history lives in `Archived_HE_Data/*.xlsx`, one file per week, which `facultydata.csv` etc. are derived from incrementally).

| Column | Type | Notes |
|---|---|---|
| `Title` | text | |
| `Location` | text | |
| `Posted_Date` | text | Original posted-date text, format varies by source platform. |
| `Institution` | text | Canonical institution name (`canonicalize_he_institution()` applied). |
| `Link` | text | Direct link to the posting. |
| `Pay_Min` | number | Lowest pay the posting itself states, in `Pay_Unit`; never converted between units. `NA` when no figure is stated, and for every source except the NEOGOV colleges (LCCC, Casper, Central Wyoming, Gillette), read from each listing card's pay line, or from its description preview when the pay line is blank. NEOGOV's "$0.00" placeholder counts as no figure. |
| `Pay_Max` | number | Highest stated amount (equals `Pay_Min` for a single figure). |
| `Pay_Unit` | text | `hour`, `day`, `month`, `year`, `credit hour`, or `stipend`; `NA` when the stated unit is implausible for the amount (e.g. Gillette's $900 "Hourly" adjunct rate). |
| `Pay_Text` | text | The board's own pay wording the figure was read from, e.g. "Full-Time - $61,356.00 - $76,695.00 Annually". |
| `Est_Pay` / `Est_Pay_Unit` / `Est_Occupation` / `Est_Area` / `Est_Year` | number / text | For non-faculty postings: the institution's IPEDS average pay for full-time staff in the posting's category (`attach_ipeds_staff_estimates()` in `ipeds_staff_salaries.R`, from `ipeds_staff_salaries.csv`), per year; `Est_Occupation` is the category, `Est_Area` the institution. The app's labeled fallback ("Typical pay (IPEDS est.)") where no pay is posted. Not in the raw archive (added after it is written) or in `facultydata.csv`. |
| `Archive_Date` | date | |

### `ipeds_staff_salaries.csv` — IPEDS pay for full-time non-faculty staff, by category (annual)

One row per institution x IPEDS occupational category with at least 3 full-time staff (smaller groups are too noisy to show): the Urban Institute Education Data Portal's `college-university/ipeds/salaries-noninstructional-staff` endpoint, latest available year (2024 as of 2026-10-31; 8 institutions). Refreshed by the HE salary chunk each run and read by the next run's HE munge step; a failed fetch keeps the existing file.

| Column | Type | Notes |
|---|---|---|
| `IPEDS_UNITID` | integer | Joins to `salarymap.csv`'s `IPEDS_UNITID`. |
| `Staff_Category` | integer | Urban's `staff_category` code (2 research ... 14 production and transportation). |
| `Category_Label` | text | Shortened label shown in the app's tooltip. |
| `Staff_Count` | number | Full-time non-instructional staff in the category. |
| `Avg_Salary` | number | Salary outlays / staff count, rounded. |
| `Year` | text | IPEDS year. |

### `facultydata.csv` — full history, faculty + adjunct postings only

Row-level history of every `"Instructor/Teacher/Faculty"` and `"Adjunct/Part-Time Faculty"` posting ever scraped. Powers "New This Week," the faculty-vacancy-rate numerator, and every faculty trend chart, including the full-time/adjunct split on Current Faculty Trends. **Accumulated.**

| Column | Type | Notes |
|---|---|---|
| `Title` | text | |
| `Location` | text | |
| `Posted_Date` | text | |
| `Institution` | text | Canonical institution name. |
| `Link` | text | |
| `Archive_Date` | date | |
| `Job_Type` | text | `"Instructor/Teacher/Faculty"` or `"Adjunct/Part-Time Faculty"` from `classify_he_job_type()` — kept separate so a standing adjunct pool posting isn't counted the same as full-time faculty hiring. |
| `Category` | text | Subject area from `classify_he_faculty_category()`. |

### `allsum_he.csv` — longitudinal category counts, by institution and statewide

Same shape/logic as K-12's `allsum.csv`, plus a `Job_Type` dimension. One row per (`Category`, `Archive_Date`, `Institution`, `Job_Type`), plus `Institution == "Total"` rows summing across institutions for that (`Category`, `Archive_Date`, `Job_Type`). **Accumulated.**

| Column | Type | Notes |
|---|---|---|
| `Category` | text | |
| `Archive_Date` | date | |
| `Institution` | text | A real institution name, or `"Total"`. |
| `Job_Type` | text | |
| `sum` | integer | Posting count for that combination. |

### `allnow_he.csv` — current-week category counts

Same shape as `allsum_he.csv` minus `Archive_Date`. Not accumulated.

| Column | Type | Notes |
|---|---|---|
| `Category` | text | |
| `Job_Type` | text | |
| `Sum` | integer | Capital S, same pre-existing inconsistency as K-12's `allnow.csv`. |
| `Institution` | text | A real institution name, or `"Total"`. |

### `he_institution_weekly_totals.csv` — all-category weekly totals per institution

One row per (`Institution`, `Archive_Date`), all job types. Same purpose as K-12's `k12_district_weekly_totals.csv`. **Accumulated.**

| Column | Type | Notes |
|---|---|---|
| `Institution` | text | |
| `Archive_Date` | date | |
| `n` | integer | |

### `he_job_duration_spells.csv` — collegiate faculty vacancy duration spells

Longitudinal spell panel tracking vacancy duration (time-to-fill) across weekly snapshots for higher education institutions. Derived from `facultydata.csv` via `scripts/generate_he_job_duration_spells.R`.

| Column | Type | Notes |
|---|---|---|
| `Institution` | text | Canonical collegiate institution name matching `salarymap.csv`. |
| `IPEDS_UNITID` | integer | Official 6-digit federal NCES IPEDS Unit ID. |
| `Title` | text | Standardized collegiate job vacancy title. |
| `Category` | text | Academic discipline or functional assignment area. |
| `Job_Type` | text | Appointment type (`Instructor/Teacher/Faculty` or `Adjunct/Part-Time Faculty`). |
| `First_Seen_Date` | text | Earliest weekly snapshot date (YYYY-MM-DD) where posting was active. |
| `Last_Seen_Date` | text | Latest weekly snapshot date (YYYY-MM-DD) where posting was active. |
| `Weeks_Observed` | integer | Total weekly snapshots the posting appeared. |
| `Estimated_Days_Open` | integer | Estimated calendar duration open (`as.Date(Last_Seen_Date) - as.Date(First_Seen_Date) + 7`). |
| `Status` | text | `"Still Active"` or `"Closed / Filled"`. |
| `is_stem` | integer | 1 if STEM discipline; 0 otherwise. |
| `is_health` | integer | 1 if Health Sciences or Nursing; 0 otherwise. |
| `is_cte` | integer | 1 if Career & Technical Education; 0 otherwise. |
| `is_adjunct` | integer | 1 if Adjunct / Part-Time Faculty; 0 otherwise. |
| `is_faculty` | integer | 1 if Regular / Full-Time Instructional Faculty; 0 otherwise. |

### `salarymap.csv` — one row per institution, static reference + refreshed figures

One current row per Wyoming's 9 public HE institutions. `Name`/`Longitude`/`Latitude`/`Link` are hand-maintained; the salary/staffing figures refresh from IPEDS every run (walking backward from the current year until a non-empty response is found, since IPEDS publishes with a 1-2 year lag).

| Column | Type | Source | Notes |
|---|---|---|---|
| `Name` | text | hand-maintained | Canonical name; the join key. |
| `Longitude` / `Latitude` | numeric | hand-maintained | |
| `Link` | text | hand-maintained | Careers page, shown in the map popup. |
| `Faculty_Avg_Salary` / `Faculty_Avg_Salary_Professor` | numeric | IPEDS via Urban Institute | "All ranks combined" and Professor-rank average salary. |
| `Faculty_Count` | numeric | IPEDS | Vacancy-rate denominator (full-time instructional staff only, matching `Job_Type == "Instructor/Teacher/Faculty"`'s scope). |
| `Salary_Year` | text | IPEDS | The calendar year IPEDS surveyed as of (Nov 1 of that year), not an academic year. |
| `Salary_Note` | text | IPEDS | Non-`NA` only for Sheridan/Gillette, explaining they're still one IPEDS-reported entity — see `Data_Coverage`'s HE analogue below. |
| `Salary_Source` | text | IPEDS | |
| `Faculty_Avg_Salary_Y1Ago` / `Y2Ago` | numeric | IPEDS | Real multi-year trend (unlike K-12's WSBA data, IPEDS is queryable by year indefinitely). |
| `Enrollment` / `Enrollment_Year` | numeric / text | IPEDS via Urban Institute (`ipeds_enrollment_scraper.R`, weekly refresh) | Added 2026-08-06. Fall enrollment (FTE), IPEDS's own `fall-enrollment` survey component (a different component from the instructional-staff salary survey above) -- summed across every `level_of_study` (undergraduate/graduate/first-professional/postbaccalaureate) an institution reports. `Students_Per_Teacher` (`app.R`'s HE analogue of the K-12 District Summary table's same-named column) is *not* stored here -- computed on the fly as `Enrollment / Faculty_Count`, mirroring how K-12's own `Students_Per_Teacher` is computed from `Enrollment / Teachers_Total_FTE` rather than stored. |
| `Enrollment_Change_Pct` | numeric | IPEDS via Urban Institute (`ipeds_enrollment_scraper.R`, weekly refresh) | Added 2026-08-06. Institution-level 5-year enrollment trend (this year's `Enrollment` vs. the same figure 5 years earlier) -- the HE analogue of `salarymap2.csv`'s county-level `Population_Change_Pct`, but a more direct job-security signal since it's the actual institution, not just its county. Confirmed with real data: 8 of 9 WY institutions shrank 1-23% over 2018-2023, Central Wyoming College grew +2.4% -- a real mix, not a uniform trend. |
| `Pell_Recipient_Share` / `Pell_Year` | numeric / text | US Dept. of Education, Federal Student Aid, via Urban Institute (`fsa_pell_scraper.R`, weekly refresh) | Added 2026-08-06. Real, unduplicated Pell Grant recipient count for that institution and year (`grant_type == 4` on the FSA `grants` endpoint) divided by `Enrollment` from the SAME year (not the latest available `Enrollment`, since FSA's data lags IPEDS's by roughly 2 years -- confirmed live: FSA tops out at 2021, IPEDS fall-enrollment goes through 2023) -- the closest HE analogue to `Child_Poverty_Rate` below, since SAIPE itself has no HE equivalent. An honest headcount-of-recipients-over-FTE-enrollment ratio, not two directly comparable counts -- called out in `app.R`'s footnote, not silently presented as exact. |
| `County` | text | hand-maintained | Added 2026-08-06. Each of Wyoming's 9 public HE institutions is a single campus with one real home county — same hand-maintained pattern as `Latitude`/`Longitude`, and the join key for the row below. No two HE institutions share a county (unlike K-12's 48 districts across 23 counties), so there's no sibling-sharing case here. |
| `Median_Household_Income` / `Median_Gross_Rent` / `Mining_Employment_Share` / `Population_Change_Pct` / `ACS_Year` | numeric | Census ACS 5-Year (`census_acs_scraper.R`, weekly refresh) | Added 2026-08-06 — the exact same `fetch_census_county_context()` pipeline `salarymap2.csv` already used for K-12, fetched independently and joined on the `County` column above. Same meaning as `salarymap2.csv`'s equivalent columns (see that entry above). No `Child_Poverty_Rate`/`SAIPE_Year` equivalent here — SAIPE is a K-12 school-district-specific federal program (child poverty) with no natural HE analogue. |

**Sheridan College / Gillette College**: both institutions currently share one IPEDS-reported `Faculty_Count` and `Faculty_Avg_Salary` (they're legally one entity, Northern Wyoming Community College District, mid-split into two separate colleges). `app.R` gives both a shared *joint* vacancy rate (combined postings ÷ the one shared `Faculty_Count`) rather than showing neither — flagged via a `Vacancy_Rate_Shared` column computed in `app.R`, not stored in this file. `check_salary_drift.R` watches IPEDS's institution directory every run for a new, separate unitid; when the split becomes official there, updating `IPEDS_UNITID_MAP` in `ipeds_salary_scraper.R` is the only change needed to restore independent per-campus figures.

---

## Notes on the data model

- **Accumulation, not rebuilding.** Every "full history" file above (`k12jobanalysis.csv`, `allsum.csv`, `k12_district_weekly_totals.csv`, `facultydata.csv`, `allsum_he.csv`, `he_institution_weekly_totals.csv`) is appended to weekly, not regenerated from the raw archive from scratch — see `history_accumulator.R`. The raw weekly snapshots (`Archivek12_Data/*.csv`, `Archived_HE_Data/*.xlsx`) are still written every run as the durable source of truth; `scripts/rebuild_*_history_from_archive.R` rebuild any of the accumulated files from that raw archive from scratch, for disaster recovery or to verify the incremental path hasn't drifted.
- **Missingness is real, not a data quality bug.** `NA` in a salary/staffing column means that source's fetch failed or the row wasn't yet matched — not that the underlying district/institution has no data. Both K-12 and HE staffing/salary fetches leave the relevant file untouched on a failed run rather than blanking out last week's good values.
- **Classification lives in one place.** `classify_k12_position()`, `classify_k12_subject()`, `classify_k12_broad_category()`, `classify_he_job_type()`, `classify_he_faculty_category()`, and the `canonicalize_*` functions all live in `k12_he_classification.R`, sourced everywhere they're needed — the current-week and accumulated-history code paths can't silently classify the same title two different ways.
- **Schema is enforced, not just documented.** `schema_check.R`'s `REQUIRED_SCHEMAS` is checked against every shipped file before each weekly commit (`.github/scripts/verify_schema.R`) and again at `app.R`'s own startup (`validate_and_pad_schema()`, which also shows a Home-tab banner naming the exact file/column if something's ever missing).
- **One source needs its own credential.** Everything else in this pipeline is a public, keyless API or a plain public web page. `census_acs_scraper.R` is the exception — the Census Data API requires a free `CENSUS_API_KEY` (a GitHub Actions secret in CI, `.Renviron` locally); missing or invalid, the affected chunk (both the K-12 and the HE county-context chunks reuse this file) fails loudly and leaves that file's county-context columns untouched, the same "leave it alone, don't blank it out" behavior every other salary/staffing chunk already has on a failed fetch.
- **Every source is watched for drift, not just job postings.** `.github/scripts/check_census_drift.R` runs the same coverage + value-plausibility checks `check_salary_drift.R` already does for WSBA/IPEDS salary data, but for the Census ACS/SAIPE columns above — a known-fixed-universe coverage assertion (23 WY counties, 48 districts, 9 HE institutions) plus generous sanity bounds on the dollar/ratio figures themselves. Diagnostic-only (`continue-on-error` in the weekly workflow), same as every other monitoring step.
