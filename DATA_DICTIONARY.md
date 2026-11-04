# Wyoming Education Jobs Dashboard — Data Dictionary & Codebook

Comprehensive variable codebook, schema definitions, and methodological provenance for datasets compiled and published by the **Wyoming Education Jobs Dashboard**.

Each dataset is accessible via the **Data Sources & Open Research Downloads** tab in the interactive web dashboard or by downloading machine-readable CSVs directly from this repository.

---

## 1. Datasets Overview

| Dataset Layer | Primary File | Description | Granularity | Refresh Cadence |
| :--- | :--- | :--- | :--- | :--- |
| **Active K-12 Jobs** | `combinedclean.csv` | All active public school job vacancies currently open across school districts. | Posting level | Weekly |
| **K-12 Historical Archive** | `k12jobanalysis.csv` | Multi-year longitudinal archive of all weekly K-12 job postings with standardized categorizations. | Posting level | Weekly |
| **K-12 Job Duration Spells** | `job_duration_spells.csv` | Longitudinal panel tracking vacancy duration (time-to-fill) and shortage dummy flags. | Posting spell | Weekly |
| **District Profiles & Benchmarks** | `salarymap2.csv` | District-level staffing, compensation benchmarks, and Census context. | District level | Weekly / Annual |
| **WDE602 District Staffing Matrix** | `wde602_district_staff_salaries.csv` | Staff headcount, FTE, and average base/total compensation across 30+ assignment categories. | District × Role | Annual |
| **Higher Ed Historical Postings** | `facultydata.csv` | Multi-year longitudinal postings for instructional faculty, adjuncts, and staff. | Posting level | Weekly |
| **Higher Ed Job Duration Spells** | `he_job_duration_spells.csv` | Longitudinal spell panel tracking collegiate faculty search duration (time-to-fill) and discipline flags. | Posting spell | Weekly |
| **Higher Ed Institutional Profiles** | `salarymap.csv` | College and university profiles: IPEDS instructional faculty salaries, enrollment, and Pell recipient shares. | Institution level | Weekly / Annual |
| **K-12 Weekly Vacancy Time Series** | `k12_district_weekly_totals.csv` | Weekly aggregate open job vacancy counts by school district over time. | District × Week | Weekly |
| **Higher Ed Weekly Vacancy Time Series** | `he_institution_weekly_totals.csv` | Weekly aggregate open job vacancy counts by higher education institution. | Institution × Week | Weekly |
| **Data Dictionary & Codebook** | `data_dictionary.csv` | Machine-readable column definitions, types, and primary source authorities across all files. | Variable level | Static / Maintained |

---

## 2. Universal Identifiers

To support seamless merges with federal, state, and academic datasets, the following universal identifiers are embedded:

- **`NCES_LEAID`**: Official 7-digit federal National Center for Education Statistics Local Education Agency identifier. Enables 1:1 joining with the **Stanford Educational Data Archive (SEDA)**, **Civil Rights Data Collection (CRDC)**, and **Census F-33 School District Finance Survey**.
- **`IPEDS_UNITID`**: Official 6-digit federal Integrated Postsecondary Education Data System identifier.
- **`District_ID`**: 6-digit Wyoming Department of Education state district identification code.

---

## 3. Data Dictionary: Detailed Variable Definitions

### A. Active K-12 Jobs (`combinedclean.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `title` | String | District Job Board | Official job vacancy title as published by the school district or charter school. |
| `Archive_Date` | Date (YYYY-MM-DD) | Project Scraper | Weekly snapshot observation date on which the vacancy was captured as actively open. |
| `date_posted` | String / Date | District Job Board | Original posted date string as published on the district applicant tracking board. |
| `position` | Categorical | Classification Model | Broad staff position group (Teacher, Administrator, Support Services, etc.). |
| `location` | String | District Job Board | Specific school building, school campus, facility, or district administrative office. |
| `url` | URL | District Job Board | Direct link to the job posting or district applicant tracking portal. |
| `posting_id` | String | Project Pipeline | Durable unique posting identifier used to track openings longitudinally across weekly snapshots. |
| `District` | String | Canonical Registry | Canonical public school district or charter school name. |
| `Close_Date` | String | District ATS | The posting's closing date as its board states it, unparsed (a date, or "When Filled", "Open Until Filled" and similar). AppliTrack rows only so far (from 2026-11-03); NA where the board gives none and for other sources. |
| `Pay_Min` | Numeric | Job posting | Lowest pay amount stated in the posting itself, in `Pay_Unit` (NA when no dollar figure is stated). Never converted between units. |
| `Pay_Max` | Numeric | Job posting | Highest stated amount (equals `Pay_Min` for a single figure). |
| `Pay_Unit` | String | Job posting | `hour`, `day`, `month`, `year`, `credit hour`, or `stipend` (season/activity amount); NA when undeterminable. |
| `Pay_Text` | String | Job posting | The posting text the pay was read from, for verification (for RedRover, the board's own pay line, e.g. "$13.50 - $14.65 per hour"; "per unit" there means a stipend). |
| `Est_Pay` | Numeric | BLS OEWS | Where the posting states no pay of its own: the Bureau of Labor Statistics' median pay for the posting's role in its region (see `Est_Occupation`, `Est_Area`), in `Est_Pay_Unit`. A regional estimate across all employers of that occupation, not this job's offer. NA for roles with no fair match (coaches, unclassified postings). |
| `Est_Pay_Unit` | String | BLS OEWS | `hour` for roles usually paid hourly where BLS publishes an hourly median; otherwise `year`. |
| `Est_Occupation` | String | BLS OEWS | The OEWS occupation the posting was matched to from its position and title (e.g. "Teaching assistants", "School bus drivers"). |
| `Est_Area` | String | BLS OEWS | The OEWS area the figure is for: the metro or nonmetro area holding the district's county, or the state where the county is unknown or BLS suppresses the local figure. |
| `Est_Year` | String | BLS OEWS | OEWS reference year (May of that year). |

---

### B. K-12 Historical Archive (`k12jobanalysis.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `title` | String | District Job Board | Standardized job vacancy title as published by the school district or charter school. |
| `Archive_Date` | Date (YYYY-MM-DD) | Project Scraper | Weekly snapshot observation date on which the vacancy was captured as open. |
| `position` | Categorical | Classification Model | Broad staff position group (Teacher, Administrator, Support Services, etc.). |
| `location` | String | District Job Board | Specific school building, facility, or district administrative office location. |
| `url` | URL | District Job Board | Direct URL link to the official job vacancy listing or district portal. |
| `posting_id` | String | Project Pipeline | Durable unique posting identifier tracking vacancies across multi-year weekly snapshots. The source's own posting ID where it has one (e.g. `schoolspring:laramie1.schoolspring.com:5857967`, `applitrack:tcsd:836` from 2026-11-03); otherwise a fallback fingerprint, `fallback \| <district> \| <url> \| <title> \| <location> \| <posted date>`, lower-cased. |
| `District` | String | Canonical Registry | Canonical public school district or charter school Local Education Agency (LEA) name. |
| `Category` | String | Classification Model | Fine-grained subject specialization or departmental role assignment. |
| `Broad_Category` | Categorical | Classification Model | Clustered high-level category grouping (Elementary, STEM, Special Education, Humanities, etc.). |

**Repeated rows.** 129 rows are exact repeats of another row in the same week (78 of them in the first week, 2024-08-09; the other 51 February-July 2026, all SchoolSpring postings in Albany, Campbell and Laramie County School Districts 1). Older snapshots dropped the building that told these postings apart (e.g. one coach per elementary school), so they can be separate openings and are kept. Counts use distinct `posting_id` within a week, so each set counts once; for those SchoolSpring sets that is an under-count. Weeks since August 2026 have none (`test-weekly-data-quality.R` checks the newest week).

**ID format change.** Before November 2026 the fallback fingerprint's parts were joined with a carriage return, which showed up as line breaks inside the field, and weeks rebuilt from the raw archive used spaces instead. Every week was regenerated in the current form, so a posting's ID is consistent across the whole file.

---

### C. K-12 Job Duration Spells (`job_duration_spells.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `District` | String | Canonical Registry | Canonical public school district or charter school name. |
| `NCES_LEAID` | String (7-digit) | NCES CCD | Official federal National Center for Education Statistics Local Education Agency 7-digit identifier. |
| `Title` | String | District Job Board | Standardized job vacancy title. |
| `Category` | String | Classification Model | Fine-grained subject area or departmental functional assignment. |
| `Broad_Category` | Categorical | Classification Model | High-level clustered role grouping (Elementary, STEM, Special Education, etc.). |
| `Position` | Categorical | Classification Model | Broad staff position group (Teacher, Administrator, Support Services). |
| `First_Seen_Date` | Date (YYYY-MM-DD) | Project Scraper | Earliest weekly scraper run date in which this job vacancy was observed active. |
| `Last_Seen_Date` | Date (YYYY-MM-DD) | Project Scraper | Most recent weekly scraper run date in which this job vacancy was observed active. |
| `Weeks_Observed` | Integer | Project Derived | Number of distinct calendar weeks (ISO weeks) in which this posting was observed during the spell. A posting absent for more than 21 days that reappears under the same title starts a new spell. |
| `Estimated_Days_Open` | Integer | Project Derived | Estimated calendar duration of the vacancy spell (proxy for time-to-fill and labor shortage persistence). |
| `Status` | Categorical | Project Derived | Vacancy spell state: Still Active or Closed / Filled as of latest weekly pipeline sweep. |
| `is_stem` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Science, Technology, Engineering, or Mathematics; 0 otherwise. |
| `is_sped` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Special Education; 0 otherwise. |
| `is_elementary` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Elementary Education; 0 otherwise. |
| `is_secondary` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Middle School or High School; 0 otherwise. |
| `is_admin` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Administration, Principal, or Superintendent; 0 otherwise. |
| `is_support_staff` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Paraprofessional, Aide, Custodial, Bus, or Food Service; 0 otherwise. |

---

### D. District Profiles & Context (`salarymap2.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `District` | String | Canonical Registry | Canonical public school district or charter school name. |
| `NCES_LEAID` | String (7-digit) | NCES CCD | Official federal NCES Local Education Agency 7-digit identifier. |
| `County` | String | US Census Bureau | County in which the district central administrative headquarters is located. |
| `Latitude` | Decimal Degrees | US Census / Geocoding | Geographic latitude coordinate of district central headquarters. |
| `Longitude` | Decimal Degrees | US Census / Geocoding | Geographic longitude coordinate of district central headquarters. |
| `Link` | URL | District Registry | Official website home page URL for the school district. |
| `Job_Link` | URL | District Registry | Official employment, careers, or applicant tracking portal link for the district. |
| `Teacher_Base_Salary` | Currency ($) | WSBA Salary Survey | Published starting base salary for a certified teacher with a Bachelors degree and Step 1 experience. |
| `Teacher_Base_Salary_Prior_Year` | Currency ($) | WSBA Salary Survey | Published starting base salary for BA Step 1 in the preceding academic school year. |
| `Salary_Year` | String | WSBA Salary Survey | Academic school year corresponding to the published WSBA base salary survey. |
| `Superintendent_Salary` | Currency ($) | WSBA Salary Survey | Contracted annual salary of the district superintendent. |
| `Superintendent_Contract_Days` | Integer | WSBA Salary Survey | Contracted annual working days for the district superintendent. |
| `Salary_Source` | String | WSBA Salary Survey | Publishing authority and survey title for district teacher base salaries. |
| `Data_Coverage` | Categorical | Project Methodology | Scraper coverage fidelity tier: Full (direct ATS integration) vs Partial (heuristic site or state feed scrape). |
| `Teachers_Total_FTE` | Numeric | NCES CCD | Total full-time-equivalent certified classroom teachers employed by the district. |
| `Enrollment` | Integer | NCES CCD | Total pre-K through grade 12 student fall enrollment headcount. |
| `CCD_Year` | Integer | NCES CCD | Reporting school year for federal Common Core of Data staffing and enrollment counts. |
| `CCD_Source` | String | NCES CCD | Federal source citation for NCES CCD public school district universe data. |
| `Median_Household_Income` | Currency ($) | US Census ACS 5-Year | County median annual household income in inflation-adjusted dollars. |
| `Median_Gross_Rent` | Currency ($) | US Census ACS 5-Year | County median monthly gross rent including estimated utilities. |
| `Population` | Integer | US Census ACS 5-Year | Total estimated resident population of the county. |
| `ACS_Year` | Integer | US Census ACS 5-Year | Data release year for US Census American Community Survey 5-year estimates. |
| `Mining_Employment_Share` | Proportion (0-1) | US Census ACS 5-Year | Proportion of county civilian employed workforce in agriculture, forestry, mining, and energy extraction. |
| `Population_Change_Pct` | Proportion | US Census ACS 5-Year | County 5-year percentage change in total resident population. |
| `Child_Poverty_Rate` | Proportion (0-1) | US Census SAIPE | Estimated proportion of school-age children (ages 5-17) living in families below the federal poverty line. |
| `SAIPE_Year` | Integer | US Census SAIPE | Release year of federal Small Area Income and Poverty Estimates (SAIPE) data. |
| `Teacher_Avg_Salary` | Currency ($) | WDE602 Staff Collection | Actual average total compensation paid across all employed certified classroom teachers in the district. |
| `Teacher_Avg_Base_Salary` | Currency ($) | WDE602 Staff Collection | Actual average base contract salary paid across all employed certified classroom teachers in the district. |
| `WDE_Salary_Year` | String | WDE602 Staff Collection | Academic school year corresponding to the WDE602 certified staff collection. |
| `WDE_Salary_Source` | String | WDE602 Staff Collection | Publishing state agency authority for Wyoming Department of Education staff collections. |

---

### E. WDE602 Staff Salary Collection (`wde602_district_staff_salaries.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `School_Year` | String | Wyoming Dept of Education | Official state reporting academic school year for the certified and classified staff collection. |
| `Data_Scope` | String | Wyoming Dept of Education | Scope classification of staff reporting (District-level collection). |
| `District_ID` | Integer | Wyoming Dept of Education | Official 6-digit Wyoming Department of Education state district identification number. |
| `District_Name` | String | Wyoming Dept of Education | Official district name as recorded in WDE state reporting collections. |
| `District` | String | Canonical Registry | Standardized canonical district name harmonized with project job boards and benchmarks. |
| `NCES_LEAID` | String (7-digit) | NCES CCD | Official federal NCES Local Education Agency 7-digit identifier. |
| `Group_Scope` | String | Wyoming Dept of Education | Staff assignment classification scope (All Staff, Certified, Classified). |
| `Group_Name` | String | Wyoming Dept of Education | Specific staff assignment code group (Teachers, Principals, Aides, Bus Drivers, Custodians, Food Service). |
| `Male_FTE` | Numeric | Wyoming Dept of Education | Total full-time-equivalent male employees in this staff assignment group. |
| `Female_FTE` | Numeric | Wyoming Dept of Education | Total full-time-equivalent female employees in this staff assignment group. |
| `Total_FTE` | Numeric | Wyoming Dept of Education | Total full-time-equivalent staff employed in that specific assignment group. |
| `Average_Base_Salary` | Currency ($) | Wyoming Dept of Education | Average contracted base salary for staff in that assignment group. |
| `Average_Total_Salary` | Currency ($) | Wyoming Dept of Education | Average total compensation including stipends and benefits for staff in that assignment group. |

---

### F. Higher Ed Historical Archive (`facultydata.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `Title` | String | Institutional Job Board | Published collegiate job vacancy title as advertised on the college or university careers portal. |
| `Location` | String | Institutional Job Board | Campus location, city, or remote work designation. |
| `Posted_Date` | String / Date | Institutional Job Board | Original date of vacancy posting as published by the college or university. |
| `Institution` | String | IPEDS Directory | Canonical name of the public college or university. |
| `Link` | URL | Institutional Job Board | Direct URL link to the official higher education employment vacancy announcement. |
| `Archive_Date` | Date (YYYY-MM-DD) | Project Scraper | Weekly snapshot observation date on which the vacancy was captured as open. |
| `Job_Type` | Categorical | Classification Model | Collegiate appointment category: Instructor/Teacher/Faculty, Adjunct/Part-Time Faculty, or Staff. |
| `Category` | String | Classification Model | Academic discipline, instructional field, or administrative functional area. |

**Repeated rows.** 170 rows are exact repeats of another row in the same week (August 2024 to January 2026, 1-5 per week, mostly Sheridan College's "Part-time Faculty Pool" listings and University of Wyoming faculty titles). Those snapshots link to the listing page rather than the posting, so two identical rows can be two real openings with the same title; they are kept, and counted as separate postings. Weeks since January 2026 have none.

---

### G. Higher Ed Job Duration Spells (`he_job_duration_spells.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `Institution` | String | IPEDS Directory | Canonical name of the public college or university. |
| `IPEDS_UNITID` | String (6-digit) | NCES IPEDS | Official 6-digit federal NCES Integrated Postsecondary Education Data System Unit ID. |
| `Title` | String | Institutional Job Board | Standardized collegiate job vacancy title. |
| `Category` | String | Classification Model | Academic discipline or collegiate functional assignment area. |
| `Job_Type` | Categorical | Classification Model | Collegiate appointment classification: Instructor/Teacher/Faculty or Adjunct/Part-Time Faculty. |
| `First_Seen_Date` | Date (YYYY-MM-DD) | Project Scraper | Earliest weekly scraper run date in which this collegiate job posting was observed active. |
| `Last_Seen_Date` | Date (YYYY-MM-DD) | Project Scraper | Most recent weekly scraper run date in which this collegiate job posting was observed active. |
| `Weeks_Observed` | Integer | Project Derived | Number of distinct calendar weeks (ISO weeks) in which this posting was observed during the spell. A posting absent for more than 21 days that reappears under the same title starts a new spell. |
| `Estimated_Days_Open` | Integer | Project Derived | Estimated calendar duration of the collegiate search (proxy for search length and time-to-fill). |
| `Status` | Categorical | Project Derived | Search status: Still Active or Closed / Filled as of the latest pipeline sweep. |
| `is_stem` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Science, Technology, Engineering, or Mathematics; 0 otherwise. |
| `is_health` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Nursing, Health Sciences, or Medical; 0 otherwise. |
| `is_cte` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Career & Technical Education or Trades; 0 otherwise. |
| `is_adjunct` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Part-Time or Adjunct Faculty; 0 otherwise. |
| `is_faculty` | Binary (0/1) | Shortage Taxonomy | Binary indicator: 1 if position is Full-Time Instructional Faculty; 0 otherwise. |

---

### H. Higher Ed Institutional Profiles (`salarymap.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `Name` | String | IPEDS Directory | Official legal institution name of the public college or university. |
| `IPEDS_UNITID` | String (6-digit) | NCES IPEDS | Official 6-digit federal NCES Integrated Postsecondary Education Data System Unit ID. |
| `Longitude` | Decimal Degrees | IPEDS Directory | Geographic longitude coordinate of main campus centroid. |
| `Latitude` | Decimal Degrees | IPEDS Directory | Geographic latitude coordinate of main campus centroid. |
| `Link` | URL | Institutional Directory | Official careers or employment opportunities portal URL for the institution. |
| `County` | String | US Census Bureau | County in which the collegiate campus is located. |
| `Faculty_Avg_Salary` | Currency ($) | NCES IPEDS | Average full-time instructional faculty salary across all academic ranks combined. |
| `Faculty_Avg_Salary_Professor` | Currency ($) | NCES IPEDS | Average full-time instructional faculty salary for the Full Professor academic rank. |
| `Faculty_Count` | Integer | NCES IPEDS | Total count of full-time instructional staff employed by the institution. |
| `Salary_Year` | Integer | NCES IPEDS | Academic reporting year corresponding to the IPEDS faculty compensation survey. |
| `Salary_Note` | String | Project Methodology | Methodological reporting note (e.g., joint IPEDS reporting for multi-campus community college districts). |
| `Salary_Source` | String | NCES IPEDS | Data provenance and primary survey source citation for instructional faculty pay. |
| `Faculty_Avg_Salary_Y1Ago` | Currency ($) | NCES IPEDS | Average full-time instructional faculty salary in the prior academic year (t-1). |
| `Faculty_Avg_Salary_Y2Ago` | Currency ($) | NCES IPEDS | Average full-time instructional faculty salary two academic years prior (t-2). |
| `Enrollment` | Integer | NCES IPEDS | Total unduplicated fall student headcount enrollment. |
| `Enrollment_Year` | Integer | NCES IPEDS | Academic reporting year for IPEDS fall enrollment figures. |
| `Enrollment_Change_Pct` | Proportion | NCES IPEDS | 5-year percentage change in total fall student enrollment. |
| `Pell_Recipient_Share` | Proportion (0-1) | US Dept of Education FSA | Share of undergraduate degree-seeking students awarded federal need-based Pell Grants. |
| `Pell_Year` | Integer | US Dept of Education FSA | Award year for Federal Student Aid (FSA) Pell Grant recipient figures. |
| `Median_Household_Income` | Currency ($) | US Census ACS 5-Year | County median annual household income in inflation-adjusted dollars. |
| `Median_Gross_Rent` | Currency ($) | US Census ACS 5-Year | County median monthly gross rent including estimated utilities. |
| `Population` | Integer | US Census ACS 5-Year | Total estimated resident population of the county in which the campus is located. |
| `ACS_Year` | Integer | US Census ACS 5-Year | Data release year for US Census American Community Survey 5-year estimates. |
| `Mining_Employment_Share` | Proportion (0-1) | US Census ACS 5-Year | Proportion of county civilian employed workforce in agriculture, forestry, mining, and energy extraction. |
| `Population_Change_Pct` | Proportion | US Census ACS 5-Year | County 5-year percentage change in total resident population. |

---

### I. K-12 Weekly Vacancy Time Series (`k12_district_weekly_totals.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `District` | String | Canonical Registry | Canonical public school district or charter school name. |
| `Archive_Date` | Date (YYYY-MM-DD) | Project Scraper | Snapshot observation date for that weekly pipeline sweep. |
| `n` | Integer | Project Scraper | Total count of active job postings observed open for that district in that weekly snapshot. |

---

### J. Higher Ed Weekly Vacancy Time Series (`he_institution_weekly_totals.csv`)

| Column Name | Data Type | Source Authority | Description |
| :--- | :--- | :--- | :--- |
| `Institution` | String | IPEDS Directory | Canonical public college or university name. |
| `Archive_Date` | Date (YYYY-MM-DD) | Project Scraper | Snapshot observation date for that weekly pipeline sweep. |
| `n` | Integer | Project Scraper | Total count of active job postings observed open for that higher education institution in that weekly snapshot. |

---
