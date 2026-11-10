# Montana (`bostickmike/Montana-Education-Jobs-Dashboard`)

Pipeline `Mt_ED_Jobs.Rmd` → dashboard `Mt_Ed_Jobs/app.R`. Weekly CI runs on Tuesdays. Counts as of 2026-11-09: **201 K-12 districts** of about 398 nominal (many are Elem+HS pairs sharing one board), and **23 HE institutions**. Check the current numbers with `wc -l k12_district_registry.csv he_institution_registry.csv` (subtract the header).

## Registries
- `k12_district_registry.csv`: `District, City, County, Latitude, Longitude, Platform, Slug, Job_Link`. `Platform` is a shared platform name (`AppliTrack`, `SchoolSpring`, `TedK12`, `TylerPortico`, `Apptegy`, `RedRoverK12`) or a per-district `<Name>Heuristic`.
- `he_institution_registry.csv`: `Institution, County, Latitude, Longitude, Platform, Feed_URL, Job_Link`.
- `llm_extract_targets.csv`: the quarantined LLM shadow pilot for prose-only pages (`LLM_EXTRACT.md`). Not in the registry and not on the dashboard.

## How a registry row reaches its scraper
`drift_check.R` → `SCRAPER_CALL_SPECS` + `resolve_scraper_call()`:
- Shared platforms map to one `fetch_*` with registry columns as arguments (e.g. `AppliTrack` → `fetch_applitrack_postings(Slug)`).
- A K-12 `<Name>Heuristic` platform resolves **by convention** to `fetch_<lowercase name>_postings(Job_Link)`, so a new heuristic needs a function of exactly that name. (`CutBankHeuristic` → `fetch_cutbank_postings`.)
- K-12 `Apptegy` / `RedRoverK12` dispatch through `APPTEGY_DISTRICT_SCRAPERS` in `misc_district_scrapers.R`, keyed by the exact `District` name, so a new Apptegy district needs an entry there.
- HE heuristics are listed explicitly in `SCRAPER_CALL_SPECS`, so add one there.
- `test-drift-check.R` asserts every function named in the specs exists.

Scraper files: `direct_api_scrapers.R` (real ATS APIs, K-12 + HE), `misc_district_scrapers.R`, `misc_college_scrapers.R`. Also check how `Mt_ED_Jobs.Rmd` calls a platform (some heuristics are called individually in their own chunk, and chromote districts share one session).

## Statewide feed and master list
- **OPI** (`apps.opi.mt.gov/mtjobsforteachers/…`) is the redundancy/statewide feed. OPI-only rows reach the Jobs Table but not the Map/District Summary.
- **Master target list:** OPI's `ddlDistrict` dropdown (602 entries; codes < 4000 = 387 real public LEAs, about 307 unique towns after pairing Elem+HS). Saved copies are in the user's Claude memory dir (`opi_raw_district_dropdown_2026-08-24.csv`, `opi_full_master_target_list.csv`). To refresh, re-scrape the dropdown.
- **`DISTRICT_BACKLOG.md`** (repo root) is the working checklist of every uncovered district, grouped by problem: 1 LLM-suitable prose · 2 recheck later · 3 need correct MT URL · 4 multi-page · 5 OCR · 6 behind login/Drive · 7 stale/broken · 8 likely dead ends · 9 out of scope · 10 not yet investigated. Update its Status column for every district you touch.

## Files that must change together when adopting a K-12 district
1. `k12_district_registry.csv` row (county as "X County"). **The NCES id and coordinates come from CCD, not a search result:** the id is the district's LEAID (check name *and* county; CCD has repeated names such as two "Cottonwood Elem"), and the lat/long is its school's CCD location (`schools/ccd/directory`), not the town center. Six dots were 11-40 km off and one id pointed at another county's district before `tools/reference_audit.R` (toolkit) existed; run it after adding a row.
2. Scraper wiring: a new heuristic fn by convention, or an `APPTEGY_DISTRICT_SCRAPERS` entry, or nothing for AppliTrack/SchoolSpring/TedK12/Portico. Plus the `Mt_ED_Jobs.Rmd` call if that platform isn't registry-looped.
3. Fixture + test in `tests/testthat/test-misc-district-scrapers.R` (heuristics/Apptegy) or `test-direct-api-scrapers.R`.
4. `.github/copilot-allowlist.txt`: add the district's domain (`test-copilot-allowlist.R` fails otherwise). While the Copilot path is still enabled, tell the user to paste it into repo Settings → Copilot → Internet access too.
5. DLI teacher pay: look the district up in DLI's regional PDFs (`fetch_dli_region_pdf_text(<region>)` in `salary_scrapers.R`; nine regions, search every one, since names repeat across regions: Gallatin's and Hill's "Cottonwood Elementary" are in 4Rivers and HiLine). Found: add an `MT_DLI_DISTRICT_MAP` entry (pay "ND" or a count "<5" parse to NA, a real gap). Not found: add the district to `test-salary-scrapers.R`'s `setdiff(registry$District, names(MT_DLI_DISTRICT_MAP))` list.
6. Documented counts: grep for the old count (`grep -rn "128" README.md CLAUDE.md DATA_COOKBOOK.md`) and update each mention.
7. `DISTRICT_BACKLOG.md` status.

Context data needs no hand mapping since 2026-11-09: CCD staffing, SAIPE child poverty, OPI finance and the free/reduced lunch share come from `mt_district_crosswalk.csv` by the registry's NCES id (plus the town's high-school district by the crosswalk's rules), and `salarymap2.csv` gets the row on the next run. The hand maps (`MT_CCD_LEA_MAP`, `MT_SAIPE_DISTRICT_MAP`, `MT_OPI_FINANCE_LEA_MAP`) still win for the districts in them; add an entry only when the crosswalk ties the wrong districts.

**Apptegy districts** (the most common platform): the scaffold script has no Apptegy mode yet. Follow `references/adopt-new-source.md` § Apptegy.

**Postings from employers that aren't public districts** (co-ops, tribal and private schools, a hospital school) reach the map through `other_employers.csv` (repo root), not the registry: one row per posting name with its NCES record (CCD, CCD's BIE directory, or the Private School Universe Survey). A new one is listed in the weekly drift issue ("Employers with postings but no map dot").

## HE
All 23 in-scope institutions were verified 2026-08-24. Two defunct schools and six trade schools are deliberately out of scope. Platforms: PeopleAdmin (Atom feed), SharesBoard (shared board, no single-source check), JazzHR, Paycom, NEOGOV, ADP, isolved, plus per-college heuristics in `misc_college_scrapers.R`.

## Known site quirks (don't "fix" these)
Rocky Mountain College returns HTTP 429 (site-side). Custer's apex domain has no A record, so `www.` is pinned. Roberts gets a retry on Cloudflare 5xx. Butte, Belgrade, and Hamilton migrated off AppliTrack in 2026-09: when a district goes quiet, check `scrape_log.csv` for a dead tenant before touching a parser.
