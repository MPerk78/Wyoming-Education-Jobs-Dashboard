# Wyoming (`bostickmike/Wyoming-Education-Jobs-Dashboard`)

Pipeline `Wy_ED_Jobs.Rmd` → dashboard `Wy_Ed_Jobs/app.R`. Weekly CI runs on Fridays. Wyoming has a small, fixed universe: **48 public school districts**, plus a few charters and **9 public HE institutions** (UW + the community colleges).

## Two K-12 registries (unlike Montana)
- `k12_district_registry.csv`: `District, Platform, Job_Link, Org_ID`, for structured platforms only (`Applitrack` [note the spelling], `SchoolSpring`, `RedRoverK12`, `TedK12`). No lat/long here.
- `misc_district_registry`: an R `data.frame` **inside `misc_district_scrapers.R`** (`District`, `platform`, `url`). It holds districts and charters scraped from their own sites, keyed by a lowercase **platform-level** parser: `wordpress`, `apptegy`, `smartsites`, `schoolblocks`, `edlio`, `googlesites`, `educational_networks`, `prairieview`. `fetch_misc_district_postings(platform, url, session)` dispatches. Wyoming prefers reusable per-CMS parsers over per-district heuristics. Follow that: a new district on Edlio is a one-line registry addition.
- HE scrapers are called directly from the `.Rmd` (UW, PeopleAdmin atom feeds, NEOGOV/schooljobs.com, Paylocity, simplehire, Oracle Cloud, Dawson-style heuristics). There's no HE registry CSV.

`edjobs::resolve_scraper_call(name)`, reading `state_config.R`'s `scraper_sources`, resolves a source name across both registries and the hardcoded `.Rmd` calls. `scripts/repro_scraper.R "<name>"` uses it.

## Statewide feed
**WSBA vacancies** (`wsba-wy.org/vacancies`, `fetch_wsba_vacancies()` in `misc_district_scrapers.R`) covers districts without their own structured board, and is de-duplicated against direct postings (`dedupe_against_wsba`, `remove_wsba_direct_duplicates`). Districts that appear on WSBA but have no direct scraper are the natural discovery list.

## Discovery list
There's no backlog file. Build one from the 48-district universe (WDE directory, WSBA's district names via `canonicalize_wsba_district()`) minus both registries. Coverage is already high, so the remaining wins are mostly districts that only post through WSBA, and charter schools. If you start tracking declines, propose a `DISTRICT_BACKLOG.md` in the same format as Montana's, rather than leaving them in chat.

## Files that change together when adopting
1. Either a `k12_district_registry.csv` row or a `misc_district_registry` entry (all three vectors in the data.frame stay aligned, so append to each).
2. A parser only when the CMS is new to Wyoming. Add it as a platform-level `fetch_<cms>_postings` / `parse_<cms>_postings` pair, plus a branch in `fetch_misc_district_postings()`.
3. Fixture + tests (`test-misc-district-platform-extractors.R` for platform parsers, `test-misc-district-orchestration.R` for registry/dispatch).
4. `.github/copilot-allowlist.txt` + `test-copilot-allowlist.R` (same contract as Montana).
5. `misc_district_coverage_tiers()` / Data_Coverage disclosure, if the district's coverage tier changes.
6. Counts in `README.md` / `CLAUDE.md` / `DATA_COOKBOOK.md`.

## Quirks
Sheridan/Gillette had a district split that has its own test (`test-sheridan-gillette-split.R`). UW's paginated feed has had duplicate-row bugs (`repair_uw_pagination_duplicates.R`). Repairs follow `--dry-run` then `--apply`.
