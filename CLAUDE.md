# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An R pipeline + Shiny dashboard that scrapes K-12 and higher-ed job postings across Wyoming weekly, layers in salary/vacancy-rate/socioeconomic context from federal and state sources, and republishes a live dashboard. Runs unattended via GitHub Actions every Friday. Originally built by Mark Perkins, substantially rewritten since.

**Shared code lives in the `edjobs` package** ([bostickmike/edjobs](https://github.com/bostickmike/edjobs); migration plan in the workspace's `claude-notes/2026-11-10_r-package-migration-plan.md`). This repo pins a tagged version in `EDJOBS_VERSION`; every workflow that runs the tests installs it (`weekly-scrape.yml`, and `.github/actions/setup-r-env` for the PR, Copilot and Claude auto-fix workflows). Locally: `pak::pak("bostickmike/edjobs@v0.0.1")` (the version in `EDJOBS_VERSION`), or `devtools::load_all("../edjobs")` while changing the package. `test-run-expectations.R` fails if the installed version isn't the pinned one. A function that has moved to the package is deleted here in the same change: a local copy would mask the package's. Bump `EDJOBS_VERSION` only after the package's tag passes its own check, and not within a day of this state's weekly run.

## Commands

Install deps (two separate sets — pipeline vs. dashboard):
```r
install.packages(c(
  "rmarkdown", "dplyr", "purrr", "readr", "readxl", "rvest",
  "stringr", "tidyverse", "writexl", "xml2", "jsonlite",
  "httr2", "lubridate", "chromote", "rsconnect", "testthat",
  "withr", "here", "pdftools",
  "shiny", "bslib", "shinyWidgets", "DT", "data.table",
  "leaflet", "plotly", "scales", "shinycssloaders"
))
```

Run the full test suite:
```r
testthat::test_dir("tests/testthat")
```
Run a single test file: `testthat::test_file("tests/testthat/test-k12-classification.R")`.

Run the dashboard against data already committed (no scraping):
```r
shiny::runApp("Wy_Ed_Jobs")
```

Re-run the full scrape/build pipeline (slow, hits every live source):
```r
rmarkdown::render("Wy_ED_Jobs.Rmd")
```
Needs a `CENSUS_API_KEY` env var (`.Renviron` locally) for the Census ACS chunk only — everything else runs without it.

Post-pipeline checks the CI workflow runs, in order (useful to run manually when debugging a bad pipeline run):
```r
Rscript .github/scripts/sanity_check.R      # blocking: refuses if either dataset lost >50% of rows
Rscript .github/scripts/verify_schema.R     # blocking: every column app.R expects is still present
Rscript .github/scripts/check_drift.R       # diagnostic only: per-source count vs. historical baseline
Rscript .github/scripts/check_salary_drift.R   # diagnostic: WSBA/IPEDS coverage vs. known-fixed universe
Rscript .github/scripts/check_census_drift.R   # diagnostic: ACS/SAIPE coverage + value plausibility
Rscript .github/scripts/check_pay_coverage.R   # diagnostic: sources whose posted pay went missing vs. their own history, plus implausible amounts blanked this run
Rscript .github/scripts/check_title_quality.R  # diagnostic: titles that aren't job titles (page text, summaries, pay Posted Pay lacks), and sources whose Other share jumped vs. their last 8 weeks
RUN_DATE=2026-11-13 Rscript .github/scripts/check_run_expectations.R  # diagnostic: this run's files against run_expectations.csv
```
`run_expectations.csv` holds one row per thing a person expects after a change lands (file, R filter, R measure, min/max, the run dates it applies to; logic in the `edjobs` package). Write expectations for the next run there rather than as a hand-read watch-list; `test-run-expectations.R` validates the file and the pinned `edjobs` version, and failures go in the drift issue. A `guard` job ahead of the scrape skips a *scheduled* run when main already has that day's `Automated weekly data refresh` commit (GitHub starts the cron hours late, after a manual dispatch has pushed); manual dispatches always run, and `concurrency` keeps two runs from overlapping.
`check_salary_drift.R` also flags the pay-estimate reference tables (OEWS, QCEW, IPEDS staff pay, UW salary book) whose refresh failed, was rejected by `write_reference_file()`, didn't run, or went stale -- see DATA_COOKBOOK.md, "Pay data-quality guards".

Disaster-recovery / verification rebuilds (reprocess the full raw archive from scratch, bypassing the incremental accumulator):
```r
source("scripts/rebuild_k12_history_from_archive.R"); rebuild_k12_history_from_archive()
source("scripts/rebuild_he_history_from_archive.R"); rebuild_he_history_from_archive()
```
The rebuild reproduces the committed files exactly (`Rscript .github/scripts/check_rebuild_parity.R`, also a non-blocking weekly step), because `build_k12_combinedclean()` / `build_k12_teacher_rows()` (`history_accumulator.R`) are shared with the pipeline. After changing `k12_he_classification.R`, reclassify the committed data with `Rscript scripts/reclassify_committed_data.R` (dry run; `--apply` writes): `test-weekly-data-quality.R` fails until every committed bucket equals what the classifier gives its title (and the posting spells' `Position`). Review a classifier change with `Rscript scripts/classifier_archive_diff.R` (add `--also ../<sibling repo>` for both states' archives): it lists every archived title whose bucket moves, and every line should be intended; the reclassify script prints it too. A bare word in a pattern matches inside other words ("Aide" in "Raiders", "Para" in "Paradise"): anchor with `\b`, and add a golden-set guard row.

One-off repair scripts follow a `--dry-run` then `--apply` pattern, e.g.:
```
Rscript scripts/repair_uw_pagination_duplicates.R --dry-run
Rscript scripts/repair_uw_pagination_duplicates.R --apply
```

## Architecture

**`Wy_ED_Jobs.Rmd` is the single pipeline entry point.** It's a linear sequence of knitr chunks: scrape every K-12 platform (AppliTrack, TedK12, SchoolSpring/RedRoverK12, misc-district registry, WSBA statewide feed) → combine/classify/dedupe → K-12 salary (WSBA PDFs) → K-12 staffing (CCD via Urban Institute) → Census ACS/SAIPE context → then the same shape again for Higher Ed (per-institution scrapers → combine/classify → IPEDS salary/enrollment → FSA Pell → Census ACS). Every scrape call is wrapped in `safe_scrape()` (`scrape_helpers.R`) so one source failing degrades to "missing from this week" instead of aborting the whole run, and logs to `scrape_log.csv`.

**Shared logic lives outside the Rmd** specifically so it can't drift between "what the notebook produces next run" and "what a from-archive rebuild produces now": `k12_he_classification.R` (title-encoding repair + position/subject/category classification, used by both K-12 and HE), `scrape_helpers.R` (`safe_scrape`/logging), `schema_check.R` (column-presence contract for every shipped file), `drift_check.R` (baseline-vs-current count comparison).

**`history_accumulator.R` is the incremental-append layer.** Most datasets in `Wy_Ed_Jobs/` are *accumulated* (grow by one week's rows per run, idempotent on same-day re-runs) rather than rebuilt from scratch every time — `append_weekly_rows()` fails loudly on a column mismatch rather than silently writing a jagged file. `combinedclean.csv`, `hedata.xlsx`, `allnow.csv`, `allnow_he.csv` are the exceptions: current-week-only snapshots, rebuilt fresh every run. `DATA_COOKBOOK.md` documents every shipped file's exact schema and semantics — treat `schema_check.R`'s `REQUIRED_SCHEMAS` as the enforced source of truth if the two ever disagree.

**Two independent durable archives are the disaster-recovery source of truth**: `Archivek12_Data/*.csv` and `Archived_HE_Data/*.xlsx`, one dated raw snapshot per week since August 2024, written every run regardless of the incremental path. `scripts/rebuild_*_history_from_archive.R` reprocess these from scratch and must stay equivalent to what `history_accumulator.R`'s incremental path produces — `tests/testthat/test-history-accumulator.R` is the test that enforces that equivalence.

**`Wy_Ed_Jobs/app.R`** is the deployed Shiny dashboard, and is deployed *standalone* to Posit Connect Cloud (only that subfolder is bundled, not the repo root) — it cannot `source()` anything at the repo root. Consequently it carries its own copies of the schema guard (`validate_and_pad_schema()` — missing columns become NA-backfilled plus a Home-tab banner, not a crash) and HE appointment classification, deliberately kept in sync with `schema_check.R` / `k12_he_classification.R` by hand rather than by import.

**Tests are built on real captured fixtures** (real scraped HTML, downloaded PDFs, API responses) rather than synthetic data — `tests/testthat/helper-setup.R` sources every scraper/classifier/repair module before any test file runs. `test-data-integrity.R` reads the actual committed summary CSVs (not fixtures) as a live data-contract check.

**CI (`.github/workflows/weekly-scrape.yml`)** runs the full test suite first (blocks on failure) → renders the pipeline → `sanity_check.R` and `verify_schema.R` (both blocking) → drift/salary-drift/census-drift/pay-coverage checks (`continue-on-error: true`, diagnostic-only, results filed as a GitHub Issue) → a meta-check that a diagnostic step *crashing* (vs. cleanly finding nothing) is still surfaced despite `continue-on-error` → regenerates and validates the Connect Cloud manifest → commits and pushes changed data files, which triggers the Connect Cloud redeploy.

## Git conventions and gotchas

- This repo is a fork of `MPerk78/Wyoming-Education-Jobs-Dashboard` (`git remote -v` shows both `origin` and `upstream`).
- The weekly CI run commits and pushes automatically with the message `Automated weekly data refresh (YYYY-MM-DD)`, touching only the specific `Wy_Ed_Jobs/*`, `Archived_HE_Data/*.xlsx`, and `Archivek12_Data/*.csv` paths listed explicitly in `weekly-scrape.yml`'s commit step — don't assume other generated files get picked up the same way.
- **Never commit `OneDrive_1_8-4-2026/`** — it's gitignored specifically because it contains a live OpenAI API key (`OpenAI.txt`) plus large RSelenium binaries; it's Mark's old working folder, kept locally for reference only.
- Root-level `all_direct_k12_job_listings.csv` (and the `.Rproj` file) are untracked local artifacts, not pipeline output — the real accumulated datasets live under `Wy_Ed_Jobs/`, not the repo root.
- K-12 direct-platform scraping (Applitrack/TedK12/SchoolSpring/RedRoverK12) is driven by `k12_district_registry.csv` (District/Platform/Job_Link/Org_ID, one row per district) — a single source of truth that replaced three separately-maintained link CSVs (`Frontline_job_links.csv`, `tedk12_job_links.csv`, `springer_job_links.csv`) which, in practice, had come to carry the same district under three different misspellings. `misc_district_registry` (in `misc_district_scrapers.R`) remains its own separate table for the 10 districts scraped heuristically from their own page rather than a structured platform, plus the charter schools.

## Working with scraped-title text

Sources mix UTF-8 and raw Windows-1252 bytes in job titles (e.g. en dashes in time ranges). `fix_title_encoding()` in `k12_he_classification.R` is the single point where this is repaired — applied once at ingestion in `Wy_ED_Jobs.Rmd`, not just inside the classifier functions — because a private copy fixed only for regex matching and then discarded still leaves the mangled bytes in what gets archived and displayed. Never re-add per-source encoding handling; route through this function.
