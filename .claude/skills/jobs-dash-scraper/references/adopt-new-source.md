# Adopting a verified district or institution

You only get here once you've seen real current postings on the live page and know its platform (see `discovery.md`). Adoption touches several files that have to agree with each other. The state reference has the exact list for that repo, and the test suite enforces most of it.

## Steps (Automated Workflow)

1. **Probe the live URL**:
   ```bash
   Rscript scripts/probe_district.R "<url>"
   ```
   Confirms live postings exist and flags bot-gate status / platform before downloading.

2. **Capture fixture with automatic secret sanitization**:
   ```bash
   Rscript scripts/capture_fixture.R --url "<url>" --slug "<slug>"
   ```
   Saves to `tests/testthat/fixtures/<slug>_jobs.html` with all `AIza...` Google API tokens automatically redacted.

3. **Scaffold mechanical changes across 6 coupled files**:
   ```bash
   Rscript scripts/scaffold_district.R \
     --district "Full District Name" \
     --city "City" \
     --county "County Name" \
     --lat 48.1234 \
     --lon -114.5678 \
     --url "https://..." \
     --slug "<slug>"
   ```

4. **Implement parser selector & update test assertions**:
   - Write selector logic in `parse_<slug>_postings` inside `misc_district_scrapers.R`.
   - Update `tests/testthat/test-misc-district-scrapers.R` with expected titles from the fixture.

5. **Verify the batch**:
   ```bash
   Rscript scripts/verify_batch.R
   ```
   Runs all allowlist, salary setdiff, schema check, data integrity, deduplication, and unit tests, and guards against committing Claude autofix experiment files.

6. **Verify live scraper execution**:
   ```bash
   Rscript scripts/repro_scraper.R "<new registry name>"
   ```

7. **Commit per batch** of 2–4 districts.

## Apptegy (Montana)

Apptegy sites sit behind a Fastly "Client Challenge" that a plain request can't pass, so they ride the pipeline's shared headless-Chrome session (`fetch_apptegy_k12_postings()`). Olney-Bissell (2026-11-09, text blocks) and Savage (accordion panels) are the worked examples.

1. **Capture the rendered page** (headless Chrome, as the pipeline renders it; `AIza...` keys redacted; refuses a page without page-builder data):
   ```bash
   Rscript scripts/capture_fixture.R --apptegy --url "<employment page>" --slug "<slug>"
   ```
   Saves `tests/testthat/fixtures/apptegy_<slug>_pagedata_<today>.html` (about 1.2 MB). Look at the page's structure: `decode_apptegy_pagebuilder_nodes()` + `find_all_apptegy_nodes()` give the content tree. Postings are either the bold (`<strong>`) text of `CONTENT_NODE_TEXT` blocks (Olney-Bissell) or lines inside `CONTENT_NODE_PANEL` accordions headed e.g. "Certified Positions" (Savage).
2. **Scaffold** (`--cms apptegy` is picked automatically when that fixture exists):
   ```bash
   Rscript scripts/scaffold_district.R --district "<exact registry name>" --city "<town>" \
     --county "<County> County" --lat <lat> --lon <lon> --url "<employment page>" \
     --slug "<slug>" --leaid <NCES id> [--node text|panel] [--panel-match "Positions"]
   ```
   It writes the registry row (`Platform` `Apptegy`), allowlist host, DLI-unmapped test entry, the name in the Rmd's Apptegy `safe_scrape()` label, `parse_<slug>_postings()`/`fetch_<slug>_postings(chromote_session, url)` before the dispatcher plus the `APPTEGY_DISTRICT_SCRAPERS` entry, unit tests asserting the exact titles the parser returns on the fixture, a `fake_chromote_session()` fetch test, the dispatcher-test fixture/routing/counts, and the README/cookbook counts. It prints the titles: **check them against the live page**; zero titles or wrong ones mean the other `--node`, or a hand-written parser (then fix the tests' expected titles). It warns if an existing routing pattern in the dispatcher test already matches the URL. Rerunning is safe (each step skips what's there).
3. **Finish by hand:** the TODO comment above the parser (what the page holds, how confirmed), title filters if the page has bold notices the default filter misses, `DISTRICT_BACKLOG.md`, CLAUDE.md's district count, the DLI map if there is one, the repo's Copilot internet-access setting.
4. Tests (`test-misc-district-scrapers.R`, `test-salary-scrapers.R`, `test-copilot-allowlist.R`), then `scripts/repro_scraper.R "<District>"` for the live run.

## Sanity checks that have caught real mistakes

- The live count you report in the commit is what the scraper returned, not what you eyeballed.
- Titles look like jobs: no "Apply Now", "Employment Application", "Substitute Application" (unless that source counts subs), or nav text.
- `Location` is the district/town, not the state or a blank.
- `Link` is a per-posting URL when the site has one. If not, it's the board URL, and the posting-identity fallback handles it. Don't invent anchors.
