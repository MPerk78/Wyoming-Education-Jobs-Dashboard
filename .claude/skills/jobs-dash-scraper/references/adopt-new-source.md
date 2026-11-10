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

## Apptegy (Montana; no scaffold mode yet)

Apptegy sites sit behind a Fastly "Client Challenge" that a plain request can't pass, so they ride the pipeline's shared headless-Chrome session (`fetch_apptegy_k12_postings()`). Olney-Bissell (2026-11-09) is the worked example:

1. **Read the rendered page** with chromote, the way the pipeline does (`Page$navigate`, wait, `document.documentElement.outerHTML`). Find the posting's structural marker. Apptegy pages carry their content as page-builder JSON in the HTML; `decode_apptegy_pagebuilder_nodes()` + `find_all_apptegy_nodes(..., type == "CONTENT_NODE_TEXT")` give each text block's HTML. Olney-Bissell's titles are the block's `<strong>` text; Savage's are accordion panels (`CONTENT_NODE_PANEL`).
2. **Fixture:** the outerHTML, dated (`apptegy_<slug>_pagedata_YYYY-MM-DD.html`), with any `AIza...` key redacted. About 1.2 MB, like Savage's.
3. **Code:** `parse_<slug>_postings(html_text, url)` and `fetch_<slug>_postings(chromote_session, url)` in `misc_district_scrapers.R`, and an entry in `APPTEGY_DISTRICT_SCRAPERS` keyed by the exact registry `District`. Registry `Platform` is `Apptegy`. Add the district's name to the Apptegy `safe_scrape()` label in `Mt_ED_Jobs.Rmd` (only its prefix is matched).
4. **Tests:** unit tests on the fixture (exact titles; no instructions or "Application" links; zero rows on a page without page-builder data), a `fetch_*` test with `fake_chromote_session()`, and the district added to the big `fetch_apptegy_k12_postings` dispatcher test (fixture, URL match, total count, per-district count).
5. Then the usual: allowlist, DLI, counts, backlog; `scripts/repro_scraper.R "<District>"` for the live run.

## Sanity checks that have caught real mistakes

- The live count you report in the commit is what the scraper returned, not what you eyeballed.
- Titles look like jobs: no "Apply Now", "Employment Application", "Substitute Application" (unless that source counts subs), or nav text.
- `Location` is the district/town, not the state or a blank.
- `Link` is a per-posting URL when the site has one. If not, it's the board URL, and the posting-identity fallback handles it. Don't invent anchors.
