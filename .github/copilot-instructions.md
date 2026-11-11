# Copilot instructions — Wyoming Education Jobs Dashboard

R project: a weekly scrape pipeline (`Wy_ED_Jobs.Rmd`) feeding a Shiny dashboard (`Wy_Ed_Jobs/app.R`) of Wyoming K-12 and higher-ed job postings. There is no package structure; files are `source()`d. Your environment (R, packages, headless Chrome) is set up by `.github/workflows/copilot-setup-steps.yml`.

## Most tasks here: fixing one scraper

Issues labeled `scraper-autofix` are filed automatically by the weekly drift check when a source's live page has postings its scraper no longer finds. The issue body names the source, live URL, platform and scraper entry point. Its **What CI saw** section lists this source's existing fixtures (and other districts' fixtures for the same parser), the full page text the drift check rendered in CI, and, when this source has a text fixture, a diff of it against that text. Usual cause: the site's markup changed, and the diff usually shows exactly where.

0. **Reproduce first:** `Rscript scripts/repro_scraper.R "<source name>"` prints which `fetch_*` function the source uses and where it's configured, runs the tests for its `fetch_*`/`parse_*` functions, and runs it against the live site. Run it again after your fix.
1. **Capture a real fixture, in the form the parser consumes.** If the issue has a **Full page text captured in CI** section, save that text verbatim as the new dated `.txt` fixture. It's real captured data, taken outside your firewall, in the exact form an innerText scraper parses. Otherwise fetch the live page the same way the scraper does. Fixtures are named `<platform>_<district>...`: misc-district ones live in `tests/testthat/fixtures/misc_districts/` (e.g. `apptegy_niobrara_rendered.txt`, `edlio_weston1.html`), API ones in `tests/testthat/fixtures/` (e.g. `neogov_cwc.html`). Add the date to the new one's name. **Never hand-write or edit a fixture.** Tests in this repo run against real captured data only.
2. **Keep the old fixtures and their tests passing.** Sites flip between layouts, so the parser must handle both.
3. **Several districts share one parser.** `parse_apptegy_postings()` serves Niobrara 1, Platte 2, Sheridan 3 and Weston 7; the edlio, smartsites and educational_networks parsers are shared too. Every other district's fixture test must still pass, so don't special-case one district's markup in a way that changes another's output.
4. **Make the smallest change that works** in the existing `parse_*` function, in the file's existing style. Update the comment above the function to say what changed on the site and when.
5. **Add a regression test** in the matching `tests/testthat/test-*.R` file that asserts the exact titles in the new fixture.
6. **Run the full suite:** `Rscript -e 'testthat::test_dir("tests/testthat")'`. It must be green.
7. Reference the issue in the PR body with `Fixes #<n>`. CI uses that link to re-run the fixed scraper against the live site.

### Your sandbox is not the real site
You work behind a firewall; CI and the weekly scrape don't. Before blaming the site, rule out your sandbox:
- **The drift check already rendered this page in CI** with chromote and `document.body.innerText`. That's where the issue's title list came from. If your render shows none of those titles, your sandbox is the likelier culprit. Check the firewall's blocked-host warning. Asset CDNs (e.g. Apptegy's `apptegy.net`, `5il.co`) are needed for the page to hydrate.
- The firewall intercepts TLS, so headless Chrome may reject certificates. In your own session only, `chromote::set_chrome_args(c(chromote::get_chrome_args(), "--ignore-certificate-errors"))` before creating a session. `scripts/repro_scraper.R` does this for you. Never commit that flag.
- **Don't change how a scraper fetches the page** (e.g. innerText → parsing embedded JSON or `outerHTML`) to work around your sandbox. If you can't get a render that matches what the drift check saw, say so in the PR and stop. A human will capture the fixture.

### Keep the parser's existing scope
Only restore what the parser used to find. The issue's title list is what an LLM read off the whole page, so it can include things this source deliberately excludes, such as contact details or application-form links. Read the comment above the `parse_*` function and the existing tests, and follow that source's convention. If the page gained a genuinely new category, mention it in the PR rather than adding it.

The fixture tests only show that the parser handles the fixture. The PR's live check shows the scraper works against the real site, so before opening the PR, check that your fixture matches what the site serves.

### Stop and say so instead of forcing a fix when:
- The page genuinely lists no postings.
- The source moved to a different platform/ATS or URL. Changing the platform or URL is a registry change, which a human does.
- The site is returning errors (HTTP 429/5xx) rather than changed markup.

## Never touch
- `k12_district_registry.csv`, or `misc_district_registry` / `WSBA_ONLY_ORGS` in `misc_district_scrapers.R`
- Anything under `Wy_Ed_Jobs/` (accumulated data the dashboard ships), `Archivek12_Data/`, `Archived_HE_Data/`
- `.github/workflows/*`, or the `REQUIRED_SCHEMAS` in `schema_check.R`

## Useful context
- Where a source's scraper is configured: `k12_district_registry.csv` (Applitrack, TedK12, SchoolSpring, RedRoverK12 districts), `misc_district_registry` in `misc_district_scrapers.R` (districts' own pages, dispatched by `fetch_misc_district_postings()`), or a hardcoded call in `Wy_ED_Jobs.Rmd` (higher ed, the charter school). `resolve_scraper_call()` in `drift_check.R` maps a source name to its call.
- Scraper files: `direct_api_scrapers.R` (real ATS APIs) and `misc_district_scrapers.R` (heuristic HTML/chromote scrapers, plus the WSBA statewide vacancy feed).
- Column case varies: Applitrack and TedK12 return `title`, the others `Title`.
- HTTP goes through `perform_with_retry()` (from the shared `edjobs` package; don't redefine it here), not bare `req_perform()`.
