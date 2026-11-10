# Fixing a broken scraper

A source "breaks" in one of four ways, and only one of them is a parser fix. Classify first, because the fix for each is different and three of them aren't code at all.

| What you see | Likely cause | Who fixes it |
|---|---|---|
| Live page shows postings, scraper returns 0 or few | **Markup changed** | You: parser fix + new fixture |
| HTTP 404, parked domain, "we've moved to…", new ATS link on the page | **Platform migration** | Registry swap (locally); a human in CI |
| HTTP 429 / 5xx / Cloudflare / DNS failure | **Site-side outage** | Nobody, wait (maybe add a retry/host pin if it's chronic) |
| Live page genuinely lists nothing | **Actually empty** | Nobody |

## 1. Find the evidence

- `scrape_log.csv` (repo root) shows per-source status and counts for recent runs. A district that "went quiet" usually shows up here first as an HTTP error or a count drop.
- For a `scraper-autofix` issue, the body has a **What CI saw** section: the page text the drift check rendered in CI, the existing fixtures, and a diff of the newest text fixture against what CI saw. That diff usually points straight at the markup change.
- Reproduce with the repo's one-command repro:
  ```bash
  Rscript scripts/repro_scraper.R "<exact registry District/Institution name>"
  ```
  It prints the resolved `fetch_*`/`parse_*` function, runs that source's fixture tests, runs the scraper live, and for innerText scrapers saves the rendered text (`--save-text PATH`) in exactly the form a fixture takes.
- If repro_scraper can't resolve the source (Wyoming `misc_district_registry` rows, statewide feeds), find the scraper by grepping the registry name / platform in the scraper files and the `.Rmd`, then call `fetch_*` directly.

## 2. Check for a platform migration before touching the parser

Run `scripts/fingerprint_platform.py` on the district's homepage and its employment page. If the district now links to a different ATS (AppliTrack → Tyler Portico / SchoolSpring / Red Rover is the common pattern), the fix is a **registry change** (Platform, Slug/Org_ID, Job_Link) with no parser change. Check that the existing platform scraper returns real postings for the new slug before you commit anything. A dead tenant often still answers 200 with an empty board, so don't take a 200 as proof it works.

## 3. Parser fix procedure

1. **Capture a real fixture first.** Prefer the CI-rendered text from the issue if there is one (it was taken outside any sandbox). Otherwise fetch it the way the scraper does. Save it under `tests/testthat/fixtures/` with a dated name, next to the old one.
2. **Write the failing test**: in the matching `tests/testthat/test-*.R`, assert the exact titles (and count) that are really on the new fixture. Read them off the fixture yourself. A test that asserts "> 0 rows" can't catch a parser grabbing nav links.
3. **Make the smallest change** to the existing `parse_*` function, in the file's style, that makes the new test pass while the old fixture's tests stay green. Update the comment above the function: what changed on the site, and when.
4. **Re-run** `scripts/repro_scraper.R` live, then the full suite.

## 4. Sandbox vs. site

When your render of the page shows none of the titles CI saw, suspect your environment before the site: blocked asset CDNs (Apptegy needs `*.apptegy.net` and `5il.co` to hydrate), TLS interception, or a headless-Chrome flag. **Don't change *how* a scraper fetches the page** (innerText → embedded JSON / outerHTML) to work around your own environment. That's how the first Copilot auto-fix (Lone Rock, issue #7) went wrong. If you can't get a render matching what CI saw, say so and stop.

## 5. Chronic site-side flakiness

If a source fails intermittently for an identifiable reason, a narrow, commented fix is fine. Precedents: Custer's apex domain has no A record so the `www.` host is pinned, and Roberts gets a retry on Cloudflare 5xx. A blanket retry or timeout increase is not. Rocky Mountain College's 429s are known site-side and deliberately left alone.

## 6. A site that blocks GitHub's servers

A source that errors with **HTTP 403 on every CI run but loads from home** (MT Arrowhead #75, Cloudflare, since 2026-09-07) is blocking the runners' data-center addresses, not breaking. A headless browser, the Copilot agent or the Claude auto-fix workflow all run on the same GitHub machines and get the same 403.

- **Don't work around it.** Changing the user agent, routing through a proxy or third-party fetcher, or solving a bot challenge with a headless browser all get past protection the site chose. No scheduled jobs on Mike's computer either (a "fetch from home" fallback was built and withdrawn on 2026-11-09).
- **The options:** ask the district or its web host to let the weekly request through (Cloudflare can allow one visitor), use OPI or another public listing when the district posts there, or retire the source and say so on the dashboard. Keep the source meanwhile, so the coverage calendar shows the 403 each week.
- In CI, report it on the issue and stop: no code change fixes a block.
