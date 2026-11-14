# Detects when a source that reliably has real postings suddenly reports
# none -- the same failure signature as the Applitrack encoding bug found
# 2026-08-03 (silent NA -> zero rows, indistinguishable in the scrape log
# from a district with genuinely no openings). That bug was caught by hand;
# this automates the same kind of check.
#
# Two tiers, kept as separate functions so each is independently testable:
#   1. flag_drift() -- cheap, no network calls. Compares this week's
#      per-source counts against each source's own trailing historical
#      baseline. Pure function, easy to unit test with synthetic history.
#   2. score_page_text_for_job_signal() -- the scoring half of the chromote
#      corroboration step (see .github/scripts/chromote_corroborate.R for
#      the live-fetch half, which isn't unit-tested for the same reason
#      fetch_*() functions elsewhere in this repo aren't: it's a thin I/O
#      wrapper around a real network call).

# Only snapshots from this date forward are valid drift-detection baseline
# candidates. Everything before it predates the Applitrack encoding fix
# (and the WSBA/misc-district/direct-HTTP work before that) -- comparing
# against pre-fix data would flag the FIX itself as suspicious drift on
# every affected district.
# the shared drift, title-lint, pay-coverage, salary and auto-fix helpers moved to the edjobs package in phase 2 (2026-11-11); attached here so
# every script that sources this file still has them.
suppressMessages(library(edjobs))
# The state's feed labels and drift baseline date (state_config.R), which the
# drift functions in edjobs read; loaded here unless the caller already did.
if (is.null(current_state_config())) source("state_config.R")

WSBA_STATEWIDE_SOURCE <- current_state_config()$feed$label
WSBA_VACANCIES_URL <- current_state_config()$feed$url

# --------------------------------------------------------------------------
# Tier 1: per-source historical drift detection
# --------------------------------------------------------------------------



# --------------------------------------------------------------------------
# Posted-pay coverage
# --------------------------------------------------------------------------

# A pay source that breaks doesn't lose postings -- each one just comes
# back with Pay_* blank -- so flag_drift() above can't see it. This compares
# each source's share of postings with stated pay against its own history
# in the raw archives. Archives written before a source captured pay have
# no Pay_* column and are skipped, not counted as zero.






# --------------------------------------------------------------------------
# Tier 0: salary-source structural/coverage checks
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Tier 0b: salary VALUE plausibility (as opposed to coverage/row-count)
# --------------------------------------------------------------------------
#
# check_salary_coverage() above (and flag_drift() for job postings) only
# ever check that a source returned enough ROWS -- neither one can catch a
# source whose page/PDF layout shifted just enough to silently misparse
# VALUES while still returning a plausible row count. The WSBA teacher-
# salary PDF is the clearest real risk for exactly this: it's parsed by
# hardcoded pixel-position windows (see salary_scrapers.R), so a column
# nudge in a future WSBA export could swap or shift every district's
# current/prior salary and still produce 48 rows, passing every check
# above without anyone noticing.
#
# Two complementary checks, both pure functions:
#   1. check_salary_value_bounds() -- a hard sanity range. Cheap, catches
#      the most obvious garbage (a location code or a stray digit landing
#      in a dollar column), but a bound wide enough to never false-positive
#      on real salary growth is too wide to catch a subtler misalignment.
#   2. check_salary_yoy_plausibility() -- real historical signal instead of
#      a guessed bound. WSBA's own PDF already reports both the prior and
#      current year's base salary in one scrape (Base_Salary_Prior_Year/
#      Base_Salary_Current_Year), so every run already has 48 real
#      district-level year-over-year changes to compare against EACH
#      OTHER (cross-sectional), without needing this project's own
#      multi-year archive to have accumulated enough history yet (as of
#      2026-08-05 it has exactly one year -- see
#      k12_salary_history.csv). A district moving 47% while every other
#      district moved 2-6% is far more likely a parser misalignment than a
#      genuine outlier settlement.



# --------------------------------------------------------------------------
# Source name -> public URL lookup, for the chromote corroboration step
# --------------------------------------------------------------------------

# edjobs::build_source_url_lookup(): the registries' links and the colleges' pages
# (state_config.R's scraper_sources), later groups winning on a name collision.

# --------------------------------------------------------------------------
# Tier 2: chromote corroboration scoring (pure function half)
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Tier 2b: fold an LLM read of the page into the text-signal verdict
# --------------------------------------------------------------------------





# --------------------------------------------------------------------------
# Tier 2c: Central Safety Net status summary & report formatting
# --------------------------------------------------------------------------



# --------------------------------------------------------------------------
# Tier 3: per-source auto-fix issues (Copilot coding agent hand-off)
# --------------------------------------------------------------------------
#
# Ported from the Montana dashboard. The rolling "Scraper drift check"
# issue stays the human-facing summary. On top of it, each "likely_broken"
# source -- the live page demonstrably has postings the scraper missed --
# gets its own issue that .github/scripts/file_autofix_issues.R assigns to
# the Copilot coding agent. confirmed_broken (HTTP 429/5xx) and
# genuinely-empty verdicts are deliberately NOT eligible: those are
# site-side, and a code change "fixing" them is exactly the wrong move.
#
# The issue body carries an HTML-comment marker naming the source, so
# .github/scripts/live_check_autofix.R can find which scraper a PR claims
# to fix (via the PR's linked issue) and re-run just that scraper live.









# Scraper dispatch (resolve_scraper_call(), run_scraper_call()), the evidence
# helpers and build_autofix_issue_body() are in edjobs since 0.9.0; this
# state's sources and how each is called are in state_config.R.
SCRAPER_FILES <- current_state_config()$autofix$scraper_files

# --------------------------------------------------------------------------
# Title quality (classification safeguards items 5 and 6; run weekly by
# .github/scripts/check_title_quality.R, non-blocking)
# --------------------------------------------------------------------------

# A scraper that starts reading the wrong element still returns rows, and
# the row count looks fine to flag_drift(): what changes is what the "title"
# holds -- a schedule sentence, a summary paragraph, a level without its role.
# lint_title_shape() names the problem per title (NA when the title looks
# like a job title). Each rule was measured against every archived week in
# both states (2026-11-05): it flags the misreads found by hand in the
# 2026-11-01 classification audit and nothing that is merely an unusual but
# real title. Long real titles run to ~190 characters (UW, Goshen), so
# max_chars is 200. A start date a district writes into its own title
# ("beginning the 2026-27 school year.") isn't flagged; "- Open Until
# Filled." is (WY ledger: Fremont #21).











# Frozen boards (WY spells trust audit 2026-11-05, finding 2): frozen_board_runs()
# and build_frozen_board_report_markdown() are in edjobs since phase 4.
