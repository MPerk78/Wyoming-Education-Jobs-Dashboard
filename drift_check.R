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

WSBA_STATEWIDE_SOURCE <- "WSBA statewide vacancies"
WSBA_VACANCIES_URL <- "https://www.wsba-wy.org/vacancies"

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

# Human-facing pages, not necessarily the API endpoint the real scraper
# hits -- the point of this lookup is "what would a person visiting the
# site actually see," as independent corroboration.
he_institution_urls <- c(
  "Laramie County Community College"  = "https://www.governmentjobs.com/careers/lcccwy",
  "Casper College"                    = "https://www.schooljobs.com/careers/caspercollege",
  # Was schooljobs.com/careers/westernwyoming (a real but wrong NEOGOV
  # agency, found returning a plausible "0 jobs found" page instead of an
  # error) -- Western is actually on PeopleAdmin like Eastern/Sheridan/
  # Northwest; see Wy_ED_Jobs.Rmd's "western wyoming" chunk for the story.
  "Western Wyoming Community College" = "https://wwcwy.peopleadmin.com/postings/all_jobs",
  "Central Wyoming College"           = "https://www.schooljobs.com/careers/cwc",
  "Gillette College"                  = "https://www.schooljobs.com/careers/gillettecollege",
  "Eastern Wyoming Community College" = "https://ewc.peopleadmin.com/postings/all_jobs",
  "Sheridan College"                  = "https://jobs.sheridan.edu/postings/all_jobs",
  "Northwest College"                 = "https://northwestcollege.simplehire.com/postings/all_jobs",
  "University of Wyoming"             = "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1"
)

build_source_url_lookup <- function(
    k12_registry_csv = "k12_district_registry.csv",
    misc_registry = NULL) {
  k12_registry <- read.csv(k12_registry_csv, stringsAsFactors = FALSE)

  lookup <- c(
    setNames(k12_registry$Job_Link, k12_registry$District),
    he_institution_urls
  )

  if (!is.null(misc_registry)) {
    lookup <- c(lookup, setNames(misc_registry$url, misc_registry$District))
  }

  # Later sources win on name collisions (e.g. a district deliberately
  # dual-listed across two platforms) -- arbitrary but deterministic, and
  # any one live URL for a district is enough for a corroboration check.
  lookup[!duplicated(names(lookup), fromLast = TRUE)]
}

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









# Unlike Montana (two registries of the same shape), a Wyoming source name
# can come from four places, each called differently in Wy_ED_Jobs.Rmd:
#   - k12_district_registry.csv: one fetch_* per Platform, with arguments
#     derived from Job_Link/Org_ID by the same shared helpers the Rmd uses
#     (applitrack_tenant_path() etc. in direct_api_scrapers.R).
#   - misc_district_registry (misc_district_scrapers.R): platform -> the
#     fetch_* that fetch_misc_district_postings() dispatches to.
#   - Higher ed and the one charter school: hardcoded calls in the Rmd,
#     mirrored in EXTRA_SCRAPER_CALLS. test-drift-check.R asserts each one
#     still appears verbatim in Wy_ED_Jobs.Rmd, so the two can't drift.
# WSBA-only orgs (WSBA_ONLY_ORGS) have no scraper of their own and resolve
# to NULL, as does anything else not listed.

MISC_PLATFORM_FETCH_FNS <- c(
  wordpress = "fetch_wordpress_postings",
  smartsites = "fetch_smartsites_postings",
  schoolblocks = "fetch_schoolblocks_postings",
  edlio = "fetch_edlio_postings",
  googlesites = "fetch_googlesites_postings",
  educational_networks = "fetch_educational_networks_postings",
  apptegy = "fetch_apptegy_postings",
  prairieview = "fetch_prairieview_postings"
)
# The misc platforms whose fetch_* takes a chromote session first.
MISC_CHROMOTE_PLATFORMS <- c("googlesites", "apptegy", "prairieview")

EXTRA_SCRAPER_CALLS <- list(
  "Laramie County Community College"  = list(fn = "fetch_neogov_postings", args = list("https://www.governmentjobs.com", "lcccwy")),
  "Casper College"                    = list(fn = "fetch_neogov_postings", args = list("https://www.schooljobs.com", "caspercollege")),
  "Western Wyoming Community College" = list(fn = "fetch_peopleadmin_atom", args = list("https://wwcwy.peopleadmin.com/postings/all_jobs", "Western Wyoming Campus")),
  "Central Wyoming College"           = list(fn = "fetch_neogov_postings", args = list("https://www.schooljobs.com", "cwc")),
  "Eastern Wyoming Community College" = list(fn = "fetch_peopleadmin_atom", args = list("https://ewc.peopleadmin.com/postings/all_jobs", "Eastern Wyoming Campus")),
  "Gillette College"                  = list(fn = "fetch_neogov_postings", args = list("https://www.schooljobs.com", "gillettecollege")),
  "Sheridan College"                  = list(fn = "fetch_peopleadmin_atom", args = list("https://jobs.sheridan.edu/postings/all_jobs", "Sheridan College Campus")),
  "Northwest College"                 = list(fn = "fetch_peopleadmin_atom", args = list("https://northwestcollege.simplehire.com/postings/all_jobs", "Northwest College Campus")),
  "University of Wyoming"             = list(fn = "fetch_uw_postings", args = list()),
  "Laramie Montessori Charter School" = list(fn = "fetch_paylocity_jobs", args = list("70f7d03b-6c9f-4106-8e70-9d781a8bbcba", "Laramie-Montessori-School-Inc"))
)

# name: a drift-check source name (canonical district / institution name).
# Returns list(fn, args, session, platform, where), or NULL when the source
# has no single-source live check. Pure lookup -- run it with
# run_scraper_call().
resolve_scraper_call <- function(name, k12_registry, misc_registry) {
  if (name %in% k12_registry$District) {
    row <- k12_registry[k12_registry$District == name, ][1, ]
    spec <- switch(row$Platform,
      Applitrack   = list(fn = "fetch_applitrack_postings", args = list(applitrack_tenant_path(row$Job_Link))),
      TedK12       = list(fn = "fetch_tedk12_postings", args = list(row$Job_Link)),
      SchoolSpring = list(fn = "fetch_schoolspring_postings", args = list(schoolspring_domain(row$Job_Link))),
      RedRoverK12  = list(fn = "fetch_redrover_postings", args = list(as.character(row$Org_ID), redrover_org_slug(row$Job_Link))),
      NULL
    )
    if (is.null(spec)) return(NULL)
    return(c(spec, list(session = FALSE, platform = row$Platform, where = "k12_district_registry.csv")))
  }
  if (name %in% misc_registry$District) {
    row <- misc_registry[misc_registry$District == name, ][1, ]
    fn <- unname(MISC_PLATFORM_FETCH_FNS[row$platform])
    if (is.na(fn)) return(NULL)
    return(list(fn = fn, args = list(row$url), session = row$platform %in% MISC_CHROMOTE_PLATFORMS,
                platform = row$platform, where = "misc_district_registry (misc_district_scrapers.R)"))
  }
  spec <- EXTRA_SCRAPER_CALLS[[name]]
  if (is.null(spec)) return(NULL)
  c(spec, list(session = FALSE, platform = sub("^fetch_(.*?)_(postings|atom|jobs)$", "\\1", spec$fn),
               where = "a hardcoded call in Wy_ED_Jobs.Rmd"))
}

# Human-readable pointer to the code a resolved call runs.
describe_scraper_call <- function(call) sprintf("`%s()`", call$fn)

# Executes a resolve_scraper_call() result against the live source.
# session_factory is only called for chromote-backed scrapers.
run_scraper_call <- function(call, session_factory = NULL) {
  fn <- get(call$fn, mode = "function")
  if (!call$session) return(do.call(fn, call$args))
  session <- session_factory()
  on.exit(tryCatch(session$close(), error = function(e) NULL), add = TRUE)
  do.call(fn, c(list(session), call$args))
}

# ---- Evidence for the agent ------------------------------------------------
# The drift check renders each flagged page in CI, outside the agent's
# firewall. The agent's own render can't be trusted: on Montana's first
# auto-fix (its issue #7) the firewall blocked Apptegy's CDNs, the page came
# up empty, and the agent rewrote the fetch instead of fixing a renamed stop
# line. So the issue carries the text CI captured, the source's existing
# fixtures, and -- when one exists for this exact source -- a diff between
# its text fixture and that capture.
#
# Wyoming names fixtures <platform>_<district>... (misc_districts/
# apptegy_niobrara_rendered.txt), and several districts share one parser.
# So fixtures are matched by platform prefix, then by the district's place
# name; a diff is only offered against a fixture for this same district,
# never another district's page on the same platform.

SCRAPER_FILES <- c("direct_api_scrapers.R", "misc_district_scrapers.R")


# "Niobrara County School District 1" -> c(place = "niobrara", number = "1").
district_fixture_tokens <- function(name) {
  words <- tolower(unlist(strsplit(gsub("[^A-Za-z0-9 ]", " ", name), "\\s+")))
  words <- words[nzchar(words)]
  number <- utils::tail(words[grepl("^[0-9]+$", words)], 1)
  c(place = words[1], number = if (length(number)) number else "")
}

# fixture_files: paths relative to tests/testthat/fixtures. Returns
# list(platform = every fixture for this platform, own = the ones for this
# source, best match first).
source_fixtures <- function(call, source_name, fixture_files) {
  if (is.null(call)) return(list(platform = character(0), own = character(0)))
  prefix <- paste0("^", tolower(call$platform), "_")  # registry says "Applitrack", fixtures "applitrack_"
  platform <- fixture_files[grepl(prefix, tolower(basename(fixture_files)))]
  tok <- district_fixture_tokens(source_name)
  base <- tolower(basename(platform))
  exact <- nzchar(tok[["number"]]) & grepl(paste0(tok[["place"]], tok[["number"]]), base, fixed = TRUE)
  loose <- grepl(tok[["place"]], base, fixed = TRUE)
  # Platte 2's fixture is apptegy_platte2_*; Niobrara 1's is apptegy_niobrara_*.
  # A place-only match can't be another numbered district of the same place.
  loose <- loose & !grepl(paste0(tok[["place"]], "[0-9]"), base)
  own <- c(platform[exact], platform[loose & !exact])
  dates <- ifelse(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", own), sub(".*([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", own), "")
  list(platform = platform, own = own[order(dates, decreasing = TRUE, method = "radix")])
}


# Everything build_autofix_issue_body() shows the agent beyond the drift
# numbers. repo_root is a parameter only so tests can point it elsewhere.
gather_autofix_evidence <- function(call, source_name, page_text, repo_root = ".") {
  scraper_lines <- unlist(lapply(file.path(repo_root, SCRAPER_FILES), readLines, warn = FALSE))
  fixture_dir <- file.path(repo_root, "tests", "testthat", "fixtures")
  fixture_files <- list.files(fixture_dir, recursive = TRUE)

  fetch_fn <- if (is.null(call)) NA_character_ else call$fn
  fx <- source_fixtures(call, source_name, fixture_files)
  inner_text <- scraper_reads_inner_text(fetch_fn, scraper_lines)
  has_text <- length(page_text) == 1 && !is.na(page_text) && nzchar(trimws(page_text))

  diff_against <- if (inner_text && has_text) utils::head(fx$own[grepl("[.]txt$", fx$own)], 1) else character(0)
  diff <- if (length(diff_against) == 1) {
    old <- paste(readLines(file.path(fixture_dir, diff_against), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    text_diff(old, page_text, diff_against, "page text captured in CI")
  } else character(0)

  list(fetch_fn = fetch_fn, fixtures = fx$own, platform_fixtures = fx$platform, inner_text = inner_text,
       page_text = if (has_text) page_text else NA_character_,
       diff_against = if (length(diff_against) == 1) diff_against else NA_character_,
       diff = diff)
}

# The "What CI saw" section of the issue body, from gather_autofix_evidence().
autofix_evidence_markdown <- function(evidence, captured_on) {
  if (is.null(evidence)) return(character(0))
  code_list <- function(x) paste0("`", x, "`", collapse = ", ")
  out <- c("### What CI saw", "")
  if (length(evidence$fixtures) > 0) {
    out <- c(out, sprintf("Existing fixtures for this source in `tests/testthat/fixtures/`, newest first: %s",
                          code_list(evidence$fixtures)), "")
  }
  others <- setdiff(evidence$platform_fixtures, evidence$fixtures)
  if (length(others) > 0) {
    out <- c(out, sprintf("Other districts' fixtures for the same `%s()` parser (their tests must keep passing): %s",
                          evidence$fetch_fn, code_list(others)), "")
  }
  if (length(evidence$diff) > 0) {
    d <- evidence$diff
    cut <- length(d) > AUTOFIX_DIFF_MAX_LINES
    out <- c(out,
             sprintf("<details><summary>Diff: this source's text fixture (<code>%s</code>) &rarr; page text captured in CI</summary>", evidence$diff_against),
             "", "````diff", utils::head(d, AUTOFIX_DIFF_MAX_LINES),
             if (cut) sprintf("... diff truncated (%d more lines)", length(d) - AUTOFIX_DIFF_MAX_LINES),
             "````", "", "</details>", "")
  }
  if (!is.na(evidence$page_text)) {
    txt <- evidence$page_text
    cut <- nchar(txt) > AUTOFIX_PAGE_TEXT_MAX_CHARS
    if (cut) txt <- substr(txt, 1, AUTOFIX_PAGE_TEXT_MAX_CHARS)
    out <- c(out,
             sprintf("<details><summary>Full page text captured in CI (<code>document.body.innerText</code>, %s)</summary>", captured_on),
             "", "````text", txt, if (cut) "... page text truncated", "````", "", "</details>", "")
  }
  if (length(out) == 2) character(0) else out
}

# row: one likely_broken row of corroborate_drift.R's results (name, type,
# mean_count, count, url, llm_titles). call: resolve_scraper_call()'s
# result for it, or NULL. evidence: gather_autofix_evidence()'s result, or
# NULL. Returns the markdown body of the per-source auto-fix issue --
# written as the task prompt the Copilot coding agent will work from.
build_autofix_issue_body <- function(row, call = NULL, run_url = NULL,
                                     evidence = NULL, captured_on = as.character(Sys.Date())) {
  titles <- if (is.na(row$llm_titles) || !nzchar(row$llm_titles)) character(0)
            else strsplit(row$llm_titles, " | ", fixed = TRUE)[[1]]
  # The CI capture is the new fixture verbatim only when the parser reads
  # innerText; otherwise it's context and the agent captures raw HTML.
  use_capture <- !is.null(evidence) && isTRUE(evidence$inner_text) && !is.na(evidence$page_text)
  repro <- sprintf("`Rscript scripts/repro_scraper.R \"%s\"`", row$name)

  c(
    sprintf("<!-- autofix-source: %s -->", row$name),
    "",
    sprintf("The weekly drift check found that **%s** (%s) averaged %.1f postings/week but the scraper returned **%d** this run, while the live page still lists real postings. The page markup most likely changed and the parser no longer matches it.",
            row$name, row$type, row$mean_count, as.integer(row$count)),
    "",
    sprintf("- **Live page:** %s", if (is.na(row$url)) "(none on file)" else row$url),
    if (!is.null(call)) sprintf("- **Platform:** `%s` (from %s)", call$platform, call$where),
    if (!is.null(call)) sprintf("- **Scraper entry point:** %s (and the `parse_*` function it calls, if any)", describe_scraper_call(call)),
    if (!is.null(call)) sprintf("- **Reproduce:** %s runs this scraper against its fixture tests and the live site", repro),
    if (!is.null(run_url)) sprintf("- **Drift-check run:** %s", run_url),
    "",
    if (length(titles) > 0) c(
      "An LLM read these postings off the live page (a hint, not ground truth -- verify against the page itself):",
      "",
      paste0("- ", titles),
      "",
      "The list covers the whole page, so it can include things this source's parser deliberately leaves out (e.g. contact info or a standing substitute-recruiting list). Restore what the parser used to find; see the comment above its `parse_*` function.",
      ""
    ),
    if (isTRUE(call$session)) c(
      "The drift check rendered this page in CI with chromote (`document.body.innerText`), so it renders normally outside your sandbox. If your render shows no postings, check the firewall's blocked hosts before changing how the scraper fetches the page -- see `.github/copilot-instructions.md`.",
      ""
    ),
    autofix_evidence_markdown(evidence, captured_on),
    "### Task",
    "",
    if (use_capture) {
      sprintf("1. Save the **Full page text captured in CI** above, verbatim, as a **new, dated fixture** next to this source's existing ones (e.g. `tests/testthat/fixtures/misc_districts/%s_<district>_rendered_%s.txt`). It is real captured data -- the exact text this scraper parses -- so prefer it over your own render. Keep the existing fixtures; the old layout must keep parsing.", call$platform, captured_on)
    } else {
      "1. Fetch the live page and save it as a **new, dated real fixture** in `tests/testthat/fixtures/` (keep the existing fixture -- the old layout must keep parsing)."
    },
    if (length(evidence$diff) > 0) {
      "2. Start from the diff above: it shows what changed since this source's fixture. Fix the parser so it extracts the real postings from the new fixture. Keep the change minimal and in the existing style."
    } else {
      "2. Fix the parser so it extracts the real postings from the new fixture. Keep the change minimal and in the existing style."
    },
    "3. Add a regression test against the new fixture asserting the exact titles found.",
    "4. Run `testthat::test_dir(\"tests/testthat\")` and make sure everything passes.",
    "",
    "Do **not** edit `k12_district_registry.csv`, `misc_district_registry`, the accumulated data under `Wy_Ed_Jobs/`, or archives. If the source has moved to a different platform or URL, or the page genuinely has no postings, don't force a parser change -- say so in the PR description and stop.",
    "",
    sprintf("The PR must reference this issue (`Fixes #<n>`) so the live check can find the source. Label: `%s`.", AUTOFIX_LABEL)
  )
}




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
