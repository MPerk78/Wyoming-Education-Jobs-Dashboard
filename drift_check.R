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

BASELINE_VALID_FROM <- as.Date("2026-08-03")

WSBA_STATEWIDE_SOURCE <- "WSBA statewide vacancies"
WSBA_VACANCIES_URL <- "https://www.wsba-wy.org/vacancies"

# --------------------------------------------------------------------------
# Tier 1: per-source historical drift detection
# --------------------------------------------------------------------------

# archive_files: named character vector of {date string -> file path}, e.g.
# from list.files(archive_dir, pattern=..., full.names=TRUE) with dates
# parsed out of the filenames. Kept as a plain argument (not a directory
# scan) so this is testable against a handful of synthetic in-memory data
# frames instead of real files on disk.
build_historical_counts <- function(archive_snapshots, name_col) {
  # archive_snapshots: named list of data.frames, names are "YYYY-MM-DD"
  # dates, each data.frame has a `name_col` column of source names (one row
  # per posting, same shape as combinedclean.csv/hedata.xlsx).
  valid_dates <- names(archive_snapshots)[as.Date(names(archive_snapshots)) >= BASELINE_VALID_FROM]

  if (length(valid_dates) == 0) {
    return(data.frame(name = character(0), n_weeks = integer(0), mean_count = numeric(0)))
  }

  counts_by_week <- lapply(valid_dates, function(d) {
    df <- archive_snapshots[[d]]
    as.data.frame(table(df[[name_col]]), stringsAsFactors = FALSE)
  })

  all_counts <- do.call(rbind, counts_by_week)
  names(all_counts) <- c("name", "count")
  all_counts$count <- as.numeric(all_counts$count)

  aggregate(count ~ name, data = all_counts, FUN = function(x) c(n = length(x), mean = mean(x))) -> agg
  data.frame(
    name = agg$name,
    n_weeks = agg$count[, "n"],
    mean_count = agg$count[, "mean"],
    stringsAsFactors = FALSE
  )
}

# current_counts: data.frame(name, count) for this week's just-rendered data.
# baseline: output of build_historical_counts().
# min_weeks: a source needs at least this many valid historical weeks before
#   it's eligible to be flagged at all -- with 0 or 1 data points there's no
#   real baseline yet, just noise.
# min_mean_count: a source whose historical average is below this is exempt.
#   A district that averages 1-2 postings and now has 0 is ordinary
#   week-to-week churn, not the silent-parser-failure signature this check
#   exists to catch (that one hides *dozens* of real postings) -- flagging
#   it every week just trains the reader to ignore the alert. The threshold
#   is deliberately low so a source that genuinely sustained even 3/week and
#   broke to 0 is still caught.
# drop_threshold: flag if current count <= mean_count * drop_threshold.
flag_drift <- function(current_counts, baseline, min_weeks = 2, min_mean_count = 3,
                       drop_threshold = 0.2) {
  merged <- merge(baseline, current_counts, by = "name", all.x = TRUE)
  merged$count[is.na(merged$count)] <- 0

  eligible <- merged[merged$n_weeks >= min_weeks & merged$mean_count >= min_mean_count, ]
  flagged <- eligible[eligible$count <= eligible$mean_count * drop_threshold, ]
  flagged[order(-flagged$mean_count), c("name", "mean_count", "n_weeks", "count")]
}

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

# Summarizes the status of the Central Safety Net statewide vacancy fallback
# (WSBA in Wyoming).
#
# Returns a list with:
#   - total_feed_vacancies: total vacancies from the statewide feed
#   - feed_name: display name of the statewide feed
#   - fallback_districts: named list of {district -> count} for registered districts
#                         with active fallback postings
#   - total_fallback_vacancies: sum of fallback vacancies across registered districts
#   - discrepancies: data.frame of unmatched discrepancies for direct districts
#   - markdown: character vector of markdown lines for the alert report
summarize_central_safety_net <- function(
  combined_df = NULL,
  registry = NULL,
  discrepancy_report = NULL,
  scrape_log = NULL,
  feed_label = WSBA_STATEWIDE_SOURCE,
  feed_short_name = "WSBA",
  discrepancy_path = "wy_discrepancy_report.csv"
) {
  # 1. Total vacancies on the statewide feed
  feed_total <- 0L
  if (!is.null(scrape_log) && nrow(scrape_log) > 0 && "source" %in% names(scrape_log)) {
    feed_log <- scrape_log[grepl(feed_short_name, scrape_log$source, ignore.case = TRUE) |
                           grepl(feed_label, scrape_log$source, fixed = TRUE), , drop = FALSE]
    if (nrow(feed_log) > 0 && "n_rows" %in% names(feed_log)) {
      latest_rows <- feed_log$n_rows[nrow(feed_log)]
      if (!is.na(latest_rows) && is.numeric(latest_rows)) {
        feed_total <- as.integer(latest_rows)
      }
    }
  }

  if (feed_total == 0L && !is.null(combined_df) && nrow(combined_df) > 0) {
    if ("Posting_Source" %in% names(combined_df)) {
      feed_total <- sum(!is.na(combined_df$Posting_Source) & combined_df$Posting_Source == feed_label)
    } else {
      is_feed <- (!is.na(combined_df$url) & grepl(feed_short_name, combined_df$url, ignore.case = TRUE)) |
                 (!is.na(combined_df$posting_id) & grepl(feed_short_name, combined_df$posting_id, ignore.case = TRUE))
      feed_total <- sum(is_feed)
    }
  }

  # 2. Registered districts receiving fallback postings
  registered_districts <- if (!is.null(registry) && "District" %in% names(registry)) {
    unique(registry$District[!is.na(registry$District) & nzchar(registry$District)])
  } else {
    character(0)
  }

  fallback_districts <- list()
  if (!is.null(combined_df) && nrow(combined_df) > 0 && length(registered_districts) > 0) {
    is_feed_row <- if ("Posting_Source" %in% names(combined_df)) {
      !is.na(combined_df$Posting_Source) & combined_df$Posting_Source == feed_label
    } else {
      (!is.na(combined_df$url) & grepl(feed_short_name, combined_df$url, ignore.case = TRUE)) |
      (!is.na(combined_df$posting_id) & grepl(feed_short_name, combined_df$posting_id, ignore.case = TRUE))
    }
    feed_reg_rows <- combined_df[is_feed_row & combined_df$District %in% registered_districts, , drop = FALSE]
    if (nrow(feed_reg_rows) > 0) {
      tbl <- table(feed_reg_rows$District)
      fallback_districts <- as.list(as.integer(tbl))
      names(fallback_districts) <- names(tbl)
    }
  }

  # 3. Discrepancy report
  if (is.null(discrepancy_report) && !is.null(discrepancy_path) && file.exists(discrepancy_path)) {
    discrepancy_report <- tryCatch(
      read.csv(discrepancy_path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }

  disc_unmatched <- if (!is.null(discrepancy_report) && nrow(discrepancy_report) > 0 && "Discrepancy_Type" %in% names(discrepancy_report)) {
    discrepancy_report[!grepl("ghost", discrepancy_report$Discrepancy_Type, ignore.case = TRUE), , drop = FALSE]
  } else {
    data.frame()
  }

  # If fallback_districts is empty but discrepancy_report had matching fallback districts
  if (length(fallback_districts) == 0 && nrow(disc_unmatched) > 0 && "District" %in% names(disc_unmatched)) {
    reg_disc <- disc_unmatched[disc_unmatched$District %in% registered_districts, , drop = FALSE]
    if (nrow(reg_disc) > 0) {
      tbl <- table(reg_disc$District)
      fallback_districts <- as.list(as.integer(tbl))
      names(fallback_districts) <- names(tbl)
    }
  }

  total_fallback_vacancies <- sum(unlist(fallback_districts), 0L)

  # 4. Generate Markdown
  md <- c(
    sprintf("### %s postings at districts we scrape directly", feed_short_name),
    "",
    paste("Openings the statewide feed lists for a district we scrape directly, counted because the district's own",
          "board doesn't list them: the district posts some jobs only on the feed, or our scrape misses them.",
          "A district with many is worth a look."),
    "",
    sprintf("- **Statewide Feed (%s)**: %d active vacancies captured.", feed_short_name, feed_total)
  )

  if (length(fallback_districts) == 0) {
    md <- c(md, sprintf("- **Counted from %s at directly scraped districts**: none (every %s opening at those districts is also on the district's own board).", feed_short_name, feed_short_name))
  } else {
    md <- c(
      md,
      sprintf("- **Counted from %s at directly scraped districts**: %d posting(s) at %d district(s), not on the district's own board:",
              feed_short_name, total_fallback_vacancies, length(fallback_districts))
    )
    for (d_name in names(fallback_districts)) {
      md <- c(md, sprintf("  - **%s**: %d", d_name, fallback_districts[[d_name]]))
    }
  }

  if (nrow(disc_unmatched) == 0) {
    md <- c(md, "- **Central Feed Discrepancies**: 0 unmatched vacancies detected for active direct scrapers.")
  } else {
    md <- c(
      md,
      sprintf("- **Central Feed Discrepancies**: %d unmatched vacancy/vacancies detected on central feed for direct-scraped districts (investigate potential ATS drift):", nrow(disc_unmatched))
    )
    shown_n <- min(nrow(disc_unmatched), 5L)
    for (i in seq_len(shown_n)) {
      r <- disc_unmatched[i, ]
      dp <- if ("Date_Posted" %in% names(r) && !is.na(r$Date_Posted) && nzchar(r$Date_Posted)) sprintf(" (posted %s)", r$Date_Posted) else ""
      t_title <- if ("Title" %in% names(r)) r$Title else ""
      d_dist <- if ("District" %in% names(r)) r$District else ""
      md <- c(md, sprintf("  - **%s**: \"%s\"%s", d_dist, t_title, dp))
    }
    if (nrow(disc_unmatched) > shown_n) {
      md <- c(md, sprintf("  - ... and %d more discrepancies", nrow(disc_unmatched) - shown_n))
    }
  }
  md <- c(md, "")

  list(
    total_feed_vacancies = feed_total,
    feed_name = feed_label,
    fallback_districts = fallback_districts,
    total_fallback_vacancies = total_fallback_vacancies,
    discrepancies = disc_unmatched,
    markdown = md
  )
}

build_drift_report_markdown <- function(
  actionable,
  safety_net_summary = NULL,
  date = Sys.Date(),
  feed_short_name = "WSBA",
  intro_explanation = NULL
) {
  if (nrow(actionable) == 0) return(character(0))

  default_intro <- if (is.null(intro_explanation)) {
    "A source is flagged when it has at least 2 weeks of history averaging real postings, and this week's count dropped to 20% or less of that average. This is the same failure signature that hid ~200 real postings across 10 Applitrack districts on 2026-08-03 (a Windows-1252 encoding bug, not real absence of jobs) -- these entries are corroborated against the actual live page but still need a human look."
  } else {
    intro_explanation
  }

  lines <- c(
    paste0("Automated drift check flagged ", nrow(actionable), " source(s) as of ", date, "."),
    "",
    default_intro,
    ""
  )

  for (verdict_group in c("confirmed_broken", "likely_broken", "inconclusive", "no_url_available")) {
    subset_rows <- actionable[actionable$verdict == verdict_group, ]
    if (nrow(subset_rows) == 0) next

    label <- switch(verdict_group,
      confirmed_broken = "### Confirmed broken -- the scraper itself errored this run (not just a low count)",
      likely_broken = "### Likely broken -- live page has real job-posting content, scraper reported little/none",
      inconclusive = "### Inconclusive -- drift detected, live page didn't clearly confirm either way",
      no_url_available = "### No URL on file -- couldn't corroborate automatically"
    )
    lines <- c(lines, label, "")
    for (i in seq_len(nrow(subset_rows))) {
      r <- subset_rows[i, ]
      url_part <- if (is.na(r$url)) "" else sprintf(" -- %s", r$url)
      error_part <- if (verdict_group == "confirmed_broken" && !is.na(r$error_message) && nzchar(r$error_message)) sprintf(" (%s)", r$error_message) else ""
      lines <- c(lines, sprintf("- **%s** (%s): averaged %.1f/week, now %d%s%s", r$name, r$type, r$mean_count, r$count, url_part, error_part))
      if ("llm_note" %in% names(r) && !is.na(r$llm_note) && nzchar(r$llm_note)) lines <- c(lines, sprintf("  - %s", r$llm_note))

      if (r$type == "K-12" && !is.null(safety_net_summary)) {
        sn_count <- if ("safety_net_vacancies" %in% names(r) && !is.na(r$safety_net_vacancies)) {
          as.integer(r$safety_net_vacancies)
        } else if (!is.null(safety_net_summary$fallback_districts[[r$name]])) {
          as.integer(safety_net_summary$fallback_districts[[r$name]])
        } else {
          0L
        }

        if (sn_count > 0) {
          lines <- c(lines, sprintf("  - **On %s**: %d posting%s for this district that its own board doesn't list (counted from %s)",
                                    feed_short_name, sn_count, if (sn_count == 1) "" else "s", feed_short_name))
        } else {
          lines <- c(lines, sprintf("  - **On %s**: nothing for this district beyond what its own board lists", feed_short_name))
        }
      }
    }
    lines <- c(lines, "")
  }

  if (!is.null(safety_net_summary) && !is.null(safety_net_summary$markdown)) {
    lines <- c(lines, safety_net_summary$markdown)
  }

  lines
}

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


# Titles from a scraper's result. Wyoming's scrapers don't agree on case:
# Applitrack/TedK12 return `title`, everything else `Title`.
scraper_titles <- function(result) {
  col <- intersect(c("Title", "title"), names(result))
  if (length(col) == 0) character(0) else as.character(result[[col[1]]])
}

# result: the scraper's data.frame, or a condition object if it errored.
# Returns list(pass, markdown). Fails only on the unambiguous cases -- an
# error or zero rows (the exact symptom the issue was filed for). Fewer
# rows than the LLM read, or titles it didn't match, are reported for the
# human reviewer but don't fail: the LLM list is a hint, not ground truth.
summarize_live_check <- function(source_name, result, expected_titles = character(0)) {
  header <- sprintf("### Live scraper check: %s", source_name)
  if (inherits(result, "condition")) {
    return(list(pass = FALSE, markdown = c(header, "", sprintf(":x: The scraper **errored** against the live site: `%s`", conditionMessage(result)))))
  }
  titles <- scraper_titles(result)
  if (length(titles) == 0) {
    return(list(pass = FALSE, markdown = c(header, "", ":x: The scraper still returns **0 postings** from the live site.")))
  }

  norm <- function(x) tolower(trimws(x))
  matched <- vapply(expected_titles, function(t) any(grepl(norm(t), norm(titles), fixed = TRUE) |
                                                     vapply(norm(titles), grepl, logical(1), x = norm(t), fixed = TRUE)),
                    logical(1))
  out <- c(header, "",
           sprintf(":white_check_mark: The scraper returned **%d posting(s)** from the live site:", length(titles)),
           "", paste0("- ", utils::head(titles, 25)),
           if (length(titles) > 25) sprintf("- ... and %d more", length(titles) - 25), "")
  if (length(expected_titles) > 0) {
    out <- c(out, sprintf("Matched %d of %d title(s) the drift check's LLM read off the page.", sum(matched), length(expected_titles)))
    if (any(!matched)) out <- c(out, "", "Not matched (check by hand -- the LLM list is a hint, not ground truth):", "",
                                paste0("- ", expected_titles[!matched]))
  }
  list(pass = TRUE, markdown = out)
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











# --------------------------------------------------------------------------
# Frozen boards (WY spells trust audit 2026-11-05, finding 2)
# --------------------------------------------------------------------------
# A board whose whole posting set has stayed identical for weeks is a stale
# page or cache, not a district that stopped hiring: Fremont #38 returned the
# same 14 postings every week from 2024-10 to 2025-11, all counted as live.
# `frozen` is frozen_board_runs() (posting_spells.R) over recent archives;
# only stretches still running this week are reported.
build_frozen_board_report_markdown <- function(frozen, date = Sys.Date()) {
  if (is.null(frozen) || nrow(frozen) == 0) return(character(0))
  frozen <- frozen[frozen$Ongoing, , drop = FALSE]
  if (nrow(frozen) == 0) return(character(0))
  board <- ifelse(is.na(frozen$Posting_Source) | !nzchar(frozen$Posting_Source) | frozen$Posting_Source == frozen$District,
                  "", paste0(" (", frozen$Posting_Source, ")"))
  c(
    paste0("### Boards that haven't changed in weeks (", date, ")"),
    "",
    paste("Each board below has listed exactly the same postings every week for the number of weeks shown.",
          "Real boards change; this is usually a stale page, a cache, or a scraper reading an old copy, and",
          "its postings keep counting as live in the weekly totals. Open the board and compare."),
    "",
    sprintf("- **%s**%s (%s): the same %d posting(s) for %d weeks, since %s",
            frozen$District, board, frozen$Segment, frozen$Postings, frozen$Runs, frozen$From),
    ""
  )
}
