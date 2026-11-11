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

# One row per source: postings (n) and how many state pay (with_pay).
pay_coverage_counts <- function(names, has_pay) {
  if (length(names) == 0) {
    return(data.frame(name = character(0), n = integer(0), with_pay = integer(0),
                      stringsAsFactors = FALSE))
  }
  n <- tapply(rep(1L, length(names)), names, sum)
  with_pay <- tapply(as.integer(has_pay), names, sum)
  data.frame(name = names(n), n = as.integer(n), with_pay = as.integer(with_pay[names(n)]),
             stringsAsFactors = FALSE)
}

# current: pay_coverage_counts() for this run. history: a list of the same,
# one per archived week that had Pay_*. A source is flagged when it has at
# least min_weeks weeks with min_postings or more postings, its average
# share with pay over those weeks is at least min_share, and this run --
# with min_postings or more postings, so a source that lost its postings is
# left to flag_drift() -- has a share at or below drop_threshold times that
# average.
flag_pay_coverage_drop <- function(current, history, min_weeks = 2, min_postings = 3,
                                   min_share = 0.4, drop_threshold = 0.2) {
  empty <- data.frame(name = character(0), mean_share = numeric(0), n_weeks = integer(0),
                      n = integer(0), with_pay = integer(0), stringsAsFactors = FALSE)
  weeks <- do.call(rbind, history)
  if (is.null(weeks) || nrow(weeks) == 0 || nrow(current) == 0) return(empty)
  weeks <- weeks[weeks$n >= min_postings, , drop = FALSE]
  if (nrow(weeks) == 0) return(empty)
  weeks$share <- weeks$with_pay / weeks$n
  baseline <- data.frame(
    name = sort(unique(weeks$name)),
    stringsAsFactors = FALSE
  )
  baseline$n_weeks <- as.integer(table(weeks$name)[baseline$name])
  baseline$mean_share <- as.numeric(tapply(weeks$share, weeks$name, mean)[baseline$name])

  merged <- merge(baseline, current, by = "name")
  merged <- merged[merged$n >= min_postings & merged$n_weeks >= min_weeks &
                     merged$mean_share >= min_share, , drop = FALSE]
  flagged <- merged[merged$with_pay / merged$n <= merged$mean_share * drop_threshold, , drop = FALSE]
  if (nrow(flagged) == 0) return(empty)
  flagged[order(-flagged$mean_share), c("name", "mean_share", "n_weeks", "n", "with_pay")]
}

build_pay_coverage_report_markdown <- function(flagged, date = Sys.Date()) {
  if (nrow(flagged) == 0) return(character(0))
  c(
    paste0("### Posted pay went missing (", date, ")"),
    "",
    paste("These sources still return postings, but far fewer of them carry the pay the posting states",
          "than usual -- the pay field or detail request the scraper reads has likely changed.",
          "Postings and counts are unaffected; the Posted Pay column is blank for them."),
    "",
    sprintf("- **%s** (%s): usually %.0f%% of postings state pay, now %d of %d",
            flagged$name, flagged$type, 100 * flagged$mean_share, flagged$with_pay, flagged$n),
    ""
  )
}

# A source dropping to (near) zero is ambiguous on count alone: a genuinely
# quiet week looks identical to a scraper that started erroring. But
# safe_scrape() (scrape_helpers.R) already logs which one happened, to
# scrape_log.csv, in the very same pipeline run that produced this week's
# drift-flagged counts -- so check there first, before spending a live
# chromote render on a guess. A source whose most recent logged attempt
# this run was a real "error" (not "empty") is a much stronger and cheaper
# signal: the registered URL itself is broken (a dead ATS tenant, a DNS
# failure, a migrated platform, an HTTP error perform_with_retry() couldn't
# recover from), not just "no visible postings right now". scrape_log's
# `source` strings aren't always an exact match for a flagged `name` (a
# platform-prefixed source name, e.g. "Apptegy/chromote: <District>",
# would still need this even though nothing in this project currently logs
# that way), so match by substring containment rather than requiring
# equality.
attach_scrape_log_errors <- function(flagged, scrape_log) {
  flagged$scrape_error <- rep(NA_character_, nrow(flagged))
  if (nrow(flagged) == 0 || nrow(scrape_log) == 0) return(flagged)

  # Keep only each source's single most recent logged attempt -- a source
  # that errored earlier in the run but succeeded on a later retry/re-run
  # must NOT be reported as currently broken, so status is checked on the
  # latest attempt, not on "was there ever an error this run".
  latest <- scrape_log[order(scrape_log$timestamp), ]
  latest <- latest[!duplicated(latest$source, fromLast = TRUE), ]
  errors <- latest[!is.na(latest$status) & latest$status == "error", ]
  if (nrow(errors) == 0) return(flagged)

  for (i in seq_len(nrow(flagged))) {
    hits <- which(vapply(errors$source, function(s) grepl(flagged$name[i], s, fixed = TRUE), logical(1)))
    if (length(hits) > 0) flagged$scrape_error[i] <- errors$error_message[hits[1]]
  }
  flagged
}

# Report section for this week's posted amounts that drop_implausible_pay()
# blanked (rows of `bad`: source, title, Pay_Min, Pay_Max, Pay_Unit,
# Pay_Text). A misread -- a fee, a benefit's value -- usually means the
# extractor needs a new skip rule; a real but unusual figure means
# PAY_PLAUSIBLE_RANGE needs widening.
build_implausible_pay_report_markdown <- function(bad, date = Sys.Date(), max_rows = 25) {
  if (nrow(bad) == 0) return(character(0))
  shown <- utils::head(bad, max_rows)
  amount <- ifelse(shown$Pay_Min == shown$Pay_Max | is.na(shown$Pay_Max),
                   sprintf("$%s", format(shown$Pay_Min, big.mark = ",")),
                   sprintf("$%s-$%s", format(shown$Pay_Min, big.mark = ","), format(shown$Pay_Max, big.mark = ",")))
  c(
    paste0("### Posted pay blanked as implausible (", date, ")"),
    "",
    paste("These amounts were read from the postings but fall outside PAY_PLAUSIBLE_RANGE",
          "(k12_he_classification.R) for their unit, so the dashboard shows no posted pay for them.",
          "The raw archive keeps them. A misread needs an extractor fix; a real figure needs the range widened."),
    "",
    sprintf("- **%s** (%s) \"%s\": %s per %s -- \"%s\"",
            shown$source, shown$type, shown$title, amount, ifelse(is.na(shown$Pay_Unit), "unknown unit", shown$Pay_Unit),
            substr(gsub("[\r\n]+", " ", shown$Pay_Text), 1, 120)),
    if (nrow(bad) > max_rows) sprintf("- ...and %d more", nrow(bad) - max_rows),
    ""
  )
}

# --------------------------------------------------------------------------
# Tier 0: salary-source structural/coverage checks
# --------------------------------------------------------------------------

# Salary data (WSBA for K-12, IPEDS for Higher Ed -- see salary_scrapers.R
# and ipeds_salary_scraper.R) has a small, essentially fixed universe (48
# WY school districts, 9 WY public HE institutions) and changes far less
# often than job postings (once a year, not weekly), so a trailing
# statistical baseline like flag_drift() doesn't fit. Instead this is a
# hard assertion against that known universe size: a parser silently
# extracting fewer matched records than the known-fixed count is the same
# failure signature as the Applitrack encoding bug -- the scrape "succeeds"
# (safe_scrape logs status "ok", n_rows > 0) while quietly returning much
# less real data than before, because the source changed its page/PDF/API
# layout out from under the parser.
check_salary_coverage <- function(name, actual, expected, min_ok = expected) {
  if (actual >= min_ok) return(NULL)
  data.frame(name = name, expected = expected, actual = actual, stringsAsFactors = FALSE)
}

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

# actual: named numeric vector (name = district/institution, value = salary).
check_salary_value_bounds <- function(name, actual, min_ok, max_ok) {
  bad <- actual < min_ok | actual > max_ok
  bad[is.na(bad)] <- FALSE
  if (!any(bad)) return(NULL)
  data.frame(
    name = name, entity = names(actual)[bad], value = unname(actual[bad]),
    min_ok = min_ok, max_ok = max_ok, stringsAsFactors = FALSE
  )
}

# current/prior: named numeric vectors (name = district/institution),
# compared pairwise by name. Flags an entity whose |% change| both (a)
# exceeds hard_ceiling outright (a settlement essentially never moves this
# much in one real year) and (b) is a real statistical outlier against
# every OTHER entity's change this same run (median absolute deviation,
# robust to one or two entities genuinely having an unusual year) --
# requiring both avoids flagging a single district with a real large
# settlement while every other district also moved a lot (a real
# statewide event, not a parser bug), and avoids flagging a small
# percentage move that's just normal variation.
check_salary_yoy_plausibility <- function(current, prior, hard_ceiling = 0.25, mad_multiplier = 5) {
  common <- intersect(names(current), names(prior))
  cur <- current[common]
  pri <- prior[common]
  valid <- !is.na(cur) & !is.na(pri) & pri != 0
  if (sum(valid) < 3) return(NULL)  # too few points for a cross-sectional outlier check to mean anything

  pct_change <- (cur[valid] - pri[valid]) / pri[valid]
  center <- stats::median(pct_change)
  spread <- stats::mad(pct_change)

  is_outlier <- if (spread > 0) {
    abs(pct_change - center) > mad_multiplier * spread & abs(pct_change) > hard_ceiling
  } else {
    # A zero spread means every peer moved by (near) the exact same amount
    # -- a MAD-based threshold would then be 0, and "greater than 0" would
    # flag ordinary tiny floating-point variation between otherwise-equal
    # changes. Fall back to the hard ceiling alone: still lets a genuine
    # uniform statewide move of, say, 15% through untouched (below the
    # ceiling), while still catching one district at 47% against five
    # peers that didn't move at all (this function's whole reason to
    # exist), which a spread-based threshold of 0 could never do since
    # nothing can exceed a threshold of Inf.
    abs(pct_change) > hard_ceiling
  }
  if (!any(is_outlier)) return(NULL)

  data.frame(
    name = names(pct_change)[is_outlier],
    prior = unname(pri[valid][is_outlier]),
    current = unname(cur[valid][is_outlier]),
    pct_change = unname(pct_change[is_outlier]),
    stringsAsFactors = FALSE
  )
}

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

# page_text: visible rendered text of a live page (document.body.innerText
# via chromote, same technique used for the 2026-08-03 manual spot-check).
# Returns one of "likely_broken", "looks_genuinely_empty", or
# "inconclusive" -- deliberately three-valued rather than a boolean, since
# a page that's neither clearly job-signal-positive nor clearly says "no
# openings" (e.g. a fetch error, a redirect to an unrelated page) shouldn't
# be silently folded into either bucket.
score_page_text_for_job_signal <- function(page_text) {
  if (is.na(page_text) || nchar(trimws(page_text)) == 0) {
    return("inconclusive")
  }

  negative_signal <- grepl(
    "no (open |current )?(job|position|vacan)|no openings|not currently (hiring|accepting)|there are (currently )?no",
    page_text, ignore.case = TRUE
  )

  positive_hits <- lengths(regmatches(
    page_text,
    gregexpr("apply now|view details|job title|posted:|closing date|date posted|JobID", page_text, ignore.case = TRUE)
  ))

  if (negative_signal && positive_hits < 3) {
    "looks_genuinely_empty"
  } else if (positive_hits >= 3) {
    "likely_broken"
  } else {
    "inconclusive"
  }
}

# --------------------------------------------------------------------------
# Tier 2b: fold an LLM read of the page into the text-signal verdict
# --------------------------------------------------------------------------

# verdict: from score_page_text_for_job_signal() ("likely_broken" /
#   "looks_genuinely_empty" / "inconclusive"), or "confirmed_broken" /
#   "no_url_available" set upstream in corroborate_drift.R.
# llm_titles: from llm_titles_from_page_text() -- character(0) when the LLM
#   step was skipped (no key) or found nothing.
#
# Returns list(verdict, note):
#   - LLM found real postings and we weren't already at confirmed_broken ->
#     promote to "likely_broken" and say what it found (the scraper returned
#     ~0 but these are demonstrably on the page).
#   - LLM found nothing AND the text signal was only "inconclusive" ->
#     downgrade to "looks_genuinely_empty" (conservative: both weak signals
#     now agree there's nothing there).
#   - otherwise: unchanged.
# Standing substitute-recruiting lists ("We are always looking for
# substitutes: bus driver substitutes, kitchen substitutes, ...") read like
# postings to an LLM, but most boards that run them aren't posting jobs there.
# Lone Rock (MT issue #13, 2026-11-10): its two real openings came down, the
# scraper rightly returned 0, and the LLM's six substitute lines made it
# "likely_broken". Whether a substitute line is a missed posting depends on
# the source: 127 of 183 MT and 49 of 59 WY direct sources list substitute
# postings themselves (last 8 weeks, 2026-11-10). So the lines are set aside
# only when the source's own recent titles have none.
STANDING_POOL_PATTERN <- "(?i)\\bsub(stitute)?s?\\b"

# llm_titles minus substitute lines when history_titles (the source's own
# recent titles) has no substitute title; NULL history keeps everything.
drop_standing_pool_titles <- function(llm_titles, history_titles = NULL) {
  if (is.null(history_titles) || !length(llm_titles)) return(llm_titles)
  if (any(grepl(STANDING_POOL_PATTERN, history_titles, perl = TRUE))) return(llm_titles)
  llm_titles[!grepl(STANDING_POOL_PATTERN, llm_titles, perl = TRUE)]
}

# The titles one source returned over its last `weeks` archived runs (K-12:
# Archivek12_Data District/title; "Higher Ed": Archived_HE_Data Institution/Title), or
# NULL when there are no archives to read.
recent_source_titles <- function(name, type = "K-12", weeks = 8, k12_dir = "Archivek12_Data", he_dir = "Archived_HE_Data") {
  he <- type %in% c("Higher Ed", "HE")   # check_drift.R labels HE sources "Higher Ed"
  files <- utils::tail(sort(list.files(if (he) he_dir else k12_dir,
                                       pattern = if (he) "^hedata_\\d{4}-\\d{2}-\\d{2}\\.xlsx$" else "^combined_\\d{4}-\\d{2}-\\d{2}\\.csv$",
                                       full.names = TRUE)), weeks)
  if (!length(files)) return(NULL)
  unlist(lapply(files, function(f) tryCatch({
    x <- if (he) as.data.frame(readxl::read_excel(f, col_types = "text")) else utils::read.csv(f, colClasses = "character")
    if (he) x$Title[x$Institution %in% name] else x$title[x$District %in% name]
  }, error = function(e) character(0))))
}

combine_verdict_with_llm <- function(verdict, llm_titles, history_titles = NULL) {
  kept <- drop_standing_pool_titles(llm_titles, history_titles)
  pooled <- length(llm_titles) - length(kept)
  llm_titles <- kept
  n <- length(llm_titles)

  if (n > 0 && !identical(verdict, "confirmed_broken")) {
    shown <- paste(utils::head(llm_titles, 8L), collapse = "; ")
    return(list(
      verdict = "likely_broken",
      note = paste0("an LLM read ", n, " posting(s) off the live page: ",
                    shown, if (n > 8L) ", ..." else "")
    ))
  }

  if (n == 0 && identical(verdict, "inconclusive")) {
    return(list(verdict = "looks_genuinely_empty",
                note = if (pooled > 0) paste0("an LLM read only a standing substitute list (", pooled,
                                              " line(s)) off the live page, and this source never posts substitute jobs")
                       else "an LLM read no postings off the live page either"))
  }

  list(verdict = verdict, note = NA_character_)
}

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

AUTOFIX_LABEL <- "scraper-autofix"
AUTOFIX_ELIGIBLE_VERDICTS <- "likely_broken"
AUTOFIX_MARKER_RE <- "<!--\\s*autofix-source:\\s*(.+?)\\s*-->"

autofix_issue_title <- function(name) paste0("Scraper auto-fix: ", name)

# Who works a new auto-fix issue, from the AUTOFIX_ENGINE repo variable:
# "claude" (default) dispatches .github/workflows/claude-autofix.yml,
# "copilot" assigns the issue to the Copilot coding agent via COPILOT_TOKEN,
# "none" just files it. Both agents on one issue would race to open
# competing PRs, so it's one or the other. A typo fails the step loudly
# rather than quietly filing issues nobody works.
AUTOFIX_ENGINES <- c("claude", "copilot", "none")
AUTOFIX_CLAUDE_WORKFLOW <- "claude-autofix.yml"

autofix_engine <- function(value) {
  v <- tolower(trimws(if (length(value) == 0 || is.na(value)) "" else value))
  if (!nzchar(v)) return("claude")
  if (!v %in% AUTOFIX_ENGINES) {
    stop("AUTOFIX_ENGINE must be one of ", paste(AUTOFIX_ENGINES, collapse = ", "), ", not '", value, "'")
  }
  v
}

# Claude as the fallback engine (2026-11-11). With AUTOFIX_ENGINE = "copilot",
# an issue Copilot hasn't fixed by a later weekly run goes to the Claude
# workflow once: still flagged, at least AUTOFIX_ESCALATION_MIN_DAYS old, not
# already handed over (the marker in a comment), no open PR with changes
# waiting for review, and no merged fix (merged but still flagged is a human
# problem). A week's wait lets Copilot finish, and lets a board that was
# briefly empty recover on its own, before a second agent is paid for.
# `prs` is every PR whose body says "Fixes #<issue>": columns state
# ("OPEN"/"CLOSED"/"MERGED") and changed_files. Returns the decision as a
# word, "escalate" or the reason not to.
AUTOFIX_ESCALATION_MARKER <- "<!-- autofix-escalated: claude -->"
AUTOFIX_ESCALATION_MIN_DAYS <- 5

autofix_escalation_decision <- function(issue_created, today, comment_bodies, prs) {
  age <- as.numeric(as.Date(today) - as.Date(substr(issue_created, 1, 10)))
  if (is.na(age) || age < AUTOFIX_ESCALATION_MIN_DAYS) return("too_new")
  if (any(grepl(AUTOFIX_ESCALATION_MARKER, comment_bodies, fixed = TRUE))) return("already_escalated")
  if (NROW(prs) > 0) {
    if (any(prs$state == "MERGED")) return("fix_merged")
    if (any(prs$state == "OPEN" & prs$changed_files > 0)) return("pr_awaiting_review")
  }
  "escalate"
}

# PRs (from `gh pr list --json number,state,changedFiles,body`) that say
# "Fixes/Closes/Resolves #<issue>", as autofix_escalation_decision() wants them.
autofix_linked_prs <- function(prs, issue_number) {
  empty <- data.frame(number = integer(0), state = character(0), changed_files = integer(0))
  if (NROW(prs) == 0) return(empty)
  pat <- sprintf("(?i)\\b(fix(es|ed)?|close[sd]?|resolve[sd]?)\\s+#%s\\b", issue_number)
  hit <- grepl(pat, ifelse(is.na(prs$body), "", prs$body), perl = TRUE)
  if (!any(hit)) return(empty)
  data.frame(number = prs$number[hit], state = prs$state[hit], changed_files = prs$changedFiles[hit])
}

parse_autofix_marker <- function(body) {
  if (length(body) == 0 || is.na(body)) return(NA_character_)
  m <- regmatches(body, regexec(AUTOFIX_MARKER_RE, body, perl = TRUE))[[1]]
  if (length(m) < 2) NA_character_ else m[2]
}

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

AUTOFIX_PAGE_TEXT_MAX_CHARS <- 20000
AUTOFIX_DIFF_MAX_LINES <- 200
SCRAPER_FILES <- c("direct_api_scrapers.R", "misc_district_scrapers.R")

# Whether fetch_fn's parser consumes document.body.innerText -- the form the
# drift check captured, so the capture can be the new fixture verbatim.
scraper_reads_inner_text <- function(fetch_fn, scraper_lines) {
  if (is.na(fetch_fn)) return(FALSE)
  start <- grep(sprintf("^%s <- function", fetch_fn), scraper_lines)
  if (length(start) == 0) return(FALSE)
  later <- grep("^[A-Za-z_.][A-Za-z0-9_.]* <- ", scraper_lines)
  end <- c(later[later > start[1]], length(scraper_lines) + 1)[1] - 1
  any(grepl("document.body.innerText", scraper_lines[start[1]:end], fixed = TRUE))
}

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

# Unified diff of two texts after trimming lines and dropping blanks
# (innerText and html_text2() disagree on blank lines, which is noise).
# Uses the system `diff`; returns character(0) when identical or absent.
text_diff <- function(old_text, new_text, old_label = "old", new_label = "new") {
  norm <- function(x) { x <- trimws(unlist(strsplit(x, "\n", fixed = TRUE))); x[nzchar(x)] }
  if (!nzchar(Sys.which("diff"))) return(character(0))
  f_old <- tempfile(); f_new <- tempfile()
  on.exit(unlink(c(f_old, f_new)))
  writeLines(norm(old_text), f_old, useBytes = TRUE)
  writeLines(norm(new_text), f_new, useBytes = TRUE)
  out <- suppressWarnings(system2("diff", c("-u", "--label", shQuote(old_label), "--label", shQuote(new_label),
                                            shQuote(f_old), shQuote(f_new)), stdout = TRUE, stderr = FALSE))
  as.character(out)
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

# The LLM-read titles build_autofix_issue_body() listed -- the live check's
# (hint-quality) expectation for what the fixed scraper should now return.
parse_autofix_expected_titles <- function(body) {
  lines <- strsplit(body, "\n", fixed = TRUE)[[1]]
  start <- grep("^An LLM read these postings", lines)
  # First heading after the list -- "### What CI saw" (whose diff and page
  # text can hold "- " lines) or "### Task".
  end <- grep("^### ", lines)
  end <- end[end > start[1]]
  if (length(start) == 0 || length(end) == 0) return(character(0))
  block <- lines[(start[1] + 1):(end[1] - 1)]
  sub("^- ", "", block[startsWith(block, "- ")])
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

# Abbreviations whose trailing period isn't the end of a sentence.
TITLE_ABBREVIATION_PATTERN <- paste0(
  "(?i)\\b(Jr|Sr|Asst|Assist|Assoc|Coord|Dept|Ed|Sp|St|Supt|Admin|Tech|Spec|Mgr|Dir|Fed|Govt|",
  "Mr|Mrs|Ms|Dr|No|Vo|Ag|Phys|Res|Prof|Approx|Hrs|P\\.E|Ft)\\."
)

lint_title_shape <- function(title, max_chars = 200) {
  t <- as.character(title)
  t[is.na(t)] <- ""
  t <- trimws(t)
  no_abbrev <- gsub(TITLE_ABBREVIATION_PATTERN, "", t, perl = TRUE)
  level_words <- "(?i)\\b(high school|middle school|junior high|jr\\.? high|elementary|hs|ms|head|assistant|asst\\.?|varsity|junior varsity|jv)\\b"
  rules <- list(
    "blank title" = !nzchar(t),
    "no job named" = grepl("^(other|none|n/?a|tbd|tba|positions?|jobs?|vacanc(y|ies)|openings?|see (job )?description|untitled)$",
                           t, ignore.case = TRUE),
    "level and rank only, no role" = nzchar(t) &
      grepl("(?i)\\b(head|assistant|asst)\\b", t, perl = TRUE) &
      !grepl("[A-Za-z0-9]", gsub(level_words, "", t, perl = TRUE)),
    "schedule or detail sentence" = grepl("\\.\\s*$", no_abbrev) &
      (grepl("[A-Za-z]\\.\\s+[A-Za-z]", no_abbrev) |
         grepl("\\b(must|required|requires|hiring|open until|hours?|hrs|per (day|hour|week)|full[- ]time|part[- ]time|year[- ]round|apply|mon|fri)\\b",
               no_abbrev, ignore.case = TRUE)),
    "field label" = grepl("^(summary|description|job description|job summary|position summary|position|job title|title|location|department|salary|pay|wage|posted|closing date|status|requirements|qualifications)\\s*:",
                          t, ignore.case = TRUE),
    "markup left in" = grepl("StartFragment|EndFragment|&[a-z]+;|&#[0-9]+;|<[a-z/][^>]*>|​", t, ignore.case = TRUE, perl = TRUE),
    "file name" = grepl("[A-Za-z]{2,}_[A-Za-z]{2,}|\\s-\\sCopy$", t),
    "too long" = nchar(t) > max_chars
  )
  reasons <- do.call(cbind, lapply(names(rules), function(r) ifelse(rules[[r]], r, NA_character_)))
  if (length(t) == 0) return(character(0))
  out <- apply(matrix(reasons, nrow = length(t)), 1, function(r) paste(r[!is.na(r)], collapse = "; "))
  out[!nzchar(out)] <- NA_character_
  out
}

# Titles that state pay ("Secretary - $14 per hour", "Substitute Teachers
# $150/day") on postings whose Posted Pay is blank: no scraper reads pay from
# the title, so the posting's own pay isn't shown. Uses extract_posted_pay()
# (k12_he_classification.R), which ignores amounts that aren't pay ("$10,000
# Hiring Incentive").
title_pay_unrecorded <- function(title, pay_min) {
  t <- as.character(title)
  has_dollar <- !is.na(t) & grepl("\\$\\s*[0-9]", t)
  out <- rep(FALSE, length(t))
  if (!any(has_dollar & is.na(pay_min))) return(out)
  idx <- which(has_dollar & is.na(pay_min))
  out[idx] <- !is.na(extract_posted_pay(t[idx])$Pay_Min)
  out
}

# hits: data.frame(source, type, title, reason). At most max_per_source
# titles are listed per source.
build_title_lint_report_markdown <- function(hits, date = Sys.Date(), max_per_source = 5) {
  if (nrow(hits) == 0) return(character(0))
  key <- paste(hits$type, hits$source, sep = "\r")
  lines <- unlist(lapply(unique(key), function(k) {
    h <- hits[key == k, , drop = FALSE]
    n_postings <- nrow(h)
    h <- h[!duplicated(h$title), , drop = FALSE]
    shown <- utils::head(h, max_per_source)
    c(sprintf("- **%s** (%s), %d posting(s):", h$source[1], h$type[1], n_postings),
      sprintf("  - \"%s\" -- %s", substr(gsub("[\r\n]+", " ", shown$title), 1, 120), shown$reason),
      if (nrow(h) > max_per_source) sprintf("  - ...and %d more", nrow(h) - max_per_source))
  }))
  c(
    paste0("### Titles that don't look like job titles (", date, ")"),
    "",
    paste("This week's postings whose title looks like something other than a job title: page text, a",
          "summary, a heading, or pay the Posted Pay column doesn't carry. Usually the scraper is reading",
          "the wrong element. These titles are classified as they are, so they can land in the wrong bucket."),
    "",
    lines,
    ""
  )
}

# One row per source: postings (n) and how many the classifier puts in Other
# (other).
other_share_counts <- function(names, is_other) {
  out <- pay_coverage_counts(names, is_other)
  names(out)[names(out) == "with_pay"] <- "other"
  out
}

# current: other_share_counts() for this run. history: a list of the same,
# one per recent archived week (the caller picks the window). A source is
# flagged when it has at least min_weeks weeks with min_postings or more
# postings, and this run has min_postings or more postings, at least
# min_other of them in Other, and an Other share at least `jump` above its
# average and at least `ratio` times it. Replayed over every archived week
# (8-week window, 2026-11-05) these defaults flag 2 MT and 2 WY source-weeks,
# all real scraper problems (Florence-Carlton schedule lines, Albany section
# headings); stricter settings flag none of them.
flag_other_share_jump <- function(current, history, min_weeks = 2, min_postings = 3, min_other = 2,
                                  jump = 0.15, ratio = 1.5) {
  empty <- data.frame(name = character(0), mean_share = numeric(0), n_weeks = integer(0),
                      n = integer(0), other = integer(0), stringsAsFactors = FALSE)
  weeks <- do.call(rbind, history)
  if (is.null(weeks) || nrow(weeks) == 0 || nrow(current) == 0) return(empty)
  weeks <- weeks[weeks$n >= min_postings, , drop = FALSE]
  if (nrow(weeks) == 0) return(empty)
  weeks$share <- weeks$other / weeks$n
  baseline <- data.frame(name = sort(unique(weeks$name)), stringsAsFactors = FALSE)
  baseline$n_weeks <- as.integer(table(weeks$name)[baseline$name])
  baseline$mean_share <- as.numeric(tapply(weeks$share, weeks$name, mean)[baseline$name])
  merged <- merge(baseline, current, by = "name")
  share <- merged$other / merged$n
  flagged <- merged[merged$n_weeks >= min_weeks & merged$n >= min_postings & merged$other >= min_other &
                      share >= merged$mean_share + jump & share >= ratio * merged$mean_share, , drop = FALSE]
  if (nrow(flagged) == 0) return(empty)
  flagged[order(-(flagged$other / flagged$n)), c("name", "mean_share", "n_weeks", "n", "other")]
}

# flagged: flag_other_share_jump() rows plus type and examples (a few of the
# source's Other titles this run).
build_other_share_report_markdown <- function(flagged, date = Sys.Date()) {
  if (nrow(flagged) == 0) return(character(0))
  c(
    paste0("### More postings than usual land in Other (", date, ")"),
    "",
    paste("These sources' share of postings the classifier can't place jumped against their own recent",
          "weeks. Usually the scraper started picking up page text instead of titles; otherwise the",
          "classifier needs a rule for a new kind of title."),
    "",
    sprintf("- **%s** (%s): usually %.0f%% Other, now %d of %d -- e.g. %s",
            flagged$name, flagged$type, 100 * flagged$mean_share, flagged$other, flagged$n, flagged$examples),
    ""
  )
}

# --------------------------------------------------------------------------
# Keyword conflicts (classification safeguards item 9)
# --------------------------------------------------------------------------
# A title carrying a word that names one bucket while the classifier put it
# in another: the hand audit of 2026-11-01, as rules. Each rule is a signal
# (regex, case-insensitive), the buckets that are fine for it, and an
# optional `unless` regex for titles where the word is legitimately
# secondary (a nutrition-services secretary, a "Secretary/Para" dual role).
# Non-blocking: listed in the drift issue so a person decides whether the
# classifier needs a rule. Tuned 2026-11-08 against every archived title
# since 2025-10-01 in both states, where only real misroutes remained.
k12_keyword_conflict_rules <- list(
  list(id = "coach aide outside Athletics",
       signal = "\\bAides?\\b.*(Football|Basketball|Volleyball|Wrestling|Soccer|\\bSwim|\\bGolf\\b|\\bTrack\\b|Cross Country|Baseball|Softball|Tennis)|(Football|Basketball|Volleyball|Wrestling|Soccer|\\bSwim|\\bGolf\\b|\\bTrack\\b|Cross Country|Baseball|Softball|Tennis).*\\bAides?\\b",
       ok = c("Athletics", "Substitute", "Activities/Extracurricular")),
  list(id = "food-service word outside Food Services",
       signal = "\\b(Kitchen|Cooks?|Nutrition|Cafeteria|Food Services?|Dishwasher)\\b",
       ok = c("Food Services", "Substitute"),
       unless = "Teacher|Instructor|Culinary|Director|Supervisor|Manager|Coordinator|Secretary|Bookkeeper|Clerk|Administrative Assistant|Driver|Warehouse|Transport|Custodian|Custodial|/"),
  list(id = "bus aide or monitor outside Transportation",
       signal = "\\bBus\\b.*\\b(Aides?|Monitors?|Para\\w*)\\b|\\b(Aides?|Monitors?)\\b.*\\bBus\\b",
       ok = c("Transportation", "Substitute"), unless = "/|\\bor\\b|&|\\band\\b"),
  list(id = "bus driver outside Transportation", signal = "\\bBus Drivers?\\b",
       ok = c("Transportation", "Substitute"), unless = "/|\\bor\\b|&|\\band\\b|Driver'?s?\\s*Ed"),
  list(id = "custodian outside Custodial/Maintenance", signal = "Custodian|Custodial|Janitor",
       ok = c("Custodial/Maintenance", "Substitute"), unless = "/|\\bor\\b|&|\\band\\b|Secretary|Supervisor|Director|Manager"),
  list(id = "teacher title in Paraprofessional", signal = "\\bTeacher\\b",
       ok = setdiff(c("Substitute", "Paraprofessional", "Activities/Extracurricular", "Transportation", "Food Services",
                      "Athletics", "Support Services", "Staff", "Custodial/Maintenance", "Administration",
                      "Part-Time Teacher", "Teacher", "Child Care", "Other"), "Paraprofessional"),
       unless = "Assistant|Aides?\\b|Associate|Para|Helper"),
  list(id = "principal or superintendent outside Administration", signal = "\\b(Principal|Superintendent)\\b",
       ok = c("Administration", "Substitute"), unless = "Secretary|Assistant to|Clerk|Administrative Assistant|Office|/"),
  list(id = "secretary or clerk outside Staff", signal = "\\b(Secretary|Clerk)\\b",
       ok = c("Staff", "Substitute"), unless = "/")
)
he_keyword_conflict_rules <- list(
  list(id = "custodian outside Maintenance & Service", signal = "Custodian|Custodial|Janitor",
       ok = c("Maintenance & Service", "Student Positions", "Temporary Position"),
       unless = "Supervisor|Director|Manager|Dish|Cook|Kitchen"),
  list(id = "postdoc outside Research & Science", signal = "Post-?doc", ok = "Research & Science"),
  list(id = "adjunct outside Adjunct/Part-Time Faculty", signal = "\\bAdjunct\\b", ok = "Adjunct/Part-Time Faculty")
)

# The first rule each title trips, or NA.
find_keyword_conflicts <- function(title, bucket, rules) {
  title <- as.character(title)
  out <- rep(NA_character_, length(title))
  for (r in rules) {
    hit <- grepl(r$signal, title, ignore.case = TRUE, perl = TRUE) & !(bucket %in% r$ok)
    if (!is.null(r$unless)) hit <- hit & !grepl(r$unless, title, ignore.case = TRUE, perl = TRUE)
    out[hit & is.na(out)] <- r$id
  }
  out
}

# hits: data.frame(source, type, title, bucket, rule).
build_keyword_conflict_report_markdown <- function(hits, date = Sys.Date(), max_rows = 25) {
  if (nrow(hits) == 0) return(character(0))
  h <- hits[!duplicated(paste(hits$type, hits$title)), , drop = FALSE]
  shown <- utils::head(h, max_rows)
  c(
    paste0("### Titles whose words point to a different bucket (", date, ")"),
    "",
    paste("This week's postings whose title names one kind of job while the classifier put it in another",
          "(a custodian outside Custodial, a bus aide outside Transportation). Either the classifier needs",
          "a rule, or the title is a real exception; add it to the golden set either way."),
    "",
    sprintf("- \"%s\" (%s, %s): %s -- %s", substr(gsub("[\r\n]+", " ", shown$title), 1, 120), shown$source,
            shown$type, shown$bucket, shown$rule),
    if (nrow(h) > max_rows) sprintf("- ...and %d more", nrow(h) - max_rows),
    ""
  )
}

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
