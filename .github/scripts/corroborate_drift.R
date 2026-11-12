# Tier 2 of the drift-alert system. Only runs when check_drift.R flagged
# something. For each flagged source, renders its live public page with
# chromote (same technique as the manual 2026-08-03 spot-check that found
# the Applitrack bug) and scores the visible text with
# score_page_text_for_job_signal() -- UNLESS check_drift.R already attached
# a real scrape_error (this run's own safe_scrape() logged a live HTTP
# failure for that source, not just an empty result -- see drift_check.R's
# attach_scrape_log_errors()), in which case that's a stronger, cheaper
# signal than a page-text guess and the render is skipped entirely: a
# broken/migrated tenant can still read as a normal, well-formed page when
# rendered fresh in chromote -- scoring its text would misreport it as
# "looks_genuinely_empty" or "inconclusive" instead of "this scraper is
# erroring." Writes /tmp/drift_report.md -- only created if there's
# something worth a human looking at; a report full of nothing but
# looks_genuinely_empty isn't worth alerting on.
#
# When GEMINI_API_KEY is set, it also runs the already-rendered page text
# through the LLM extractor (llm_titles_from_page_text) and folds the result
# into the verdict via combine_verdict_with_llm(): an "inconclusive" text
# score plus real LLM-read titles becomes "likely_broken" with the exact
# postings the scraper is missing listed in the issue. Ported from the
# Montana dashboard, where it replaced a by-hand diagnosis. No key -> this
# step is a no-op and the behaviour is unchanged.

source("drift_check.R")
source("misc_district_scrapers.R") # for misc_district_registry
source("k12_he_classification.R") # for canonicalize_k12_district
library(edjobs)
library(edjobs)      # for llm_titles_from_page_text
library(chromote)

flagged <- read.csv("/tmp/drift_flagged.csv", stringsAsFactors = FALSE)

if (nrow(flagged) == 0) {
  cat("Nothing flagged, no corroboration to do.\n")
  quit(status = 0)
}

if (!"scrape_error" %in% names(flagged)) flagged$scrape_error <- NA_character_
if (!"safety_net_vacancies" %in% names(flagged)) flagged$safety_net_vacancies <- NA_integer_

combined_raw <- if (file.exists("Wy_Ed_Jobs/combinedclean.csv")) read.csv("Wy_Ed_Jobs/combinedclean.csv", stringsAsFactors = FALSE) else NULL
registry <- if (file.exists("k12_district_registry.csv")) read.csv("k12_district_registry.csv", stringsAsFactors = FALSE) else NULL
scrape_log <- if (file.exists("scrape_log.csv")) read.csv("scrape_log.csv", stringsAsFactors = FALSE) else NULL

safety_net_summary <- summarize_central_safety_net(
  combined_df = combined_raw,
  registry = registry,
  scrape_log = scrape_log,
  feed_label = WSBA_STATEWIDE_SOURCE,
  feed_short_name = "WSBA",
  discrepancy_path = "wy_discrepancy_report.csv"
)

url_lookup <- build_source_url_lookup()
# check_drift.R canonicalizes district names before flagging, so a flagged
# K-12 name is the corrected spelling ("Converse County School District 2")
# while these lookup keys come straight from the link CSVs, which still
# carry the scraper's raw spelling ("Converse County School Distrcit 2").
# Canonicalize the keys to match, or every corrected-name district would
# fall through to "No URL on file". (canonicalize_k12_district() passes HE
# institution names through untouched.)
names(url_lookup) <- canonicalize_k12_district(names(url_lookup))
url_lookup <- url_lookup[!duplicated(names(url_lookup), fromLast = TRUE)]

b <- NULL
get_session <- function() {
  if (is.null(b)) b <<- ChromoteSession$new()
  b
}
results <- data.frame(name = character(0), type = character(0), mean_count = numeric(0),
                       count = numeric(0), url = character(0), verdict = character(0),
                       error_message = character(0), llm_note = character(0),
                       llm_titles = character(0), page_text = character(0),
                       safety_net_vacancies = integer(0),
                       stringsAsFactors = FALSE)

for (i in seq_len(nrow(flagged))) {
  row <- flagged[i, ]
  url <- unname(url_lookup[row$name])
  if (is.na(url)) url <- NULL
  llm_note <- NA_character_
  llm_titles <- character(0)
  text <- NA_character_

  fb_count <- if (!is.na(row$safety_net_vacancies)) {
    as.integer(row$safety_net_vacancies)
  } else if (!is.null(safety_net_summary$fallback_districts[[row$name]])) {
    as.integer(safety_net_summary$fallback_districts[[row$name]])
  } else {
    0L
  }

  if (!is.na(row$scrape_error)) {
    verdict <- "confirmed_broken"
  } else if (is.null(url)) {
    verdict <- "no_url_available"
  } else {
    text <- tryCatch({
      s <- get_session()
      s$Page$navigate(url, wait_ = TRUE)
      s$Page$loadEventFired(wait_ = TRUE)
      Sys.sleep(2.5)
      s$Runtime$evaluate("document.body.innerText")$result$value
    }, error = function(e) NA_character_)
    verdict <- score_page_text_for_job_signal(text)

    # The page is already rendered -- ask an LLM what postings it sees, and
    # fold that into the verdict (no key -> no-op).
    llm_titles <- tryCatch(llm_titles_from_page_text(text, url = url),
                           error = function(e) character(0))
    combined <- combine_verdict_with_llm(verdict, llm_titles,
                                         tryCatch(recent_source_titles(row$name, row$type), error = function(e) NULL))
    verdict <- combined$verdict
    llm_note <- combined$note
  }

  # If a K-12 scraper was flagged and text signal scored empty, but Central Safety Net
  # is actively preserving postings for it on the statewide feed, the scraper is broken.
  if (verdict == "looks_genuinely_empty" && fb_count > 0) {
    verdict <- "likely_broken"
    llm_note <- sprintf("the statewide feed lists %d posting(s) for it that its own board doesn't", fb_count)
  }

  cat(sprintf("%-40s baseline=%.1f current=%d -> %s%s\n", row$name, row$mean_count,
              row$count, verdict, if (is.na(llm_note)) "" else paste0("  [", llm_note, "]")))
  results <- rbind(results, data.frame(
    name = row$name, type = row$type, mean_count = row$mean_count,
    count = row$count, url = if (is.null(url)) NA_character_ else url,
    verdict = verdict, error_message = row$scrape_error, llm_note = llm_note,
    llm_titles = paste(llm_titles, collapse = " | "),
    # Kept for the auto-fix issue: the agent's firewalled sandbox can't be
    # trusted to render the page the same way.
    page_text = if (length(text) == 1) text else NA_character_,
    safety_net_vacancies = fb_count,
    stringsAsFactors = FALSE
  ))
}

if (!is.null(b)) invisible(b$close())

# Machine-readable copy for file_autofix_issues.R (per-source Copilot
# auto-fix issues) -- written unconditionally, since that step also needs
# to see which sources are no longer broken in order to close their issues.
write.csv(results, "/tmp/drift_results.csv", row.names = FALSE)

actionable <- results[results$verdict %in% c("confirmed_broken", "likely_broken", "inconclusive", "no_url_available"), ]

if (nrow(actionable) == 0) {
  cat("All flagged sources corroborated as genuinely empty -- nothing to report.\n")
  quit(status = 0)
}

lines <- build_drift_report_markdown(
  actionable = actionable,
  safety_net_summary = safety_net_summary,
  date = Sys.Date(),
  feed_short_name = "WSBA",
  intro_explanation = paste("A source is flagged when it has at least 2 weeks of history averaging real postings, and this week's count dropped to 20% or less of that average.",
                            "This is the same failure signature that hid ~200 real postings across 10 Applitrack districts on 2026-08-03 (a Windows-1252 encoding bug, not real absence of jobs) -- these entries are corroborated against the actual live page but still need a human look.")
)

writeLines(lines, "/tmp/drift_report.md")
cat("Report written to /tmp/drift_report.md\n")
