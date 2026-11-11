# One-command repro for a single scraper, named the way the drift check
# names sources (canonical district / institution name):
#
#   Rscript scripts/repro_scraper.R "Niobrara County School District 1"
#   Rscript scripts/repro_scraper.R "Casper College"
#   Rscript scripts/repro_scraper.R "Niobrara County School District 1" --save-text /tmp/niobrara.txt
#
# Prints which fetch_* function the source resolves to (drift_check.R's
# resolve_scraper_call(): k12_district_registry.csv, misc_district_registry,
# or a hardcoded Wy_ED_Jobs.Rmd call), its fixtures, runs the tests for its
# fetch_*/parse_* functions, then runs the scraper against the live site
# and prints what it found. For innerText-based chromote scrapers it also
# saves the rendered page text (--save-text, default /tmp/<fetch_fn>_live.txt)
# in the exact form a fixture takes.
#
# Ported from the Montana dashboard, written for the Copilot coding agent's
# scraper auto-fix issues. Uses the same resolve/run path as the PR live
# check, so "works here" means "works there".

suppressMessages({
  library(httr2); library(rvest); library(dplyr); library(chromote); library(here)
})
setwd(here::here())
library(edjobs)
source("direct_api_scrapers.R")
source("misc_district_scrapers.R")
source("drift_check.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop('usage: Rscript scripts/repro_scraper.R "<source name>" [--save-text PATH]')
source_name <- args[1]
save_at <- which(args == "--save-text")
save_path <- if (length(save_at) == 1 && length(args) > save_at) args[save_at + 1] else NULL

k12 <- read.csv("k12_district_registry.csv", stringsAsFactors = FALSE)
call <- resolve_scraper_call(source_name, k12, misc_district_registry)
if (is.null(call)) stop("No single-source scraper for '", source_name, "' (see resolve_scraper_call() in drift_check.R)")
evidence <- gather_autofix_evidence(call, source_name, NA_character_)
live_url <- unname(build_source_url_lookup(misc_registry = misc_district_registry)[source_name])

cat("== ", source_name, " ==\n", sep = "")
cat("Platform:     ", call$platform, "(from", call$where, ")\n")
cat("Entry point:  ", describe_scraper_call(call), "\n")
cat("Parses:       ", if (evidence$inner_text) "document.body.innerText (text fixtures)" else "raw HTML / API response", "\n")
cat("Live URL:     ", if (is.na(live_url)) "(none on file)" else live_url, "\n")
cat("Fixtures:     ", if (length(evidence$fixtures)) paste(evidence$fixtures, collapse = ", ") else "(none for this source)", "\n")
others <- setdiff(evidence$platform_fixtures, evidence$fixtures)
if (length(others)) cat("Same parser:  ", paste(others, collapse = ", "), "\n")
cat("\n")

# ---- fixture tests --------------------------------------------------------
stem <- sub("^fetch_(.*?)_(postings|atom|jobs)$", "\\1", call$fn)
pattern <- sprintf("(fetch|parse)_%s_", stem)
for (f in list.files("tests/testthat", pattern = "^test-.*[.]R$", full.names = TRUE)) {
  lines <- readLines(f, warn = FALSE)
  descs <- sub('^\\s*test_that\\("(.*?)",.*$', "\\1", grep("^\\s*test_that\\(", lines, value = TRUE), perl = TRUE)
  for (d in descs[grepl(pattern, descs)]) {
    cat("-- test:", basename(f), "::", d, "\n")
    testthat::test_file(f, desc = d, reporter = "summary")
  }
}

# ---- live run -------------------------------------------------------------
# The Copilot agent's firewall intercepts TLS; headless Chrome rejects its
# certificate unless told not to. Sandbox-only -- never in a scraper.
in_copilot_sandbox <- identical(Sys.getenv("COPILOT_AGENT_FIREWALL_ENABLED"), "true")
if (call$session && in_copilot_sandbox) {
  chromote::set_chrome_args(c(chromote::get_chrome_args(), "--ignore-certificate-errors"))
}

cat("\n-- live run of", describe_scraper_call(call), "\n")
result <- tryCatch(run_scraper_call(call, session_factory = function() ChromoteSession$new()),
                   error = function(e) e)
if (inherits(result, "condition")) {
  cat("ERRORED:", conditionMessage(result), "\n")
} else {
  titles <- scraper_titles(result)
  cat(length(titles), "posting(s):\n")
  if (length(titles)) cat(paste0("  - ", titles), sep = "\n")
}

if (call$session && evidence$inner_text && !is.na(live_url)) {
  if (is.null(save_path)) save_path <- file.path("/tmp", paste0(call$fn, "_live.txt"))
  s <- ChromoteSession$new()
  text <- tryCatch({
    s$Page$navigate(live_url, wait_ = TRUE)
    s$Page$loadEventFired(wait_ = TRUE, timeout_ = 30)
    Sys.sleep(2.5)
    s$Runtime$evaluate("document.body.innerText")$result$value
  }, error = function(e) NA_character_, finally = tryCatch(s$close(), error = function(e) NULL))
  if (length(text) == 1 && !is.na(text)) {
    writeLines(text, save_path, useBytes = TRUE)
    cat("\nRendered page text (", nchar(text), " chars) saved to ", save_path, "\n", sep = "")
  } else {
    cat("\nCouldn't render the page for its text.\n")
  }
  if (in_copilot_sandbox) {
    cat("NOTE: you're behind the Copilot firewall. If this render shows fewer postings than the issue's",
        "'Full page text captured in CI', trust the CI capture: a blocked asset host, not the site, is",
        "the likelier cause. Don't change how the scraper fetches the page to work around it.\n", sep = "\n")
  }
}
