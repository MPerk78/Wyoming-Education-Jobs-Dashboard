# Live check for scraper-fix PRs (pr-tests.yml). The fixture tests prove
# the parser handles the fixture; they can't prove the fixture is what the
# live site actually serves -- and on an auto-fix PR, the same agent wrote
# both. So: find which source the PR claims to fix (its linked issue's
# autofix-source marker -- see drift_check.R's Tier 3), re-run just that
# scraper against the live site with the PR's code, and report.
#
# Writes /tmp/live_check.md (posted as a PR comment by the workflow) and
# exits non-zero only on the unambiguous failures summarize_live_check()
# defines. A PR with no linked auto-fix issue is simply not applicable.

suppressMessages({
  library(httr2); library(rvest); library(dplyr); library(chromote)
})
library(edjobs)
source("direct_api_scrapers.R")
source("misc_district_scrapers.R")
source("drift_check.R")

repo <- Sys.getenv("GITHUB_REPOSITORY")
pr <- Sys.getenv("PR_NUMBER")

gh_json <- function(args) {
  out <- system2("gh", shQuote(c(args, "--repo", repo)), stdout = TRUE)
  jsonlite::fromJSON(paste(out, collapse = "\n"))
}

pr_info <- gh_json(c("pr", "view", pr, "--json", "closingIssuesReferences,body"))
linked <- pr_info$closingIssuesReferences$number
# Fallback for a PR that mentions its issue without a closing keyword.
if (length(linked) == 0 && !is.null(pr_info$body)) {
  linked <- as.integer(unique(regmatches(pr_info$body, gregexpr("(?<=#)[0-9]+", pr_info$body, perl = TRUE))[[1]]))
}

source_name <- NA_character_
issue_body <- ""
for (n in linked) {
  body <- tryCatch(gh_json(c("issue", "view", n, "--json", "body"))$body, error = function(e) "")
  src <- parse_autofix_marker(body)
  if (!is.na(src)) { source_name <- src; issue_body <- body; break }
}

if (is.na(source_name)) {
  cat("No linked scraper auto-fix issue -- live check not applicable.\n")
  quit(status = 0)
}

k12 <- read.csv("k12_district_registry.csv", stringsAsFactors = FALSE)
call <- resolve_scraper_call(source_name, k12, misc_district_registry)

if (is.null(call)) {
  writeLines(c(sprintf("### Live scraper check: %s", source_name), "",
               ":grey_question: No single-source live check exists for this source (see resolve_scraper_call() in drift_check.R) -- verify the fix against the live page by hand."),
             "/tmp/live_check.md")
  quit(status = 0)
}

cat("Running", describe_scraper_call(call), "for", source_name, "against the live site\n")
result <- tryCatch(run_scraper_call(call, session_factory = function() ChromoteSession$new()),
                   error = function(e) e)
summary <- summarize_live_check(source_name, result, parse_autofix_expected_titles(issue_body))
writeLines(summary$markdown, "/tmp/live_check.md")
cat(summary$markdown, sep = "\n")
quit(status = if (summary$pass) 0 else 1)
