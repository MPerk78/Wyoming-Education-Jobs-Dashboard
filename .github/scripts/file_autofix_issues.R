# Tier 3 of the drift-alert system (ported from the Montana dashboard):
# one issue per "likely_broken" source, handed to an agent that can open a
# fix PR -- Claude (dispatches claude-autofix.yml, the default) or the Copilot
# coding agent, picked by the AUTOFIX_ENGINE repo variable (autofix_engine()
# in drift_check.R). See the "Tier 3" block in drift_check.R for which
# verdicts are eligible and why.
#
# Reads /tmp/drift_results.csv (written by corroborate_drift.R, including
# the page text it rendered in CI -- see gather_autofix_evidence()). No file ->
# nothing was flagged this run, which is also the signal that every open
# auto-fix issue's source has recovered.
#
# Per run:
#   - likely_broken, no open issue   -> create one, assign to Copilot
#   - likely_broken, issue open      -> comment "still flagged"
#   - open issue, source not flagged -> close (recovered)
#   - open issue, genuinely empty    -> close (nothing left to parse)
#   - open issue, confirmed_broken / inconclusive -> comment, leave open
#
# GH_TOKEN (the workflow's own token) does the issue work. Assigning to
# Copilot needs a user token -- COPILOT_TOKEN -- since the Actions token
# can't start a coding-agent session. Without it, issues are still filed,
# just not assigned.

source("drift_check.R")
suppressMessages({ library(httr2); library(rvest); library(dplyr) })
library(edjobs)
source("direct_api_scrapers.R")   # applitrack_tenant_path() etc., for resolve_scraper_call()
source("misc_district_scrapers.R") # misc_district_registry

# A shared-platform outage could flag many sources at once; that's a human
# problem, not one Copilot session per district.
MAX_NEW_ISSUES_PER_RUN <- 3

repo <- Sys.getenv("GITHUB_REPOSITORY")
run_url <- if (nzchar(repo)) sprintf("%s/%s/actions/runs/%s", Sys.getenv("GITHUB_SERVER_URL"), repo, Sys.getenv("GITHUB_RUN_ID")) else NULL
copilot_token <- Sys.getenv("COPILOT_TOKEN")
engine <- autofix_engine(Sys.getenv("AUTOFIX_ENGINE"))
cat("Auto-fix engine:", engine, "\n")
today <- as.character(Sys.Date())

gh <- function(args, token = NULL) {
  env <- if (is.null(token)) character(0) else paste0("GH_TOKEN=", token)
  # system2() pastes args into a shell command unquoted -- titles and
  # comments have spaces and parens, so quote every one.
  out <- suppressWarnings(system2("gh", shQuote(c(args, "--repo", repo)), stdout = TRUE, stderr = TRUE, env = env))
  status <- attr(out, "status")
  list(ok = is.null(status) || status == 0, out = out)
}

gh_comment <- function(number, text) {
  f <- tempfile(fileext = ".md"); writeLines(text, f)
  gh(c("issue", "comment", number, "--body-file", f))
}

results <- if (file.exists("/tmp/drift_results.csv")) {
  read.csv("/tmp/drift_results.csv", stringsAsFactors = FALSE)
} else {
  data.frame(name = character(0), verdict = character(0))
}
eligible <- results[results$verdict %in% AUTOFIX_ELIGIBLE_VERDICTS, , drop = FALSE]

invisible(gh(c("label", "create", AUTOFIX_LABEL, "--color", "D93F0B", "--force",
     "--description", "Per-source scraper fix, auto-filed by the weekly drift check")))

listed <- gh(c("issue", "list", "--label", AUTOFIX_LABEL, "--state", "open",
               "--limit", "100", "--json", "number,body"))
if (!listed$ok) stop("gh issue list failed: ", paste(listed$out, collapse = "\n"))
open_issues <- jsonlite::fromJSON(paste(listed$out, collapse = "\n"))
if (length(open_issues) == 0) open_issues <- data.frame(number = integer(0), body = character(0))
open_issues$source <- vapply(open_issues$body, parse_autofix_marker, character(1), USE.NAMES = FALSE)

k12 <- read.csv("k12_district_registry.csv", stringsAsFactors = FALSE)

# ---- new / still-flagged sources ------------------------------------------
created <- 0
for (i in seq_len(nrow(eligible))) {
  row <- eligible[i, ]
  existing <- open_issues$number[open_issues$source %in% row$name]

  if (length(existing) > 0) {
    gh_comment(existing[1], sprintf("Still flagged as of %s: scraper returned %d vs. a %.1f/week baseline.",
                                    today, as.integer(row$count), row$mean_count))
    cat("Commented on #", existing[1], " (", row$name, ")\n", sep = "")
    next
  }

  if (created >= MAX_NEW_ISSUES_PER_RUN) {
    cat("Cap of", MAX_NEW_ISSUES_PER_RUN, "new auto-fix issues reached; skipping", row$name, "\n")
    next
  }

  call <- resolve_scraper_call(row$name, k12, misc_district_registry)
  page_text <- if ("page_text" %in% names(row)) row$page_text else NA_character_
  evidence <- tryCatch(gather_autofix_evidence(call, row$name, page_text),
                       error = function(e) { cat("  Couldn't gather evidence:", conditionMessage(e), "\n"); NULL })
  body_file <- tempfile(fileext = ".md")
  writeLines(build_autofix_issue_body(row, call, run_url, evidence), body_file, useBytes = TRUE)
  made <- gh(c("issue", "create", "--title", autofix_issue_title(row$name),
               "--label", AUTOFIX_LABEL, "--body-file", body_file))
  if (!made$ok) { cat("Failed to create issue for", row$name, ":", made$out, "\n"); next }
  created <- created + 1
  number <- sub(".*/issues/([0-9]+).*", "\\1", tail(made$out, 1))
  cat("Created #", number, " for ", row$name, "\n", sep = "")

  if (engine == "none") next
  if (engine == "claude") {
    # The Actions token can dispatch a workflow (unlike issue events it
    # creates, which never trigger one) -- needs `actions: write`.
    sent <- gh(c("workflow", "run", AUTOFIX_CLAUDE_WORKFLOW, "-f", paste0("issue_number=", number)))
    if (sent$ok) {
      cat("  Dispatched", AUTOFIX_CLAUDE_WORKFLOW, "\n")
    } else {
      cat("  Dispatch failed:", sent$out, "\n")
      gh_comment(number, paste0("Couldn't start the Claude auto-fix workflow automatically (`", paste(sent$out, collapse = " "),
                                "`). Run **", AUTOFIX_CLAUDE_WORKFLOW, "** from the Actions tab with this issue number, ",
                                "or check that the weekly workflow has `actions: write`."))
    }
    next
  }
  if (!nzchar(copilot_token)) {
    cat("  COPILOT_TOKEN unset -- left unassigned\n")
    next
  }
  assigned <- gh(c("issue", "edit", number, "--add-assignee", "@copilot"), token = copilot_token)
  if (assigned$ok) {
    cat("  Assigned to Copilot\n")
  } else {
    cat("  Copilot assignment failed:", assigned$out, "\n")
    gh_comment(number, paste0("Couldn't assign this to Copilot automatically (`", paste(assigned$out, collapse = " "),
                              "`). Assign it by hand, or check that COPILOT_TOKEN is valid and the Copilot coding agent is enabled for this repo."))
  }
}

# ---- open issues whose source is no longer likely_broken ------------------
for (i in seq_len(nrow(open_issues))) {
  src <- open_issues$source[i]
  number <- open_issues$number[i]
  if (is.na(src) || src %in% eligible$name) next

  verdict <- results$verdict[results$name == src]
  verdict <- if (length(verdict) == 0) "not_flagged" else verdict[1]

  if (verdict %in% c("not_flagged", "looks_genuinely_empty")) {
    msg <- if (verdict == "not_flagged") {
      sprintf("Closing: %s is no longer flagged by the drift check as of %s (its count is back within range of its baseline).", src, today)
    } else {
      sprintf("Closing: as of %s the live page for %s has no postings either, so there's nothing left for the scraper to miss. Reopen if it comes back.", today, src)
    }
    closed <- gh(c("issue", "close", number, "--comment", msg))
    cat(if (closed$ok) "Closed #" else "FAILED to close #", number, " (", src, ": ", verdict, ")\n", sep = "")
  } else {
    gh_comment(number, sprintf("As of %s, %s is now `%s` rather than likely-broken -- leaving open for a human look.", today, src, verdict))
    cat("Commented on #", number, " (", src, ": ", verdict, ")\n", sep = "")
  }
}
