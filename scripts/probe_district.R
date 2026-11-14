#!/usr/bin/env Rscript
# scripts/probe_district.R
# Probes a school or district URL, detects CMS and ATS platform signatures,
# checks for bot-gates (Fastly Client Challenge, Cloudflare), and extracts
# candidate job posting titles and links without flooding the LLM context.
#
# Usage:
#   Rscript scripts/probe_district.R "<URL>"

suppressMessages({
  library(httr2)
  library(rvest)
  library(xml2)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cat("Usage:\n")
  cat("  Rscript scripts/probe_district.R <URL>\n")
  quit(status = 1)
}

url <- args[1]
cat(sprintf("=== Probing Target: %s ===\n", url))

req <- request(url) %>%
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36") %>%
  req_timeout(20) %>%
  req_error(is_error = function(resp) FALSE)

resp <- tryCatch({
  req_perform(req)
}, error = function(e) {
  cat(sprintf("[ERROR] Network fetch failed: %s\n", e$message))
  quit(status = 1)
})

status <- resp_status(resp)
final_url <- resp_url(resp)
cat(sprintf("HTTP Status: %d\n", status))
cat(sprintf("Final URL:   %s\n", final_url))

body_text <- resp_body_string(resp)

# 1. Bot-gate detection
is_bot_gated <- FALSE
gate_reason <- ""
if (status == 403) {
  is_bot_gated <- TRUE
  gate_reason <- "HTTP 403 Forbidden"
} else if (grepl("Fastly Client Challenge|_fs-ch-", body_text, ignore.case = TRUE)) {
  is_bot_gated <- TRUE
  gate_reason <- "Apptegy Fastly Client Challenge JS bot-gate"
} else if (grepl("cf-browser-verification|challenge-platform|Cloudflare", body_text, ignore.case = TRUE) && status >= 400) {
  is_bot_gated <- TRUE
  gate_reason <- "Cloudflare Challenge"
}

if (is_bot_gated) {
  cat(sprintf("[WARNING] Bot-gate detected: %s!\n", gate_reason))
  cat("  -> Static httr2 scraping will NOT work. Requires chromote headless session or dynamic bypass.\n\n")
}

# 2. Platform Fingerprinting
platforms <- character(0)
# ATS checks
if (grepl("applitrack\\.com/([A-Za-z0-9_-]+)/onlineapp", body_text, ignore.case = TRUE)) {
  m <- regmatches(body_text, regexec("applitrack\\.com/([A-Za-z0-9_-]+)/onlineapp", body_text, ignore.case = TRUE))
  platforms <- c(platforms, sprintf("AppliTrack (ATS slug: %s)", m[[1]][2]))
}
if (grepl("([A-Za-z0-9-]+)\\.schoolspring\\.com", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "SchoolSpring (ATS)")
}
if (grepl("tedk12\\.com", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "TedK12 (ATS)")
}
if (grepl("tylerportico\\.com", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "TylerPortico (ATS)")
}
if (grepl("redroverk12\\.com", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "RedRoverK12 (ATS)")
}

# CMS checks
if (grepl("edliocdn|uREC_ID=|/apps/pages/|\\bedlio\\b", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "Edlio (CMS)")
}
if (grepl("sites\\.google\\.com", body_text, ignore.case = TRUE) || grepl("sites\\.google\\.com", final_url)) {
  platforms <- c(platforms, "Google Sites (CMS)")
}
if (grepl("weebly\\.com|wsite-", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "Weebly (CMS)")
}
if (grepl("apptegy\\.net|thrillshare|5il\\.co|aptg\\.co", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "Apptegy (CMS)")
}
if (grepl("wp-content|wp-json|divi", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "WordPress / Divi (CMS)")
}
if (grepl("schoolblocks", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "SchoolBlocks (CMS)")
}
if (grepl("gabbart", body_text, ignore.case = TRUE)) {
  platforms <- c(platforms, "Gabbart (CMS)")
}

if (length(platforms) == 0) {
  platforms <- "Custom / Unidentified HTML"
}
cat(sprintf("Detected Platform: %s\n\n", paste(platforms, collapse = ", ")))

# 3. Content Analysis
doc <- tryCatch(read_html(body_text), error = function(e) NULL)
if (is.null(doc)) {
  cat("[WARNING] Could not parse body as valid HTML XML document.\n")
  quit(status = 0)
}

page_title <- html_text2(xml_find_first(doc, "//title"))
cat(sprintf("Page Title: %s\n\n", page_title))

# Look for candidate job titles / listings
job_regex <- "(teacher|para|aide|substitute|driver|coach|custodian|clerk|superintendent|principal|special education|certified|classified|vacancy|position|opening|employment|elementary|middle|high school)"
stop_words <- c("apply online", "click here", "job opportunities", "employment opportunities",
                "back to top", "home", "calendar", "staff directory", "menu", "search",
                "district office", "mission statement", "board of trustees")

# Extract text nodes from headings, list items, table cells, and paragraphs
candidate_nodes <- xml_find_all(doc, "//h1 | //h2 | //h3 | //h4 | //h5 | //li | //td | //p")
texts <- html_text2(candidate_nodes)
texts <- trimws(texts)
texts <- texts[nzchar(texts)]

# Filter for texts matching job regex, not too long, not pure stop words
matching_texts <- character(0)
for (t in texts) {
  # Split multi-line text into lines
  lines <- trimws(strsplit(t, "[\r\n]+")[[1]])
  for (line in lines) {
    if (nchar(line) >= 4 && nchar(line) <= 120) {
      if (grepl(job_regex, line, ignore.case = TRUE)) {
        lower_line <- tolower(line)
        if (!lower_line %in% stop_words && !startsWith(lower_line, "http")) {
          matching_texts <- c(matching_texts, line)
        }
      }
    }
  }
}

matching_texts <- unique(matching_texts)

cat("--- Candidate Positions / Text Snippets (Top 15) ---\n")
if (length(matching_texts) == 0) {
  cat("  (No direct job-like text found in static HTML)\n")
} else {
  preview <- head(matching_texts, 15)
  for (i in seq_along(preview)) {
    cat(sprintf("  [%02d] %s\n", i, preview[i]))
  }
}
cat("\n")

# 4. Candidate Job Links (PDFs, external application forms, subpages)
links <- xml_find_all(doc, "//a[@href]")
hrefs <- xml_attr(links, "href")
link_texts <- html_text2(links)

useful_links <- data.frame(
  text = trimws(link_texts),
  href = trimws(hrefs),
  stringsAsFactors = FALSE
)
useful_links <- useful_links[nzchar(useful_links$href), ]

# Filter to links mentioning jobs or pointing to PDFs/ATS
is_job_link <- grepl(job_regex, useful_links$text, ignore.case = TRUE) |
               grepl("job|employ|career|opening|vacanc|application", useful_links$href, ignore.case = TRUE) |
               grepl("\\.pdf$", useful_links$href, ignore.case = TRUE)

useful_links <- useful_links[is_job_link, ]
useful_links <- unique(useful_links)

cat("--- Relevant Links / Documents (Top 10) ---\n")
if (nrow(useful_links) == 0) {
  cat("  (No direct job links found)\n")
} else {
  preview_links <- head(useful_links, 10)
  for (i in seq_len(nrow(preview_links))) {
    cat(sprintf("  [%02d] %s -> %s\n", i, preview_links$text[i], preview_links$href[i]))
  }
}

if (!is_bot_gated) {
  domain <- sub("^www\\.", "", tolower(sub("^https?://([^/:?#]+).*$", "\\1", final_url)))
  suggested_slug <- tolower(gsub("[^a-zA-Z0-9]", "", strsplit(domain, "\\.")[[1]][1]))
  suggested_cms <- "generic"
  if (any(grepl("weebly", platforms, ignore.case = TRUE))) {
    suggested_cms <- "weebly"
  } else if (any(grepl("wordpress|divi", platforms, ignore.case = TRUE))) {
    suggested_cms <- "wordpress"
  } else if (any(grepl("edlio", platforms, ignore.case = TRUE))) {
    suggested_cms <- "edlio"
  } else if (any(grepl("google sites", platforms, ignore.case = TRUE))) {
    suggested_cms <- "google_sites"
  } else if (any(grepl("finalsite", platforms, ignore.case = TRUE))) {
    suggested_cms <- "finalsite"
  }

  cat("\n--- Suggested Tool Commands ---\n")
  cat(sprintf("Capture fixture:\n  Rscript scripts/capture_fixture.R --url \"%s\" --slug \"%s\"\n\n", final_url, suggested_slug))
  cat(sprintf("Scaffold district:\n  Rscript scripts/scaffold_district.R --district \"<Name>\" --city \"<City>\" --county \"<County>\" --lat <lat> --lon <lon> --url \"%s\" --slug \"%s\" --cms \"%s\"\n", final_url, suggested_slug, suggested_cms))
}

cat("\n=== Probe Complete ===\n")
