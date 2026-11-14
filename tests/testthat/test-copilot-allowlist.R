# Keeps .github/copilot-allowlist.txt in sync with every host a scraper
# actually hits. The Copilot cloud agent (scraper auto-fix issues -- see
# drift_check.R's Tier 3) works behind a firewall; a host missing from
# the allowlist means it can't fetch the live page it's been asked to fix.
# Ported from the Montana dashboard; Wyoming's sources also live in
# misc_district_registry, he_institution_urls and hardcoded Rmd calls.

read_allowlist <- function() {
  lines <- trimws(readLines(here::here(".github", "copilot-allowlist.txt"), warn = FALSE))
  tolower(lines[nzchar(lines) & !startsWith(lines, "#")])
}

url_hosts <- function(urls) {
  urls <- urls[!is.na(urls) & grepl("^https?://", urls)]
  unique(sub("^www[.]", "", tolower(sub("^https?://([^/:?#]+).*$", "\\1", urls))))
}

covered_by <- function(host, allowlist) {
  any(host == allowlist | endsWith(host, paste0(".", allowlist)))
}

expect_all_covered <- function(hosts) {
  allow <- read_allowlist()
  missing <- hosts[!vapply(hosts, covered_by, logical(1), allowlist = allow)]
  expect_equal(missing, character(0),
               info = "add these to .github/copilot-allowlist.txt AND the repo's Copilot firewall settings")
}

test_that("every registry, misc-district and HE page host is on the Copilot allowlist", {
  k12 <- read.csv(here::here("k12_district_registry.csv"), stringsAsFactors = FALSE)
  expect_all_covered(url_hosts(c(k12$Job_Link, misc_district_registry$url, he_institution_urls)))
})

test_that("every URL hardcoded in a scraper file or the pipeline Rmd is on the Copilot allowlist", {
  files <- here::here(c("direct_api_scrapers.R", "misc_district_scrapers.R", "Wy_ED_Jobs.Rmd"))
  text <- unlist(lapply(files, readLines, warn = FALSE))
  urls <- unlist(regmatches(text, gregexpr("https?://[A-Za-z0-9.-]+", text)))
  # w3.org only appears in XML namespace strings, never fetched.
  expect_all_covered(setdiff(url_hosts(urls), "w3.org"))
})

# Hosts a chromote-rendered platform's pages load assets from. They never
# appear in a registry URL or a scraper file, so the tests above can't see
# them -- but blocking them leaves the agent a page with no postings. The
# Apptegy pair came from the firewall's blocked-host report on the Montana
# dashboard's first auto-fix session.
RENDER_ASSET_HOSTS <- list(apptegy = c("apptegy.net", "5il.co"))

test_that("every chromote platform's render-time asset hosts are on the Copilot allowlist", {
  platforms <- intersect(names(RENDER_ASSET_HOSTS), misc_district_registry$platform)
  expect_all_covered(unlist(RENDER_ASSET_HOSTS[platforms], use.names = FALSE))
})

test_that("covered_by matches subdomains but not look-alike suffixes", {
  expect_true(covered_by("crook1.schoolspring.com", "schoolspring.com"))
  expect_true(covered_by("schoolspring.com", "schoolspring.com"))
  expect_false(covered_by("evilschoolspring.com", "schoolspring.com"))
})
