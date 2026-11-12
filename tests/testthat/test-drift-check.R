
# ---- auto-fix evidence (the CI capture handed to the agent) ----------------
# Montana's first auto-fix (its issue #7) failed because the agent's own
# render of an Apptegy page came up empty behind its firewall. The issue now
# carries the text the drift check rendered in CI. autofix_niobrara_
# innertext_2026-09-30.txt is that text for Niobrara 1, captured live with
# scripts/repro_scraper.R; against apptegy_niobrara_rendered.txt it shows
# the real posting churn in ~20 lines.

niobrara_capture <- function() {
  paste(readLines(test_path("fixtures", "drift_check", "autofix_niobrara_innertext_2026-09-30.txt"),
                  warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
niobrara_row <- function() {
  data.frame(name = "Niobrara County School District 1", type = "K-12", mean_count = 5, count = 0,
             url = "https://www.growingluskleaders.org/page/human-resources",
             llm_titles = "Music/Band Teacher | SPED Paraprofessional", stringsAsFactors = FALSE)
}
wy_fixture_files <- function() list.files(test_path("fixtures"), recursive = TRUE)
resolve_wy <- function(name) resolve_scraper_call(name)

test_that("build_source_url_lookup covers the k12 registry, the misc districts, the colleges and WSBA", {
  lookup <- build_source_url_lookup()
  k12 <- read.csv(here::here("k12_district_registry.csv"), stringsAsFactors = FALSE)
  expect_true(all(k12$District %in% names(lookup)))
  # A district in both registries takes the misc registry's page (later groups win).
  expect_equal(unname(lookup[misc_district_registry$District]), misc_district_registry$url)
  expect_equal(lookup[names(he_institution_urls)], he_institution_urls)
  expect_length(he_institution_urls, 9)
  expect_equal(unname(lookup[WSBA_STATEWIDE_SOURCE]), WSBA_VACANCIES_URL)
  expect_false(anyDuplicated(names(lookup)) > 0)
})

# ---- Tier 3: per-source auto-fix issues ------------------------------------
# The dispatch logic is tested in edjobs (test-scraper-dispatch.R). These
# check Wyoming's data: state_config.R's three groups of sources. The "every ... resolves" tests are what
# catch a wrong call spec -- the live check and repro script depend on it.

wy_k12_registry <- function() read.csv(here::here("k12_district_registry.csv"), stringsAsFactors = FALSE)

test_that("every k12_district_registry.csv district resolves to a real scraper function", {
  k12 <- wy_k12_registry()
  for (name in k12$District) {
    call <- resolve_scraper_call(name)
    expect_false(is.null(call), info = name)
    expect_true(exists(call$fn, mode = "function"), info = call$fn)
    expect_length(call$args, length(formals(get(call$fn))) - sum(nzchar(vapply(formals(get(call$fn)), deparse, ""))))
  }
})

test_that("registry-backed calls derive their arguments with the Rmd's own helpers", {
  k12 <- wy_k12_registry()
  a <- k12[k12$Platform == "Applitrack", ][1, ]
  expect_equal(resolve_scraper_call(a$District)$args, list(applitrack_tenant_path(a$Job_Link)))
  r <- k12[k12$Platform == "RedRoverK12", ][1, ]
  expect_equal(resolve_scraper_call(r$District)$args,
               list(as.character(r$Org_ID), redrover_org_slug(r$Job_Link)))
  rmd <- paste(readLines(here::here("Wy_ED_Jobs.Rmd"), warn = FALSE), collapse = "\n")
  for (helper in c("applitrack_tenant_path(row$Job_Link)", "schoolspring_domain(row$Job_Link)", "redrover_org_slug(row$Job_Link)")) {
    expect_true(grepl(helper, rmd, fixed = TRUE), info = helper)
  }
})

test_that("every misc_district_registry district resolves to the fetch_* its dispatcher uses", {
  dispatch <- paste(deparse(body(fetch_misc_district_postings)), collapse = "\n")
  k12 <- wy_k12_registry()
  for (i in seq_len(nrow(misc_district_registry))) {
    row <- misc_district_registry[i, ]
    call <- resolve_scraper_call(row$District)
    expect_false(is.null(call), info = row$District)
    expect_true(exists(call$fn, mode = "function"), info = call$fn)
    expect_true(grepl(sprintf("%s = %s(", row$platform, call$fn), dispatch, fixed = TRUE), info = row$platform)
    # session iff the scraper's first argument is a chromote session
    expect_equal(call$session, names(formals(get(call$fn)))[1] == "chromote_session", info = call$fn)
  }
})

test_that("every EXTRA_SCRAPER_CALLS entry appears verbatim in Wy_ED_Jobs.Rmd", {
  rmd <- paste(readLines(here::here("Wy_ED_Jobs.Rmd"), warn = FALSE), collapse = "\n")
  for (name in names(EXTRA_SCRAPER_CALLS)) {
    spec <- EXTRA_SCRAPER_CALLS[[name]]
    expect_true(exists(spec$fn, mode = "function"), info = spec$fn)
    # Named arguments are written as the pipeline writes them (location_from_author = FALSE).
    shown <- vapply(spec$args, deparse, "")
    nm <- names(spec$args); if (!is.null(nm)) shown <- ifelse(nzchar(nm), paste0(nm, " = ", shown), shown)
    needle <- if (length(spec$args) == 0) spec$fn else
      sprintf("%s(%s)", spec$fn, paste(shown, collapse = ", "))
    expect_true(grepl(needle, rmd, fixed = TRUE), info = needle)
  }
})

test_that("every higher-ed drift source has a scraper call", {
  k12 <- wy_k12_registry()
  for (name in names(he_institution_urls)) {
    expect_false(is.null(resolve_scraper_call(name)), info = name)
  }
})

test_that("resolve_scraper_call returns NULL for WSBA-only orgs and unknown names", {
  k12 <- wy_k12_registry()
  for (name in c(WSBA_ONLY_ORGS, "No Such District")) {
    expect_null(resolve_scraper_call(name))
  }
})

test_that("build_autofix_issue_body round-trips its source marker and lists the LLM titles", {
  k12 <- wy_k12_registry()
  row <- data.frame(name = "Casper College", type = "Higher Ed", mean_count = 8, count = 0,
                    url = "https://www.schooljobs.com/careers/caspercollege",
                    llm_titles = "Custodian | Adjunct Instructor", stringsAsFactors = FALSE)
  call <- resolve_scraper_call(row$name)
  body <- paste(build_autofix_issue_body(row, call, "https://run"), collapse = "\n")
  expect_equal(parse_autofix_marker(body), "Casper College")
  expect_match(body, "- Custodian", fixed = TRUE)
  expect_match(body, "`fetch_neogov_postings()`", fixed = TRUE)
  expect_match(body, "a hardcoded call in Wy_ED_Jobs.Rmd", fixed = TRUE)
  expect_match(body, "https://run", fixed = TRUE)
  expect_match(body, "deliberately leaves out", fixed = TRUE)
  # NEOGOV is plain HTTP -- no sandbox-render hint.
  expect_no_match(body, "rendered this page in CI", fixed = TRUE)
})

test_that("scraper_reads_inner_text tells innerText chromote scrapers from HTTP/JSON ones", {
  lines <- unlist(lapply(here::here(SCRAPER_FILES), readLines, warn = FALSE))
  expect_true(scraper_reads_inner_text("fetch_apptegy_postings", lines))
  expect_true(scraper_reads_inner_text("fetch_googlesites_postings", lines))
  expect_false(scraper_reads_inner_text("fetch_prairieview_postings", lines))  # reads headings JSON
  expect_false(scraper_reads_inner_text("fetch_edlio_postings", lines))
  expect_false(scraper_reads_inner_text(NA_character_, lines))
})

test_that("fixtures_named_for_source picks this district's fixture, not another's on the same parser", {
  files <- wy_fixture_files()
  nio <- fixtures_named_for_source(resolve_wy("Niobrara County School District 1"), "Niobrara County School District 1", files)
  expect_equal(nio$own, "misc_districts/apptegy_niobrara_rendered.txt")
  expect_true("misc_districts/apptegy_platte2_rendered.txt" %in% nio$platform)

  w7 <- fixtures_named_for_source(resolve_wy("Weston County School District 7"), "Weston County School District 7", files)
  expect_equal(w7$own, "misc_districts/apptegy_weston7_rendered.txt")
  # Platte 1 is smartsites -- its fixture must not show up as Platte 2's.
  p1 <- fixtures_named_for_source(resolve_wy("Platte County School District 1"), "Platte County School District 1", files)
  expect_equal(p1$own, "misc_districts/smartsites_platte1.html")

  # Sheridan 3 has its own fixture since 2026-11-07; without it, it would get
  # no "own" match, so no diff against some other district's page.
  s3 <- fixtures_named_for_source(resolve_wy("Sheridan County School District 3"), "Sheridan County School District 3", files)
  expect_equal(s3$own, "misc_districts/apptegy_sheridan3_rendered.txt")
  s3_none <- fixtures_named_for_source(resolve_wy("Sheridan County School District 3"), "Sheridan County School District 3",
                             files[!grepl("sheridan3", files)])
  expect_equal(s3_none$own, character(0))
  expect_true(length(s3_none$platform) >= 3)

  expect_equal(fixtures_named_for_source(NULL, "x", files)$own, character(0))
})

test_that("an innerText scraper's issue hands the agent the CI capture and a same-district diff", {
  skip_if(!nzchar(Sys.which("diff")), "no system diff")
  call <- resolve_wy("Niobrara County School District 1")
  ev <- gather_autofix_evidence(call, "Niobrara County School District 1", niobrara_capture(), repo_root = here::here())
  expect_true(ev$inner_text)
  expect_equal(ev$diff_against, "misc_districts/apptegy_niobrara_rendered.txt")
  expect_true(any(startsWith(ev$diff, "-Elementary Teacher:")))
  expect_lt(length(ev$diff), 40)

  body <- paste(build_autofix_issue_body(niobrara_row(), call, evidence = ev, captured_on = "2026-09-30"), collapse = "\n")
  expect_match(body, "### What CI saw", fixed = TRUE)
  expect_match(body, "Full page text captured in CI", fixed = TRUE)
  expect_match(body, "1. Save the **Full page text captured in CI**", fixed = TRUE)
  expect_match(body, "misc_districts/apptegy_<district>_rendered_2026-09-30.txt", fixed = TRUE)
  expect_match(body, "Other districts' fixtures for the same `fetch_apptegy_postings()` parser", fixed = TRUE)
  expect_match(body, "rendered this page in CI", fixed = TRUE)
  expect_match(body, 'scripts/repro_scraper.R "Niobrara County School District 1"', fixed = TRUE)
  expect_equal(parse_autofix_expected_titles(body), c("Music/Band Teacher", "SPED Paraprofessional"))
})

test_that("a shared-parser district with no fixture of its own gets no diff", {
  # A copy of the repo without Sheridan 3's own fixture (added 2026-11-07).
  root <- withr::local_tempdir()
  for (f in SCRAPER_FILES) file.copy(here::here(f), file.path(root, f))
  dir.create(file.path(root, "tests", "testthat", "fixtures"), recursive = TRUE)
  file.copy(list.files(here::here("tests", "testthat"), pattern = "^test-.*[.]R$", full.names = TRUE),
            file.path(root, "tests", "testthat"))
  fx <- list.files(here::here("tests", "testthat", "fixtures"), recursive = TRUE)
  fx <- fx[!grepl("sheridan3", fx)]
  for (f in fx) {
    dir.create(dirname(file.path(root, "tests", "testthat", "fixtures", f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(here::here("tests", "testthat", "fixtures", f), file.path(root, "tests", "testthat", "fixtures", f))
  }
  call <- resolve_wy("Sheridan County School District 3")
  ev <- gather_autofix_evidence(call, "Sheridan County School District 3", niobrara_capture(), repo_root = root)
  expect_true(is.na(ev$diff_against))
  expect_equal(ev$diff, character(0))
})

test_that("an HTTP scraper's issue keeps the capture as context and asks for a raw fixture", {
  call <- resolve_wy("Casper College")
  ev <- gather_autofix_evidence(call, "Casper College", "Custodian\nAdjunct", repo_root = here::here())
  expect_false(ev$inner_text)
  expect_equal(ev$diff, character(0))
  body <- paste(build_autofix_issue_body(niobrara_row(), call, evidence = ev), collapse = "\n")
  expect_match(body, "1. Fetch the live page", fixed = TRUE)
  expect_no_match(body, "Diff: this source's text fixture", fixed = TRUE)
})

test_that("the drift report's feed (state_config.R) is the WSBA feed the rest of the pipeline labels", {
  feed <- edjobs::current_state_config()$feed
  expect_equal(feed$label, WSBA_STATEWIDE_SOURCE)
  expect_equal(feed$url, WSBA_VACANCIES_URL)
  expect_equal(edjobs::current_state_config()$drift_baseline_from, as.Date("2026-08-03"))
})
