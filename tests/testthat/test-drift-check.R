
test_that("build_historical_counts excludes pre-fix snapshots and computes correct means", {
  snapshots <- list(
    "2026-08-03" = data.frame(District = c("A", "A", "A", "B")),
    "2026-08-10" = data.frame(District = c("A", "A", "B", "B")),
    "2026-08-17" = data.frame(District = c("A", "A", "A")),
    # Predates the Applitrack encoding fix -- must be excluded, or the fix
    # itself would register as suspicious drift on every affected district.
    "2026-02-27" = data.frame(District = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"))
  )

  baseline <- build_historical_counts(snapshots, "District")

  expect_equal(baseline$n_weeks[baseline$name == "A"], 3)
  expect_equal(baseline$mean_count[baseline$name == "A"], (3 + 2 + 3) / 3)
  expect_equal(baseline$n_weeks[baseline$name == "B"], 2)
})

test_that("build_historical_counts returns an empty frame when no snapshots are post-fix", {
  snapshots <- list("2026-02-27" = data.frame(District = c("A", "A")))
  baseline <- build_historical_counts(snapshots, "District")
  expect_equal(nrow(baseline), 0)
})

test_that("flag_drift flags a real drop and leaves a stable source alone", {
  baseline <- data.frame(
    name = c("BigDistrict", "StableDistrict"),
    n_weeks = c(5, 5),
    mean_count = c(50, 10)
  )
  current <- data.frame(name = c("BigDistrict", "StableDistrict"), count = c(0, 9))

  flagged <- flag_drift(current, baseline)

  expect_equal(flagged$name, "BigDistrict")
})

test_that("flag_drift exempts sources whose historical average is below the noise floor", {
  baseline <- data.frame(
    name = c("TinySource", "RealSource"),
    n_weeks = c(6, 6),
    mean_count = c(2, 12)
  )
  current <- data.frame(name = c("TinySource", "RealSource"), count = c(0, 0))

  flagged <- flag_drift(current, baseline)

  # TinySource averaged 2/week -- dropping to 0 is churn, not a parser
  # failure. RealSource averaged 12 and is still flagged.
  expect_equal(flagged$name, "RealSource")
})

test_that("flag_drift requires a minimum number of historical weeks before flagging", {
  baseline <- data.frame(name = "BrandNewDistrict", n_weeks = 1, mean_count = 10)
  current <- data.frame(name = "BrandNewDistrict", count = 0)

  flagged <- flag_drift(current, baseline, min_weeks = 2)

  expect_equal(nrow(flagged), 0)
})

test_that("flag_drift treats a source missing from current data as a count of zero", {
  baseline <- data.frame(name = "VanishedDistrict", n_weeks = 3, mean_count = 20)
  current <- data.frame(name = character(0), count = numeric(0))

  flagged <- flag_drift(current, baseline)

  expect_equal(flagged$name, "VanishedDistrict")
  expect_equal(flagged$count, 0)
})

test_that("build_source_url_lookup reads the unified k12 registry and later sources win on collision", {
  k12_registry <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(
    District = c("Test District", "Other District"),
    Platform = c("Applitrack", "SchoolSpring"),
    Job_Link = c("https://frontline.example", "https://registry.example"),
    Org_ID = c(NA, NA),
    stringsAsFactors = FALSE
  ), k12_registry, row.names = FALSE)

  misc_registry <- data.frame(District = c("Misc District", "Other District"),
                              url = c("https://misc.example", "https://misc-other.example"),
                              stringsAsFactors = FALSE)

  lookup <- build_source_url_lookup(k12_registry, misc_registry)

  expect_equal(unname(lookup["Test District"]), "https://frontline.example")
  # "Other District" appears in both the k12 registry and misc_registry --
  # misc_registry (added later in the combination order) should win,
  # matching duplicated(fromLast = TRUE).
  expect_equal(unname(lookup["Other District"]), "https://misc-other.example")
  expect_equal(unname(lookup["Misc District"]), "https://misc.example")
  expect_true("University of Wyoming" %in% names(lookup))
})

# ---- Tier 3: per-source auto-fix issues ------------------------------------
# Ported from the Montana dashboard, with resolve_scraper_call() rebuilt for
# Wyoming's four kinds of source. The "every ... resolves" tests are what
# catch a wrong call spec -- the live check and repro script depend on it.

wy_k12_registry <- function() read.csv(here::here("k12_district_registry.csv"), stringsAsFactors = FALSE)

test_that("every k12_district_registry.csv district resolves to a real scraper function", {
  k12 <- wy_k12_registry()
  for (name in k12$District) {
    call <- resolve_scraper_call(name, k12, misc_district_registry)
    expect_false(is.null(call), info = name)
    expect_true(exists(call$fn, mode = "function"), info = call$fn)
    expect_length(call$args, length(formals(get(call$fn))) - sum(nzchar(vapply(formals(get(call$fn)), deparse, ""))))
  }
})

test_that("registry-backed calls derive their arguments with the Rmd's own helpers", {
  k12 <- wy_k12_registry()
  a <- k12[k12$Platform == "Applitrack", ][1, ]
  expect_equal(resolve_scraper_call(a$District, k12, misc_district_registry)$args, list(applitrack_tenant_path(a$Job_Link)))
  r <- k12[k12$Platform == "RedRoverK12", ][1, ]
  expect_equal(resolve_scraper_call(r$District, k12, misc_district_registry)$args,
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
    call <- resolve_scraper_call(row$District, k12, misc_district_registry)
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
    needle <- if (length(spec$args) == 0) spec$fn else
      sprintf("%s(%s)", spec$fn, paste(vapply(spec$args, deparse, ""), collapse = ", "))
    expect_true(grepl(needle, rmd, fixed = TRUE), info = needle)
  }
})

test_that("every higher-ed drift source has a scraper call", {
  k12 <- wy_k12_registry()
  for (name in names(he_institution_urls)) {
    expect_false(is.null(resolve_scraper_call(name, k12, misc_district_registry)), info = name)
  }
})

test_that("resolve_scraper_call returns NULL for WSBA-only orgs and unknown names", {
  k12 <- wy_k12_registry()
  for (name in c(WSBA_ONLY_ORGS, "No Such District")) {
    expect_null(resolve_scraper_call(name, k12, misc_district_registry))
  }
})

test_that("run_scraper_call gives only chromote-backed scrapers a session", {
  fake_http <- function(url) data.frame(Title = paste("got", url))
  fake_chromote <- function(chromote_session, url) data.frame(Title = paste(chromote_session$id, url))
  assign("fake_http", fake_http, envir = globalenv())
  assign("fake_chromote", fake_chromote, envir = globalenv())
  on.exit(rm(fake_http, fake_chromote, envir = globalenv()))
  closed <- FALSE
  session <- list(id = "s1", close = function() closed <<- TRUE)
  expect_equal(run_scraper_call(list(fn = "fake_http", args = list("u"), session = FALSE))$Title, "got u")
  expect_equal(run_scraper_call(list(fn = "fake_chromote", args = list("u"), session = TRUE),
                                session_factory = function() session)$Title, "s1 u")
  expect_true(closed)
})

test_that("build_autofix_issue_body round-trips its source marker and lists the LLM titles", {
  k12 <- wy_k12_registry()
  row <- data.frame(name = "Casper College", type = "Higher Ed", mean_count = 8, count = 0,
                    url = "https://www.schooljobs.com/careers/caspercollege",
                    llm_titles = "Custodian | Adjunct Instructor", stringsAsFactors = FALSE)
  call <- resolve_scraper_call(row$name, k12, misc_district_registry)
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

test_that("build_autofix_issue_body tolerates no LLM titles and no scraper call", {
  row <- data.frame(name = "Mystery", type = "K-12", mean_count = 4, count = 0, url = NA_character_,
                    llm_titles = NA_character_, stringsAsFactors = FALSE)
  body <- paste(build_autofix_issue_body(row), collapse = "\n")
  expect_equal(parse_autofix_marker(body), "Mystery")
  expect_no_match(body, "An LLM read")
  expect_no_match(body, "repro_scraper")
})

test_that("parse_autofix_expected_titles recovers the titles build_autofix_issue_body wrote", {
  row <- data.frame(name = "X", type = "K-12", mean_count = 3, count = 0, url = "u",
                    llm_titles = "Route Bus Drivers | Daycare Manager", stringsAsFactors = FALSE)
  body <- paste(build_autofix_issue_body(row), collapse = "\n")
  expect_equal(parse_autofix_expected_titles(body), c("Route Bus Drivers", "Daycare Manager"))
  expect_equal(parse_autofix_expected_titles("no titles here"), character(0))
})

test_that("summarize_live_check fails on an error or zero rows and passes otherwise", {
  expect_false(summarize_live_check("X", simpleError("HTTP 520."))$pass)
  expect_false(summarize_live_check("X", data.frame(Title = character(0)))$pass)

  ok <- summarize_live_check("X", data.frame(Title = c("Route Bus Drivers", "Cook")),
                             expected_titles = c("route bus drivers", "Daycare Manager"))
  expect_true(ok$pass)
  md <- paste(ok$markdown, collapse = "\n")
  expect_match(md, "Matched 1 of 2", fixed = TRUE)
  expect_match(md, "- Daycare Manager", fixed = TRUE)
})

test_that("summarize_live_check reads Applitrack/TedK12's lowercase title column", {
  # Before scraper_titles(), a fixed Applitrack scraper read as "0 postings".
  ok <- summarize_live_check("X", data.frame(title = c("Custodian", "Para")))
  expect_true(ok$pass)
  expect_match(paste(ok$markdown, collapse = "\n"), "2 posting(s)", fixed = TRUE)
})

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
resolve_wy <- function(name) resolve_scraper_call(name, wy_k12_registry(), misc_district_registry)

test_that("scraper_reads_inner_text tells innerText chromote scrapers from HTTP/JSON ones", {
  lines <- unlist(lapply(here::here(SCRAPER_FILES), readLines, warn = FALSE))
  expect_true(scraper_reads_inner_text("fetch_apptegy_postings", lines))
  expect_true(scraper_reads_inner_text("fetch_googlesites_postings", lines))
  expect_false(scraper_reads_inner_text("fetch_prairieview_postings", lines))  # reads headings JSON
  expect_false(scraper_reads_inner_text("fetch_edlio_postings", lines))
  expect_false(scraper_reads_inner_text(NA_character_, lines))
})

test_that("source_fixtures picks this district's fixture, not another's on the same parser", {
  files <- wy_fixture_files()
  nio <- source_fixtures(resolve_wy("Niobrara County School District 1"), "Niobrara County School District 1", files)
  expect_equal(nio$own, "misc_districts/apptegy_niobrara_rendered.txt")
  expect_true("misc_districts/apptegy_platte2_rendered.txt" %in% nio$platform)

  w7 <- source_fixtures(resolve_wy("Weston County School District 7"), "Weston County School District 7", files)
  expect_equal(w7$own, "misc_districts/apptegy_weston7_rendered.txt")
  # Platte 1 is smartsites -- its fixture must not show up as Platte 2's.
  p1 <- source_fixtures(resolve_wy("Platte County School District 1"), "Platte County School District 1", files)
  expect_equal(p1$own, "misc_districts/smartsites_platte1.html")

  # Sheridan 3 has its own fixture since 2026-11-07; without it, it would get
  # no "own" match, so no diff against some other district's page.
  s3 <- source_fixtures(resolve_wy("Sheridan County School District 3"), "Sheridan County School District 3", files)
  expect_equal(s3$own, "misc_districts/apptegy_sheridan3_rendered.txt")
  s3_none <- source_fixtures(resolve_wy("Sheridan County School District 3"), "Sheridan County School District 3",
                             files[!grepl("sheridan3", files)])
  expect_equal(s3_none$own, character(0))
  expect_true(length(s3_none$platform) >= 3)

  expect_equal(source_fixtures(NULL, "x", files)$own, character(0))
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

test_that("diff and page-text lines never leak into the live check's expected titles", {
  ev <- list(fetch_fn = "fetch_x_postings", fixtures = "x.txt", platform_fixtures = "x.txt", inner_text = TRUE,
             page_text = "- Not A Title\nreal text", diff_against = "x.txt",
             diff = c("--- x.txt", "+++ ci", "- Also Not A Title", "+new"))
  body <- paste(build_autofix_issue_body(niobrara_row(), resolve_wy("Niobrara County School District 1"), evidence = ev),
                collapse = "\n")
  expect_equal(parse_autofix_expected_titles(body), c("Music/Band Teacher", "SPED Paraprofessional"))
})

test_that("a long capture is truncated to fit an issue body", {
  ev <- list(fetch_fn = "fetch_x_postings", fixtures = character(0), platform_fixtures = character(0),
             inner_text = TRUE, page_text = strrep("x", AUTOFIX_PAGE_TEXT_MAX_CHARS * 3),
             diff_against = NA_character_, diff = paste0("+", seq_len(AUTOFIX_DIFF_MAX_LINES * 2)))
  md <- autofix_evidence_markdown(ev, "2026-09-30")
  expect_lt(sum(nchar(md)), 60000)  # GitHub caps issue bodies at 65,536 chars
  expect_true(any(grepl("page text truncated", md, fixed = TRUE)))
  expect_true(any(grepl("diff truncated", md, fixed = TRUE)))
})

test_that("summarize_central_safety_net handles empty or NULL inputs gracefully", {
  summary <- summarize_central_safety_net()
  expect_equal(summary$total_feed_vacancies, 0L)
  expect_equal(summary$total_fallback_vacancies, 0L)
  expect_equal(length(summary$fallback_districts), 0)
  expect_equal(nrow(summary$discrepancies), 0)
  expect_true(any(grepl("Counted from WSBA at directly scraped districts**: none", summary$markdown, fixed = TRUE)))
})

test_that("summarize_central_safety_net identifies active fallback districts and counts", {
  registry <- data.frame(
    District = c("Fremont County School District 24", "Park County School District 1", "Natrona School District 1"),
    stringsAsFactors = FALSE
  )
  combined <- data.frame(
    District = c("Fremont County School District 24", "Fremont County School District 24", "Park County School District 1", "Unregistered Town", "Natrona School District 1"),
    url = c(
      WSBA_VACANCIES_URL,
      WSBA_VACANCIES_URL,
      WSBA_VACANCIES_URL,
      WSBA_VACANCIES_URL,
      "https://natrona.schoolspring.com"
    ),
    posting_id = c("wsba1", "wsba2", "wsba3", "wsba4", "direct1"),
    stringsAsFactors = FALSE
  )

  summary <- summarize_central_safety_net(
    combined_df = combined,
    registry = registry,
    feed_label = WSBA_STATEWIDE_SOURCE,
    feed_short_name = "WSBA"
  )

  expect_equal(summary$total_feed_vacancies, 4L)
  expect_equal(summary$total_fallback_vacancies, 3L)
  expect_equal(summary$fallback_districts[["Fremont County School District 24"]], 2L)
  expect_equal(summary$fallback_districts[["Park County School District 1"]], 1L)
  expect_null(summary$fallback_districts[["Natrona School District 1"]])

  md <- paste(summary$markdown, collapse = "\n")
  expect_match(md, "3 posting(s) at 2 district(s), not on the district's own board", fixed = TRUE)
  expect_match(md, "Fremont County School District 24**: 2", fixed = TRUE)
  expect_match(md, "Park County School District 1**: 1", fixed = TRUE)
})

test_that("summarize_central_safety_net reads discrepancy report for unmatched postings", {
  disc <- data.frame(
    District = c("Laramie County School District 1", "Natrona School District 1"),
    Source = c("WSBA", "WSBA"),
    Title = c("Math Teacher", "Custodian (stale)"),
    Date_Posted = c("2026-10-25", "2026-05-01"),
    Discrepancy_Type = c("Unmatched on direct scraper (investigate ATS drift)", "Potential ghost posting on WSBA"),
    stringsAsFactors = FALSE
  )

  summary <- summarize_central_safety_net(
    discrepancy_report = disc,
    feed_short_name = "WSBA"
  )

  expect_equal(nrow(summary$discrepancies), 1)
  expect_equal(summary$discrepancies$District, "Laramie County School District 1")
  md <- paste(summary$markdown, collapse = "\n")
  expect_match(md, "1 unmatched vacancy/vacancies detected on central feed", fixed = TRUE)
  expect_match(md, "Laramie County School District 1**: \"Math Teacher\" (posted 2026-10-25)", fixed = TRUE)
})

test_that("build_drift_report_markdown includes Central Safety Net status annotations and summary", {
  actionable <- data.frame(
    name = c("Fremont County School District 24", "Park County School District 1", "University of Wyoming"),
    type = c("K-12", "K-12", "Higher Ed"),
    mean_count = c(5.0, 10.0, 30.0),
    count = c(0, 0, 2),
    url = c("https://fremont24.com", "https://park1.net", "https://uwyo.edu"),
    verdict = c("confirmed_broken", "likely_broken", "likely_broken"),
    error_message = c("500 Server Error", NA, NA),
    llm_note = c(NA, "an LLM read 4 postings", NA),
    safety_net_vacancies = c(2L, 0L, NA_integer_),
    stringsAsFactors = FALSE
  )

  safety_net_summary <- list(
    total_feed_vacancies = 50L,
    fallback_districts = list("Fremont County School District 24" = 2L),
    total_fallback_vacancies = 2L,
    markdown = c(
      "### WSBA postings at districts we scrape directly",
      "",
      "- **Statewide Feed (WSBA)**: 50 active vacancies captured.",
      "- **Counted from WSBA at directly scraped districts**: 2 posting(s) at 1 district(s), not on the district's own board:",
      "  - **Fremont County School District 24**: 2",
      "- **Central Feed Discrepancies**: 0 unmatched vacancies detected for active direct scrapers.",
      ""
    )
  )

  lines <- build_drift_report_markdown(
    actionable = actionable,
    safety_net_summary = safety_net_summary,
    date = as.Date("2026-10-29"),
    feed_short_name = "WSBA"
  )

  report <- paste(lines, collapse = "\n")
  # Fremont 24: active fallback
  expect_match(report, "Fremont County School District 24", fixed = TRUE)
  expect_match(report, "On WSBA**: 2 postings for this district that its own board doesn't list (counted from WSBA)", fixed = TRUE)
  # Park 1: inactive fallback
  expect_match(report, "Park County School District 1", fixed = TRUE)
  expect_match(report, "On WSBA**: nothing for this district beyond what its own board lists", fixed = TRUE)
  # Higher Ed: no Central Safety Net bullet under its entry
  expect_match(report, "University of Wyoming", fixed = TRUE)
  expect_false(grepl("University of Wyoming.*\\n\\s+- \\*\\*On WSBA", report))
  # Bottom summary block included
  expect_match(report, "### WSBA postings at districts we scrape directly", fixed = TRUE)
  expect_match(report, "Statewide Feed (WSBA)**: 50 active vacancies captured", fixed = TRUE)
})
