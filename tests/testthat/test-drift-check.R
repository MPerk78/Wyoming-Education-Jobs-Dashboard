test_that("attach_scrape_log_errors attaches the latest error message for a matching source", {
  flagged <- data.frame(name = c("Lincoln County School District 1", "Stable District"),
                        mean_count = c(10, 5), n_weeks = c(3, 3), count = c(0, 4),
                        stringsAsFactors = FALSE)
  scrape_log <- data.frame(
    timestamp = c("2026-08-11 15:39:20", "2026-08-11 15:40:00"),
    source = c("Lincoln County School District 1", "Stable District"),
    status = c("error", "ok"),
    n_rows = c(0, 4),
    error_message = c("HTTP 500 Internal Server Error.", NA_character_),
    stringsAsFactors = FALSE
  )

  result <- attach_scrape_log_errors(flagged, scrape_log)

  expect_equal(result$scrape_error[result$name == "Lincoln County School District 1"], "HTTP 500 Internal Server Error.")
  expect_true(is.na(result$scrape_error[result$name == "Stable District"]))
})

test_that("attach_scrape_log_errors matches by substring for a platform-prefixed scrape_log source name", {
  flagged <- data.frame(name = "Niobrara County School District 1", mean_count = 8, n_weeks = 3, count = 0,
                        stringsAsFactors = FALSE)
  scrape_log <- data.frame(
    timestamp = "2026-08-11 15:39:00",
    source = "Apptegy/chromote: Niobrara County School District 1",
    status = "error", n_rows = 0, error_message = "Page structure changed",
    stringsAsFactors = FALSE
  )

  result <- attach_scrape_log_errors(flagged, scrape_log)

  expect_equal(result$scrape_error, "Page structure changed")
})

test_that("attach_scrape_log_errors keeps only the most recent attempt when a source logged twice", {
  flagged <- data.frame(name = "Sublette County School District 9", mean_count = 4.7, n_weeks = 3, count = 0,
                        stringsAsFactors = FALSE)
  scrape_log <- data.frame(
    timestamp = c("2026-08-11 15:39:22", "2026-08-11 15:45:00"),
    source = c("Sublette County School District 9", "Sublette County School District 9"),
    status = c("error", "ok"),
    n_rows = c(0, 12),
    error_message = c("HTTP 429 Too Many Requests.", NA_character_),
    stringsAsFactors = FALSE
  )

  result <- attach_scrape_log_errors(flagged, scrape_log)

  # The later attempt (a retry/re-run) succeeded -- no error should be attached.
  expect_true(is.na(result$scrape_error))
})

test_that("attach_scrape_log_errors leaves scrape_error NA when there's no scrape_log or no flagged rows", {
  flagged <- data.frame(name = "Some District", mean_count = 5, n_weeks = 3, count = 0, stringsAsFactors = FALSE)
  empty_log <- data.frame(timestamp = character(0), source = character(0), status = character(0),
                          n_rows = integer(0), error_message = character(0))

  result <- attach_scrape_log_errors(flagged, empty_log)
  expect_true(is.na(result$scrape_error))

  empty_flagged <- data.frame(name = character(0), mean_count = numeric(0), n_weeks = integer(0), count = numeric(0))
  result2 <- attach_scrape_log_errors(empty_flagged, empty_log)
  expect_equal(nrow(result2), 0)
  expect_true("scrape_error" %in% names(result2))
})

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

test_that("check_salary_coverage flags a source that fell below its expected count", {
  flagged <- check_salary_coverage("K-12 teacher base salary (WSBA)", actual = 12, expected = 48)
  expect_equal(flagged$name, "K-12 teacher base salary (WSBA)")
  expect_equal(flagged$expected, 48)
  expect_equal(flagged$actual, 12)
})

test_that("check_salary_coverage returns NULL when a source meets its expected count", {
  expect_null(check_salary_coverage("K-12 teacher base salary (WSBA)", actual = 48, expected = 48))
})

test_that("check_salary_coverage supports a tolerance below the ideal expected count", {
  # IPEDS Professor-rank coverage is legitimately sparse (some two-year
  # colleges report no "Professor" rank at all) -- min_ok lets a check use
  # a looser floor than the full expected universe without that being
  # confused with actual drift.
  expect_null(check_salary_coverage("HE avg faculty salary (IPEDS)", actual = 8, expected = 9, min_ok = 8))
  flagged <- check_salary_coverage("HE avg faculty salary (IPEDS)", actual = 5, expected = 9, min_ok = 8)
  expect_equal(flagged$actual, 5)
})

test_that("check_salary_value_bounds flags a value outside the sane dollar range", {
  actual <- c(A = 51000, B = 52500, C = 5)  # C is obvious garbage (a parser misread)
  flagged <- check_salary_value_bounds("K-12 teacher base salary (WSBA)", actual, min_ok = 25000, max_ok = 150000)
  expect_equal(flagged$entity, "C")
  expect_equal(flagged$value, 5)
})

test_that("check_salary_value_bounds returns NULL when every value is in range", {
  actual <- c(A = 51000, B = 52500, C = 48000)
  expect_null(check_salary_value_bounds("K-12 teacher base salary (WSBA)", actual, min_ok = 25000, max_ok = 150000))
})

test_that("check_salary_value_bounds ignores NA rather than flagging it", {
  actual <- c(A = 51000, B = NA_real_)
  expect_null(check_salary_value_bounds("K-12 teacher base salary (WSBA)", actual, min_ok = 25000, max_ok = 150000))
})

test_that("check_salary_yoy_plausibility flags a district whose change is a real outlier against its peers", {
  # 5 districts move a normal 2-6%; one (Z) jumps 47% -- the signature of a
  # WSBA PDF column misalignment, not a real settlement.
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000, Z = 50000)
  current <- c(A = 51500, B = 49500, C = 53500, D = 50500, E = 53500, Z = 73500)
  flagged <- check_salary_yoy_plausibility(current, prior)
  expect_equal(flagged$name, "Z")
  expect_equal(round(flagged$pct_change, 2), 0.47)
})

test_that("check_salary_yoy_plausibility does not flag a statewide event where every district moves a lot", {
  # Same ~15% move across the board -- a real (if unusual) statewide
  # settlement year, not a parser bug; no district is an outlier relative
  # to its peers even though the hard ceiling alone would catch the wrong
  # thing here if checked in isolation.
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000)
  current <- prior * 1.15
  expect_null(check_salary_yoy_plausibility(current, prior))
})

test_that("check_salary_yoy_plausibility does not flag normal small variation", {
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000)
  current <- c(A = 51000, B = 49200, C = 53000, D = 50100, E = 51800)
  expect_null(check_salary_yoy_plausibility(current, prior))
})

test_that("check_salary_yoy_plausibility returns NULL with too few comparable entities", {
  prior <- c(A = 50000, B = 48000)
  current <- c(A = 70000, B = 48500)
  expect_null(check_salary_yoy_plausibility(current, prior))
})

test_that("check_salary_yoy_plausibility still flags an outlier when every peer moved by exactly zero (MAD == 0)", {
  # Regression: when every OTHER entity's change is identical (here, all
  # zero), stats::mad(pct_change) is itself 0 -- a naive "flag if the
  # deviation exceeds a multiple of the spread" check then requires
  # exceeding a threshold of 0 * mad_multiplier = 0, and an earlier faulty
  # version of this function instead fell back to a threshold of Inf in
  # this exact case, which made the outlier undetectable no matter how
  # extreme. The fallback must make flagging EASIER when peer spread is
  # near zero, not impossible.
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000, Z = 50000)
  current <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000, Z = 73500)
  flagged <- check_salary_yoy_plausibility(current, prior)
  expect_equal(flagged$name, "Z")
})

test_that("check_salary_yoy_plausibility does not flag a uniform statewide move even with zero peer spread", {
  # Every district moves by the exact same multiplier -- spread is 0 (same
  # edge case as above), but the move itself is below hard_ceiling, so
  # nothing should be flagged.
  prior <- c(A = 50000, B = 48000, C = 52000, D = 49000, E = 51000)
  current <- prior * 1.15
  expect_null(check_salary_yoy_plausibility(current, prior))
})

test_that("score_page_text_for_job_signal identifies real hidden postings as likely_broken", {
  # Real fixture: this is the actual Sweetwater County SD1 page text that
  # exposed the Applitrack encoding bug -- 69 real postings, scraper said 0.
  text <- paste(readLines(test_path("fixtures", "drift_check", "real_postings_sweetwater.txt"), warn = FALSE), collapse = "\n")
  expect_equal(score_page_text_for_job_signal(text), "likely_broken")
})

test_that("score_page_text_for_job_signal identifies a genuinely empty page", {
  # Real fixture: Western Wyoming CC's actual "No jobs at this time." page.
  text <- paste(readLines(test_path("fixtures", "drift_check", "genuinely_empty_western.txt"), warn = FALSE), collapse = "\n")
  expect_equal(score_page_text_for_job_signal(text), "looks_genuinely_empty")
})

test_that("score_page_text_for_job_signal is honestly inconclusive on ambiguous real content", {
  # Real fixture: Sheridan County SD3's Apptegy page -- real postings exist
  # ("Bus Drivers", a coaching opening) but in a bare-line format with none
  # of the structured keywords (JobID, "posted:", etc.) this heuristic looks
  # for, and no explicit "no openings" phrase either. Getting this wrong in
  # either direction would be worse than admitting the heuristic can't tell.
  text <- paste(readLines(test_path("fixtures", "drift_check", "ambiguous_bare_lines_sheridan3.txt"), warn = FALSE), collapse = "\n")
  expect_equal(score_page_text_for_job_signal(text), "inconclusive")
})

test_that("score_page_text_for_job_signal returns inconclusive for NA or empty input", {
  expect_equal(score_page_text_for_job_signal(NA_character_), "inconclusive")
  expect_equal(score_page_text_for_job_signal(""), "inconclusive")
  expect_equal(score_page_text_for_job_signal("   "), "inconclusive")
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

# --- LLM corroboration (Tier 2b) ----------------------------------------

test_that("combine_verdict_with_llm promotes inconclusive to likely_broken when the LLM read real titles", {
  out <- combine_verdict_with_llm("inconclusive", c("Bus Driver", "3rd Grade Teacher"))
  expect_equal(out$verdict, "likely_broken")
  expect_match(out$note, "2 posting\\(s\\)")
  expect_match(out$note, "Bus Driver; 3rd Grade Teacher")
})

test_that("combine_verdict_with_llm truncates a long LLM title list in the note", {
  out <- combine_verdict_with_llm("inconclusive", paste("Role", 1:12))
  expect_equal(out$verdict, "likely_broken")
  expect_match(out$note, "Role 8, \\.\\.\\.$")
})

test_that("combine_verdict_with_llm downgrades inconclusive to genuinely_empty only when the LLM also found nothing", {
  out <- combine_verdict_with_llm("inconclusive", character(0))
  expect_equal(out$verdict, "looks_genuinely_empty")
  expect_match(out$note, "no postings")
})

test_that("combine_verdict_with_llm does not touch looks_genuinely_empty or a confirmed_broken verdict", {
  expect_equal(combine_verdict_with_llm("looks_genuinely_empty", character(0))$verdict, "looks_genuinely_empty")
  expect_true(is.na(combine_verdict_with_llm("looks_genuinely_empty", character(0))$note))
  # confirmed_broken stays put even if the LLM happens to read something
  expect_equal(combine_verdict_with_llm("confirmed_broken", c("Bus Driver"))$verdict, "confirmed_broken")
})

test_that("combine_verdict_with_llm keeps an already-likely_broken verdict and still lists what the LLM found", {
  out <- combine_verdict_with_llm("likely_broken", c("Custodian"))
  expect_equal(out$verdict, "likely_broken")
  expect_match(out$note, "Custodian")
})

test_that("llm_titles_from_page_text returns character(0) with no key or a blank page (never throws)", {
  withr::with_envvar(c(GEMINI_API_KEY = "", LLM_EXTRACT_KEY_ENV = ""), {
    expect_identical(llm_titles_from_page_text("Food Service Worker\nBus Driver"), character(0))
  })
  withr::with_envvar(c(GEMINI_API_KEY = "fake-key"), {
    expect_identical(llm_titles_from_page_text(NA_character_), character(0))
    expect_identical(llm_titles_from_page_text("   "), character(0))
  })
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

test_that("autofix_engine defaults to claude and rejects typos", {
  expect_equal(autofix_engine(""), "claude")
  expect_equal(autofix_engine(NA_character_), "claude")
  expect_equal(autofix_engine(" Copilot "), "copilot")
  expect_equal(autofix_engine("none"), "none")
  expect_error(autofix_engine("claud"), "AUTOFIX_ENGINE must be one of")
})

test_that("parse_autofix_marker returns NA for bodies without a marker", {
  expect_true(is.na(parse_autofix_marker("just a normal issue")))
  expect_true(is.na(parse_autofix_marker(NA_character_)))
  expect_true(is.na(parse_autofix_marker(character(0))))
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

# --- Posted-pay coverage -----------------------------------------------------

test_that("pay_coverage_counts tallies postings and postings with pay per source", {
  out <- pay_coverage_counts(c("A", "A", "B", "A"), c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(out$name, c("A", "B"))
  expect_equal(out$n, c(3L, 1L))
  expect_equal(out$with_pay, c(2L, 0L))
  expect_equal(nrow(pay_coverage_counts(character(0), logical(0))), 0)
})

test_that("flag_pay_coverage_drop flags a source whose postings arrive but lost their pay", {
  week <- function(n, with_pay) data.frame(name = c("Paycom College", "Free Text District"),
                                           n = n, with_pay = with_pay)
  history <- list(week(c(30, 10), c(16, 1)), week(c(31, 12), c(15, 2)))
  current <- data.frame(name = c("Paycom College", "Free Text District"), n = c(31, 11), with_pay = c(0, 0))
  out <- flag_pay_coverage_drop(current, history)
  # The free-text district never carried much pay, so its 0 is not a drop.
  expect_equal(out$name, "Paycom College")
  expect_equal(out$with_pay, 0)
  expect_equal(out$n_weeks, 2L)
  expect_equal(round(out$mean_share, 3), round(mean(c(16 / 30, 15 / 31)), 3))
})

test_that("flag_pay_coverage_drop stays quiet without enough history or postings", {
  hist1 <- list(data.frame(name = "A", n = 10, with_pay = 9))
  hist2 <- c(hist1, hist1)
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 10, with_pay = 0), hist1)), 0)
  # Too few postings this run to judge (and a lost-postings source is flag_drift()'s job).
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 2, with_pay = 0), hist2)), 0)
  # A normal week-to-week dip is not flagged.
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 10, with_pay = 6), hist2)), 0)
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 10, with_pay = 0), list())), 0)
  expect_equal(nrow(flag_pay_coverage_drop(data.frame(name = "A", n = 10, with_pay = 0), hist2)), 1)
})

test_that("build_pay_coverage_report_markdown names each flagged source and its numbers", {
  expect_equal(build_pay_coverage_report_markdown(data.frame()), character(0))
  flagged <- data.frame(name = "Paycom College", mean_share = 0.51, n_weeks = 2L, n = 31L,
                        with_pay = 0L, type = "Higher Ed")
  md <- build_pay_coverage_report_markdown(flagged, date = as.Date("2026-11-07"))
  expect_true(any(grepl("Posted pay went missing (2026-11-07)", md, fixed = TRUE)))
  expect_true(any(grepl("**Paycom College** (Higher Ed): usually 51% of postings state pay, now 0 of 31", md, fixed = TRUE)))
})

# ---- Title quality (classification safeguards items 5 and 6) ----
# Titles below are real, copied from the MT and WY raw archives.

test_that("lint_title_shape flags the misread titles found in the archives", {
  bad <- c(
    "High School - Head" = "level and rank only, no role",                      # Plevna
    "Jr. High - Asst." = "level and rank only, no role",                        # Plevna
    "FULL TIME. MON - FRI." = "schedule or detail sentence",                    # Florence-Carlton
    "YEAR-ROUND POSITION." = "schedule or detail sentence",                     # Florence-Carlton
    "Bus Drivers- Must have a CDL with a school bus passenger endorsement." = "schedule or detail sentence",
    "Food Service is hiring for a cook position." = "schedule or detail sentence",  # Uinta 6
    "Special Education Teachers - Open Until Filled." = "schedule or detail sentence",  # Fremont 21
    "Dish_Server_Cashiers 26" = "file name",                                    # Livingston
    "Dish_Server_Cashier 26 - Copy" = "file name",
    "Other" = "no job named"                                                    # OPI subject field
  )
  expect_equal(lint_title_shape(names(bad)), unname(bad))
  ennis <- paste("Summary: Ennis Schools is seeking a Virtual Programs Paraprofessional to join our team for the",
                 "remainder of the 2026-2027 school year. The ideal candidate will work closely with the building",
                 "principal to ensure proper supervision of students, work with staff to help provide appropriate",
                 "virtual classes, and help to develop virtual programs at the secondary level.")
  expect_equal(lint_title_shape(ennis), "schedule or detail sentence; field label; too long")
  expect_equal(lint_title_shape("CustodianEndFragment (NEW)$18.90–$20.00/hour"), "markup left in")  # Cheyenne Classical
  expect_equal(lint_title_shape(c(NA_character_, "", "  ")), rep("blank title", 3))
  expect_equal(lint_title_shape(character(0)), character(0))
})

test_that("lint_title_shape leaves real titles alone, abbreviations and start dates included", {
  good <- c(
    "Junior High 7-8", "Track Coach - Jr. High - Asst.", "COMMUNICATIONS COORD.",
    "Instructional Asst./Technology Asst.", "Director of Budget and Finance.", "Seasonal Grounds Keeper.",
    "Assistant High School Girls Basketball Coach.",
    "Journey Administrative Specialist beginning for the 2026-2027 school year.",
    "Project Coordinator, Senior - Fed. Relations/Employee Success Coord.", "Elementary",
    "Substitute Certified Teachers $150/day",
    "Special Education Maternity Leave (2) - PMS - (8/26/26-12/14/26)/TCH00532 - (11/2/26-3/15/27)/TCH00073",
    paste("Monthly Pooled - WCC AmeriCorps Individual Placement - Habitat Biologist Technician with Wyoming",
          "Game and Fish Department and Bighorn National Forest")
  )
  expect_equal(lint_title_shape(good), rep(NA_character_, length(good)))
})

test_that("title_pay_unrecorded flags pay stated in the title only when Posted Pay is blank", {
  t <- c("Secretary – $14 per hour, up to 32 hours per week",                       # Turner
         "Substitute Certified Teachers $150/day",                                      # Platte 2
         "Science Teacher - Torrington Middle School - Eligible for $10,000 Hiring Incentive Based on Certification",
         "Custodian", NA)
  expect_equal(title_pay_unrecorded(t, c(NA, 150, NA, NA, NA)), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(title_pay_unrecorded(character(0), numeric(0)), logical(0))
})

test_that("build_title_lint_report_markdown counts postings and caps the titles shown per source", {
  hits <- data.frame(source = c(rep("OPI", 7), "Turner"), type = "K-12",
                     title = c("Other", "Other", paste("T", 1:5), "Secretary $14"),
                     reason = "x", stringsAsFactors = FALSE)
  md <- build_title_lint_report_markdown(hits, date = "2026-11-05", max_per_source = 3)
  expect_true(any(grepl("**OPI** (K-12), 7 posting(s):", md, fixed = TRUE)))
  expect_true(any(grepl("...and 3 more", md, fixed = TRUE)))    # 6 distinct titles, 3 shown
  expect_equal(sum(grepl("\"Other\"", md, fixed = TRUE)), 1)
  expect_length(build_title_lint_report_markdown(hits[0, ]), 0)
})

test_that("flag_other_share_jump flags a source whose Other share jumped against its recent weeks", {
  # Florence-Carlton, MT 2026-09-08: 2 of 10 postings were schedule lines, none before.
  history <- list(data.frame(name = c("Florence", "Steady"), n = c(8, 10), other = c(0, 1)),
                  data.frame(name = c("Florence", "Steady"), n = c(9, 10), other = c(0, 1)))
  current <- data.frame(name = c("Florence", "Steady"), n = c(10, 10), other = c(2, 2))
  out <- flag_other_share_jump(current, history)
  expect_equal(out$name, "Florence")
  expect_equal(out$mean_share, 0)
  expect_equal(out$other, 2)
})

test_that("flag_other_share_jump stays quiet without enough history, postings or Other rows", {
  hist1 <- list(data.frame(name = "A", n = 10, other = 0))
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 10, other = 5), hist1)), 0)
  hist2 <- c(hist1, hist1)
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 2, other = 2), hist2)), 0)
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 10, other = 1), hist2)), 0)
  # already mostly Other: a rise from 50% to 60% isn't a jump
  hist3 <- list(data.frame(name = "A", n = 10, other = 5), data.frame(name = "A", n = 10, other = 5))
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 10, other = 6), hist3)), 0)
  expect_equal(nrow(flag_other_share_jump(data.frame(name = "A", n = 10, other = 5), list())), 0)
})

test_that("other_share_counts and build_other_share_report_markdown", {
  counts <- other_share_counts(c("A", "A", "B"), c(TRUE, FALSE, FALSE))
  expect_equal(counts$other, c(1L, 0L))
  flagged <- data.frame(name = "Florence", mean_share = 0, n_weeks = 8L, n = 10L, other = 2L,
                        type = "K-12", examples = "\"YEAR-ROUND POSITION.\"", stringsAsFactors = FALSE)
  md <- build_other_share_report_markdown(flagged, date = "2026-11-05")
  expect_true(any(grepl("**Florence** (K-12): usually 0% Other, now 2 of 10", md, fixed = TRUE)))
  expect_length(build_other_share_report_markdown(flagged[0, ]), 0)
})
