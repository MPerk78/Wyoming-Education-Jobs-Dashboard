# misc_district_scrapers.R is sourced directly (not an R package), so
# testthat::local_mocked_bindings() -- which requires pkgload/a dev
# package -- doesn't work here. mock_globals() is a small dependency-free
# stand-in: temporarily reassigns named functions in the global
# environment (where source()'d functions live) and restores the
# originals via testthat's own teardown hook, same lifecycle
# local_mocked_bindings() would give inside a package.
mock_globals <- function(bindings) {
  originals <- mget(names(bindings), envir = globalenv())
  for (name in names(bindings)) assign(name, bindings[[name]], envir = globalenv())
  withr::defer({
    for (name in names(originals)) assign(name, originals[[name]], envir = globalenv())
  }, envir = parent.frame())
}

test_that("misc_district_registry has exactly the 13 confirmed districts/schools, each with a known platform", {
  # 10 real districts + 3 charter schools (Cheyenne Classical Academy and
  # Wyoming Classical Academy on the Educational Networks CMS, Prairie
  # View Community School on its own bespoke platform -- all found
  # 2026-08-06) -- see the registry's own comment for why standalone
  # schools belong here too, not just LEAs.
  expect_equal(nrow(misc_district_registry), 13)
  expect_true(all(misc_district_registry$platform %in%
    c("wordpress", "apptegy", "smartsites", "schoolblocks", "edlio", "googlesites", "educational_networks", "prairieview")))
  expect_equal(length(unique(misc_district_registry$District)), 13)
})

test_that("misc_district_coverage_tiers labels the 10 registry districts and WSBA-only orgs distinctly", {
  tiers <- misc_district_coverage_tiers()
  expect_equal(nrow(tiers), nrow(misc_district_registry) + length(WSBA_ONLY_ORGS))
  expect_true(all(misc_district_registry$District %in% tiers$District[tiers$Data_Coverage == "Partial (WSBA + own page)"]))
  expect_true(all(WSBA_ONLY_ORGS %in% tiers$District[tiers$Data_Coverage == "Partial (WSBA only)"]))
  # No district should be double-classified.
  expect_equal(length(unique(tiers$District)), nrow(tiers))
})

test_that("fetch_misc_district_postings dispatches to the right platform function", {
  # Fake single-row result so this test doesn't touch the network;
  # confirms routing, not the extractors themselves (already covered in
  # test-misc-district-platform-extractors.R).
  mock_globals(list(
    fetch_wordpress_postings = function(url) data.frame(Title = "wp-hit", Location = NA, Posted_Date = NA, Link = NA)
  ))
  result <- fetch_misc_district_postings("wordpress", "http://example.com")
  expect_equal(result$Title, "wp-hit")
})

test_that("fetch_misc_district_postings errors clearly on an unknown platform", {
  expect_error(fetch_misc_district_postings("not-a-real-platform", "http://example.com"), "unknown platform")
})

test_that("fetch_all_misc_district_postings combines WSBA and district-own postings, deduplicated, with no chromote available", {
  # No chromote_session_factory passed -> browser_session stays NULL ->
  # fetch_apptegy_postings(NULL, url)/fetch_googlesites_postings(NULL, url)
  # are called for the 4 Apptegy districts and Uinta County SD6 and
  # (mocked here) return a fixed result regardless of the NULL session,
  # standing in for what a real browser fetch would return.
  mock_globals(list(
    fetch_wsba_vacancies = function(url) data.frame(
      Title = c("K-6 Elementary Teacher", "K-12 Band Teacher", "Middle School Math Teacher", "Science Teacher"),
      District = c("Niobrara County School District 1", "Niobrara County School District 1",
                   "Snowy Range Academy", "Laramie County School District 1"),
      Location = c("Lusk", "Lusk", "Laramie", "Cheyenne"),
      Posted_Date = c("2026-05-18", "2026-05-11", "2026-05-20", "2026-05-22"),
      Url = c("https://example.com/1", "https://example.com/2", "https://example.com/3", "https://example.com/4"),
      stringsAsFactors = FALSE
    ),
    fetch_wordpress_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_smartsites_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_schoolblocks_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_edlio_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_googlesites_postings = function(session, url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_educational_networks_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_prairieview_postings = function(session, url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_apptegy_postings = function(session, url) data.frame(
      Title = c("K-6 Elementary Teacher", "SPED Paraprofessional"),  # first is a WSBA duplicate, second is new
      Location = NA_character_, Posted_Date = NA_character_, Link = NA_character_
    )
  ))

  result <- fetch_all_misc_district_postings()

  niobrara <- result[result$District == "Niobrara County School District 1", ]
  # 2 from WSBA + 1 from Apptegy after dedup (the WSBA-duplicate title
  # "K-6 Elementary Teacher" must NOT appear twice).
  expect_equal(nrow(niobrara), 3)
  expect_equal(sum(niobrara$title == "K-6 Elementary Teacher"), 1)
  expect_true("SPED Paraprofessional" %in% niobrara$title)

  # Regression: Snowy Range Academy is a WSBA_ONLY_ORGS entry, not one of
  # the 12 misc_district_registry districts -- confirms its WSBA row
  # actually makes it into the result now, instead of being silently
  # dropped (see WSBA_ONLY_ORGS's comment for the history here).
  snowy_range <- result[result$District == "Snowy Range Academy", ]
  expect_equal(nrow(snowy_range), 1)
  expect_equal(snowy_range$title, "Middle School Math Teacher")
  expect_equal(snowy_range$url, "https://example.com/3")

  # WSBA rows for direct-platform districts must also be forwarded; the
  # final K-12 merge reconciles true overlaps with those direct feeds.
  laramie_1 <- result[result$District == "Laramie County School District 1", ]
  expect_equal(nrow(laramie_1), 1)
  expect_equal(laramie_1$title, "Science Teacher")
  expect_equal(laramie_1$url, "https://example.com/4")

  expect_equal(names(result), c("title", "date_posted", "position", "location", "url", "District", "Posting_Source"))
  expect_true(all(is.na(result$position)))
  # Every row says where it was read: WSBA's rows the feed, the district's own page its platform.
  expect_equal(niobrara$Posting_Source[niobrara$title == "SPED Paraprofessional"], "Apptegy district site (direct board)")
  expect_equal(unique(niobrara$Posting_Source[niobrara$title != "SPED Paraprofessional"]), WSBA_POSTING_SOURCE)
  expect_equal(c(snowy_range$Posting_Source, laramie_1$Posting_Source), rep(WSBA_POSTING_SOURCE, 2))
})

test_that("fetch_all_misc_district_postings returns the right empty schema when every source is empty", {
  mock_globals(list(
    fetch_wsba_vacancies = function(url) data.frame(Title = character(0), District = character(0), Location = character(0), Posted_Date = character(0), Url = character(0)),
    fetch_wordpress_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_smartsites_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_schoolblocks_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_edlio_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_googlesites_postings = function(session, url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_educational_networks_postings = function(url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_prairieview_postings = function(session, url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0)),
    fetch_apptegy_postings = function(session, url) data.frame(Title = character(0), Location = character(0), Posted_Date = character(0), Link = character(0))
  ))

  result <- fetch_all_misc_district_postings()
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("title", "date_posted", "position", "location", "url", "District", "Posting_Source"))
})

test_that("remove_wsba_direct_duplicates removes only exact district/title/date overlaps", {
  wsba <- data.frame(
    title = c("Science Teacher", "Science Teacher", "Science Teacher", "Math Teacher"),
    date_posted = as.Date(c("2026-05-22", "2026-05-23", "2026-05-22", NA)),
    District = c(
      "Laramie County School District No. 1",
      "Laramie County School District 1",
      "Natrona School District 1",
      "Laramie County School District 1"
    ),
    stringsAsFactors = FALSE
  )
  direct <- data.frame(
    title = "SCIENCE teacher",
    date_posted = as.Date("2026-05-22"),
    District = "Laramie County School District 1",
    stringsAsFactors = FALSE
  )

  result <- remove_wsba_direct_duplicates(wsba, direct)

  expect_equal(nrow(result), 3)
  expect_equal(as.character(result$date_posted), c("2026-05-23", "2026-05-22", NA))
})

test_that("a WSBA row linking a job ID the district board lists is a duplicate, whatever its wording or date", {
  # Park #6, 2026-11-05: WSBA's title and date differ from the board's.
  wsba <- data.frame(
    title = c("Middle School ASD Special Education Teacher", "Elementary Rural Teacher", "Bus Driver"),
    date_posted = c("2026-03-27", "2026-09-30", "2026-10-01"),
    District = "Park County School District 6",
    url = c("https://www.applitrack.com/park6/OnlineApp/JobPostings/View.asp?AppliTrackJobId=1026",
            "https://www.applitrack.com/park6/OnlineApp/JobPostings/View.asp?AppliTrackJobId=1077",
            "https://www.wsba-wy.org/vacancies"),
    stringsAsFactors = FALSE
  )
  direct <- data.frame(
    title = "ASD Special Education Teacher", date_posted = "2025-12-01", District = "Park County School District 6",
    url = "https://www.applitrack.com/park6/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=1026",
    posting_id = "applitrack:park6:1026", stringsAsFactors = FALSE
  )
  result <- remove_wsba_direct_duplicates(wsba, direct)
  expect_equal(result$title, c("Elementary Rural Teacher", "Bus Driver"))
})

test_that("direct_also_on_wsba flags the direct postings WSBA also lists, by key or by job ID", {
  wsba <- data.frame(
    title = c("Middle School ASD Special Education Teacher", "Science Teacher", "Bus Driver"),
    date_posted = c("2026-03-27", "2026-05-22", "2026-10-01"),
    District = c("Park County School District 6", "Laramie County School District 1", "Park County School District 6"),
    url = c("https://www.applitrack.com/park6/OnlineApp/JobPostings/View.asp?AppliTrackJobId=1026",
            "https://www.wsba-wy.org/vacancies", "https://www.wsba-wy.org/vacancies"),
    stringsAsFactors = FALSE)
  direct <- data.frame(
    title = c("ASD Special Education Teacher", "SCIENCE teacher", "Custodian"),
    date_posted = c("2025-12-01", "2026-05-22", "2026-10-01"),
    District = c("Park County School District 6", "Laramie County School District 1", "Park County School District 6"),
    url = c("https://www.applitrack.com/park6/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=1026",
            "https://laramie1.schoolspring.com/jobs/1", "https://www.applitrack.com/park6/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=2000"),
    posting_id = c("applitrack:park6:1026", NA, "applitrack:park6:2000"), stringsAsFactors = FALSE)
  # job ID match, district/title/date match, no match
  expect_equal(direct_also_on_wsba(direct, wsba), c(TRUE, TRUE, FALSE))
  # The same rows WSBA's own dedup drops: one WSBA row per flagged direct posting here.
  expect_equal(nrow(wsba) - nrow(remove_wsba_direct_duplicates(wsba, direct)), sum(direct_also_on_wsba(direct, wsba)))
  expect_equal(direct_also_on_wsba(direct, wsba[0, ]), c(FALSE, FALSE, FALSE))
  expect_equal(direct_also_on_wsba(direct[0, ], wsba), logical(0))
})

test_that("every registry and misc platform has a Posting_Source label, and WSBA's is the feed label", {
  k12 <- read.csv(here::here("k12_district_registry.csv"), stringsAsFactors = FALSE)
  expect_false(anyNA(k12_posting_source_label(unique(k12$Platform))))
  expect_false(anyNA(k12_posting_source_label(unique(misc_district_registry$platform))))
  expect_warning(lab <- k12_posting_source_label(c("Applitrack", "NoSuchPlatform")), "No Posting_Source label")
  expect_equal(lab, c("AppliTrack (direct district board)", "NoSuchPlatform (direct board)"))
  expect_equal(WSBA_POSTING_SOURCE, edjobs::current_state_config()$feed$label)
  expect_equal(WSBA_FEED_LABEL, WSBA_POSTING_SOURCE)
  expect_equal(WSBA_FEED_URL, edjobs::current_state_config()$feed$url)
  # The backfill writes the same wording the scrape does.
  expect_true(all(c(K12_ID_PREFIX_SOURCES, K12_LINK_HOST_SOURCES) %in% K12_PLATFORM_SOURCE_LABELS))
})

test_that("remove_wsba_direct_duplicates provides full central safety net fallback when direct scraper is empty", {
  wsba <- data.frame(
    title = c("Elementary Teacher", "Substitute Teacher"),
    date_posted = as.Date(c("2026-10-28", "2026-10-29")),
    District = c("Fremont County School District 24", "Fremont County School District 24"),
    stringsAsFactors = FALSE
  )
  # Direct scraper errored or returned 0 rows for Fremont 24
  direct <- data.frame(
    title = "Math Teacher",
    date_posted = as.Date("2026-10-28"),
    District = "Natrona School District 1",
    stringsAsFactors = FALSE
  )

  result <- remove_wsba_direct_duplicates(wsba, direct)

  expect_equal(nrow(result), 2)
  expect_equal(result$District, c("Fremont County School District 24", "Fremont County School District 24"))
  expect_equal(result$title, c("Elementary Teacher", "Substitute Teacher"))
})

test_that("generate_wsba_discrepancy_report categorizes ghosts, unmatched jobs, and non-district rows", {
  wsba <- data.frame(
    title = c(
      "Science Teacher",                  # Duplicate of direct (should NOT appear in report)
      "Warehouse Specialist",             # Unmatched, recent (<60d) for Laramie 1
      "Elementary Teacher",               # Unmatched, stale (>60d) for Laramie 1 (ghost job)
      "Undated Role",                     # Unmatched, no date for Laramie 1
      "Speech Pathologist"                # Unmatched, but STRIDE has no direct scraper (ignore)
    ),
    date_posted = c(
      as.character(Sys.Date() - 5),
      as.character(Sys.Date() - 10),
      as.character(Sys.Date() - 120),
      NA,
      as.character(Sys.Date() - 5)
    ),
    location = c("Cheyenne", "Cheyenne", "Cheyenne", "Cheyenne", "Cheyenne"),
    url = c("https://wsba-wy.org/1", "https://wsba-wy.org/2", "https://wsba-wy.org/3", "https://wsba-wy.org/4", "https://wsba-wy.org/5"),
    District = c(
      "Laramie County School District 1",
      "Laramie County School District No. 1",
      "Laramie County School District 1",
      "Laramie County School District 1",
      "STRIDE Learning Center"
    ),
    stringsAsFactors = FALSE
  )

  direct <- data.frame(
    title = "SCIENCE teacher",
    date_posted = as.character(Sys.Date() - 5),
    District = "Laramie County School District 1",
    stringsAsFactors = FALSE
  )

  report <- generate_wsba_discrepancy_report(wsba, direct)

  expect_equal(nrow(report), 3)
  expect_equal(report$Title, c("Warehouse Specialist", "Elementary Teacher", "Undated Role"))
  expect_equal(unique(report$District), "Laramie County School District 1")
  expect_equal(unique(report$Source), "WSBA")
  expect_true(grepl("investigate ATS drift", report$Discrepancy_Type[1]))
  expect_true(grepl("Potential ghost posting on WSBA", report$Discrepancy_Type[2]))
  expect_equal(report$Discrepancy_Type[3], "Unmatched on direct scraper (no date)")
  expect_equal(report$Detected_Date[1], as.character(Sys.Date()))

  # Verify report_path writes out the file during deduplication
  tmp <- tempfile(fileext = ".csv")
  deduped <- remove_wsba_direct_duplicates(wsba, direct, report_path = tmp)
  expect_true(file.exists(tmp))
  read_back <- read.csv(tmp, stringsAsFactors = FALSE)
  expect_equal(nrow(read_back), 3)
  expect_equal(nrow(deduped), 4) # 1 duplicate dropped, 4 survived (including STRIDE)
  unlink(tmp)
})
