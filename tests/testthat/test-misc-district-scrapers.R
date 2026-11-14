test_that("canonicalize_wsba_district maps 'No. N' and '# N' format to project convention", {
  expect_equal(canonicalize_wsba_district("Niobrara County School District No. 1"), "Niobrara County School District 1")
  expect_equal(canonicalize_wsba_district("Weston County School District No. 7"), "Weston County School District 7")
  expect_equal(canonicalize_wsba_district("Fremont County School District #21"), "Fremont County School District 21")
})

test_that("canonicalize_wsba_district leaves non-matching org names unchanged rather than dropping them", {
  expect_equal(canonicalize_wsba_district("Snowy Range Academy"), "Snowy Range Academy")
})

test_that("canonicalize_wsba_district drops 'County' for the Big Horn/Natrona naming exception", {
  # Every other district keeps "County" (see the test above), but these two
  # don't -- matching canonicalize_ccd_district_name() (ccd_staff_scraper.R),
  # canonicalize_wsba_district_name() (salary_scrapers.R), and the SAIPE
  # district-name fix (census_saipe_scraper.R). A WSBA vacancy for either
  # district previously landed under "<...> County School District <N>",
  # silently splitting it from the real district everywhere else in the
  # pipeline canonicalizes it without "County".
  expect_equal(canonicalize_wsba_district("Natrona County School District No. 1"), "Natrona School District 1")
  expect_equal(canonicalize_wsba_district("Big Horn County School District No. 2"), "Big Horn School District 2")
  # Unaffected districts still keep "County".
  expect_equal(canonicalize_wsba_district("Carbon County School District No. 1"), "Carbon County School District 1")
})

test_that("parse_wsba_vacancies extracts real entries and direct URLs from the committed WSBA fixture", {
  # Real fixture: full HTML captured from https://www.wsba-wy.org/vacancies.
  # Confirmed this is server-rendered directly (Google Sites splits text
  # across many <span> tags, which is why a naive substring grep misses it
  # -- rvest::html_text2() reconstructs it correctly, no browser needed).
  html <- paste(readLines(test_path("fixtures", "wsba_vacancies.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  result <- parse_wsba_vacancies(html)

  expect_gt(nrow(result), 100)
  expect_equal(names(result), c("Title", "District", "Location", "Posted_Date", "Url"))
  expect_true(all(!is.na(result$Url) & nzchar(result$Url)))
  expect_true(any(grepl("applitrack\\.com|tedk12\\.com|schoolspring\\.com", result$Url)))

  niobrara <- result[result$District == "Niobrara County School District 1", ]
  expect_equal(nrow(niobrara), 2)
  expect_true("K-6 Elementary Teacher" %in% niobrara$Title)
  expect_true("K-12 Band Teacher" %in% niobrara$Title)

  weston1 <- result[result$District == "Weston County School District 1", ]
  # Regression: WSBA is known to be incomplete (confirmed by comparing
  # against Weston 1's own site, which lists 16 postings vs. WSBA's 6) --
  # with flexible delimiter matching, all 6 postings WSBA reports are captured.
  expect_equal(nrow(weston1), 6)
})

test_that("parse_wsba_vacancies returns zero rows (not an error) when no entries match the expected pattern", {
  result <- parse_wsba_vacancies("<html><body><p>No vacancies</p></body></html>")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "District", "Location", "Posted_Date", "Url"))
})

test_that("normalize_title collapses case/punctuation/whitespace differences", {
  expect_equal(normalize_title("K-12 Music/Band Instructor"), normalize_title("k 12  music band instructor!!"))
})

test_that("dedupe_against_wsba removes only same-district, same-normalized-title matches", {
  district_postings <- data.frame(
    Title = c("K-6 Elementary Teacher", "SPED Paraprofessional", "Bus Driver"),
    stringsAsFactors = FALSE
  )
  wsba_postings <- data.frame(
    Title = c("K-6 Elementary Teacher", "K-12 Band Teacher"),
    District = c("Niobrara County School District 1", "Niobrara County School District 1"),
    stringsAsFactors = FALSE
  )

  result <- dedupe_against_wsba(district_postings, wsba_postings, "Niobrara County School District 1")

  expect_equal(nrow(result), 2)
  expect_false("K-6 Elementary Teacher" %in% result$Title)
  expect_true(all(c("SPED Paraprofessional", "Bus Driver") %in% result$Title))
})

test_that("dedupe_against_wsba does not remove a matching title from a DIFFERENT district", {
  district_postings <- data.frame(Title = c("Substitute Teacher"), stringsAsFactors = FALSE)
  wsba_postings <- data.frame(
    Title = "Substitute Teacher",
    District = "Some Other District",
    stringsAsFactors = FALSE
  )

  result <- dedupe_against_wsba(district_postings, wsba_postings, "Niobrara County School District 1")
  expect_equal(nrow(result), 1)
})

test_that("dedupe_against_wsba is a no-op when the district has no WSBA entries at all", {
  district_postings <- data.frame(Title = c("Anything"), stringsAsFactors = FALSE)
  wsba_postings <- data.frame(Title = character(0), District = character(0), stringsAsFactors = FALSE)

  result <- dedupe_against_wsba(district_postings, wsba_postings, "Lincoln County School District 2")
  expect_equal(nrow(result), 1)
})

test_that("dedup_title_key matches the wrappers district pages add, and keeps distinct jobs apart", {
  # Real pairs from the WY archives (2026-08 to 2026-11): district page vs WSBA.
  same <- list(
    c("Elementary Special Education Paraprofessional 7/14/2026", "Elementary Special Education Paraprofessional"),
    c("High School Girls Assistant Basketball Coach 8/7/2026.docx", "High School Girls Assistant Basketball Coach"),
    c("K-12 Music Teacher 2026-27 School Year", "K-12 Music Teacher"),
    c("Speech Language Pathologist Job Description", "Speech-Language Pathologist"),
    c("MS Boys Basketball Coach", "Middle School Boys Basketball Coach"),
    c("Middle School  Special Education Paraprofessional 08/04/2026", "Middle School Special Education Paraprofessional"))
  for (p in same) expect_identical(dedup_title_key(p[1]), dedup_title_key(p[2]), label = p[1])
  # Different jobs a looser (subset) rule would have merged.
  different <- list(
    c("Speech-Language Pathologist", "Speech Language Pathologist Assistant (SLPA) 26-27.docx"),
    c("Assistant Girls Wrestling Coach", "Middle School Assistant Girls Wrestling Coach"),
    c("JH Assistant Basketball Coaches", "Junior High Assistant Boys Basketball Coach"))
  for (p in different) expect_false(identical(dedup_title_key(p[1]), dedup_title_key(p[2])), label = p[1])
})

test_that("dedupe_against_wsba drops a district-page copy whose title only adds a date or file name", {
  own <- data.frame(Title = c("Speech Language Pathologist Job Description", "Head Boys Swimming Coach"),
                    Location = NA, Posted_Date = NA, Link = NA, stringsAsFactors = FALSE)
  wsba <- data.frame(Title = "Speech-Language Pathologist", District = "Weston County School District 1",
                     Location = NA, Posted_Date = NA, Url = NA, stringsAsFactors = FALSE)
  expect_equal(dedupe_against_wsba(own, wsba, "Weston County School District 1")$Title, "Head Boys Swimming Coach")
})

test_that("a district's one posting for both teams gives way to WSBA's Boys and Girls rows", {
  # Sheridan #3, 2026-11-08: the district page lists "JH Assistant Basketball
  # Coaches", WSBA lists a Boys and a Girls row; two positions in all.
  wsba <- data.frame(Title = c("Junior High Assistant Boys Basketball Coach", "Junior High Assistant Girls Basketball Coach",
                               "Substitute Custodial Positions"),
                     District = "Sheridan County School District 3", Location = NA, Posted_Date = NA, Url = NA,
                     stringsAsFactors = FALSE)
  own <- data.frame(Title = c("JH Assistant Basketball Coaches", "Substitute Custodial Positions"),
                    Location = NA, Posted_Date = NA, Link = NA, stringsAsFactors = FALSE)
  expect_equal(nrow(dedupe_against_wsba(own, wsba, "Sheridan County School District 3")), 0)
  # A gendered district title is never dropped for the other team's WSBA row.
  own2 <- data.frame(Title = "Junior High Assistant Boys Basketball Coach", Location = NA, Posted_Date = NA, Link = NA,
                     stringsAsFactors = FALSE)
  wsba2 <- wsba[2, ]
  expect_equal(nrow(dedupe_against_wsba(own2, wsba2, "Sheridan County School District 3")), 1)
})
