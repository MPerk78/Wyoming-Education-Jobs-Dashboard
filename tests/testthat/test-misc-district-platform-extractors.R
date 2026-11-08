# All fixtures here are real captured HTML/text from the actual 12
# districts' own pages, not synthetic approximations -- every one of these
# extractors was built against and debugged with real data, and several
# genuine bugs (attribute-name casing, non-breaking spaces, unscoped
# selectors pulling in unrelated forms/salary schedules) were only caught
# by testing against these exact fixtures. See misc_district_scrapers.R's
# header comment for the full platform-to-district mapping.

fixture_path <- function(name) test_path("fixtures", "misc_districts", name)
read_fixture <- function(name) paste(readLines(fixture_path(name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# ---------------------------------------------------------------------------
# Edlio (Washakie County SD2, Weston County SD1)
# ---------------------------------------------------------------------------

test_that("parse_edlio_postings extracts prose-with-colon titles (Washakie 2's real page)", {
  html <- read_fixture("edlio_washakie2.html")
  result <- parse_edlio_postings(html)

  expect_equal(nrow(result), 2)
  expect_true("K-12 Music Teacher 2026-27 School Year" %in% result$Title)
  expect_true("High School Girls Basketball Coach" %in% result$Title)
})

test_that("parse_edlio_postings extracts file-list titles scoped to the Open Positions heading only (Weston 1's real page)", {
  # Regression: Weston 1's real page has 3 separate file-list widgets --
  # "Open Positions", an application-forms widget, and a salary-schedule
  # widget. An earlier version pulled titles from all 3, including
  # "WCSD Classified Application" and salary schedule PDFs alongside real
  # postings.
  html <- read_fixture("edlio_weston1.html")
  result <- parse_edlio_postings(html)

  expect_equal(nrow(result), 16)
  expect_true("Business Manager Position" %in% result$Title)
  expect_true("Route Bus Drivers" %in% result$Title)
  expect_false(any(grepl("Application|Salary Schedule|Employee Benefits", result$Title)))
})

test_that("parse_edlio_postings extracts scrollingTabs br-separated titles across all 3 category tabs (Uinta 4's real page)", {
  # Regression: a materially different Edlio page template from the other
  # two districts -- Certified/Classified/Coaching tabs, each a single
  # JSON-embedded HTML string with postings as plain <br>-separated lines
  # and no per-posting markup at all. The first line in each tab carries a
  # leading <p><font ...> prefix (precedes the first <br> rather than
  # following one) that must be stripped per-line, not just once for the
  # whole content string, or "K-12 Special Education Teacher" and "School
  # Bus Route Driver" (both first-in-tab) are silently dropped while every
  # later line in the same tab extracts fine.
  html <- read_fixture("edlio_uinta4_tabs.html")
  result <- parse_edlio_postings(html)

  expect_equal(nrow(result), 13)
  expect_true("K-12 Special Education Teacher" %in% result$Title)  # Certified, first line
  expect_true("SPED Consultant Case Manager (part-time)" %in% result$Title)  # Certified, last line
  expect_true("School Bus Route Driver" %in% result$Title)  # Classified, first line
  expect_true("Bus Assistant" %in% result$Title)  # Classified, last line
})

test_that("parse_edlio_postings extracts <li>-based Coaching-tab postings alongside a sibling tab's <br>-based ones", {
  # Regression: found 2026-09-04 re-checking Uinta 4's Partial-tier
  # coverage live -- the Coaching tab on this same fixture has ZERO <br>
  # tags at all (postings are <li> items inside <ul> lists, with
  # "High School"/"Middle School" category sub-headers as separate <p>
  # tags outside any <li>), so the <br>-only extraction above previously
  # treated the whole tab -- headers, postings, trailing prose -- as one
  # un-splittable blob that failed the length filter and silently
  # vanished: 8 real coaching postings, missing with no error and no
  # visible signal (fetch_edlio_postings() still returned a non-empty,
  # plausible-looking result from the other two tabs). The Classified
  # tab's OWN "Classified Job Application"/"Bus Driver Job Application"
  # links are ALSO <li> items in a separate <ul> further down that same
  # tab -- confirms real <br> postings and an unrelated <li> list can
  # coexist in one tab, and that the shared "application" filter (not an
  # either/or choice of which shape to look for) is what correctly keeps
  # the real Classified <br> titles while dropping those two links.
  html <- read_fixture("edlio_uinta4_tabs.html")
  result <- parse_edlio_postings(html)

  expect_true(all(c(
    "HS Varsity Wrestling", "HS Assistant Boys Basketball (Freshman)",
    "Assistant Football", "Assistant Volleyball (pending participation numbers)",
    "Assistant 8th Girls Basketball", "Head 7th Girls Basketball",
    "Head and Assistant 7th Boys Basketball", "Head and Assistant Girls Wrestling"
  ) %in% result$Title))
  expect_false(any(grepl("Application", result$Title, ignore.case = TRUE)))
  # Category sub-headers are <p> tags outside any <li> -- must not leak in
  # as if they were postings themselves.
  expect_false(any(result$Title %in% c("High School", "Middle School")))
})

test_that("parse_edlio_postings returns zero rows (not an error) for a page with no matching content", {
  result <- parse_edlio_postings("<html><body><p>nothing here</p></body></html>")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

# ---------------------------------------------------------------------------
# SchoolBlocks (Sublette County SD1)
# ---------------------------------------------------------------------------

test_that("parse_schoolblocks_postings un-escapes the double-escaped embedded JSON and extracts real items", {
  # Regression: the "Current Job Announcements" widget's JSON is embedded
  # as a STRING within the page's own JS state, so the source contains
  # literal backslash-quote (\") sequences that must be un-escaped once
  # before any title/date extraction works at all.
  html <- read_fixture("schoolblocks_sublette1.html")
  result <- parse_schoolblocks_postings(html)

  expect_equal(nrow(result), 8)
  expect_true("Substitute Bus Driver" %in% result$Title)
  expect_true("2026-07-14" %in% result$Posted_Date)
})

test_that("parse_schoolblocks_postings returns zero rows (not an error) when the named widget isn't present", {
  result <- parse_schoolblocks_postings("<html>no matching widget here</html>")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

# ---------------------------------------------------------------------------
# SmartSites/ParentSquare (Park County SD16, Platte County SD1)
# ---------------------------------------------------------------------------

test_that("parse_smartsites_postings extracts document-list titles/dates and excludes application forms (Park 16's real page)", {
  # Regression: data-searchText (camelCase in the source) is lowercased by
  # the HTML parser to data-searchtext; querying the camelCase name
  # silently returns all-NA titles instead of erroring. Also regression:
  # the same document-list widget mixes blank application forms in with
  # real postings ("classified application"/"certified application"),
  # indistinguishable by markup, only by content.
  html <- read_fixture("smartsites_park16.html")
  result <- parse_smartsites_postings(html)

  expect_equal(nrow(result), 3)
  expect_true("HS Head Track Coach" %in% result$Title)
  expect_true("2026-04-28" %in% result$Posted_Date)
  expect_false(any(grepl("application", result$Title, ignore.case = TRUE)))
})

test_that("parse_smartsites_postings extracts image-alt titles when that's the only content (Platte 1's real page)", {
  # Regression: this template leaves its <a> self-closed (<a href=... />),
  # so <figure>/<img> are SIBLINGS of <a> in the parsed DOM, not
  # descendants -- the semantically "correct" `a > figure > img` selector
  # matches nothing on this real page. Also regression: application-form
  # links use the identical widget with no distinguishing heading, same
  # content-based exclusion as the document-list variant.
  html <- read_fixture("smartsites_platte1.html")
  result <- parse_smartsites_postings(html)

  expect_equal(nrow(result), 12)
  expect_true("Bus Driver" %in% result$Title)
  expect_true("WHS SPED Para" %in% result$Title)
  expect_false(any(grepl("application", result$Title, ignore.case = TRUE)))
})

test_that("parse_smartsites_postings keeps each document's date with its own title when an item has no link", {
  # Regression (found by the reviewer regression run, 2026-10-06): labels
  # were read with html_elements() across all items, so an item without a
  # link shifted every later date onto the wrong title.
  html <- paste0(
    '<ul><li class="ss-document-item" data-searchText="Bus Driver"><a class="ss-document-link" aria-label="Bus Driver added 2026-09-01"></a></li>',
    '<li class="ss-document-item" data-searchText="Classified Application"></li>',
    '<li class="ss-document-item" data-searchText="Para"><a class="ss-document-link" aria-label="Para added 2026-09-20"></a></li></ul>'
  )
  result <- parse_smartsites_postings(html)
  expect_equal(result$Title, c("Bus Driver", "Para"))
  expect_equal(result$Posted_Date, c("2026-09-01", "2026-09-20"))
})

test_that("parse_smartsites_postings drops decorative \"logo\" gallery images", {
  # Regression: Platte 1's gallery started carrying alt="logo"/"Logo" images
  # (2026-08-03), each scraped as a fake "Other" posting; the logo/Logo pair
  # also shared one posting_id and failed the 2026-09-25 weekly run.
  html <- paste0(
    '<ul><li class="picseries_image"><a href="/a" /><figure><img alt="Bus Driver"></figure></li>',
    '<li class="picseries_image"><a href="/b" /><figure><img alt="logo"></figure></li>',
    '<li class="picseries_image"><a href="/c" /><figure><img alt=" Logo "></figure></li></ul>'
  )
  expect_equal(parse_smartsites_postings(html)$Title, "Bus Driver")
})

test_that("parse_smartsites_postings returns zero rows (not an error) for a page with neither widget type", {
  result <- parse_smartsites_postings("<html><body>nothing</body></html>")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

# ---------------------------------------------------------------------------
# WordPress/Elementor (Lincoln County SD2)
# ---------------------------------------------------------------------------

test_that("parse_wordpress_postings captures every category after the Open Positions section, not just Classified/Certified", {
  # Regression: an earlier version whitelisted only "Classified"/
  # "Certified" by name and silently dropped the "Substitutes" and
  # "Coaching" categories entirely -- the exact same completeness gap
  # already found in WSBA (systematically missing substitute/coaching
  # postings). Also regression: naively capturing every heading on the
  # page (instead of scoping to after "Current Open Positions") pulls in
  # the page's own intro/contact-info text as if it were job titles.
  html <- read_fixture("wordpress_lincoln2.html")
  result <- parse_wordpress_postings(html)

  expect_equal(nrow(result), 17)
  expect_true("Activity Bus Driver- Star Valley" %in% result$Title)   # Classified
  expect_true("Educational Sign Language Interpreter- District Wide" %in% result$Title)  # Certified
  expect_true("Substitute Bus Aides- Star Valley" %in% result$Title)  # Substitutes
  expect_true("Volunteer Coach/Advisor" %in% result$Title)            # Coaching
  expect_false(any(grepl("close at 5:00 p.m.|non-discrimination|equal employment", result$Title, ignore.case = TRUE)))
})

test_that("parse_wordpress_postings returns zero rows (not an error) when there's no Current Open Positions section", {
  result <- parse_wordpress_postings("<html><body><h3 class='elementor-heading-title'>Something Else</h3></body></html>")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

# ---------------------------------------------------------------------------
# Google Sites (Uinta County SD6)
# ---------------------------------------------------------------------------

test_that("parse_googlesites_postings extracts both numbered-title patterns from a real rendered page", {
  # Plain rendered text (document.body.innerText), not raw HTML -- the page
  # itself only ever contains real posting text once JS-rendered (confirmed
  # live 2026-09-04: a plain fetch of this same page has none of the
  # posting text below at all), so parse_googlesites_postings() takes
  # already-rendered text the same way parse_apptegy_postings()/
  # parse_prairieview_postings() do, not HTML it parses itself.
  rendered <- read_fixture("googlesites_uinta6_rendered.txt")
  result <- parse_googlesites_postings(rendered)

  expect_equal(nrow(result), 5)
  expect_setequal(result$Title, c(
    "High School Boys Assistant Swim Coach",  # title-before-district pattern
    "HS Musical Assistant Director",          # title-after-district pattern
    "maintenance position",
    "substitute school bus drivers",
    "substitute school bus aides"
  ))
})

test_that("parse_googlesites_postings returns zero rows (not an error) when no numbered posting pattern is present", {
  result <- parse_googlesites_postings("Contact us for openings.")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

# ---------------------------------------------------------------------------
# Apptegy (Niobrara County SD1, Platte County SD2, Sheridan County SD3,
# Weston County SD7)
# ---------------------------------------------------------------------------

test_that("parse_apptegy_postings extracts both posting formats and excludes contact info (Niobrara 1's real rendered page)", {
  # Regression x3, all against this one real fixture:
  # 1. A non-breaking space (U+00A0) immediately after "Elementary
  #    Teacher:" is not matched by PCRE's non-Unicode \s, silently
  #    dropping that entire real posting from every downstream regex with
  #    no error.
  # 2. "Business Manager: Michelle Neuenschwander" (a contact-info line in
  #    the page's own "Additional Info" section) satisfies a naive
  #    colon-plus-long-text check and gets misidentified as a posting.
  # 3. "BUS DRIVER SUBSTITUTES- District Wide" and "CUSTODIAL SUBSTITUTE-
  #    District Wide" use a dash, not a colon, and have no description at
  #    all -- silently missed by the colon-only pattern, which would have
  #    repeated the classified/substitute-postings gap already found in
  #    WSBA.
  text <- read_fixture("apptegy_niobrara_rendered.txt")
  result <- parse_apptegy_postings(text)

  expect_equal(nrow(result), 8)
  expect_true("Elementary Teacher" %in% result$Title)
  expect_true("BUS DRIVER SUBSTITUTES" %in% result$Title)
  expect_true("CUSTODIAL SUBSTITUTE" %in% result$Title)
  expect_false(any(grepl("Business Manager", result$Title)))
})

test_that("parse_apptegy_postings extracts the dollar-rate posting format (Platte 2's real rendered page)", {
  # Regression: a third real Apptegy posting format found by checking all
  # 4 Apptegy districts' actual current pages (not just Niobrara 1) --
  # "Title $NNN/day" with neither a colon nor a dash, which the first two
  # patterns silently miss entirely.
  text <- read_fixture("apptegy_platte2_rendered.txt")
  result <- parse_apptegy_postings(text)

  expect_equal(nrow(result), 2)
  expect_true("Substitute Certified Teachers" %in% result$Title)
  expect_true("Substitute Paraeducator" %in% result$Title)
})

test_that("parse_apptegy_postings extracts the lead-in-plus-flat-list format (Weston 7's real rendered page)", {
  # Regression: a fourth real Apptegy posting format, found 2026-09-04 while
  # A/B-verifying this district's page was live-checked as "0 rows" and
  # turned out to have 6 real postings the first 3 patterns above all miss:
  # a lead-in sentence ending in a colon ("...is currently accepting
  # applications for the following ... positions ... school year:")
  # followed by a flat list of bare title lines, with the lead-in itself
  # wrapped across multiple blank-line-separated paragraphs by innerText.
  # The fixture has two independent instances of this shape on the same
  # page (5 substitute titles, then later a single "Paraprofessional -
  # Technology Aide" posting) plus a third, unrelated "Subs Needed" prose
  # block that restates the same standing substitute categories in
  # sentence form -- deliberately NOT extracted a second time from there,
  # since it would just duplicate the first block's titles under messier
  # phrasing, not surface anything new.
  text <- read_fixture("apptegy_weston7_rendered.txt")
  result <- parse_apptegy_postings(text)

  expect_equal(nrow(result), 6)
  expect_setequal(result$Title, c(
    "Substitute Teacher", "Substitute Paraprofessional", "Substitute Custodian",
    "Substitute Bus Driver (CDL preferred but willing to train)", "Substitute Lunchroom Cook",
    "Paraprofessional - Technology Aide"
  ))
})

test_that("parse_apptegy_postings extracts bare titles after a block of form links (Sheridan 3's real rendered page)", {
  # Regression, 2026-11-07: Sheridan 3's page lists its openings as bare lines
  # right after its Application/Salary links, matching none of the four
  # shapes above, so the scrape returned 0 while WSBA listed its postings.
  text <- read_fixture("apptegy_sheridan3_rendered.txt")
  result <- parse_apptegy_postings(text)
  expect_setequal(result$Title, c("JH Assistant Basketball Coaches", "Substitute Custodial Positions"))
})

test_that("parse_apptegy_postings returns zero rows (not an error) for rendered text with no matching pattern", {
  result <- parse_apptegy_postings("Just some regular page text with no postings.")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

# ---------------------------------------------------------------------------
# Educational Networks / "EN CMS" (Cheyenne Classical Academy, Wyoming
# Classical Academy)
# ---------------------------------------------------------------------------

test_that("parse_educational_networks_postings extracts real titles and excludes filled/empty cells (Cheyenne Classical's real page)", {
  # Regression x3, all against this one real fixture:
  # 1. A trailing " NEW" tag on the title's own line ("Paraeducator NEW")
  #    must be stripped, not kept as part of the title.
  # 2. "(All Positions Filled)" appears both on the title's own line
  #    ("6th Grade Teacher (All Positions Filled)") and on its own
  #    separate line two rows later ("K-7 Latin Teacher" / next line
  #    "(All Positions Filled)") -- both must be excluded regardless of
  #    which line the marker lands on.
  # 3. A stray cell containing nothing but an "Apply Now" button (no
  #    descriptive text at all) must not surface as a fake posting titled
  #    "Apply Now" -- the real bug caught by this fixture during
  #    development, via nzchar(NA_character_) being TRUE in R.
  html <- read_fixture("educational_networks_cheyenne_classical.html")
  result <- parse_educational_networks_postings(html)

  expect_equal(nrow(result), 3)
  expect_equal(sort(result$Title), sort(c("Paraeducator", "IT Support Technician", "Substitute Teacher Positions")))
  expect_false(any(grepl("Filled", result$Title)))
  expect_false("Apply Now" %in% result$Title)
  expect_false(any(c("Instructional Coach", "Art Teacher", "K–7 Latin Teacher", "6th Grade Teacher",
                      "Long-Term Substitute Teacher") %in% result$Title))
})

test_that("parse_educational_networks_postings extracts real titles from a differently-formatted table on the same platform (Wyoming Classical's real page)", {
  # Same CMS platform as Cheyenne Classical, but its cells are formatted
  # completely differently (no bold title span, title and the "VIEW JOB
  # POSTING" link text sometimes concatenated with no separator at all --
  # e.g. "Grades K-2 Classroom TeacherVIEW JOB POSTING") -- confirms the
  # extraction isn't accidentally over-fit to one school's specific markup.
  html <- read_fixture("educational_networks_wyoming_classical.html")
  result <- parse_educational_networks_postings(html)

  expect_equal(nrow(result), 19)
  expect_true("Grades K-2 Classroom Teacher" %in% result$Title)
  expect_true("Custodian" %in% result$Title)
  expect_false(any(grepl("VIEW JOB POSTING", result$Title, ignore.case = TRUE)))
})

test_that("parse_educational_networks_postings returns zero rows (not an error) for a page with no matching table", {
  result <- parse_educational_networks_postings("<html><body><p>Not this platform</p></body></html>")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

# ---------------------------------------------------------------------------
# Prairie View Community School
# ---------------------------------------------------------------------------

test_that("parse_prairieview_postings scopes to headings strictly between Open Positions and Application Process (real captured headings)", {
  # Regression: the page's OWN marketing headings ("Why Work With Us",
  # "Mission-Driven Work", "Small, Collaborative Team", "Professional
  # Growth", "Work-Life Balance", all before "Open Positions") and its
  # application-process sub-steps ("Review the Job Description", "Submit
  # Your Application", "Interview & Background Check", all after
  # "Application Process") must NOT be mistaken for job titles -- this
  # fixture is the real heading list captured live from the rendered page
  # on 2026-08-06, in real page order.
  headings_json <- read_fixture("prairieview_headings.json")
  result <- parse_prairieview_postings(headings_json)

  expect_equal(nrow(result), 4)
  expect_equal(result$Title, c("Custodian / Maintenance", "Paraprofessional", "Substitute Teacher", "Bus Driver"))
  expect_false(any(grepl("Why Work|Mission-Driven|Review the Job|Application Process", result$Title)))
})

test_that("parse_prairieview_postings returns zero rows (not an error) when neither section heading is present", {
  result <- parse_prairieview_postings('["Home","About","Contact"]')
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("Title", "Location", "Posted_Date", "Link"))
})

test_that("parse_prairieview_postings returns zero rows when Open Positions has no headings before Application Process", {
  result <- parse_prairieview_postings('["Employment","Open Positions","Application Process","Contact"]')
  expect_equal(nrow(result), 0)
})
