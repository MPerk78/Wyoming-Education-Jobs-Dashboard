# Scrapers for the "miscellaneous" K-12 districts that don't sit on any of
# the major platforms covered by direct_api_scrapers.R (previously hand-
# maintained in otherjobs.xlsx, which no code in this repo ever wrote).
#
# Two complementary sources, deliberately kept separate rather than forced
# into one:
#
# 1. WSBA (Wyoming School Boards Association) publishes a statewide
#    vacancies page (wsba-wy.org/vacancies) with clean, reliably-parseable
#    structured entries (title/district/location/posted-date). It's
#    actively current, but a direct comparison against several districts'
#    own sites (Niobrara County SD1, Weston County SD1) found it
#    systematically omits classified/support/substitute/assistant-coach
#    postings -- only ~25-40% of a district's real open positions
#    typically appear there. It is NOT a complete source on its own.
#
# 2. Each district's own site, to catch what WSBA misses. These sit on six
#    different small-business/school-CMS platforms (Apptegy, Edlio,
#    SchoolBlocks, SmartSites/ParentSquare, WordPress, Google Sites), none
#    of which model job postings as structured data -- postings are free-
#    text prose, bulleted lists, embedded-JSON file listings, or (Platte
#    County SD1 specifically) image alt-text with no visible title text at
#    all. Extraction here is necessarily heuristic per platform family,
#    tested against real captured content, but inherently less reliable
#    than every other source in this pipeline. All but Apptegy turned out
#    to be server-rendered (confirmed by comparing plain httr2 output
#    against a chromote-rendered page) -- only Apptegy's 4 districts
#    (Niobrara 1, Platte 2, Sheridan 3, Weston 7) genuinely need a browser.
#
# Entries found on a district's own page are deduplicated against that
# same district's WSBA entries by normalized title (lowercased, whitespace/
# punctuation collapsed, dates/file names/"Job Description" dropped,
# abbreviations spelled out) before being combined -- see
# dedupe_against_wsba() and dedup_title_key(). This is a heuristic match, not exact: differently-
# worded titles for the same real posting (e.g. WSBA's "K-12 Music/Band
# Instructor" vs. a district's own "Job Description - Music Teacher K12")
# will not be recognized as duplicates and may appear twice. Prefer a
# missed duplicate over a silently dropped real posting.

suppressMessages({
  library(httr2)
  library(rvest)
  library(dplyr)
  library(stringr)
})

# ---------------------------------------------------------------------------
# WSBA statewide vacancies page
# ---------------------------------------------------------------------------

# The page is a Google Sites embed of a Google Doc; despite that, the full
# vacancy list is server-rendered directly in the raw HTML (confirmed: a
# plain HTTP fetch + rvest::html_text2() reproduces byte-identical content
# to a chromote-rendered version), so no browser is needed here either.
fetch_wsba_vacancies <- function(url = "https://www.wsba-wy.org/vacancies") {
  resp <- request(url) %>% perform_with_retry()
  parse_wsba_vacancies(resp_body_string(resp))
}

# Maps WSBA's "<County> County School District No. <N>" naming to this
# project's canonical "<County> County School District <N>" (no "No.").
# Only covers districts this project actually tracks in the misc bucket;
# an unmapped WSBA district name is left as-is rather than dropped, so a
# genuinely new/renamed district still surfaces (just without
# canonicalization) instead of silently vanishing.
#
# Big Horn and Natrona are this project's one canonical-naming exception:
# every other district keeps "County", but these two drop it (see
# canonicalize_ccd_district_name() in ccd_staff_scraper.R, the SAIPE
# district-name fix in census_saipe_scraper.R, and
# canonicalize_wsba_district_name() in salary_scrapers.R, which already
# apply the same fix for their own sources). Without this, a WSBA vacancy
# for either district lands under "<...> County School District <N>" -- a
# phantom duplicate combinedclean's own canonicalize_k12_district() never
# catches (it only fixes misspellings, not this naming exception) -- and
# that phantom row would carry no salary/vacancy-rate join at all.
# Then canonicalize_k12_district() for WSBA's own misspellings ("Cabon County
# School District 2", 2026-10-30), so its rows meet the direct rows they
# duplicate in remove_wsba_direct_duplicates().
canonicalize_wsba_district <- function(district) {
  district <- str_replace(district, " School District (?:No\\.|#) ?(\\d+)$", " School District \\1")
  canonicalize_k12_district(str_replace(district, "^(Big Horn|Natrona) County School District", "\\1 School District"))
}

parse_wsba_vacancies <- function(html_text) {
  soup <- rvest::read_html(html_text)
  delim <- "(?:\\s+-\\s*|\\s*-\\s+)"
  entry_pattern <- paste0("^(.+?)", delim, "(.+? School District(?: (?:No\\.|#) \\d+)?|[^-]+?)", delim, "([^,]+),\\s*Wyoming$")
  date_pattern <- "^Posted (\\d{1,2}/\\d{1,2}/\\d{4})$"

  ps <- rvest::html_elements(soup, "p")
  p_texts <- rvest::html_text(ps, trim = TRUE)
  entry_idx <- which(grepl(entry_pattern, p_texts, perl = TRUE))

  if (length(entry_idx) > 0) {
    rows <- lapply(entry_idx, function(i) {
      m <- regmatches(p_texts[i], regexec(entry_pattern, p_texts[i], perl = TRUE))[[1]]
      title <- trimws(m[2])
      district <- canonicalize_wsba_district(trimws(m[3]))
      location <- trimws(m[4])

      a_elem <- rvest::html_element(ps[i], "a")
      href <- if (!is.na(a_elem)) rvest::html_attr(a_elem, "href") else NA_character_
      url <- if (!is.na(href) && nzchar(trimws(href))) trimws(href) else WSBA_VACANCIES_URL

      j <- i + 1
      while (j <= length(p_texts) && !nzchar(p_texts[j])) j <- j + 1
      posted_date <- if (j <= length(p_texts) && grepl(date_pattern, p_texts[j])) {
        d <- sub(date_pattern, "\\1", p_texts[j])
        as.character(as.Date(d, format = "%m/%d/%Y"))
      } else {
        NA_character_
      }

      data.frame(Title = title, District = district, Location = location,
                 Posted_Date = posted_date, Url = url, stringsAsFactors = FALSE)
    })
    return(do.call(rbind, rows))
  }

  # Fallback to plain lines if no <p> tags matched
  text <- rvest::html_text2(soup)
  lines <- strsplit(text, "\n")[[1]]
  line_idx <- grep(entry_pattern, lines, perl = TRUE)

  if (length(line_idx) == 0) {
    return(data.frame(Title = character(0), District = character(0),
                       Location = character(0), Posted_Date = character(0),
                       Url = character(0), stringsAsFactors = FALSE))
  }

  rows <- lapply(line_idx, function(i) {
    m <- regmatches(lines[i], regexec(entry_pattern, lines[i], perl = TRUE))[[1]]
    title <- trimws(m[2])
    district <- canonicalize_wsba_district(trimws(m[3]))
    location <- trimws(m[4])

    j <- i + 1
    while (j <= length(lines) && !nzchar(trimws(lines[j]))) j <- j + 1
    posted_date <- if (j <= length(lines) && grepl(date_pattern, lines[j])) {
      d <- sub(date_pattern, "\\1", lines[j])
      as.character(as.Date(d, format = "%m/%d/%Y"))
    } else {
      NA_character_
    }

    data.frame(Title = title, District = district, Location = location,
               Posted_Date = posted_date, Url = WSBA_VACANCIES_URL, stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Deduplication: district's own postings vs. that district's WSBA entries
# ---------------------------------------------------------------------------

normalize_title <- function(title) {
  title <- tolower(title)
  title <- str_replace_all(title, "[^a-z0-9]+", " ")
  str_trim(str_squish(title))
}

# The key dedupe_against_wsba() matches on: normalize_title() after dropping
# what district pages wrap around a title and WSBA doesn't -- file
# extensions ("...Coach 8/7/2026.docx"), dates ("Paraprofessional
# 7/14/2026", "Closes 8/26"), school years ("2026-27 School Year"), "Job
# Description" -- with the common abbreviations (JH, HS, MS, Asst, SPED)
# spelled out and plurals dropped. Still an exact match on what's left: a
# subset rule was measured and rejected (it paired "Speech-Language
# Pathologist" with "...Pathologist Assistant"). 2026-11-08, last 30 WY
# archives: 7 more duplicate pairs (Sublette #1, Washakie #2, Weston #1,
# Park #16), nothing distinct merged.
dedup_title_key <- function(title) {
  t <- tolower(as.character(title))
  t <- gsub("\\.(docx?|pdf)\\b", " ", t)
  t <- gsub("\\b(closes?|closing|posted|deadline)\\b[^a-z]*\\d{1,2}/\\d{1,2}(/\\d{2,4})?", " ", t)
  t <- gsub("\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\b", " ", t)
  t <- gsub("\\b(20)?\\d{2}\\s*[-/\u2013]\\s*(20)?\\d{2}\\b(\\s+school\\s+year)?", " ", t)
  t <- gsub("\\b(job\\s+)?description\\b", " ", t)
  t <- gsub("[^a-z0-9]+", " ", t)
  abbrev <- c(jh = "junior high", hs = "high school", ms = "middle school", asst = "assistant",
              sped = "special education")
  vapply(strsplit(trimws(t), " +"), function(w) {
    w <- ifelse(w %in% names(abbrev), abbrev[w], w)
    w <- unlist(strsplit(w, " "))
    w <- sub("(?<=[a-z]{3})(es|s)$", "", w, perl = TRUE)
    paste(w[nzchar(w)], collapse = " ")
  }, character(1))
}

# Returns district_postings with any row whose dedup_title_key() matches a
# WSBA title's for the SAME district removed. Matching is exact on that
# key, not fuzzy, so differently-worded duplicates will both survive -- see
# the module-level note on why that's the safer failure mode here.
dedupe_against_wsba <- function(district_postings, wsba_postings, district_name) {
  if (nrow(district_postings) == 0) return(district_postings)

  wsba_titles_here <- wsba_postings$Title[wsba_postings$District == district_name]
  if (length(wsba_titles_here) == 0) return(district_postings)

  wsba_norm <- dedup_title_key(wsba_titles_here)
  own <- dedup_title_key(district_postings$Title)
  # One district posting for both teams ("JH Assistant Basketball Coaches",
  # Sheridan #3) that WSBA lists as a Boys and a Girls row: WSBA's rows carry
  # the two positions, so the district's combined row is the duplicate
  # (decided 2026-11-09). Only a title that names no gender is matched this
  # way, so a district's "Boys ... Coach" is never dropped for WSBA's
  # "Girls ... Coach". No other case in the last 30 WY archives.
  gender <- "\\b(boy|girl|men|women)\\b"
  no_gender <- function(k) trimws(gsub("\\s+", " ", gsub(gender, " ", k)))
  both_teams <- !grepl(gender, own) & own %in% no_gender(wsba_norm[grepl(gender, wsba_norm)])
  district_postings[!(own %in% wsba_norm | both_teams), , drop = FALSE]
}

empty_misc_result <- function() {
  data.frame(Title = character(0), Location = character(0),
             Posted_Date = character(0), Link = character(0),
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Edlio (Washakie County SD2, Weston County SD1, Uinta County SD4)
# ---------------------------------------------------------------------------

# Edlio content pages mix up to three independent block types, and a given
# page can use any combination: a "files" block (an uploaded-document
# list, each posting is literally a PDF/image named after the job --
# Weston 1's pattern), a "text" block (prose with the title bolded before
# a colon, in a plain <li> -- Washakie 2's pattern), and a "scrollingTabs"
# widget (Certified/Classified/Coaching tabs, each tab's content a single
# JSON-embedded HTML string with postings as plain <br>-separated lines,
# no title markup at all -- Uinta 4's pattern, a materially different
# Edlio page template from the other two). None of the three carry a
# posted date.
#
# A page can ALSO have "files" blocks that have nothing to do with job
# postings at all: salary schedules, application forms, or (confirmed on
# Washakie 2's real page) individual attachments referenced inline within
# unrelated prose, with no heading of their own. Only a files block whose
# nearest preceding bold heading contains "Open Position" is treated as a
# real posting list -- everything else is silently excluded, verified
# against both districts' real pages (Weston 1 has 3 separate files
# blocks: "Open Positions", an application-forms block, and a salary-
# schedule block; only the first is real postings).
fetch_edlio_postings <- function(url) {
  resp <- request(url) %>% perform_with_retry()
  parse_edlio_postings(resp_body_string(resp))
}

parse_edlio_postings <- function(html_text) {
  soup <- rvest::read_html(html_text)

  file_blocks <- rvest::html_elements(soup, ".page-block-files")
  file_titles <- character(0)
  file_links <- character(0)
  for (block in file_blocks) {
    heading <- rvest::html_element(block, xpath = "preceding::strong[1]")
    heading_text <- if (!is.na(heading)) rvest::html_text2(heading) else ""
    if (grepl("open position", heading_text, ignore.case = TRUE)) {
      links_in_block <- rvest::html_elements(block, "a")
      file_titles <- c(file_titles, rvest::html_text2(links_in_block))
      file_links <- c(file_links, rvest::html_attr(links_in_block, "href"))
    }
  }

  prose_nodes <- rvest::html_elements(soup, ".page-block-text li strong")
  prose_titles <- str_trim(str_remove(rvest::html_text2(prose_nodes), ":\\s*$"))

  # scrollingTabs widget: each real job-category tab is
  # "title":"Certified/Classified/Coaching(...)","content":"<html
  # string>","active". Content is itself HTML, JSON-string-escaped (\n,
  # \") -- postings are plain text separated by literal <br> tags, with no
  # per-posting markup to key off at all, so this can only be scoped by
  # tab title (Certified/Classified/Coaching), not by any structural
  # marker. A trailing licensing/application-instructions paragraph
  # follows the postings in the same tab; excluded by requiring each
  # candidate line be short and not itself prose (no period, not
  # containing "must"/"submit"/"application").
  tab_matches <- gregexpr(
    '"title":"(Certified[^"]*|Classified[^"]*|Coaching[^"]*)","content":"(.*?)","active"',
    html_text, perl = TRUE
  )
  tab_groups <- regmatches(html_text, tab_matches)[[1]]
  tab_titles <- character(0)
  for (tab in tab_groups) {
    m <- regmatches(tab, regexec('"title":"([^"]*)","content":"(.*?)","active"', tab, perl = TRUE))[[1]]
    content <- gsub("\\\\n", " ", m[3])
    content <- gsub("&nbsp;", " ", content)

    # Two independent tab-content shapes seen on real pages -- BOTH can
    # appear in the SAME tab, not just one or the other: plain text
    # separated by literal <br> tags (Uinta 4's Certified/Classified tabs
    # use this for the real postings themselves), and postings as <li>
    # items inside one or more <ul> lists, with category sub-headers
    # ("High School"/"Middle School") as separate <p> tags OUTSIDE any
    # <li> (Uinta 4's own Coaching tab -- has zero <br> tags at all, so
    # the <br>-only extraction previously treated the ENTIRE tab, headers
    # and all, as one un-splittable blob that failed the length filter
    # below and silently vanished -- 8 real coaching postings, not caught
    # by any test because the existing fixtures only covered the <br>
    # shape). The Classified tab's OWN trailing "Classified Job
    # Application"/"Bus Driver Job Application" links are also <li> items
    # (a real, separate <ul> further down the same tab, after the <br>
    # postings) -- an either/or choice keyed on "does this tab contain any
    # <li> at all" would silently drop that tab's real <br> postings the
    # moment it also has an unrelated <li> list; extracting candidates
    # from BOTH shapes unconditionally and relying on the shared filter
    # below to drop non-postings (the application links contain the word
    # "application") is what actually handles a tab having either, both,
    # or neither shape correctly.
    br_lines <- gsub("<[^>]+>", "", strsplit(content, "<br>")[[1]])
    li_items <- regmatches(content, gregexpr("<li>.*?</li>", content, perl = TRUE))[[1]]
    li_lines <- gsub("<[^>]+>", "", li_items)
    # The first line in particular carries a leading <p><font ...> prefix
    # (confirmed on Uinta 4's real page: "K-12 Special Education Teacher"
    # arrives as "...<font size=\"5\">K-12 Special Education Teacher"),
    # since that markup precedes the first <br> rather than following one
    # -- stripping tags first (above) rather than before splitting on
    # <br> would have destroyed the <br> delimiters themselves.
    lines <- str_trim(c(br_lines, li_lines))
    lines <- lines[nzchar(lines) & nchar(lines) < 80 & !grepl("must|submit|application|\\.$", lines, ignore.case = TRUE)]
    tab_titles <- c(tab_titles, lines)
  }

  titles <- c(file_titles, prose_titles, tab_titles)
  links <- c(file_links, rep(NA_character_, length(prose_titles) + length(tab_titles)))

  if (length(titles) == 0) return(empty_misc_result())

  data.frame(Title = titles, Location = NA_character_, Posted_Date = NA_character_,
             Link = links, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# SchoolBlocks (Sublette County SD1)
# ---------------------------------------------------------------------------

# The page embeds a Google-Drive-folder-backed widget (titled "Current Job
# Announcements" on this specific district's page) as a DOUBLE-escaped JSON
# string within the page's own JS state -- i.e. the substring contains
# literal backslash-quote (\") sequences, not real JSON syntax, until
# un-escaped once. Each item is then extracted by splitting on its own
# {"viewLink":" boundary rather than full JSON-parsing the whole blob,
# since the surrounding page has other unrelated "title" fields that a
# naive whole-page regex would also match.
fetch_schoolblocks_postings <- function(url, widget_title = "Current Job Announcements") {
  resp <- request(url) %>% perform_with_retry()
  parse_schoolblocks_postings(resp_body_string(resp), widget_title)
}

parse_schoolblocks_postings <- function(html_text, widget_title = "Current Job Announcements") {
  marker <- paste0("account_title\\\":\\\"", widget_title)
  idx <- regexpr(marker, html_text, fixed = TRUE)
  if (idx == -1) return(empty_misc_result())

  window <- substr(html_text, idx, min(idx + 10000, nchar(html_text)))
  unescaped <- gsub('\\\\"', '"', window)

  items_idx <- regexpr('"items":\\[', unescaped)
  if (items_idx == -1) return(empty_misc_result())
  items_region <- substr(unescaped, items_idx, min(items_idx + 6000, nchar(unescaped)))

  chunks <- strsplit(items_region, '\\{"viewLink":"')[[1]][-1]
  if (length(chunks) == 0) return(empty_misc_result())

  rows <- lapply(chunks, function(chunk) {
    title_m <- regmatches(chunk, regexpr('"title":"[^"]+"', chunk))
    date_m <- regmatches(chunk, regexpr('"modifiedTimestamp":"[^"]+"', chunk))
    if (length(title_m) == 0) return(NULL)
    title <- sub('"title":"([^"]+)"', "\\1", title_m)
    posted_date <- if (length(date_m) > 0) {
      substr(sub('"modifiedTimestamp":"([^"]+)"', "\\1", date_m), 1, 10)
    } else {
      NA_character_
    }
    data.frame(Title = title, Location = NA_character_, Posted_Date = posted_date,
               Link = NA_character_, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(empty_misc_result())
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# SmartSites / ParentSquare (Park County SD16, Platte County SD1)
# ---------------------------------------------------------------------------

# Two independent content-block types, either or both may appear on a
# given page: a "document search" list (li.ss-document-item, title in
# data-searchText, posted date embedded in the link's aria-label as
# "...added YYYY-MM-DD") and an "image gallery" block (bare <img alt="...">
# with no date at all -- confirmed this is Platte County SD1's *only*
# visible posting content: the page has no readable title text outside
# these image alt attributes).
fetch_smartsites_postings <- function(url) {
  resp <- request(url) %>% perform_with_retry()
  parse_smartsites_postings(resp_body_string(resp))
}

parse_smartsites_postings <- function(html_text) {
  soup <- rvest::read_html(html_text)

  doc_nodes <- rvest::html_elements(soup, "li.ss-document-item")
  # libxml2/rvest lowercases HTML attribute names on parse, so the source's
  # camelCase "data-searchText" must be queried as "data-searchtext".
  doc_titles_raw <- rvest::html_attr(doc_nodes, "data-searchtext")
  # html_element() (singular): one result per item, NA when an item has no
  # link, so labels stay aligned with titles. The plural form flattened all
  # links and shifted every later date onto the wrong title.
  doc_labels_raw <- rvest::html_attr(rvest::html_element(doc_nodes, "a.ss-document-link"), "aria-label")
  # Same document-search widget lists blank application forms alongside
  # real postings with no distinguishing markup (confirmed on Park County
  # SD16's real page: "classified application"/"certified application"
  # sit in the same list as real job titles) -- same content-based
  # exclusion as the image-gallery variant below.
  is_job <- !grepl("application", doc_titles_raw, ignore.case = TRUE)
  doc_titles <- doc_titles_raw[is_job]
  doc_labels <- doc_labels_raw[is_job]
  doc_dates <- ifelse(
    grepl("added \\d{4}-\\d{2}-\\d{2}", doc_labels),
    sub(".*added (\\d{4}-\\d{2}-\\d{2}).*", "\\1", doc_labels),
    NA_character_
  )

  # The image-gallery widget's own template leaves its <a> self-closed
  # (<a href="..." />), so the <figure>/<img> are its SIBLINGS in the
  # parsed DOM, not descendants -- "a > figure > img" (the semantically
  # "correct" selector) matches nothing on real pages using this template.
  # Both real postings and unrelated application-form links use this exact
  # same widget with no distinguishing heading or class, so the only
  # available signal is content: exclude anything whose alt text names a
  # form rather than a position (confirmed against the real page: titles
  # like "Certified Application"/"Classified Application"/"Coaching
  # Application"/"Substitute Teacher Application" are the district's
  # blank application forms, not open postings).
  img_nodes <- rvest::html_elements(soup, "li.picseries_image img[alt]")
  img_titles <- str_trim(rvest::html_attr(img_nodes, "alt"))
  # The same gallery also carries decorative images whose alt text is just
  # "logo"/"Logo" (Platte County SD1, 2026-08-03 onward) -- dropped the
  # same way, or each one lands in combinedclean as a fake "Other" posting.
  img_titles <- img_titles[nzchar(img_titles) &
                             !grepl("application", img_titles, ignore.case = TRUE) &
                             !grepl("^logo$", img_titles, ignore.case = TRUE)]

  titles <- c(doc_titles, img_titles)
  dates <- c(doc_dates, rep(NA_character_, length(img_titles)))

  if (length(titles) == 0) return(empty_misc_result())

  data.frame(Title = titles, Location = NA_character_, Posted_Date = dates,
             Link = NA_character_, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# WordPress/Elementor (Lincoln County SD2)
# ---------------------------------------------------------------------------

# Postings are plain <p> lines inside an Elementor text-editor widget,
# immediately preceded by a heading widget naming the category. The real
# page has 7 headings total, in document order: "Employment Opportunities"
# (page title), "Application Information" (intro/contact-info block, NOT a
# job category), "Current Open Positions" (the section boundary), then the
# 4 real job categories -- Classified, Certified, Substitutes, Coaching.
# An earlier version of this function whitelisted only "Classified"/
# "Certified" by name and silently dropped Substitutes/Coaching entirely,
# which would have repeated the exact completeness gap already found in
# WSBA (systematically missing substitute/coaching postings). Rather than
# hardcode the 4 real category names (fragile if a 5th category is added
# later) or capture every heading (wrongly pulls in the intro/contact
# text), this scopes to every heading that comes AFTER the "Current Open
# Positions" section heading in document order -- robust to new
# categories, still excludes the non-category intro content.
fetch_wordpress_postings <- function(url) {
  resp <- request(url) %>% perform_with_retry()
  parse_wordpress_postings(resp_body_string(resp))
}

parse_wordpress_postings <- function(html_text) {
  soup <- rvest::read_html(html_text)

  heading_nodes <- rvest::html_elements(soup, ".elementor-widget-heading")
  if (length(heading_nodes) == 0) return(empty_misc_result())

  heading_texts <- str_trim(rvest::html_text2(rvest::html_elements(heading_nodes, ".elementor-heading-title")))
  section_idx <- which(heading_texts == "Current Open Positions")
  if (length(section_idx) == 0) return(empty_misc_result())

  category_nodes <- heading_nodes[(section_idx[1] + 1):length(heading_nodes)]
  if (length(category_nodes) == 0) return(empty_misc_result())

  all_titles <- character(0)
  for (heading_node in category_nodes) {
    next_widget <- rvest::html_element(heading_node, xpath = "following::div[contains(@class,'elementor-widget-text-editor')][1]")
    if (!is.na(next_widget)) {
      lines <- rvest::html_text2(rvest::html_elements(next_widget, "p"))
      all_titles <- c(all_titles, str_trim(lines[nzchar(str_trim(lines))]))
    }
  }

  if (length(all_titles) == 0) return(empty_misc_result())

  data.frame(Title = all_titles, Location = NA_character_, Posted_Date = NA_character_,
             Link = NA_character_, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Google Sites (Uinta County SD6) -- the third platform family that
# genuinely needs a browser
# ---------------------------------------------------------------------------

# Previously assumed server-rendered, "same underlying rendering as the WSBA
# page" -- wrong, confirmed directly 2026-09-04: a plain httr2 fetch of the
# real live page contains none of its actual posting text at all (checked
# for "accepting applications", not present), while a chromote render of the
# identical URL shows it in full. This had been silently returning 0 rows
# for real, live postings the whole time it was plain-HTTP -- not a
# genuinely quiet district. Two independent title patterns confirmed on the
# real rendered page, both numbered ("N-"), but in opposite title/district
# order:
#
# 1. "N-  TITLE District Name County School District #N is
#    looking/seeking/accepting..." -- title comes BEFORE the district name
#    (e.g. "1-   High School Boys Assistant Swim Coach Uinta County School
#    District #6 is taking applications..."). Whitespace after the "N-" is
#    NOT reliably zero, unlike this pattern's original single confirmed
#    example ("1-Musical Director...") -- the real page uses anywhere from
#    0 to 3 spaces, so this must tolerate `\s*`, not assume none.
# 2. "N- District Name County School District #N is accepting/taking
#    applications for [a/an] TITLE." -- the inverse order, title comes
#    AFTER the district name and a fixed "applications for" phrase, ending
#    at the next period (e.g. "1- Uinta County School District #6 is
#    accepting applications for a HS Musical Assistant Director."). Not
#    caught by pattern 1's lookahead at all, since here the district name
#    comes immediately after the number, not the title -- confirmed this
#    silently dropped 4 of the page's 5 real postings (only the swim coach
#    posting used pattern 1's order).
fetch_googlesites_postings <- function(chromote_session, url) {
  chromote_session$Page$navigate(url)
  chromote_session$Page$loadEventFired(wait_ = TRUE, timeout_ = 30)
  Sys.sleep(5)
  text <- chromote_session$Runtime$evaluate("document.body.innerText")$result$value
  parse_googlesites_postings(text)
}

parse_googlesites_postings <- function(rendered_text) {
  # document.body.innerText preserves non-breaking spaces (U+00A0) from the
  # page's own styling, which R's \s (PCRE, non-Unicode mode) does not
  # match -- same gotcha parse_apptegy_postings() already documents and
  # handles, confirmed here too: the swim-coach posting's "1-" is followed
  # by two U+00A0 characters, not regular spaces, silently failing every
  # `\s*`/`\s+` in both patterns below without erroring.
  rendered_text <- gsub("\u00a0", " ", rendered_text)

  title_before_district <- regmatches(rendered_text, gregexpr(
    "\\d+-\\s*([A-Z][A-Za-z/ ]{2,60}?)(?=\\s+[A-Z][a-z]+ County School District|\\s+is (?:looking|seeking|accepting))",
    rendered_text, perl = TRUE
  ))[[1]]
  titles_1 <- str_trim(sub("^\\d+-\\s*", "", title_before_district))

  title_after_district <- regmatches(rendered_text, gregexpr(
    "\\d+-\\s*[A-Z][a-z]+ County School District #?\\d+ is (?:accepting|taking) applications for (?:a |an )?([A-Za-z][A-Za-z0-9/ '-]{2,60}?)\\.",
    rendered_text, perl = TRUE
  ))[[1]]
  titles_2 <- str_trim(sub(
    "^\\d+-\\s*[A-Z][a-z]+ County School District #?\\d+ is (?:accepting|taking) applications for (?:a |an )?([A-Za-z][A-Za-z0-9/ '-]+)\\.$",
    "\\1", title_after_district
  ))

  titles <- unique(c(titles_1, titles_2))
  titles <- titles[nzchar(titles)]

  if (length(titles) == 0) return(empty_misc_result())

  data.frame(Title = titles, Location = NA_character_, Posted_Date = NA_character_,
             Link = NA_character_, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Educational Networks / "EN CMS" (Cheyenne Classical Academy, Wyoming
# Classical Academy) -- found 2026-08-06 while investigating charter schools
# not yet in this pipeline
# ---------------------------------------------------------------------------

# Server-rendered (a plain httr2 fetch with a real browser User-Agent
# reproduces the full page -- confirmed by comparing against a chromote
# render), but structurally the most free-form source in this file: each
# posting is one hand-edited <td> in an "en-editable-table" CMS widget, with
# no consistent internal markup between cells (even within the same
# school's own table -- confirmed directly: some titles are wrapped in a
# bold <span>, others aren't; the "NEW" tag sometimes shares the title's
# own line, sometimes gets its own; "(All Positions Filled)" appears
# inline after the title on some rows and on its own line on others).
# Requires a real browser User-Agent for the same reason TedK12 does (see
# fetch_tedk12_postings() in direct_api_scrapers.R) -- unrelated platforms,
# same underlying cause (bot-detection-driven content negotiation).
fetch_educational_networks_postings <- function(url) {
  resp <- request(url) %>%
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36") %>%
    perform_with_retry()
  parse_educational_networks_postings(resp_body_string(resp))
}

# Rather than parse each cell's specific nested-span layout (too
# inconsistent, per the header comment above), works off html_text2()'s
# flattened text: strips the fixed, known action-link labels ("View Job
# Posting"/"View Job Description"/"Apply Now" -- CMS-template button text,
# never part of a real title) from wherever they appear in the cell, then
# takes the first line of what's left as the title. A "(All Positions
# Filled)" marker anywhere in the cell's full text excludes that posting
# entirely -- it's a real posting that's no longer open, not a live one.
# An empty title after stripping (a cell containing nothing but an "Apply
# Now" button and no descriptive text at all -- confirmed a real case,
# a stray/orphaned cell on Cheyenne Classical's page) is excluded too;
# nzchar(NA_character_) is TRUE in R, so that check is written as an
# explicit is.na() first, not folded into a single nzchar() condition.
parse_educational_networks_postings <- function(html_text) {
  page <- rvest::read_html(html_text)
  cells <- rvest::html_elements(page, "table.en-editable-table td")
  if (length(cells) == 0) return(empty_misc_result())

  titles <- vapply(cells, function(cell) {
    full_text <- trimws(rvest::html_text2(cell))
    if (!nzchar(full_text)) return(NA_character_)
    if (grepl("(?i)all positions filled", full_text, perl = TRUE)) return(NA_character_)

    title <- sub("(?i)\\s*(view job posting|view job description|apply now).*$", "", full_text, perl = TRUE)
    title <- trimws(strsplit(title, "\n")[[1]][1])
    title <- trimws(sub("(?i)\\s*new\\s*$", "", title))
    if (is.na(title) || !nzchar(title)) return(NA_character_)
    title
  }, character(1))

  titles <- titles[!is.na(titles)]
  if (length(titles) == 0) return(empty_misc_result())

  data.frame(Title = titles, Location = NA_character_, Posted_Date = NA_character_,
             Link = NA_character_, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Apptegy (Niobrara County SD1, Platte County SD2, Sheridan County SD3,
# Weston County SD7) -- the one platform family that genuinely needs a
# browser
# ---------------------------------------------------------------------------

# Apptegy content pages are a client-side-rendered Nuxt app; the raw HTML
# has no posting text at all (confirmed directly -- a plain fetch returns
# an empty shell). Postings are free prose written by each district's own
# staff, with no shared template -- three patterns confirmed across real
# districts: "Title: description..." (Niobrara 1), "ALL-CAPS TITLE-
# District Wide" with no description (also Niobrara 1), and "Title
# $NNN/day" (Platte 2). fetch_apptegy_postings() takes a chromote session
# so callers can reuse one browser across all four Apptegy districts
# instead of paying startup cost per district.
#
# KNOWN INCOMPLETE, more so than every other platform here: checked all 4
# Apptegy districts' real current pages directly (not just Niobrara 1) and
# found postings these three patterns still don't catch -- Sheridan 3 has
# bare title-only lines with no punctuation marker at all ("Bus Drivers"),
# and Weston 7 mixes several different free-prose phrasings on the same
# page, including a title embedded mid-sentence ("...is currently
# accepting applications for the Middle School Football Coach starting
# with..."). Adding a bare-short-line pattern to catch Sheridan 3's style
# would false-positive heavily on Apptegy's own generic nav/footer text
# ("MENU", "SIGN IN", "Find Us", "Stay Connected", all short Title-Case-or-
# caps lines); not attempted for that reason. Apptegy districts are
# comparatively low-value to get perfect anyway since WSBA already covers
# some of what they post -- treat this specifically as best-effort partial
# coverage, not a completeness guarantee the way the other 5 platforms are.
fetch_apptegy_postings <- function(chromote_session, url) {
  chromote_session$Page$navigate(url)
  chromote_session$Page$loadEventFired(wait_ = TRUE, timeout_ = 30)
  Sys.sleep(5)
  text <- chromote_session$Runtime$evaluate("document.body.innerText")$result$value
  parse_apptegy_postings(text)
}

parse_apptegy_postings <- function(rendered_text) {
  # document.body.innerText preserves non-breaking spaces (U+00A0) from the
  # page's own styling, which R's \s (PCRE, non-Unicode mode) does not
  # match -- confirmed this silently dropped "Elementary Teacher" (a real
  # posting immediately followed by a non-breaking space after its colon)
  # from every regex below without erroring.
  rendered_text <- gsub("\u00a0", " ", rendered_text)
  lines <- strsplit(rendered_text, "\n")[[1]]

  # Two independent posting formats seen on real pages, both need to be
  # matched:
  #
  # 1. "Title: District Name in City, WY is accepting/seeking
  #    applications..." -- most postings. A naive colon-line match also
  #    catches the page's own "ADDITIONAL INFO" contact block ("Business
  #    Manager: Michelle Neuenschwander" is a person's name, not a job),
  #    which happens to be long enough to pass a bare length check -- real
  #    postings are distinguished by the description actually being about
  #    a posting (mentions the district applying/accepting/seeking), not
  #    just being long.
  colon_lines <- grep("^[A-Za-z][A-Za-z0-9/ '&-]+:\\s", lines, value = TRUE, perl = TRUE)
  colon_lines <- colon_lines[grepl(
    ":\\s+.*(accepting applications|is seeking|is looking for|School District)",
    colon_lines, ignore.case = TRUE
  )]
  colon_titles <- str_trim(sub("^([A-Za-z][A-Za-z0-9/ '&-]+):.*$", "\\1", colon_lines))

  # 2. "ALL-CAPS TITLE- District Wide" (or similar short location) --
  #    confirmed on Niobrara 1's real page for substitute-pool postings
  #    ("BUS DRIVER SUBSTITUTES- District Wide"), which have no
  #    description at all and would be silently missed by the colon
  #    pattern above. Restricted to ALL-CAPS titles specifically so this
  #    doesn't also match ordinary section headings, which never contain
  #    a dash.
  dash_lines <- grep("^[A-Z][A-Z /]+-\\s*.+$", lines, value = TRUE, perl = TRUE)
  dash_titles <- str_trim(sub("^([A-Z][A-Z /]+?)-\\s*.+$", "\\1", dash_lines))

  # 3. "Title Case Title $NNN/day" -- confirmed on Platte County SD2's real
  #    page ("Substitute Certified Teachers $160/day"); no colon, no dash,
  #    just a title followed directly by a pay rate.
  rate_lines <- grep("^[A-Z][A-Za-z /]+\\$\\d", lines, value = TRUE, perl = TRUE)
  rate_titles <- str_trim(sub("^([A-Z][A-Za-z /]+?)\\s*\\$\\d.*$", "\\1", rate_lines))

  # 4. A lead-in sentence ending in a colon ("...is currently accepting
  #    applications for the following [N] position[s] for the ... school
  #    year:") followed by a flat list of bare title lines, one per real
  #    posting -- confirmed on Weston 7's real page (5 substitute titles
  #    with no per-title punctuation marker at all -- not caught by any of
  #    the 3 patterns above -- plus a second, single-posting instance of
  #    the same lead-in shape further down the same page). Works on the
  #    full text rather than per-line, since innerText wraps the lead-in
  #    sentence itself across multiple lines/blank-line paragraph breaks
  #    (confirmed: "...for the 26/27" and "school year:" land on two
  #    separate paragraphs). Scoped tightly to avoid the false-positive
  #    risk the header comment above already flagged for a bare
  #    short-line pattern: requires the specific "accepting applications
  #    for the following" phrase (not just any colon), and stops
  #    consuming candidate titles at the first paragraph that's either
  #    long (a real prose sentence, e.g. the district's own follow-up
  #    boilerplate about how to apply) or itself contains an embedded
  #    line break, rather than a fixed count -- so it degrades to
  #    "however many short lines follow" instead of overreaching into
  #    nav/footer text the way an unscoped version would.
  leadin_starts <- gregexpr("accepting applications for the following", rendered_text, ignore.case = TRUE)[[1]]
  leadin_titles <- character(0)
  if (leadin_starts[1] != -1) {
    for (start in leadin_starts) {
      remainder <- substring(rendered_text, start)
      colon_pos <- regexpr(":", remainder, fixed = TRUE)
      if (colon_pos == -1) next
      after_colon <- substring(remainder, colon_pos + 1)
      paragraphs <- str_trim(strsplit(after_colon, "\n\\s*\n")[[1]])
      for (para in paragraphs) {
        if (!nzchar(para)) next
        if (nchar(para) > 70 || grepl("\n", para, fixed = TRUE)) break
        leadin_titles <- c(leadin_titles, para)
      }
    }
  }

  # 5. Bare title lines right after a block of form links ("Certified
  #    Application", "Classified Application", "Coaching Application",
  #    "Classified Salary", "Certified Salary"), up to the page's first prose
  #    paragraph -- confirmed on Sheridan 3's real page (2026-11-07: "JH
  #    Assistant Basketball Coaches", "Substitute Custodial Positions"), which
  #    has no colon, dash, rate or lead-in sentence. Only after two or more
  #    consecutive form-link lines, and only short lines without sentence
  #    punctuation, so ordinary page text isn't picked up.
  trimmed <- str_trim(lines)
  trimmed <- trimmed[nzchar(trimmed)]
  is_form <- grepl("^[A-Za-z /&-]{0,40}(Application|Salary|Salary Schedule)s?$", trimmed, ignore.case = TRUE)
  form_block_titles <- character(0)
  runs <- rle(is_form)
  ends <- cumsum(runs$lengths)
  for (r in which(runs$values & runs$lengths >= 2)) {
    i <- ends[r] + 1
    while (i <= length(trimmed)) {
      line <- trimmed[i]
      if (nchar(line) > 70 || grepl("[.:!?]$", line) || line %in% c("Find Us", "Stay Connected")) break
      # More form links, section headings and the district's own name
      # (Weston 7: "APPLICATION FOR EMPLOYMENT General Classified", "Job
      # Openings", "WESTON COUNTY SCHOOL DISTRICT #7") aren't postings.
      skip <- grepl("application|school district|^(job |current )?(openings?|vacancies|positions)$|^employment$|^human resources$",
                    line, ignore.case = TRUE)
      if (!skip) form_block_titles <- c(form_block_titles, line)
      i <- i + 1
    }
  }

  titles <- unique(c(colon_titles, dash_titles, rate_titles, leadin_titles, form_block_titles))
  titles <- titles[nzchar(titles)]

  if (length(titles) == 0) return(empty_misc_result())

  data.frame(Title = titles, Location = NA_character_, Posted_Date = NA_character_,
             Link = NA_character_, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Prairie View Community School -- the second platform family that
# genuinely needs a browser (found 2026-08-06)
# ---------------------------------------------------------------------------

# A modern client-side-rendered app (built with "Manus", per its own
# window.__MANUS_HOST_DEV__ global -- confirmed via a plain fetch: raw HTML
# has no posting text, only a JS bundle with no embedded job data baked in
# at build time, unlike e.g. fetch_paylocity_jobs()'s window.pageData).
# Real job titles are genuine <h1>-<h5> headings, unlike Apptegy's free
# prose -- confirmed via a live DOM query, not just innerText, since
# innerText's flattened line order didn't reliably put each title
# immediately before its own description. Scoped to the headings strictly
# between the page's own "Open Positions" and "Application Process"
# section headings, so this doesn't also pick up the page's marketing
# headings ("Why Work With Us", "Mission-Driven Work", etc.) -- same
# section-scoping technique parse_wordpress_postings() already uses via
# its own "Current Open Positions" heading, just needing a browser here
# since this page never puts that text in server-rendered HTML at all.
fetch_prairieview_postings <- function(chromote_session, url) {
  chromote_session$Page$navigate(url)
  chromote_session$Page$loadEventFired(wait_ = TRUE, timeout_ = 30)
  Sys.sleep(5)
  headings_json <- chromote_session$Runtime$evaluate(
    "JSON.stringify(Array.from(document.querySelectorAll('h1,h2,h3,h4,h5')).map(h => h.innerText.trim()).filter(t => t.length > 0))"
  )$result$value
  parse_prairieview_postings(headings_json)
}

parse_prairieview_postings <- function(headings_json) {
  headings <- jsonlite::fromJSON(headings_json)
  if (length(headings) == 0) return(empty_misc_result())

  start_idx <- which(headings == "Open Positions")
  end_idx <- which(headings == "Application Process")
  if (length(start_idx) == 0 || length(end_idx) == 0 || end_idx[1] <= start_idx[1] + 1) {
    return(empty_misc_result())
  }

  titles <- headings[(start_idx[1] + 1):(end_idx[1] - 1)]
  titles <- titles[nzchar(titles)]
  if (length(titles) == 0) return(empty_misc_result())

  data.frame(Title = titles, Location = NA_character_, Posted_Date = NA_character_,
             Link = NA_character_, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Registry + orchestration
# ---------------------------------------------------------------------------

# One row per district or standalone charter school (the same broadening
# WSBA_ONLY_ORGS already established for Snowy Range Academy -- these two
# aren't LEAs either, just orgs with their own real, scrapable job page):
# canonical name (matching the rest of the K-12 pipeline's naming), which
# platform its own page is on, and that page's URL. Everything except
# Apptegy's `platform` needs no extra per-district config;
# schoolblocks_widget_title exists because a different district on that
# same platform might title its equivalent widget differently than
# Sublette 1's "Current Job Announcements".
misc_district_registry <- data.frame(
  District = c(
    "Niobrara County School District 1",
    "Park County School District 16",
    "Platte County School District 1",
    "Sheridan County School District 3",
    "Sublette County School District 1",
    "Uinta County School District 4",
    "Uinta County School District 6",
    "Washakie County School District 2",
    "Weston County School District 1",
    "Weston County School District 7",
    "Cheyenne Classical Academy",
    "Wyoming Classical Academy",
    "Prairie View Community School"
  ),
  platform = c(
    "apptegy",
    "smartsites",
    "smartsites",
    "apptegy",
    "schoolblocks",
    "edlio",
    "googlesites",
    "edlio",
    "edlio",
    "apptegy",
    "educational_networks",
    "educational_networks",
    "prairieview"
  ),
  url = c(
    "https://www.growingluskleaders.org/page/human-resources",
    "https://pcsd16.com/271372_2",
    "https://www.platte1.org/89574_1",
    "https://www.sheridan3.com/page/hr-and-career-opportunities",
    "https://www.sub1.org/en-US/employment-and-human-resources-f26202d3",
    "https://www.uinta4.com/departments/business_office/job_opportunities/current_openings",
    "https://sites.google.com/lymanschools.org/ucsd6/home/job-openings",
    "https://www.wsh2.k12.wy.us/apps/pages/index.jsp?uREC_ID=440610&type=d&pREC_ID=1008214",
    "https://www.wcsd1.org/apps/pages/Career_Opportunities",
    "https://www.weston7.org/o/wcsd/page/employment",
    "https://www.cheyenneclassical.org/employment",
    "https://www.wyoclassical.org/employment",
    "https://prairieviewschool.org/employment"
  ),
  stringsAsFactors = FALSE
)

# Standalone orgs WSBA covers as their own "District" entry (see
# parse_wsba_vacancies()'s comment on WSBA's "<...> - <org> - <location>,
# Wyoming" line format) that aren't school districts and have no own-page
# scraper of any kind -- WSBA is the only source for these, not a
# supplement to one. Snowy Range Academy is the one confirmed case
# (Laramie Montessori Charter School, the other real charter school found,
# has its own clean Paylocity source instead -- see
# direct_api_scrapers.R::fetch_paylocity_jobs() and Wy_ED_Jobs.Rmd's
# charter-school block). Previously this project's own comments claimed
# Snowy Range "already surfaces via WSBA with no extra code needed," which
# turned out to be wrong: fetch_all_misc_district_postings() below only
# ever pulled WSBA rows for districts actually IN misc_district_registry,
# so Snowy Range's postings were silently dropped entirely -- confirmed
# absent from every committed combinedclean.csv to date. This constant and
# the loop below are the actual fix.
WSBA_ONLY_ORGS <- c(
  "Snowy Range Academy",
  "Northeast Wyoming BOCES",
  "STRIDE Learning Center",
  "Sweetwater County Child Developmental Center",
  "Wyoming Boys School"
)

# District-level data-completeness classification -- used by app.R to show
# a real, visible badge on any district whose current-openings count is
# measurably less complete than a platform-scraped district's, rather than
# leaving that fact sitting only in this file's comments where an end user
# viewing the dashboard has no way to see it.
#
# "Partial (WSBA + own page)": misc_district_registry's 10 districts --
# WSBA (confirmed ~25-40% of real postings on its own) plus a heuristic
# own-page scrape, deduplicated. Still meaningfully less complete than a
# real structured job-board platform.
# "Partial (WSBA only)": WSBA_ONLY_ORGS -- no own-page scraper exists at
# all, so this is WSBA's ~25-40% coverage with nothing to supplement it.
# Every other district (not returned here) is scraped from a genuine
# structured platform and is implicitly "Full" -- callers coalesce a
# missing match to that, rather than this function needing to enumerate
# every fully-covered district too.
misc_district_coverage_tiers <- function() {
  data.frame(
    District = c(misc_district_registry$District, WSBA_ONLY_ORGS),
    Data_Coverage = c(
      rep("Partial (WSBA + own page)", nrow(misc_district_registry)),
      rep("Partial (WSBA only)", length(WSBA_ONLY_ORGS))
    ),
    stringsAsFactors = FALSE
  )
}

# Fetches one district's own-page postings via the right platform-specific
# function. Apptegy, Prairie View, and Google Sites all require a live
# chromote session (passed in by the caller so all districts needing one
# share a single browser instance instead of paying startup cost per
# district); every other platform is plain HTTP and ignores the session
# argument entirely.
fetch_misc_district_postings <- function(platform, url, chromote_session = NULL) {
  switch(platform,
    wordpress = fetch_wordpress_postings(url),
    smartsites = fetch_smartsites_postings(url),
    schoolblocks = fetch_schoolblocks_postings(url),
    edlio = fetch_edlio_postings(url),
    googlesites = fetch_googlesites_postings(chromote_session, url),
    educational_networks = fetch_educational_networks_postings(url),
    apptegy = fetch_apptegy_postings(chromote_session, url),
    prairieview = fetch_prairieview_postings(chromote_session, url),
    stop("fetch_misc_district_postings: unknown platform '", platform, "'")
  )
}

# Full pipeline: every WSBA statewide row (one fetch) + each miscellaneous
# district's own page (deduplicated against that district's WSBA entries by
# normalized title), combined into one data frame matching the schema the
# rest of the K-12 pipeline expects (title/date_posted/position/location/
# url/District). The final K-12 merge removes exact WSBA/direct-platform
# overlaps, after direct feeds' dates have been standardized.
# `position` is intentionally NA throughout -- combinedclean's
# classify_k12_position() recomputes it from `title` for every source
# regardless, same as every other K-12 loader.
#
# safe_scrape (from scrape_helpers.R) logs each source individually: WSBA
# once, then each of the 12 districts' own-page fetch, so one district's
# selector breaking is visible in scrape_log.csv without blocking the
# other 11 or the WSBA fetch itself.
#
# chromote_session_factory: a zero-arg function returning a fresh
# chromote session (e.g. \() chromote::ChromoteSession$new()), only
# invoked (once) if the registry actually contains a district on a
# platform that needs a browser (currently Apptegy, Prairie View, or
# Google Sites's platform). Kept as an injected factory rather than a hard
# chromote::: call so this function -- and everything except those
# districts within it -- stays testable without a real browser available.
WSBA_VACANCIES_URL <- "https://www.wsba-wy.org/vacancies"

fetch_all_misc_district_postings <- function(chromote_session_factory = NULL) {
  wsba <- safe_scrape(
    "WSBA statewide vacancies",
    scrape_fn = fetch_wsba_vacancies,
    expected_cols = c("Title", "District", "Location", "Posted_Date", "Url")
  )

  needs_browser <- any(misc_district_registry$platform %in% c("apptegy", "prairieview", "googlesites"))
  browser_session <- if (needs_browser && !is.null(chromote_session_factory)) {
    chromote_session_factory()
  } else {
    NULL
  }

  all_rows <- list()

  for (i in seq_len(nrow(misc_district_registry))) {
    district <- misc_district_registry$District[i]
    platform <- misc_district_registry$platform[i]
    url <- misc_district_registry$url[i]

    own_postings <- safe_scrape(
      district,
      scrape_fn = function() fetch_misc_district_postings(platform, url, browser_session),
      expected_cols = c("Title", "Location", "Posted_Date", "Link")
    )
    own_postings <- dedupe_against_wsba(own_postings, wsba, district)

    wsba_here <- wsba[wsba$District == district, , drop = FALSE]

    wsba_urls <- if ("Url" %in% names(wsba_here) && nrow(wsba_here) > 0) {
      ifelse(!is.na(wsba_here$Url) & nzchar(wsba_here$Url), wsba_here$Url, WSBA_VACANCIES_URL)
    } else {
      rep(WSBA_VACANCIES_URL, nrow(wsba_here))
    }

    own_urls <- if ("Link" %in% names(own_postings) && nrow(own_postings) > 0) {
      ifelse(!is.na(own_postings$Link) & nzchar(own_postings$Link), own_postings$Link, url)
    } else {
      rep(url, nrow(own_postings))
    }

    combined <- dplyr::bind_rows(
      if (nrow(wsba_here) > 0) data.frame(title = wsba_here$Title, date_posted = wsba_here$Posted_Date, url = wsba_urls, stringsAsFactors = FALSE),
      if (nrow(own_postings) > 0) data.frame(title = own_postings$Title, date_posted = own_postings$Posted_Date, url = own_urls, stringsAsFactors = FALSE)
    )

    if (nrow(combined) > 0) {
      combined$position <- NA_character_
      combined$location <- NA_character_
      combined$District <- district
      all_rows[[length(all_rows) + 1]] <- combined
    }
  }

  # WSBA-only orgs (see WSBA_ONLY_ORGS's comment) -- no own-page scrape or
  # dedup needed, just WSBA's rows for that org name passed straight
  # through, same shape as every other district's combined rows above.
  for (org in WSBA_ONLY_ORGS) {
    wsba_here <- wsba[wsba$District == org, , drop = FALSE]
    if (nrow(wsba_here) > 0) {
      org_urls <- if ("Url" %in% names(wsba_here)) {
        ifelse(!is.na(wsba_here$Url) & nzchar(wsba_here$Url), wsba_here$Url, WSBA_VACANCIES_URL)
      } else {
        rep(WSBA_VACANCIES_URL, nrow(wsba_here))
      }
      all_rows[[length(all_rows) + 1]] <- data.frame(
        title = wsba_here$Title, date_posted = wsba_here$Posted_Date,
        position = NA_character_, location = wsba_here$Location,
        url = org_urls, District = org,
        stringsAsFactors = FALSE
      )
    }
  }

  # The direct-platform districts were previously excluded here even though
  # WSBA had already provided their rows. Keep those statewide listings;
  # exact overlaps are removed against the direct feeds in Wy_ED_Jobs.Rmd.
  represented_orgs <- c(misc_district_registry$District, WSBA_ONLY_ORGS)
  wsba_remaining <- wsba[!wsba$District %in% represented_orgs, , drop = FALSE]
  if (nrow(wsba_remaining) > 0) {
    rem_urls <- if ("Url" %in% names(wsba_remaining)) {
      ifelse(!is.na(wsba_remaining$Url) & nzchar(wsba_remaining$Url), wsba_remaining$Url, WSBA_VACANCIES_URL)
    } else {
      rep(WSBA_VACANCIES_URL, nrow(wsba_remaining))
    }
    all_rows[[length(all_rows) + 1]] <- data.frame(
      title = wsba_remaining$Title,
      date_posted = wsba_remaining$Posted_Date,
      position = NA_character_,
      location = wsba_remaining$Location,
      url = rem_urls,
      District = wsba_remaining$District,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(browser_session)) {
    tryCatch(browser_session$close(), error = function(e) NULL)
  }

  if (length(all_rows) == 0) {
    return(data.frame(title = character(0), date_posted = character(0), position = character(0),
                       location = character(0), url = character(0), District = character(0),
                       stringsAsFactors = FALSE))
  }

  result <- dplyr::bind_rows(all_rows)
  result[, c("title", "date_posted", "position", "location", "url", "District")]
}

# WSBA identifies a posting by district, title, and usually a posted date.
# Remove a WSBA row only when all three fields exactly match a direct-board
# row. Missing dates deliberately prevent a match: retaining an uncertain
# duplicate is safer than dropping a real vacancy.
wsba_posting_key <- function(postings) {
  title <- normalize_title(postings$title)
  district <- normalize_title(canonicalize_wsba_district(postings$District))
  posted_date <- trimws(as.character(postings$date_posted))
  valid <- !is.na(title) & nzchar(title) &
    !is.na(district) & nzchar(district) &
    !is.na(postings$date_posted) & nzchar(posted_date)
  key <- rep(NA_character_, nrow(postings))
  key[valid] <- paste(district[valid], title[valid], posted_date[valid], sep = "\r")
  key
}

generate_wsba_discrepancy_report <- function(wsba_postings, direct_postings) {
  empty <- data.frame(
    District = character(0),
    Source = character(0),
    Title = character(0),
    Date_Posted = character(0),
    Location = character(0),
    URL = character(0),
    Discrepancy_Type = character(0),
    Detected_Date = character(0),
    stringsAsFactors = FALSE
  )
  if (nrow(wsba_postings) == 0) return(empty)

  required <- c("title", "date_posted", "District")
  missing_wsba <- setdiff(required, names(wsba_postings))
  missing_direct <- setdiff(required, names(direct_postings))
  if (length(missing_wsba) > 0 || length(missing_direct) > 0) {
    stop(
      "Cannot generate discrepancy report; missing columns: ",
      paste(c(missing_wsba, missing_direct), collapse = ", "),
      call. = FALSE
    )
  }

  direct_keys <- wsba_posting_key(direct_postings)
  direct_keys <- direct_keys[!is.na(direct_keys)]

  wsba_keys <- wsba_posting_key(wsba_postings)
  is_dup <- wsba_duplicates_direct(wsba_postings, direct_postings, wsba_keys, direct_keys)
  survived_wsba <- wsba_postings[!is_dup, , drop = FALSE]
  if (nrow(survived_wsba) == 0) return(empty)

  # Check which survived rows belong to districts that have an active direct scraper
  direct_districts <- unique(canonicalize_wsba_district(direct_postings$District))
  survived_canon <- canonicalize_wsba_district(survived_wsba$District)
  is_discrepancy <- survived_canon %in% direct_districts
  discrepancies <- survived_wsba[is_discrepancy, , drop = FALSE]
  if (nrow(discrepancies) == 0) return(empty)

  parse_discrepancy_date <- function(dp) {
    if (inherits(dp, "Date")) return(dp)
    if (is.na(dp) || !nzchar(trimws(as.character(dp)))) return(as.Date(NA))
    d <- tryCatch(as.Date(as.character(dp)), error = function(e) NA)
    if (!is.na(d)) return(d)
    tryCatch(as.Date(as.character(dp), format = "%m/%d/%Y"), error = function(e) NA)
  }

  # Categorize discrepancy
  reasons <- vapply(seq_len(nrow(discrepancies)), function(i) {
    dp <- discrepancies$date_posted[i]
    parsed_date <- parse_discrepancy_date(dp)
    if (is.na(parsed_date)) {
      return("Unmatched on direct scraper (no date)")
    }
    if (parsed_date < (Sys.Date() - 60)) {
      return(paste0("Potential ghost posting on WSBA (stale date: ", dp, ")"))
    }
    "Unmatched on direct scraper (investigate ATS drift or unlisted posting)"
  }, character(1))

  loc <- if ("location" %in% names(discrepancies)) discrepancies$location else rep(NA_character_, nrow(discrepancies))
  u <- if ("url" %in% names(discrepancies)) discrepancies$url else rep(NA_character_, nrow(discrepancies))

  data.frame(
    District = canonicalize_wsba_district(discrepancies$District),
    Source = "WSBA",
    Title = as.character(discrepancies$title),
    Date_Posted = as.character(discrepancies$date_posted),
    Location = as.character(loc),
    URL = as.character(u),
    Discrepancy_Type = reasons,
    Detected_Date = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )
}

remove_wsba_direct_duplicates <- function(wsba_postings, direct_postings, report_path = NULL) {
  required <- c("title", "date_posted", "District")
  missing_wsba <- setdiff(required, names(wsba_postings))
  missing_direct <- setdiff(required, names(direct_postings))
  if (length(missing_wsba) > 0 || length(missing_direct) > 0) {
    stop(
      "Cannot deduplicate WSBA postings; missing columns: ",
      paste(c(missing_wsba, missing_direct), collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(report_path)) {
    rep <- generate_wsba_discrepancy_report(wsba_postings, direct_postings)
    write.csv(rep, report_path, row.names = FALSE)
  }

  direct_keys <- wsba_posting_key(direct_postings)
  direct_keys <- direct_keys[!is.na(direct_keys)]
  if (nrow(wsba_postings) == 0 || length(direct_keys) == 0) return(wsba_postings)

  wsba_keys <- wsba_posting_key(wsba_postings)
  wsba_postings[!wsba_duplicates_direct(wsba_postings, direct_postings, wsba_keys, direct_keys), , drop = FALSE]
}

# A WSBA row duplicates a direct posting when its district, title and posted
# date match one (wsba_posting_key()), or when its link names a job ID the
# district's own board lists this run, whatever the wording or date: WSBA
# lists Park #6's AppliTrack job 1026 as "Middle School ASD Special Education
# Teacher" (posted 2026-03-27) while the board calls it "ASD Special
# Education Teacher"; 22 of the 63 WSBA rows kept on 2026-11-05 were such
# duplicates, each counted twice.
wsba_duplicates_direct <- function(wsba_postings, direct_postings,
                                   wsba_keys = wsba_posting_key(wsba_postings),
                                   direct_keys = stats::na.omit(wsba_posting_key(direct_postings))) {
  by_key <- !is.na(wsba_keys) & wsba_keys %in% direct_keys
  direct_ids <- stats::na.omit(unique(c(
    k12_link_posting_id(direct_postings$url),
    if ("posting_id" %in% names(direct_postings)) k12_source_posting_id(direct_postings$posting_id))))
  wsba_ids <- if ("url" %in% names(wsba_postings)) k12_link_posting_id(wsba_postings$url) else rep(NA_character_, nrow(wsba_postings))
  by_id <- !is.na(wsba_ids) & wsba_ids %in% direct_ids
  by_key | by_id
}
