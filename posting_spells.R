# Posting spells, rebuilt on MT's posting-level design (MT posting_spells.R,
# 2026-11-04; WY spells trust audit 2026-11-05). One row per posting, from the
# raw archives, replacing "District + exact title, gone = filled" with:
#
# - Identity: a per-posting link where the board gives one (AppliTrack job ID,
#   SchoolSpring /jobs/<n>, Red Rover /opening/<n>, TalentEd JobID; HE
#   /postings/<n>, /jobs/<n>, Oracle /job/<n>), keyed by platform and number so
#   a host rename (Western Wyoming's wwccwy -> wwcwy PeopleAdmin, 2026-08) keeps
#   the ID. A link held by two titles in one run is a board page, never an ID.
#   Otherwise District + source + title + posted date (the audit's
#   recommendation: 70% of direct rows carry a posted date, which separates
#   most same-title postings), with an occurrence suffix within a run. Earlier
#   title-identified rows are bridged to the link-ID the same posting carries
#   later (WY's boards moved to per-posting links from 2026-08-11 on).
# - Source: the board's host (www.applitrack.com/<tenant>,
#   laramie1.schoolspring.com, ...), or "WSBA statewide feed" for rows linking
#   WSBA's vacancies page or, from 2026-11-12 on, archived with WSBA as their
#   Posting_Source: WSBA's rows link the district's own site or files (all 47
#   on 11-13), so their host said nothing about where they were read (Mike,
#   2026-11-13: host for direct rows, the archived label for WSBA's). A posting
#   whose district stops using that board is censored, not ended.
# - Start: the board's own posted date when plausible (on or before the first
#   sighting, at most 365 days before it); otherwise the first sighting, which
#   is left-censored when it was the source's first week, or the week it came
#   back after a week with no rows.
# - End: "no longer listed" only when the source was healthy the next run
#   (coverage_calendar.csv from 2026-11-06; before that, "the district returned
#   rows from that source"), and the same district + title didn't turn up on
#   another board that run (a WSBA posting picked up by the district's own
#   scrape: "censored: moved boards"). SchoolSpring endings come from
#   schoolspring_endings.csv (filled / canceled / review). An ending's date is
#   the midpoint between the last sighting and the next run.
# - Flags, excluded from durations but kept in the table: standing ads
#   (substitute lists, "always accepting", posted over a year before we first
#   saw them), pools (applicant pools, adjunct and part-time faculty listings),
#   frozen boards (a source whose whole posting set stayed identical for
#   FROZEN_MIN_RUNS+ runs with FROZEN_MIN_POSTINGS+ postings: Fremont #38's
#   stale page of 2024-10 to 2025-11), titles shared by several postings in
#   the district, and left-censored starts.
#
# Durations are Kaplan-Meier medians with delayed entry (open and censored
# postings count as censored); a median is reported only with at least
# `min_events` endings. Read them as "days a posting stays listed", not
# time-to-fill: only SchoolSpring says filled.
# Since phase 4 (2026-11-11) the spells themselves (bridging, endings, starts,
# flags, frozen boards, durations, the first-seen lookup) are built by the
# edjobs package (R/posting_spells.R) under WY_SPELL_RULES (the package's
# defaults: this design). This file keeps what is Wyoming's own: the board a
# row came from, posting identity, and reading WY's archives into panels.

library(edjobs)

WSBA_FEED_SOURCE <- "WSBA statewide feed"
WSBA_VACANCIES_URL_SPELLS <- "https://www.wsba-wy.org/vacancies"   # drift_check.R's WSBA_VACANCIES_URL
WY_SPELL_RULES <- spell_rules(feed_entity = WSBA_FEED_SOURCE)

# The board a K-12 row came from: its host (plus the tenant for AppliTrack,
# whose host is shared by every district), or the WSBA feed.
k12_posting_source <- function(url) {
  u <- tolower(spell_text(url))
  host <- sub("^https?://(www\\.)?([^/?#]+).*$", "\\2", u)
  tenant <- ifelse(grepl("applitrack\\.com/[^/]+/", u), sub("^.*applitrack\\.com/([^/]+)/.*$", "/\\1", u), "")
  out <- ifelse(is.na(u) | !nzchar(u) | !grepl("^https?://", u), "no link", paste0(host, tenant))
  out[!is.na(u) & u == WSBA_VACANCIES_URL_SPELLS] <- WSBA_FEED_SOURCE
  out
}

# k12_link_posting_id() and k12_source_posting_id(): k12_he_classification.R
# (also used by the WSBA dedup).

# Identity for one run's rows (District, title, url, source, posted, plus the
# archive's posting_id where present). A link held by two titles in the run is
# a board page.
assign_posting_identity <- function(d, link_id) {
  t <- spell_key(d$title)
  shared <- !is.na(link_id) & stats::ave(t, d$Run_Date, link_id, FUN = function(x) length(unique(x)) > 1) == "TRUE"
  link_id[shared] <- NA_character_
  stable <- !is.na(link_id)
  base <- paste("fb", spell_key(d$District), d$Posting_Source, t, ifelse(is.na(d$date_posted), "", d$date_posted),
                sep = "\u001f")
  occ <- stats::ave(seq_along(base), base, d$Run_Date, FUN = seq_along)
  d$Posting_ID <- ifelse(stable, link_id, paste0(base, "#", occ))
  d$Posting_Identity_Method <- ifelse(stable, "Stable per-posting ID",
                                      "Source fallback: district + board + title + posted date")
  d
}

# Every K-12 archive from `from` on, one row per Posting_ID per run, in the
# shape build_posting_spells() takes. Archive district names are
# canonicalized and titles repaired, as the pipeline does.
k12_posting_panel <- function(archive_dir = "Archivek12_Data", from = as.Date("2024-08-02")) {
  files <- list.files(archive_dir, "^combined_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
  run_of <- as.Date(sub("^combined_(.*)\\.csv$", "\\1", basename(files)))
  files <- files[run_of >= from]
  d <- dplyr::bind_rows(lapply(files, function(f) {
    x <- utils::read.csv(f, colClasses = "character")
    keep <- intersect(c("title", "date_posted", "url", "District", "posting_id", "Close_Date", "Posting_Source"), names(x))
    x <- x[, keep, drop = FALSE]
    x$Run_Date <- as.Date(sub("^combined_(.*)\\.csv$", "\\1", basename(f)))
    x
  }))
  if (!"posting_id" %in% names(d)) d$posting_id <- NA_character_
  if (!"Close_Date" %in% names(d)) d$Close_Date <- NA_character_
  if (!"Posting_Source" %in% names(d)) d$Posting_Source <- NA_character_
  d$Close_Date <- parse_stated_close_date(d$Close_Date)
  d$title <- spell_text(d$title)
  d$District <- canonicalize_k12_district(spell_text(d$District))
  d <- d[!is.na(d$title) & nzchar(d$title) & !is_placeholder_posting(d$title), , drop = FALSE]
  wsba_label <- current_state_config()$feed$label
  if (is.null(wsba_label)) stop("k12_posting_panel(): source state_config.R first (WSBA's archived label)")
  read_from_wsba <- !is.na(d$Posting_Source) & d$Posting_Source == wsba_label
  d$Posting_Source <- k12_posting_source(d$url)
  d$Posting_Source[read_from_wsba] <- WSBA_FEED_SOURCE
  pd <- parse_board_posted_date(d$date_posted, d$Run_Date)
  d$date_posted <- pd$date
  d$Posted_Approx <- pd$approx
  link_id <- dplyr::coalesce(k12_link_posting_id(d$url), k12_source_posting_id(d$posting_id))
  d <- assign_posting_identity(d, link_id)
  d %>%
    dplyr::distinct(Posting_ID, Run_Date, .keep_all = TRUE) %>%
    dplyr::transmute(Run_Date, Posting_ID, Posting_Identity_Method, District, title, date_posted, Posted_Approx,
                     Close_Date, url, Posting_Source, Is_Feed = Posting_Source == WSBA_FEED_SOURCE,
                     position = classify_k12_position(title))
}

# ---------------------------------------------------------------------------
# Higher ed. The raw HE archives carry no posting ID. A per-posting link is the
# ID, keyed by institution, the path word and the number (PeopleAdmin and
# SimpleHire /postings/<n>, schooljobs/governmentjobs /jobs/<n>, UW's Oracle
# /job/<n>); board pages (search?page=, /careers/cwc) fall back to
# institution + title. Only title fallbacks may be bridged across consecutive
# runs: a new requisition number is a new posting.
# ---------------------------------------------------------------------------
he_link_posting_id <- function(url, institution) {
  u <- tolower(spell_text(url)); u[is.na(u)] <- ""
  pat <- "/(postings|jobs|job)/(\\d+)(/|$|\\?|#)"
  hit <- grepl(pat, u, perl = TRUE) & !grepl("/search|requisitions\\?", u)
  out <- rep(NA_character_, length(u))
  out[hit] <- paste0("he:", spell_key(institution[hit]), ":", sub(paste0("^.*?", pat, ".*$"), "\\1/\\2", u[hit], perl = TRUE))
  out
}

he_posting_panel <- function(archive_dir = "Archived_HE_Data", from = as.Date("2024-08-09")) {
  files <- list.files(archive_dir, "^hedata_\\d{4}-\\d{2}-\\d{2}\\.xlsx$", full.names = TRUE)
  files <- files[as.Date(sub("^hedata_(.*)\\.xlsx$", "\\1", basename(files))) >= from]
  d <- dplyr::bind_rows(lapply(files, function(f) {
    x <- as.data.frame(readxl::read_excel(f, col_types = "text"))
    x$Run_Date <- as.Date(sub("^hedata_(.*)\\.xlsx$", "\\1", basename(f)))
    x[, intersect(c("Title", "Posted_Date", "Institution", "Link", "Run_Date", "Close_Date"), names(x))]
  }))
  d$Title <- spell_text(d$Title)
  d$Institution <- canonicalize_he_institution(spell_text(d$Institution))
  d <- d[!is.na(d$Title) & nzchar(d$Title) & !is_placeholder_posting(d$Title), , drop = FALSE]
  pd <- parse_board_posted_date(d$Posted_Date, d$Run_Date)
  if (!"Close_Date" %in% names(d)) d$Close_Date <- NA_character_
  d <- d %>% dplyr::transmute(Run_Date, District = Institution, title = Title, date_posted = pd$date,
                              Close_Date = parse_stated_close_date(Close_Date),
                              Posted_Approx = pd$approx, url = Link, Posting_Source = Institution)
  d <- assign_posting_identity(d, he_link_posting_id(d$url, d$District))
  d$Posting_Identity_Method <- sub("district \\+ board \\+ title \\+ posted date", "institution + title",
                                   d$Posting_Identity_Method)
  # HE fallback identity is institution + title: the boards' posted dates are
  # mostly relative ("2 weeks ago") and move week to week.
  fb <- !grepl("^Stable", d$Posting_Identity_Method)
  base <- paste("he-fb", spell_key(d$District), spell_key(d$title), sep = "\u001f")
  occ <- stats::ave(seq_along(base), base, d$Run_Date, FUN = seq_along)
  d$Posting_ID[fb] <- paste0(base, "#", occ)[fb]
  d$Bridgeable <- fb
  d %>%
    dplyr::distinct(Posting_ID, Run_Date, .keep_all = TRUE) %>%
    dplyr::mutate(Is_Feed = FALSE, position = classify_he_job_type(title))
}
