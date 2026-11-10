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
# - Source: WY archives have no Posting_Source column, so the source is the
#   board's host (www.applitrack.com/<tenant>, laramie1.schoolspring.com, ...),
#   or "WSBA statewide feed" for rows linking WSBA's vacancies page. A posting
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
POSTING_SPELL_EVENTS <- c("no longer listed", "closed (confirmed)", "filled", "canceled", "review")

# Relisted: a spell whose posting dropped off and a posting with the same
# title, at the same district and source, appears in the source's very next
# run. Usually the district re-advertising, but not always: of 23 WY
# SchoolSpring cases whose old record has a final status (2026-11-09), 8 were
# "filled", a new vacancy with the same title. So it is neither linked to the
# new posting nor counted as an ending: its outcome is unknown (censored at its
# last sighting), unless the platform itself says it was filled.
relisted_next_run <- function(district, source, title, first_seen, next_run) {
  # Punctuation aside too: "Paraeducator- SPED" relisted as "Paraeducator - SPED".
  key <- paste(district, source, trimws(gsub("[^[:alnum:]]+", " ", tolower(title))), sep = "\u001f")
  starts <- paste(key, format(first_seen), sep = "\u001f")
  !is.na(next_run) & paste(key, format(next_run), sep = "\u001f") %in% starts
}
EVERGREEN_TITLE_PATTERN <- paste0(
  "\\bsub(stitute)?s?\\b|substitute|on[- ]call|always accepting|accepting applications|",
  "ongoing|continuous|general application|interest (form|list)|^copy of\\b")
POOL_TITLE_PATTERN <- "\\bpool\\b|\\badjunct\\b|part[- ]time faculty|applicant (pool|list)|\\bstanding\\b"
WSBA_FEED_SOURCE <- "WSBA statewide feed"
WSBA_VACANCIES_URL_SPELLS <- "https://www.wsba-wy.org/vacancies"   # drift_check.R's WSBA_VACANCIES_URL
FROZEN_MIN_RUNS <- 12
FROZEN_MIN_POSTINGS <- 5
PARTIAL_MIN_PREVIOUS <- 10

spell_text <- function(x) {
  x <- fix_title_encoding(as.character(x))
  x <- gsub("[​-‍﻿]", "", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x))
}
spell_key <- function(x) { x <- tolower(spell_text(x)); x[is.na(x)] <- ""; x }

# Posted dates as WY boards print them: 2026-10-14, 10/14/2026, "Oct 14, 2026
# 9:41 AM (UTC)", "14-Oct-26", and on HE boards "3 days ago(10/27/2026 9:12 AM)"
# or a bare "Posted 2 weeks ago" / "Today". A bare relative date is dated back
# from the run (approximate: "2 weeks ago" is 14-20 days); "more than 30 days
# ago" gives no date. Returns a data frame: date (character), approx (logical).
parse_wy_posted_date <- function(x, run_date) {
  x <- trimws(gsub("\\s+", " ", as.character(x)))
  run_date <- rep_len(as.Date(run_date), length(x))
  out <- rep(as.Date(NA), length(x))
  try_fmt <- function(s, fmt) suppressWarnings(as.Date(s, format = fmt))
  inner <- ifelse(grepl("\\((\\d{1,2}/\\d{1,2}/\\d{4})", x), sub(".*\\((\\d{1,2}/\\d{1,2}/\\d{4}).*", "\\1", x), x)
  ymd <- grepl("^\\d{4}-\\d{2}-\\d{2}", inner)
  out[ymd] <- try_fmt(substr(inner[ymd], 1, 10), "%Y-%m-%d")
  mdy <- is.na(out) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}", inner)
  out[mdy] <- try_fmt(sub("^(\\d{1,2}/\\d{1,2}/\\d{4}).*", "\\1", inner[mdy]), "%m/%d/%Y")
  mon <- is.na(out) & grepl("^[A-Z][a-z]{2} \\d{1,2}, \\d{4}", inner)
  out[mon] <- try_fmt(sub("^([A-Z][a-z]{2} \\d{1,2}, \\d{4}).*", "\\1", inner[mon]), "%b %d, %Y")
  dmy <- is.na(out) & grepl("^\\d{1,2}-[A-Z][a-z]{2}-\\d{2}$", inner)
  out[dmy] <- try_fmt(inner[dmy], "%d-%b-%y")
  approx <- rep(FALSE, length(x))
  rel <- is.na(out) & !is.na(x) & grepl("^(posted )?(today|\\d+ (hour|day|week|month)s? ago)$", tolower(x))
  if (any(rel)) {
    s <- tolower(sub("^posted ", "", tolower(x[rel])))
    n <- suppressWarnings(as.numeric(sub("^(\\d+).*", "\\1", s)))
    n[s == "today"] <- 0
    unit <- ifelse(grepl("week", s), 7, ifelse(grepl("month", s), 30, ifelse(grepl("day", s), 1, 0)))
    out[rel] <- run_date[rel] - n * unit
    approx[rel] <- TRUE
  }
  # Anything after the run it came from is a parse error, not a posted date.
  bad <- !is.na(out) & !is.na(run_date) & out > run_date
  out[bad] <- NA
  approx[is.na(out)] <- FALSE
  data.frame(date = ifelse(is.na(out), NA_character_, format(out)), approx = approx, stringsAsFactors = FALSE)
}

# A stated closing date (the board's Close_Date column, from 2026-10-30 on):
# 2026-11-09, 11/9/2026, "Nov 9, 2026"; "open until filled" and the like give
# none.
parse_stated_close_date <- function(x) {
  x <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
  d[is.na(d)] <- suppressWarnings(as.Date(sub("^(\\d{1,2}/\\d{1,2}/\\d{4}).*$", "\\1", x[is.na(d)]), format = "%m/%d/%Y"))
  d[is.na(d)] <- suppressWarnings(as.Date(x[is.na(d)], format = "%b %d, %Y"))
  ifelse(is.na(d), NA_character_, format(d))
}

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
    keep <- intersect(c("title", "date_posted", "url", "District", "posting_id", "Close_Date"), names(x))
    x <- x[, keep, drop = FALSE]
    x$Run_Date <- as.Date(sub("^combined_(.*)\\.csv$", "\\1", basename(f)))
    x
  }))
  if (!"posting_id" %in% names(d)) d$posting_id <- NA_character_
  if (!"Close_Date" %in% names(d)) d$Close_Date <- NA_character_
  d$Close_Date <- parse_stated_close_date(d$Close_Date)
  d$title <- spell_text(d$title)
  d$District <- canonicalize_k12_district(spell_text(d$District))
  d <- d[!is.na(d$title) & nzchar(d$title) & !is_placeholder_posting(d$title), , drop = FALSE]
  d$Posting_Source <- k12_posting_source(d$url)
  pd <- parse_wy_posted_date(d$date_posted, d$Run_Date)
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
  pd <- parse_wy_posted_date(d$Posted_Date, d$Run_Date)
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

# A posting's start is the earliest plausible posted date it ever showed,
# preferring exact dates over relative ones. Applied after bridging.
use_earliest_posted_date <- function(panel) {
  posted <- suppressWarnings(as.Date(panel$date_posted))
  ok <- !is.na(posted) & posted <= panel$Run_Date
  approx <- if (is.null(panel$Posted_Approx)) rep(FALSE, nrow(panel)) else panel$Posted_Approx %in% TRUE
  pick <- function(sel) stats::ave(ifelse(sel, as.numeric(posted), NA_real_), panel$Posting_ID,
                                   FUN = function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE))
  exact <- pick(ok & !approx)
  rough <- pick(ok & approx)
  earliest <- ifelse(is.na(exact), rough, exact)
  panel$Posted_Approx <- is.na(exact) & !is.na(rough)
  panel$date_posted <- ifelse(is.na(earliest), NA_character_, format(as.Date(earliest, origin = "1970-01-01")))
  panel
}

# Earlier title-identified rows of a posting that later carries a stable ID:
# same source, district, title (case-insensitive) and exact posted date, the
# key unique on both sides, and the two never listed in the same run.
bridge_posting_ids <- function(panel) {
  posted <- ifelse(panel$Posted_Approx %in% TRUE, NA_character_, panel$date_posted)
  key <- paste(panel$Posting_Source, panel$District, spell_key(panel$title), posted, sep = "\u001f")
  stable <- grepl("^Stable", panel$Posting_Identity_Method)
  fallback <- !stable & !is.na(posted)
  ids_by_key <- function(sel) {
    x <- unique(data.frame(key = key[sel], id = panel$Posting_ID[sel], stringsAsFactors = FALSE))
    x[!x$key %in% x$key[duplicated(x$key)], , drop = FALSE]   # unique key -> one ID
  }
  s <- ids_by_key(stable); f <- ids_by_key(fallback)
  m <- merge(f, s, by = "key", suffixes = c("_old", "_new"))
  panel$Bridged_From <- NA_character_
  if (nrow(m) > 0) {
    runs_of <- split(panel$Run_Date, panel$Posting_ID)
    ok <- vapply(seq_len(nrow(m)), function(i) {
      length(intersect(runs_of[[m$id_old[i]]], runs_of[[m$id_new[i]]])) == 0 &&
        max(runs_of[[m$id_old[i]]]) < min(runs_of[[m$id_new[i]]])
    }, logical(1))
    m <- m[ok, , drop = FALSE]
    hit <- match(panel$Posting_ID, m$id_old)
    panel$Bridged_From[!is.na(hit)] <- panel$Posting_ID[!is.na(hit)]
    panel$Posting_ID[!is.na(hit)] <- m$id_new[hit[!is.na(hit)]]
  }
  bridge_adjacent_runs(panel)
}

# Second pass, for an ID change between two consecutive runs that the key above
# misses (no posted date before, or a board moved to per-posting links): a
# posting last seen in one run and a posting first seen in the next, same
# source, district and title (case-insensitive), unique on both sides, and the
# old posted date blank, approximate or equal to the new one. A new exact
# posted date means a real repost and stays separate. Only fallback-identified
# postings may change ID (HE: `Bridgeable`; K-12: not "Stable").
bridge_adjacent_runs <- function(panel) {
  runs <- sort(unique(panel$Run_Date))
  if (is.null(panel$Bridged_From)) panel$Bridged_From <- NA_character_
  movable <- if (!is.null(panel$Bridgeable)) panel$Bridgeable else !grepl("^Stable", panel$Posting_Identity_Method)
  movable_ids <- unique(panel$Posting_ID[movable])
  exact_posted <- ifelse(panel$Posted_Approx %in% TRUE, NA_character_, panel$date_posted)
  span <- data.frame(Posting_ID = panel$Posting_ID, Run_Date = panel$Run_Date, Posting_Source = panel$Posting_Source,
                     District = panel$District, t = spell_key(panel$title), posted = exact_posted,
                     stringsAsFactors = FALSE)
  for (k in seq_len(length(runs) - 1)) {
    s <- span %>%
      dplyr::group_by(Posting_ID) %>%
      dplyr::summarize(first = min(Run_Date), last = max(Run_Date), Posting_Source = dplyr::last(Posting_Source),
                       District = dplyr::last(District), t = dplyr::last(t),
                       posted = dplyr::last(posted), .groups = "drop")
    old <- s[s$last == runs[k] & s$Posting_ID %in% movable_ids, ]
    new <- s[s$first == runs[k + 1], ]
    keyf <- function(x) paste(x$Posting_Source, x$District, x$t, sep = "\u001f")
    old <- old[!keyf(old) %in% keyf(old)[duplicated(keyf(old))], ]
    new <- new[!keyf(new) %in% keyf(new)[duplicated(keyf(new))], ]
    if (nrow(old) == 0 || nrow(new) == 0) next
    m <- merge(transform(old, key = keyf(old)), transform(new, key = keyf(new)), by = "key", suffixes = c("_old", "_new"))
    m <- m[is.na(m$posted_old) | !nzchar(m$posted_old) | is.na(m$posted_new) | m$posted_old == m$posted_new, , drop = FALSE]
    if (nrow(m) == 0) next
    hit <- match(panel$Posting_ID, m$Posting_ID_old)
    panel$Bridged_From[!is.na(hit)] <- panel$Posting_ID[!is.na(hit)]
    panel$Posting_ID[!is.na(hit)] <- m$Posting_ID_new[hit[!is.na(hit)]]
    span$Posting_ID <- panel$Posting_ID
    movable_ids <- unique(c(movable_ids, m$Posting_ID_new[m$Posting_ID_old %in% movable_ids]))
  }
  panel
}

# ---------------------------------------------------------------------------
# Frozen boards: a source whose whole posting set (titles, case-insensitive)
# was identical run after run. Real boards change; a set unchanged for
# FROZEN_MIN_RUNS+ runs with FROZEN_MIN_POSTINGS+ postings is a stale page or
# cache, and every posting on it looks listed for as long as the freeze. Takes
# any frame with Run_Date, District, Posting_Source (or NULL: one board per
# district) and title; returns one row per frozen stretch.
# ---------------------------------------------------------------------------
frozen_board_runs <- function(rows, min_runs = FROZEN_MIN_RUNS, min_postings = FROZEN_MIN_POSTINGS) {
  empty <- data.frame(District = character(0), Posting_Source = character(0), From = character(0), To = character(0),
                      Runs = integer(0), Postings = integer(0), Ongoing = logical(0), stringsAsFactors = FALSE)
  if (is.null(rows) || nrow(rows) == 0) return(empty)
  if (is.null(rows$Posting_Source)) rows$Posting_Source <- ""
  all_runs <- sort(unique(as.Date(rows$Run_Date)))
  sets <- rows %>%
    dplyr::mutate(Run_Date = as.Date(Run_Date), t = spell_key(title)) %>%
    dplyr::group_by(District, Posting_Source, Run_Date) %>%
    dplyr::summarize(key = paste(sort(unique(t)), collapse = "\u001f"), n = dplyr::n_distinct(t), .groups = "drop") %>%
    dplyr::arrange(District, Posting_Source, Run_Date)
  # A run with no rows from the board breaks a stretch: compare consecutive
  # runs of the whole schedule, not just the board's own runs.
  sets$run_idx <- match(sets$Run_Date, all_runs)
  out <- sets %>%
    dplyr::group_by(District, Posting_Source) %>%
    dplyr::mutate(chg = c(TRUE, key[-1] != key[-dplyr::n()] | diff(run_idx) != 1), stretch = cumsum(chg)) %>%
    dplyr::group_by(District, Posting_Source, stretch) %>%
    dplyr::summarize(From = format(min(Run_Date)), To = format(max(Run_Date)), Runs = dplyr::n(),
                     Postings = dplyr::first(n), .groups = "drop") %>%
    dplyr::filter(Runs >= min_runs, Postings >= min_postings) %>%
    dplyr::mutate(Ongoing = To == format(max(all_runs))) %>%
    dplyr::select(District, Posting_Source, From, To, Runs, Postings, Ongoing) %>%
    dplyr::arrange(dplyr::desc(Runs))
  as.data.frame(out)
}

# Health of a posting's source in a run: the coverage calendar where it covers
# the run (Entity = District, or the WSBA feed); before it starts, whether the
# district returned any rows from that source.
source_health <- function(panel, calendar, district, source, is_feed, run) {
  if (is.na(run)) return(NA_character_)
  run_chr <- format(run)
  if (!is.null(calendar) && nrow(calendar) > 0 && run_chr >= min(calendar$Run_Date)) {
    entity <- if (is_feed) WSBA_FEED_SOURCE else district
    st <- calendar$Status[calendar$Run_Date == run_chr & calendar$Entity == entity]
    if (length(st) == 0) return(if (any(calendar$Run_Date == run_chr)) "not_logged" else "no_calendar")
    return(st[1])
  }
  if (any(panel$Run_Date == run & panel$District == district & panel$Posting_Source == source)) "ok_proxy" else "no_rows"
}

# `frozen`: frozen_board_runs() of the same panel. `statuses`:
# posting_status.csv (posting_status.R), what each posting's own page said
# after it dropped off its board.
build_posting_spells <- function(panel, calendar = NULL, endings = NULL, frozen = NULL, statuses = NULL) {
  if (is.null(panel$Close_Date)) panel$Close_Date <- NA_character_
  if (is.null(panel$Is_Feed)) panel$Is_Feed <- FALSE
  if (is.null(panel$Bridged_From)) panel$Bridged_From <- NA_character_
  runs <- sort(unique(panel$Run_Date))
  latest <- max(runs)
  # Shared title: postings that can't be told apart, i.e. title-identified
  # rows with the same district, board, title and posted date in one run (the
  # fallback ID's occurrence suffix #2, #3, ...: three "Building
  # Paraprofessional" rows with no posted date). Same-title postings with their
  # own posted dates or links are separate postings and stay in.
  fb_base <- ifelse(grepl("^Stable", panel$Posting_Identity_Method), NA_character_,
                    sub("#\\d+$", "", dplyr::coalesce(panel$Bridged_From, panel$Posting_ID)))
  dup_base <- unique(fb_base[!is.na(fb_base) & duplicated(paste(fb_base, panel$Run_Date))])
  dup <- panel[!is.na(fb_base) & fb_base %in% dup_base, , drop = FALSE]
  source_runs <- panel %>% dplyr::count(District, Posting_Source, Run_Date, name = "Rows")
  # A run where a source returned under half its previous run's rows (with at
  # least PARTIAL_MIN_PREVIOUS before) is a partial scrape, not a wave of
  # closings: UW returned 208 rows on 2026-07-31, then 25 on 08-04 and 08-07,
  # then ~200 again. Its postings' ends that run are censored.
  run_idx <- match(source_runs$Run_Date, runs)
  prev_rows <- source_runs$Rows[match(paste(source_runs$District, source_runs$Posting_Source, run_idx - 1L),
                                      paste(source_runs$District, source_runs$Posting_Source, run_idx))]
  partial <- source_runs[!is.na(prev_rows) & prev_rows >= PARTIAL_MIN_PREVIOUS & source_runs$Rows < 0.5 * prev_rows, ]
  first_source_run <- source_runs %>%
    dplyr::group_by(District, Posting_Source) %>%
    dplyr::summarize(Source_First_Run = min(Run_Date), .groups = "drop")

  sp <- panel %>%
    dplyr::arrange(Run_Date) %>%
    dplyr::group_by(Posting_ID) %>%
    dplyr::summarize(District = dplyr::last(District), Title = dplyr::last(title),
                     Position = dplyr::last(position), Posting_Source = dplyr::last(Posting_Source),
                     Identity = dplyr::last(Posting_Identity_Method), Is_Feed = any(Is_Feed),
                     Posted_Date = dplyr::first(date_posted[!is.na(date_posted) & nzchar(date_posted)], default = NA_character_),
                     Posted_Approx = dplyr::first((Posted_Approx %in% TRUE)[!is.na(date_posted) & nzchar(date_posted)], default = FALSE),
                     URL = dplyr::last(url), First_Seen = min(Run_Date), Last_Seen = max(Run_Date),
                     Stated_Close_Date = dplyr::last(Close_Date[!is.na(Close_Date)], default = NA_character_),
                     Runs_Seen = dplyr::n_distinct(Run_Date), .groups = "drop") %>%
    dplyr::left_join(first_source_run, by = c("District", "Posting_Source"))
  sp$Shared_Title <- sp$Posting_ID %in% dup$Posting_ID
  sp$Runs_Spanned <- match(sp$Last_Seen, runs) - match(sp$First_Seen, runs) + 1L
  sp$Next_Run <- runs[match(sp$Last_Seen, runs) + 1L]
  sp$Next_Run_Source_Status <- vapply(seq_len(nrow(sp)), function(i)
    source_health(panel, calendar, sp$District[i], sp$Posting_Source[i], sp$Is_Feed[i], sp$Next_Run[i]), character(1))
  is_partial <- paste(sp$District, sp$Posting_Source, sp$Next_Run) %in%
    paste(partial$District, partial$Posting_Source, partial$Run_Date)
  sp$Next_Run_Source_Status[is_partial & sp$Next_Run_Source_Status %in% c("ok", "ok_proxy")] <- "partial"

  # Moved boards: the same district + title listed from another board the next
  # run, under another ID (a WSBA posting the district's own scrape picks up,
  # a district changing platforms).
  nxt <- panel %>% dplyr::transmute(Run_Date, District, t = spell_key(title), Posting_Source_Next = Posting_Source,
                                    Posting_ID_Next = Posting_ID)
  sp_key <- data.frame(i = seq_len(nrow(sp)), Run_Date = sp$Next_Run, District = sp$District, t = spell_key(sp$Title),
                       Posting_Source = sp$Posting_Source, Posting_ID = sp$Posting_ID, stringsAsFactors = FALSE)
  mv <- merge(sp_key, nxt, by = c("Run_Date", "District", "t"))
  mv <- mv[mv$Posting_Source_Next != mv$Posting_Source & mv$Posting_ID_Next != mv$Posting_ID, , drop = FALSE]
  sp$Moved_Boards <- seq_len(nrow(sp)) %in% mv$i

  ss_status <- NA_character_
  if (!is.null(endings) && nrow(endings) > 0) {
    id <- dplyr::case_when(grepl("^schoolspring:\\d+$", sp$Posting_ID) ~ sub("^schoolspring:", "", sp$Posting_ID),
                           grepl("schoolspring\\.com/jobs/\\d+", sp$URL) ~ sub("^.*/jobs/(\\d+).*$", "\\1", sp$URL),
                           TRUE ~ NA_character_)
    ss_status <- endings$Job_Status[match(id, endings$job_id)]
  }
  # The posting's own page, checked after it dropped off (from 2026-11-06):
  # closed is a confirmed ending wherever the board's absence alone wasn't;
  # still open means the scrape missed it, so the end is censored.
  page <- data.frame(Status = rep(NA_character_, nrow(sp)), Since = NA_character_, Close = NA_character_,
                     Checked = NA_character_, stringsAsFactors = FALSE)
  if (!is.null(statuses) && nrow(statuses) > 0 && exists("posting_status_check_url")) {
    hit <- match(posting_status_check_url(sp$URL), statuses$Check_URL)
    page <- data.frame(Status = statuses$Status[hit], Since = statuses$Status_Since[hit],
                       Close = statuses$Stated_Close_Date[hit], Checked = statuses$Last_Checked[hit], stringsAsFactors = FALSE)
    # Only a check made after the posting's last sighting speaks to this spell.
    stale <- is.na(page$Checked) | page$Checked <= format(sp$Last_Seen)
    page$Status[stale] <- NA_character_
  }
  sp$Page_Status <- page$Status
  sp$Stated_Close_Date <- dplyr::coalesce(sp$Stated_Close_Date, page$Close)
  sp$Ending <- dplyr::case_when(
    sp$Last_Seen == latest ~ "still listed",
    !is.na(ss_status) ~ ss_status,
    sp$Page_Status %in% "closed" ~ "closed (confirmed)",
    sp$Page_Status %in% "open" ~ "censored: still open on its page",
    sp$Moved_Boards ~ "censored: moved boards",
    sp$Next_Run_Source_Status %in% c("ok", "ok_proxy") ~ "no longer listed",
    TRUE ~ paste0("censored: source ", sp$Next_Run_Source_Status))
  relisted <- relisted_next_run(sp$District, sp$Posting_Source, sp$Title, sp$First_Seen, sp$Next_Run)
  sp$Ending[relisted & sp$Ending %in% setdiff(POSTING_SPELL_EVENTS, "filled")] <- "censored: relisted next run"
  sp$Event <- sp$Ending %in% POSTING_SPELL_EVENTS

  posted <- suppressWarnings(as.Date(substr(sp$Posted_Date, 1, 10)))
  lag <- as.numeric(sp$First_Seen - posted)
  sp$Start_Basis <- ifelse(!is.na(lag) & lag >= 0 & lag <= 365,
                           ifelse(sp$Posted_Approx, "board posted date (approximate)", "board posted date"),
                           "first sighting")
  sp$Start_Date <- as.Date(ifelse(grepl("^board", sp$Start_Basis), posted, sp$First_Seen), origin = "1970-01-01")
  # Left-censored: first sighting is the start, and it was the source's first
  # run or the run it came back after a run with no rows.
  prev_idx <- match(sp$First_Seen, runs) - 1L
  prev_run <- runs[ifelse(prev_idx >= 1L, prev_idx, NA_integer_)]
  had_rows_prev <- paste(sp$District, sp$Posting_Source, prev_run) %in%
    paste(source_runs$District, source_runs$Posting_Source, source_runs$Run_Date)
  sp$Left_Censored <- sp$Start_Basis == "first sighting" &
    (sp$First_Seen == sp$Source_First_Run | (!is.na(prev_run) & !had_rows_prev))
  sp$Evergreen <- grepl(EVERGREEN_TITLE_PATTERN, sp$Title, ignore.case = TRUE, perl = TRUE) | (!is.na(lag) & lag > 365)
  sp$Pool <- grepl(POOL_TITLE_PATTERN, sp$Title, ignore.case = TRUE, perl = TRUE)

  # Frozen board: the posting spent FROZEN_MIN_RUNS+ runs inside one of its
  # board's frozen stretches.
  sp$Frozen_Runs <- 0L
  if (!is.null(frozen) && nrow(frozen) > 0) {
    for (j in seq_len(nrow(frozen))) {
      f <- frozen[j, ]
      hit <- which(sp$District == f$District & sp$Posting_Source == f$Posting_Source)
      if (length(hit) == 0) next
      a <- pmax(match(sp$First_Seen[hit], runs), match(as.Date(f$From), runs))
      b <- pmin(match(sp$Last_Seen[hit], runs), match(as.Date(f$To), runs))
      sp$Frozen_Runs[hit] <- pmax(sp$Frozen_Runs[hit], pmax(0L, b - a + 1L))
    }
  }
  sp$Frozen_Board <- sp$Frozen_Runs >= FROZEN_MIN_RUNS

  # End date: the stated closing date when the posting closed on it (it falls
  # after the last sighting and by the next run); otherwise the midpoint
  # between the last sighting and the next run.
  close <- suppressWarnings(as.Date(sp$Stated_Close_Date))
  on_close <- sp$Event & !is.na(close) & !is.na(sp$Next_Run) & close >= sp$Last_Seen & close <= sp$Next_Run
  sp$End_Basis <- ifelse(on_close, "stated closing date", ifelse(sp$Event, "midpoint to next run", "censored"))
  sp$End_Date <- as.Date(ifelse(on_close, close,
                                ifelse(sp$Event, sp$Last_Seen + as.numeric(sp$Next_Run - sp$Last_Seen) / 2, sp$Last_Seen)),
                         origin = "1970-01-01")
  sp$Days_Listed <- as.numeric(sp$End_Date - sp$Start_Date)
  # Advertised application window: start to the stated closing date, from the
  # posting itself; needs no ending. Board-wide default dates (a year or more
  # out) aren't deadlines.
  sp$Advertised_Days <- as.numeric(close - sp$Start_Date)
  sp$Advertised_Days[is.na(sp$Advertised_Days) | sp$Advertised_Days < 0 | sp$Advertised_Days > 365] <- NA
  # A date the posting outlived by more than a week was not its deadline: a
  # first-review date (Sheridan's "Initial Review Date"), or a stale field
  # (a 2023 "closing date" on a posting still listed in 2026).
  sp$Advertised_Days[!is.na(close) & sp$Last_Seen > close + 7] <- NA
  # Age when first seen (delayed entry): a posting up since April that we first
  # saw in August is only at risk of "ending under our eyes" from then on.
  sp$Days_Before_First_Seen <- as.numeric(sp$First_Seen - sp$Start_Date)
  sp$School_Year <- ifelse(as.integer(format(sp$Start_Date, "%m")) >= 7,
                           paste0(format(sp$Start_Date, "%Y"), "-", as.integer(format(sp$Start_Date, "%y")) + 1L),
                           paste0(as.integer(format(sp$Start_Date, "%Y")) - 1L, "-", format(sp$Start_Date, "%y")))
  sp$In_Durations <- !sp$Evergreen & !sp$Pool & !sp$Frozen_Board & !sp$Shared_Title & !sp$Left_Censored
  sp %>%
    dplyr::mutate(dplyr::across(c(First_Seen, Last_Seen, Next_Run, Source_First_Run, Start_Date, End_Date), format)) %>%
    dplyr::select(Posting_ID, District, Title, Position, Posting_Source, Identity, Is_Feed, URL, Posted_Date,
                  First_Seen, Last_Seen, Runs_Seen, Runs_Spanned, Next_Run, Next_Run_Source_Status, Moved_Boards,
                  Page_Status, Ending, Event, Start_Basis, Start_Date, School_Year, Left_Censored, Days_Before_First_Seen,
                  Stated_Close_Date, End_Basis, End_Date, Days_Listed, Advertised_Days, Evergreen, Pool, Frozen_Runs, Frozen_Board, Shared_Title, In_Durations) %>%
    dplyr::arrange(District, First_Seen, Title)
}

# Kaplan-Meier median days listed per group, over In_Durations spells, with
# delayed entry (each posting enters at its age when first seen). A posting
# seen once and still listed adds half a day of exposure so it stays valid.
summarize_posting_durations <- function(spells, group = NULL, min_events = 20) {
  s <- spells[spells$In_Durations & !is.na(spells$Days_Listed) & spells$Days_Listed >= 0, , drop = FALSE]
  groups <- if (is.null(group)) list(All = s) else split(s, s[[group]])
  dplyr::bind_rows(lapply(names(groups), function(g) {
    x <- groups[[g]]
    if (nrow(x) == 0) return(NULL)
    entry <- pmax(0, x$Days_Before_First_Seen)
    exit <- pmax(x$Days_Listed, entry + 0.5)
    fit <- survival::survfit(survival::Surv(entry, exit, x$Event) ~ 1)
    tab <- summary(fit)$table
    enough <- sum(x$Event) >= min_events
    data.frame(Group = g, Postings = nrow(x), Endings = sum(x$Event),
               Median_Days = if (enough) unname(tab["median"]) else NA_real_,
               Lower_95 = if (enough) unname(tab["0.95LCL"]) else NA_real_,
               Upper_95 = if (enough) unname(tab["0.95UCL"]) else NA_real_,
               Note = if (enough) "" else paste("fewer than", min_events, "endings"), stringsAsFactors = FALSE)
  }))
}

# --------------------------------------------------------------------------
# First seen, for the dashboard's "Posted / listed" column
# --------------------------------------------------------------------------
# Many boards give no posted date (MT: 546 of 1,380 K-12 postings on
# 2026-11-03, mostly Apptegy district sites). For those the app shows the
# first weekly run that found the posting instead. The app reads only its own
# folder and the posting IDs differ between files, so this writes a small
# lookup keyed on what the app shows: district/institution, title, link.
# Left_Censored = the posting was already up the first time we scraped that
# board, so its real start is earlier ("On the board since at least").
# Identical in the MT and WY repos; the app has its own copy of the key.
first_seen_key <- function(district, title, url) {
  sq <- function(x) tolower(gsub("\\s+", " ", trimws(as.character(x))))
  paste(sq(district), sq(title), trimws(as.character(url)), sep = " | ")
}
first_seen_lookup <- function(panel, spells, segment) {
  latest <- max(panel$Run_Date)
  cur <- panel[panel$Run_Date == latest, c("Posting_ID", "District", "title", "url"), drop = FALSE]
  cur <- merge(cur, spells[, c("Posting_ID", "First_Seen", "Left_Censored")], by = "Posting_ID")
  cur$Key <- first_seen_key(cur$District, cur$title, cur$url)
  cur <- cur[order(cur$Key, as.character(cur$First_Seen)), , drop = FALSE]
  cur <- cur[!duplicated(cur$Key), , drop = FALSE]
  data.frame(Segment = rep(segment, nrow(cur)), Key = cur$Key, First_Seen = as.character(cur$First_Seen),
             Left_Censored = as.logical(cur$Left_Censored), stringsAsFactors = FALSE)
}
