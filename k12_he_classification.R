# State-specific classification helpers. The shared classifier (position,
# subject, broad category, HE job type and faculty category, title encoding
# and detail, posted pay, placeholders) lives in the edjobs package since
# phase 2 of the package migration (2026-11-11; edjobs 0.3.0), and this file
# attaches it, so every script that sources this file still gets the whole
# classifier. A change to classification belongs in edjobs, not here.
#
# Kept here: this state's district and institution name corrections
# (canonicalize_k12_district(), canonicalize_he_institution()) and the K-12
# posting-identity code, which differs between the states.

suppressMessages(library(dplyr))
suppressMessages(library(edjobs))

# ---------------------------------------------------------------------------
# K-12
# ---------------------------------------------------------------------------

# Fixes known misspellings of district names at the source so the same
# district doesn't get silently split into two "different" districts in the
# dashboard's dropdowns/trend lines.
canonicalize_k12_district <- function(district) {
  dplyr::case_when(
    district == "Hot Springs School District 1" ~ "Hot Springs County School District 1",
    district == "Crook County school District 1" ~ "Crook County School District 1",
    district == "Albany Count School District 1" ~ "Albany County School District 1",
    district == "Campbell County Scholl District 1" ~ "Campbell County School District 1",
    district == "Converse County School Distrcit 2" ~ "Converse County School District 2",
    district == "Fremont County School District #21" ~ "Fremont County School District 21",
    district == "Washakie County School District No.1" ~ "Washakie County School District 1",
    district == "Cabon County School District 2" ~ "Carbon County School District 2",   # WSBA, 2026-10-30
    district == "Laramie Montesorri School" ~ "Laramie Montessori Charter School",  # Paylocity, 2026-11-09
    TRUE ~ district
  )
}

# Returns a snapshot-stable K-12 posting identity. Platforms that expose a
# durable source ID supply it directly; older sources fall back to a
# normalized fingerprint of the information they do expose. Archive_Date is
# deliberately excluded so one posting keeps the same identity across weeks.
# Fallback parts are joined with " | ": the "\r" used until 2026-11 landed in
# the CSVs as a line break inside the field.
build_k12_posting_id <- function(source_id = NULL, title, location,
                                 date_posted, url, district) {
  values <- list(source_id, title, location, date_posted, url, district)
  lengths <- lengths(values)
  n <- max(c(lengths, 1L))

  recycle <- function(x) {
    if (length(x) == 0) return(rep(NA_character_, n))
    if (length(x) == 1) return(rep(as.character(x), n))
    if (length(x) != n) {
      stop("build_k12_posting_id(): inputs must have length 1 or a shared length")
    }
    as.character(x)
  }

  normalize <- function(x) {
    x <- trimws(tolower(fix_title_encoding(recycle(x))))
    x[is.na(x) | !nzchar(x)] <- "<missing>"
    x
  }

  source_id <- normalize(source_id)
  # A rebuild from the raw archive passes each week's stored ID back in as
  # source_id. A stored fallback fingerprint is rebuilt from the fields, so
  # a rebuild gives the same ID the weekly run did.
  source_id[startsWith(source_id, "fallback")] <- "<missing>"
  fallback <- paste(
    "fallback",
    normalize(district),
    normalize(url),
    normalize(title),
    normalize(location),
    normalize(date_posted),
    sep = " | "
  )

  ifelse(source_id == "<missing>", fallback, source_id)
}

# Keeps the first row per posting_id. Runs right after the byte-identical
# distinct(), to catch rows that build_k12_posting_id() already treats as
# one posting but distinct() doesn't: the same source ID emitted twice with
# differing text, or fallback-keyed rows differing only in case, surrounding
# whitespace, a mis-encoded byte, or NA vs "" (all normalized away in the
# ID). Left in, those inflate k12_district_weekly_totals (a raw count()) and
# fail test-weekly-data-quality.R's posting_id-uniqueness gate, which
# blocked the 2026-09-25 weekly run. The dropped rows are written to stderr
# so the CI log shows which source produced them.
collapse_duplicate_posting_ids <- function(df, label = "K-12 postings") {
  dup <- duplicated(df$posting_id)
  if (any(dup)) {
    ids <- unique(df$posting_id[dup])
    cat(sprintf("%s: collapsing %d row(s) that share a posting_id with an earlier row:\n",
                label, sum(dup)), file = stderr())
    cat(utils::capture.output(print(df[df$posting_id %in% ids, , drop = FALSE])),
        sep = "\n", file = stderr())
  }
  df[!dup, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Posted pay
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Higher Ed
# ---------------------------------------------------------------------------

# Fixes the "Commmunity" (triple-m) misspelling that Eastern Wyoming
# Community College's scrape block used until it was corrected at the
# source; without this, the college's pre-2026-01-09 history and its
# current postings silently split into two different "institutions" the
# next time the full archive gets rebuilt.
canonicalize_he_institution <- function(institution) {
  dplyr::case_when(
    institution == "Eastern Wyoming Commmunity College" ~ "Eastern Wyoming Community College",
    TRUE ~ institution
  )
}

# ---------------------------------------------------------------------------
# Per-posting job IDs (posting_spells.R; remove_wsba_direct_duplicates()).
# ---------------------------------------------------------------------------
# k12_link_posting_id() (a per-posting ID from the link, keyed by platform and number) is in edjobs since
# 0.13.0, shared with MT and the daily presence check.

# A source posting_id the archive stored ("applitrack:bighorn1:1534",
# "schoolspring:5569023"), normalized to the same form as the link ID.
k12_source_posting_id <- function(posting_id) {
  p <- tolower(trimws(as.character(posting_id)))
  p[is.na(p) | !nzchar(p) | startsWith(p, "fallback")] <- NA_character_
  p <- sub("^schoolspring:[^0-9]*?(\\d+)$", "schoolspring:\\1", p)
  p
}
