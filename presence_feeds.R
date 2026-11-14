# Wyoming's statewide feed for the daily presence check (edjobs::refresh_presence(), called by
# .github/workflows/presence.yml; notes: claude-notes/2026-11-13_presence-redrover-opi-wsba.md).
#
# WSBA's vacancies page links district pages and files, never a posting, so each vacancy is keyed by
# edjobs::wsba_vacancy_key(): district, title and WSBA's own posted date (repeated 2 times in 47 on
# 2026-11-13; an occurrence suffix separates those). The weekly archive's WSBA rows (District, title,
# date_posted) give the same key. One request a day, with the weekly run's own fetch_wsba_vacancies().
# A daily ending says when WSBA took the vacancy off its list, which can lag the district.
# Not part of the weekly pipeline. Needs k12_he_classification.R and misc_district_scrapers.R sourced
# first (fetch_wsba_vacancies(), WSBA_POSTING_SOURCE from direct_api_scrapers.R).

fetch_wsba_presence <- function(known = NULL) {
  x <- fetch_wsba_vacancies()
  if (nrow(x) == 0) {
    return(data.frame(District = character(0), title = character(0), url = character(0), Link_Key = character(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(District = x$District, title = x$Title, url = x$Url,
             Link_Key = edjobs::presence_occurrence_keys(edjobs::wsba_vacancy_key(x$District, x$Title, x$Posted_Date)),
             stringsAsFactors = FALSE)
}

PRESENCE_FEEDS <- list(edjobs::presence_feed(WSBA_POSTING_SOURCE, "WSBA", fetch_wsba_presence))
