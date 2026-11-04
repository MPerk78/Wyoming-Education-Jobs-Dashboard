# Coverage calendar (posting-dates plan, step 2): for every run, whether each
# source we count postings from was healthy. A posting that stops appearing
# can only be read as "ended" when its source returned a normal result that
# run; when the source errored, came back empty or wasn't run, the posting's
# end is unknown (censored), not a vacancy that closed.
#
# Built from scrape_log.csv (safe_scrape() logs every source's outcome, but
# the log is .gitignore'd and kept only 90 days as a workflow artifact), and
# written to coverage_calendar.csv, committed by the weekly workflow. One row
# per run per entity:
#
#   Run_Date, Segment ("K-12", "HE", "Feed"), Entity (the District /
#   Institution name postings carry, or a feed's label), Source (the name it
#   logged under, at most 120 characters), Status (ok / empty / error /
#   not_logged; also "shared_board" pass-through, see below), N_Rows, Error.
#
# - Entities: registry districts and institutions, the configured feeds,
#   any other logged source whose name matches a posting's District /
#   Institution this run (WY districts scraped from their own pages outside
#   the registry), and every entity the calendar tracked before. An entity
#   with no log entry this run is "not_logged", which counts as unhealthy:
#   e.g. when the shared Apptegy browser session can't start, no
#   per-district rows are logged at all.
# - Apptegy districts log as "Apptegy/chromote: <District>"; the prefix is
#   dropped to match the registry.
# - Institutions without a board of their own (MT: Highlands College on
#   Montana Tech's JazzHR; Helena College, Missoula College and UM Western on
#   UM's NEOGOV) take their parent board's status (`shared_boards`).
# - Postings that come only through a statewide feed (OPI in MT; WSBA in WY,
#   e.g. BOCES and academies) take the feed's row.
# - Reference tables, Census/IPEDS context and the LLM shadow pilot are not
#   posting sources and are left out.
COVERAGE_CALENDAR_COLUMNS <- c("Run_Date", "Segment", "Entity", "Source", "Status", "N_Rows", "Error")
COVERAGE_HEALTHY_STATUS <- "ok"

# scrape_log.csv, including logs written before log_scrape_result() doubled
# quotes (a backslash-escaped quote in an error message broke read.csv for
# every later row).
read_scrape_log <- function(log_path) {
  txt <- gsub('\\\\"', '""', readLines(log_path, warn = FALSE, encoding = "UTF-8"))
  utils::read.csv(text = paste(txt, collapse = "\n"), stringsAsFactors = FALSE, colClasses = "character")
}

coverage_entity_name <- function(source) sub("^Apptegy/chromote: ", "", source)

# The latest logged attempt per source on run_date.
latest_log_entries <- function(scrape_log, run_date) {
  l <- scrape_log[!is.na(scrape_log$timestamp) & substr(scrape_log$timestamp, 1, 10) == format(as.Date(run_date)), , drop = FALSE]
  l <- l[order(l$timestamp), , drop = FALSE]
  l[!duplicated(l$source, fromLast = TRUE), , drop = FALSE]
}

build_coverage_calendar <- function(scrape_log, run_date, k12_entities = character(0), he_entities = character(0),
                                    data_k12 = character(0), data_he = character(0), feeds = character(0),
                                    shared_boards = character(0), previous = NULL) {
  l <- latest_log_entries(scrape_log, run_date)
  l$entity <- coverage_entity_name(l$source)

  # Feeds: named vector, label = regex on the logged source name.
  feed_rows <- lapply(names(feeds), function(label) {
    hit <- l[grepl(feeds[[label]], l$source), , drop = FALSE]
    if (nrow(hit) == 0) return(data.frame(Segment = "Feed", Entity = label, Source = NA_character_,
                                          Status = "not_logged", N_Rows = 0L, Error = NA_character_))
    hit <- hit[nrow(hit), ]
    data.frame(Segment = "Feed", Entity = label, Source = hit$source, Status = hit$status,
               N_Rows = as.integer(hit$n_rows), Error = hit$error_message)
  })

  prev_k12 <- prev_he <- character(0)
  if (!is.null(previous) && nrow(previous) > 0) {
    prev_k12 <- unique(previous$Entity[previous$Segment == "K-12"])
    prev_he <- unique(previous$Entity[previous$Segment == "HE"])
  }
  k12 <- unique(c(k12_entities, intersect(l$entity, data_k12), prev_k12))
  he <- unique(c(he_entities, intersect(l$entity, data_he), prev_he))
  he <- setdiff(he, k12)

  entity_rows <- function(entities, segment) {
    if (length(entities) == 0) return(NULL)
    via <- ifelse(entities %in% names(shared_boards), shared_boards[entities], entities)
    m <- match(via, l$entity)
    data.frame(Segment = segment, Entity = entities, Source = l$source[m],
               Status = ifelse(is.na(m), "not_logged", l$status[m]),
               N_Rows = ifelse(is.na(m), 0L, as.integer(l$n_rows[m])),
               Error = l$error_message[m])
  }

  out <- dplyr::bind_rows(entity_rows(k12, "K-12"), entity_rows(he, "HE"), dplyr::bind_rows(feed_rows))
  if (nrow(out) == 0) return(coverage_calendar_empty())
  out$Run_Date <- format(as.Date(run_date))
  out$Source <- substr(out$Source, 1, 120)
  out$Error <- substr(gsub("[\r\n]+", " ", out$Error), 1, 200)
  out <- out[, COVERAGE_CALENDAR_COLUMNS]
  out[order(out$Segment, out$Entity), , drop = FALSE]
}

coverage_calendar_empty <- function() {
  data.frame(Run_Date = character(0), Segment = character(0), Entity = character(0), Source = character(0),
             Status = character(0), N_Rows = integer(0), Error = character(0), stringsAsFactors = FALSE)
}

# Replaces this run's rows (a same-day rerun) and keeps the rest.
append_coverage_calendar <- function(calendar, rows) {
  if (!is.null(calendar) && nrow(calendar) > 0) {
    calendar <- dplyr::mutate(calendar, dplyr::across(-N_Rows, as.character), N_Rows = as.integer(N_Rows))
  }
  calendar <- dplyr::bind_rows(coverage_calendar_empty(), calendar)
  out <- dplyr::bind_rows(calendar[!calendar$Run_Date %in% rows$Run_Date, , drop = FALSE], rows)
  out[order(out$Run_Date, out$Segment, out$Entity), , drop = FALSE]
}

# Weekly step (run once every source has been scraped, i.e. after the HE
# archive is written). Returns the run's rows invisibly.
refresh_coverage_calendar <- function(path = "coverage_calendar.csv", log_path = "scrape_log.csv",
                                      run_date = Sys.Date(), ...) {
  if (!file.exists(log_path)) stop(log_path, " not found")
  scrape_log <- read_scrape_log(log_path)
  # No log at all for the run means we can't tell anything; leave the calendar
  # alone rather than marking every tracked source not_logged.
  if (nrow(latest_log_entries(scrape_log, run_date)) == 0) stop("no scrape_log entries for ", format(as.Date(run_date)))
  calendar <- if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character") else NULL
  rows <- build_coverage_calendar(scrape_log, run_date, previous = calendar, ...)
  utils::write.csv(append_coverage_calendar(calendar, rows), path, row.names = FALSE)
  message("Coverage calendar ", format(as.Date(run_date)), ": ",
          paste(names(table(rows$Status)), table(rows$Status), collapse = ", "))
  invisible(rows)
}
