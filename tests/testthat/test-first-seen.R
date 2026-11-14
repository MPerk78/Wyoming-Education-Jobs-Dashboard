# "Posted / listed" falls back to the first run that found a posting when its
# board gives no date: first_seen_lookup() (posting_spells.R) writes
# Wy_Ed_Jobs/posting_first_seen.csv, and app.R's fill_first_seen() reads it with its
# own copy of first_seen_key(), which must stay identical.
source(here::here("posting_spells.R"))

app_functions <- function(names) {
  env <- new.env()
  for (x in parse(here::here("Wy_Ed_Jobs", "app.R"), keep.source = FALSE)) {
    if (is.call(x) && identical(x[[1]], as.name("<-")) && is.name(x[[2]]) && as.character(x[[2]]) %in% names) eval(x, env)
  }
  env
}

test_that("first_seen_lookup keys this week's postings and keeps the earliest sighting", {
  panel <- data.frame(Run_Date = as.Date(c("2026-10-27", "2026-11-03", "2026-11-03", "2026-11-03")),
                      Posting_ID = c("a", "a", "b", "c"), District = c("Frazer  Public Schools", "Frazer  Public Schools",
                      "Frazer Public Schools", "Frazer Public Schools"),
                      title = c("Head Cook", "Head Cook", "Bus Driver", "Bus Driver"),
                      url = "https://www.frazerschools.org/jobs", stringsAsFactors = FALSE)
  spells <- data.frame(Posting_ID = c("a", "b", "c"), First_Seen = c("2026-10-27", "2026-11-03", "2026-09-01"),
                       Left_Censored = c(FALSE, FALSE, TRUE))
  k <- first_seen_lookup(panel, spells, "K-12")
  expect_equal(nrow(k), 2)   # this week only; one row per key
  expect_equal(k$First_Seen[grepl("head cook", k$Key)], "2026-10-27")
  # Two postings with the same key: the earliest sighting, with its flag.
  expect_equal(k$First_Seen[grepl("bus driver", k$Key)], "2026-09-01")
  expect_true(k$Left_Censored[grepl("bus driver", k$Key)])
  expect_true(all(k$Segment == "K-12"))
})

test_that("the app's first_seen_key() is the pipeline's, and blanks are filled, posted dates kept", {
  env <- app_functions(c("first_seen_key", "fill_first_seen"))
  expect_identical(deparse(env$first_seen_key), deparse(first_seen_key))
  lookup <- data.frame(Segment = c("K-12", "K-12", "HE"),
                       Key = c(first_seen_key("Frazer Public Schools", "Head Cook", "u1"),
                               first_seen_key("Frazer Public Schools", "Bus Driver", "u2"),
                               first_seen_key("Montana Tech", "Lab Aide", "u3")),
                       First_Seen = c("2026-10-27", "2026-08-18", "2026-09-01"), Left_Censored = c("FALSE", "TRUE", "FALSE"))
  got <- env$fill_first_seen(c(NA, "", "2026-10-01", NA), c("Frazer Public Schools", " frazer  public schools", "Frazer Public Schools", "X"),
                              c("Head Cook", "Bus Driver", "Head Cook", "Teacher"), c("u1", "u2", "u1", "u9"), "K-12", lookup)
  expect_equal(got, c("First seen 2026-10-27", "On the board since at least 2026-08-18", "2026-10-01", NA))
  # HE rows only match HE keys; no file means the column is unchanged.
  expect_equal(env$fill_first_seen(NA, "Montana Tech", "Lab Aide", "u3", "K-12", lookup), NA_character_)
  expect_equal(env$fill_first_seen(NA, "Montana Tech", "Lab Aide", "u3", "HE", lookup), "First seen 2026-09-01")
  expect_equal(env$fill_first_seen(c(NA, "2026-10-01"), "a", "b", "c", "K-12", NULL), c(NA, "2026-10-01"))
})

test_that("the committed posting_first_seen.csv has the shape the app reads", {
  p <- here::here("Wy_Ed_Jobs", "posting_first_seen.csv")
  skip_if_not(file.exists(p), "posting_first_seen.csv not built yet")
  f <- read.csv(p, colClasses = "character")
  expect_true(all(c("Segment", "Key", "First_Seen", "Left_Censored") %in% names(f)))
  expect_true(all(f$Segment %in% c("K-12", "HE")))
  expect_false(any(is.na(as.Date(f$First_Seen))))
  expect_false(anyDuplicated(paste(f$Segment, f$Key)) > 0)
})

test_that("relative posted dates from WY college boards become dates, not stale text", {
  env <- app_functions(c("relative_posted_label"))
  run <- as.Date("2026-11-06")
  got <- env$relative_posted_label(c("Posted 3 days ago", "Posted 2 weeks ago", "Posted Today", "Posted more than 30 days ago",
                                     "Posted more than 30 days ago", "2026-10-01", NA), run,
                                   c(NA, NA, NA, NA, "2026-08-04", NA, NA))
  expect_equal(got, c("About 2026-11-03", "About 2026-10-23", "About 2026-11-06", "Before 2026-10-07",
                      "Before 2026-08-04", "2026-10-01", NA))
})
