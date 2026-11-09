# Posting-level spells (posting_spells.R; MT's design, WY spells trust audit
# 2026-11-05). Small panels shaped like the real WY cases found in the
# archives on 2026-11-05.
source(here::here("posting_spells.R"))

ps_row <- function(run, id, title, district = "Sweetwater County School District 1", posted = "2026-09-01",
                   method = "Source fallback: district + board + title + posted date",
                   source = "applitrack.com/sweetwater", url = "https://www.applitrack.com/sweetwater/onlineapp/default.aspx?all=1",
                   position = "Custodial/Maintenance", approx = FALSE) {
  data.frame(Run_Date = as.Date(run), Posting_ID = id, Posting_Identity_Method = method, District = district,
             title = title, date_posted = posted, Posted_Approx = approx, url = url, Posting_Source = source,
             Is_Feed = source == WSBA_FEED_SOURCE, position = position, stringsAsFactors = FALSE)
}

test_that("WY posted dates parse in every format the boards print", {
  x <- c("2026-10-14", "10/14/2026", "Oct 14, 2026 9:41 AM (UTC)", "Sep 9, 2026", "14-Oct-26",
         "3 days ago(10/27/2026 9:12 AM)", "Posted 2 weeks ago", "Today", "Posted more than 30 days ago",
         "Classified", "\n                10/1/2026\n          ", "2026-11-30")
  p <- parse_wy_posted_date(x, "2026-10-30")
  expect_equal(p$date, c("2026-10-14", "2026-10-14", "2026-10-14", "2026-09-09", "2026-10-14", "2026-10-27",
                         "2026-10-16", "2026-10-30", NA, NA, "2026-10-01", NA))   # a date after the run is an error
  expect_equal(p$approx, c(rep(FALSE, 6), TRUE, TRUE, rep(FALSE, 4)))
})

test_that("per-posting links become IDs keyed by platform; board pages and the WSBA feed don't", {
  u <- c("https://www.applitrack.com/bighorn1/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=1534",
         "https://laramie1.schoolspring.com/jobs/5569023",
         "https://jobs.redroverk12.com/org/jcsd1/opening/12345",
         "https://fremont2.tedk12.com/hire/ViewJob.aspx?JobID=77",
         "https://laramie1.schoolspring.com/", "https://www.wsba-wy.org/vacancies")
  expect_equal(k12_link_posting_id(u), c("applitrack:bighorn1:1534", "schoolspring:5569023", "redrover:jcsd1:12345",
                                         "tedk12:fremont2:77", NA, NA))
  expect_equal(k12_posting_source(u), c("applitrack.com/bighorn1", "laramie1.schoolspring.com", "jobs.redroverk12.com",
                                        "fremont2.tedk12.com", "laramie1.schoolspring.com", WSBA_FEED_SOURCE))
  # The archive's own posting_id agrees with the link form.
  expect_equal(k12_source_posting_id(c("schoolspring:5569023", "applitrack:bighorn1:1534", "fallback | x")),
               c("schoolspring:5569023", "applitrack:bighorn1:1534", NA))
})

test_that("an HE requisition keeps its ID across a host rename; board pages fall back to the title", {
  # Western Wyoming moved wwccwy -> wwcwy.peopleadmin.com in 2026-08 with the same posting numbers.
  inst <- rep("Western Wyoming Community College", 3)
  id <- he_link_posting_id(c("https://wwccwy.peopleadmin.com/postings/5991", "https://wwcwy.peopleadmin.com/postings/5991",
                             "https://wwccwy.peopleadmin.com/postings/search?page="), inst)
  expect_equal(id[1], id[2])
  expect_true(is.na(id[3]))
  expect_equal(he_link_posting_id("https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/262649",
                                  "University of Wyoming"), "he:university of wyoming:job/262649")
})

test_that("a link held by two titles in one run is a board page, not an ID", {
  d <- ps_row("2026-10-30", NA, "Cook")[c(1, 1), ]
  d$title <- c("Cook", "Bus Driver")
  x <- assign_posting_identity(d, c("same-link", "same-link"))
  expect_true(all(grepl("^fb", x$Posting_ID)))
})

test_that("earlier title-identified rows join the job ID the same posting carries later", {
  p <- rbind(ps_row("2026-10-16", "fb-1", "Night Lead Custodian", posted = "2026-10-01"),
             ps_row("2026-10-23", "fb-1", "Night Lead Custodian", posted = "2026-10-01"),
             ps_row("2026-10-30", "applitrack:sweetwater:9", "Night Lead Custodian", posted = "2026-10-01",
                    method = "Stable per-posting ID"))
  b <- bridge_posting_ids(p)
  expect_equal(unique(b$Posting_ID), "applitrack:sweetwater:9")
  expect_equal(b$Bridged_From[1], "fb-1")
})

test_that("ends are censored on a partial run, a source with no rows, and a move to another board", {
  big <- do.call(rbind, lapply(1:12, function(i) ps_row("2026-07-31", paste0("u", i), paste("Job", i),
                                                       district = "University of Wyoming", source = "University of Wyoming")))
  small <- ps_row("2026-08-04", "u1", "Job 1", district = "University of Wyoming", source = "University of Wyoming")
  p <- rbind(big, small,
             # WSBA listed it, then the district's own board did.
             ps_row("2026-07-31", "w", "Science Teacher", district = "Lincoln County School District 1", source = WSBA_FEED_SOURCE,
                    url = "https://www.wsba-wy.org/vacancies"),
             ps_row("2026-08-04", "own", "Science Teacher", district = "Lincoln County School District 1",
                    source = "lincolnwy.schoolspring.com"),
             # Gone while the board was healthy: a real ending.
             ps_row("2026-07-31", "a", "Custodian"), ps_row("2026-07-31", "b", "Cook"), ps_row("2026-08-04", "b", "Cook"),
             ps_row("2026-08-11", "b", "Cook"))
  sp <- build_posting_spells(p)
  e <- setNames(sp$Ending, sp$Posting_ID)
  expect_equal(unname(e["u2"]), "censored: source partial")     # 12 rows then 1
  expect_equal(unname(e["w"]), "censored: moved boards")
  expect_equal(unname(e["a"]), "no longer listed")
  expect_equal(unname(e["own"]), "censored: source no_rows")    # its board returned nothing on 08-11
  expect_equal(sp$End_Date[sp$Posting_ID == "a"], "2026-08-02")
})

test_that("SchoolSpring endings come from the posting's record", {
  p <- rbind(ps_row("2026-10-23", "schoolspring:5569023", "Golf Coach", source = "acsd1.schoolspring.com",
                    method = "Stable per-posting ID", url = "https://acsd1.schoolspring.com/jobs/5569023"),
             ps_row("2026-10-23", "x", "Other job", source = "acsd1.schoolspring.com"),
             ps_row("2026-10-30", "x", "Other job", source = "acsd1.schoolspring.com"))
  sp <- build_posting_spells(p, endings = data.frame(job_id = "5569023", Job_Status = "filled"))
  expect_equal(sp$Ending[sp$Posting_ID == "schoolspring:5569023"], "filled")
})

test_that("frozen boards are found, broken by a missing run, and their postings flagged", {
  runs <- seq(as.Date("2024-10-18"), by = 7, length.out = 14)
  titles <- paste("Teacher", 1:5)
  p <- do.call(rbind, lapply(runs, function(r) do.call(rbind, lapply(titles, function(t)
    ps_row(format(r), paste0("fb-", t), t, district = "Fremont County School District 38", source = "applitrack.com/fcsd38")))))
  # Another board, active every run, so the schedule has all 14 runs.
  p <- rbind(p, do.call(rbind, lapply(seq_along(runs), function(i) ps_row(format(runs[i]), paste0("o", i), paste("Opening", i)))))
  f <- frozen_board_runs(p)
  expect_equal(nrow(f), 1)
  expect_equal(f$Runs, 14L)
  sp <- build_posting_spells(p, frozen = f)
  expect_true(all(sp$Frozen_Board[sp$District == "Fremont County School District 38"]))
  expect_false(any(sp$In_Durations[sp$District == "Fremont County School District 38"]))
  # A run with no rows from that board splits the stretch: 6 + 7 runs, neither long enough.
  expect_equal(nrow(frozen_board_runs(p[!(p$District == "Fremont County School District 38" & p$Run_Date == runs[7]), ])), 0)
  # Fewer than FROZEN_MIN_POSTINGS postings: a small district's unchanged board may be real.
  expect_equal(nrow(frozen_board_runs(p[!(p$title %in% titles[1:2]), ])), 0)
})

test_that("only indistinguishable same-title postings are shared; pools and standing ads are flagged", {
  p <- rbind(ps_row("2026-08-21", "fb-x#1", "Building Paraprofessional", posted = NA),
             ps_row("2026-08-21", "fb-x#2", "Building Paraprofessional", posted = NA),
             ps_row("2026-08-21", "fb-y#1", "Special Education Teacher", posted = "2026-08-01"),
             ps_row("2026-08-21", "fb-z#1", "Special Education Teacher", posted = "2026-08-02"),
             ps_row("2026-08-21", "p", "Adjunct Faculty - Biology"),
             ps_row("2026-08-21", "s", "2026-2027 Substitute Teacher - Part-Time"),
             ps_row("2026-08-28", "fb-y#1", "Special Education Teacher", posted = "2026-08-01"))
  sp <- build_posting_spells(p)
  r <- function(id, col) sp[[col]][sp$Posting_ID == id]
  expect_true(r("fb-x#1", "Shared_Title") && r("fb-x#2", "Shared_Title"))
  expect_false(r("fb-y#1", "Shared_Title") || r("fb-z#1", "Shared_Title"))
  expect_true(r("p", "Pool")); expect_false(r("p", "In_Durations"))
  expect_true(r("s", "Evergreen"))
  expect_equal(r("fb-y#1", "School_Year"), "2026-27")
})

test_that("a first sighting after a run with no rows from the board is left-censored", {
  p <- rbind(ps_row("2026-08-04", "a", "Cook"), ps_row("2026-08-04", "z", "Aide", source = "other.org"),
             ps_row("2026-08-07", "z", "Aide", source = "other.org"),
             ps_row("2026-08-11", "c", "Bus Driver", posted = NA))
  sp <- build_posting_spells(p)
  expect_true(sp$Left_Censored[sp$Posting_ID == "c"])
})

test_that("start prefers an exact posted date over a relative one", {
  p <- rbind(ps_row("2026-10-23", "u", "Analyst", posted = "2026-10-09", approx = TRUE),
             ps_row("2026-10-30", "u", "Analyst", posted = "2026-10-12", approx = FALSE))
  q <- use_earliest_posted_date(p)
  expect_equal(unique(q$date_posted), "2026-10-12")
  expect_false(any(q$Posted_Approx))
})

test_that("Kaplan-Meier medians use delayed entry and need enough endings", {
  mk <- function(before, days) data.frame(In_Durations = TRUE, Days_Before_First_Seen = before,
                                          Days_Listed = days, Event = TRUE, Position = "Teacher")
  s <- rbind(mk(rep(0, 30), rep(10, 30)), mk(rep(200, 30), rep(210, 30)))
  expect_equal(summarize_posting_durations(s)$Median_Days, 10)
  expect_true(is.na(summarize_posting_durations(s[1:5, ])$Median_Days))
})

test_that("the drift report lists only boards still frozen this run", {
  f <- data.frame(District = c("Fremont County School District 38", "Weston County School District 7"),
                  Posting_Source = c("applitrack.com/fcsd38", "weston7.org"), From = c("2024-10-18", "2024-10-18"),
                  To = c("2026-10-30", "2025-01-10"), Runs = c(57L, 13L), Postings = c(14L, 10L),
                  Ongoing = c(TRUE, FALSE), Segment = "K-12", stringsAsFactors = FALSE)
  md <- build_frozen_board_report_markdown(f, date = "2026-11-06")
  expect_true(any(grepl("Fremont County School District 38\\*\\* \\(applitrack.com/fcsd38\\).*14 posting\\(s\\) for 57 weeks", md)))
  expect_false(any(grepl("Weston", md)))
  expect_length(build_frozen_board_report_markdown(f[2, ]), 0)
})

test_that("a posting's own page confirms or overrules its ending, and dates it by a stated closing date", {
  source(here::here("posting_status.R"))
  at <- function(id) paste0("https://www.applitrack.com/sweetwater/onlineapp/default.aspx?AppliTrackLayoutMode=detail&AppliTrackJobId=", id)
  p <- rbind(ps_row("2026-10-23", "applitrack:sweetwater:1", "Cook", method = "Stable per-posting ID", url = at(1), posted = "2026-10-01"),
             ps_row("2026-10-23", "applitrack:sweetwater:2", "Aide", method = "Stable per-posting ID", url = at(2), posted = "2026-10-01"),
             ps_row("2026-10-23", "applitrack:sweetwater:3", "Clerk", method = "Stable per-posting ID", url = at(3), posted = "2026-10-01"),
             ps_row("2026-10-30", "applitrack:sweetwater:4", "Nurse", method = "Stable per-posting ID", url = at(4)))
  p$Close_Date <- c("2026-10-27", NA, NA, NA)
  st <- data.frame(Check_URL = posting_status_check_url(at(1:3)), Status = c("closed", "open", "closed"),
                   Status_Since = c("2026-10-30", NA, "2026-10-20"), Stated_Close_Date = NA,
                   Last_Checked = c("2026-10-30", "2026-10-30", "2026-10-20"), stringsAsFactors = FALSE)
  sp <- build_posting_spells(p, statuses = st)
  r <- function(id, col) sp[[col]][sp$Posting_ID == paste0("applitrack:sweetwater:", id)]
  expect_equal(r(1, "Ending"), "closed (confirmed)")
  expect_equal(r(1, "End_Date"), "2026-10-27"); expect_equal(r(1, "End_Basis"), "stated closing date")
  expect_equal(r(1, "Advertised_Days"), 26)
  expect_equal(r(2, "Ending"), "censored: still open on its page"); expect_false(r(2, "Event"))
  # A check from before the last sighting says nothing about this spell.
  expect_equal(r(3, "Ending"), "no longer listed"); expect_equal(r(3, "End_Basis"), "midpoint to next run")
})

test_that("a posting relisted under a new ID the next run is 'outcome unknown', not an ending", {
  # 2026-11-09: of 23 WY SchoolSpring relistings whose old record has a final
  # status, 8 were "filled" (a new vacancy with the same title), so relistings
  # are neither linked nor counted as endings.
  d <- c("Albany County School District 1", "Albany County School District 1", "Albany County School District 1",
         "Albany County School District 1", "Laramie County School District 1")
  src <- c("acsd1", "acsd1", "acsd1", "other board", "acsd1")
  title <- c("Mechanic", " mechanic ", "Elementary Teacher", "Mechanic", "Mechanic")
  first <- as.Date(c("2026-03-06", "2026-04-17", "2026-04-17", "2026-04-17", "2026-04-17"))
  nxt <- as.Date(c("2026-04-17", NA, NA, NA, NA))
  # Same district, source and title (case and spacing aside), first seen the old one's next run.
  expect_equal(relisted_next_run(d, src, title, first, nxt), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  # A different title, source or district, or a later run, is not a relisting.
  expect_equal(relisted_next_run(d[c(1, 3)], src[c(1, 3)], c("Custodian", "Elementary Teacher"), first[c(1, 3)],
                                 as.Date(c("2026-04-17", NA))), c(FALSE, FALSE))
  expect_false(relisted_next_run(d[1], src[1], title[1], first[1], as.Date("2026-04-24")))
  expect_false(relisted_next_run(d[1], src[1], title[1], first[1], NA))
})

test_that("the committed spells mark relistings unknown but keep platform-reported fills", {
  p <- here::here("posting_spells_k12.csv")
  skip_if_not(file.exists(p), "posting_spells_k12.csv not built")
  s <- utils::read.csv(p, colClasses = "character")
  r <- s$Ending == "censored: relisted next run"
  expect_true(all(s$Event[r] == "FALSE"))
  expect_true(all(s$End_Basis[r] == "censored"))
  # Every relisting is either marked unknown or kept as the platform's "filled";
  # no relisting is still counted as a plain ending.
  rel <- relisted_next_run(s$District, s$Posting_Source, s$Title, as.Date(s$First_Seen), as.Date(s$Next_Run))
  expect_false(any(rel & s$Ending %in% setdiff(POSTING_SPELL_EVENTS, "filled")))
  expect_true(all(r <= rel))
})
