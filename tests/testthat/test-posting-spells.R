# WY posting identity and panels (posting_spells.R) and the committed spells;
# the spell rules themselves are tested in edjobs (tests/testthat/test-posting-spells.R).
# From the WY spells trust audit (2026-11-05). Small panels shaped like the real WY cases found in the
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

test_that("the committed spells give no advertised window to a posting that outlived its stated date", {
  for (p in here::here(c("posting_spells_k12.csv", "posting_spells_he.csv"))) {
    skip_if_not(file.exists(p), "posting spells not built")
    s <- utils::read.csv(p, stringsAsFactors = FALSE)
    w <- !is.na(s$Advertised_Days)
    expect_true(all(as.Date(s$Last_Seen[w]) <= as.Date(s$Stated_Close_Date[w]) + 7), info = basename(p))
  }
})

test_that("k12_posting_panel: rows archived as read from WSBA are the WSBA feed, whatever their link's host (2026-11-13)", {
  dir <- withr::local_tempdir()
  write.csv(data.frame(title = c("K-12 Band Teacher", "Custodian"), date_posted = "2026-11-01",
                       url = c("https://platte1.org/89574_1", "https://www.applitrack.com/platte1/onlineapp/default.aspx?AppliTrackJobId=7"),
                       District = "Platte County School District 1", posting_id = NA,
                       Posting_Source = c(edjobs::current_state_config()$feed$label, "AppliTrack (direct district board)")),
            file.path(dir, "combined_2026-11-13.csv"), row.names = FALSE)
  p <- k12_posting_panel(dir, from = as.Date("2026-11-01"))
  expect_equal(p$Posting_Source[p$title == "K-12 Band Teacher"], WSBA_FEED_SOURCE)
  expect_true(p$Is_Feed[p$title == "K-12 Band Teacher"])
  expect_equal(p$Posting_Source[p$title == "Custodian"], "applitrack.com/platte1")   # direct rows keep host + tenant
  expect_false(p$Is_Feed[p$title == "Custodian"])
})
