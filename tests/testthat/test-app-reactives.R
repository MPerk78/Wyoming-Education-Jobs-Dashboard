# Loads the real app.R (with the real committed CSVs) into an isolated
# environment and exercises its reactives directly via shiny::testServer,
# rather than mocking data -- this is the same technique used to verify the
# 2026-08-02 fixes by hand; committing it here keeps that verification
# running on every future change instead of being a one-off.

skip_if_not_installed("shiny")

load_app <- function() {
  app_dir <- here::here("Wy_Ed_Jobs")
  skip_if_not(dir.exists(app_dir), "Wy_Ed_Jobs/ not found -- skipping app tests")
  old_wd <- setwd(app_dir)
  on.exit(setwd(old_wd), add = TRUE)

  env <- new.env()
  suppressMessages(sys.source("app.R", envir = env))
  env
}

# Copies the real Wy_Ed_Jobs/ app folder into a scratch tempdir, applies
# corrupt_fn to one file in the copy (not the real committed data), then
# loads app.R from there -- for testing the schema guard's behavior when a
# dataset is actually broken, without touching anything real.
load_app_with_corrupted_file <- function(file_name, corrupt_fn) {
  app_dir <- here::here("Wy_Ed_Jobs")
  skip_if_not(dir.exists(app_dir), "Wy_Ed_Jobs/ not found -- skipping app tests")

  scratch_dir <- withr::local_tempdir()
  file.copy(list.files(app_dir, full.names = TRUE), scratch_dir, recursive = TRUE)
  corrupt_fn(file.path(scratch_dir, file_name))

  old_wd <- setwd(scratch_dir)
  on.exit(setwd(old_wd), add = TRUE)

  env <- new.env()
  suppressMessages(sys.source("app.R", envir = env))
  env
}

test_that("HE longitudinal 'Total' view is not double-counted", {
  # Regression for the HE double-counting bug: filtered_hesum() used to
  # return the entire hesum_he table (Total row + every institution's row)
  # whenever "Total" was selected, and he_longitudinal_plot summed them
  # together -- inflating every category exactly 2x.
  env <- load_app()

  shiny::testServer(env$server, {
    session$setInputs(
      inst_trend = "Total",
      he_category = sort(unique(env$hesum_he$Category)),
      he_scroll = c(min(env$hesum_he$Archive_Date), max(env$hesum_he$Archive_Date)),
      he_chart_type = "all",
      he_detail_level_trends = "detail"
    )

    windowed <- he_windowed()
    expect_true(all(windowed$Institution != "Total"))

    latest_date <- max(windowed$Archive_Date)
    observed <- windowed %>%
      dplyr::filter(Archive_Date == latest_date) %>%
      dplyr::group_by(Category) %>%
      dplyr::summarize(sum = sum(sum), .groups = "drop")

    # hesum_he now carries a Job_Type column (Full Time vs Adjunct/Part-Time);
    # "All Jobs" mode sums across it, so the expected total does too.
    expected <- env$hesum_he %>%
      dplyr::filter(Archive_Date == latest_date, Institution == "Total") %>%
      dplyr::group_by(Category) %>%
      dplyr::summarize(sum = sum(sum), .groups = "drop")

    merged <- dplyr::inner_join(observed, expected, by = "Category", suffix = c("_observed", "_expected"))
    expect_gt(nrow(merged), 0)
    expect_equal(merged$sum_observed, merged$sum_expected)
  })
})

test_that("K-12 longitudinal 'Total' view is still not double-counted", {
  # Same shape of bug is structurally possible here too (filtered_k12sum()
  # also returns everything unfiltered for "Total") -- df_windowed() already
  # guards against it; this pins that guard in place.
  env <- load_app()

  shiny::testServer(env$server, {
    session$setInputs(
      district_trend = "Total",
      broad_category = sort(unique(env$k12sum$Broad_Category)),
      k12_scroll = c(min(env$k12sum$Archive_Date), max(env$k12sum$Archive_Date)),
      k12_detail_level_trends = "detail"
    )

    windowed <- df_windowed()
    expect_true(all(windowed$Broad_Category %in% unique(env$k12sum$Broad_Category)))

    latest_date <- max(windowed$Archive_Date)
    observed <- windowed %>%
      dplyr::filter(Archive_Date == latest_date) %>%
      dplyr::group_by(Broad_Category) %>%
      dplyr::summarize(sum = sum(sum), .groups = "drop")

    expected <- env$k12sum %>%
      dplyr::filter(Archive_Date == latest_date, District == "Total") %>%
      dplyr::select(Broad_Category, sum)

    merged <- dplyr::inner_join(observed, expected, by = "Broad_Category", suffix = c("_observed", "_expected"))
    expect_gt(nrow(merged), 0)
    expect_equal(merged$sum_observed, merged$sum_expected)
  })
})

test_that("HE and K-12 reactives don't error across every institution/district", {
  env <- load_app()

  shiny::testServer(env$server, {
    for (inst in sort(unique(env$hesum_he$Institution))) {
      session$setInputs(inst_trend = inst, he_category = sort(unique(env$hesum_he$Category)),
                         he_scroll = c(min(env$hesum_he$Archive_Date), max(env$hesum_he$Archive_Date)),
                         he_chart_type = "all", he_detail_level_trends = "detail")
      expect_no_error(he_windowed())
      expect_no_error(output$he_longitudinal_plot)
    }
    for (inst in sort(unique(env$henowsum_he$Institution))) {
      session$setInputs(
        inst_current = inst,
        he_detail_level_current = "detail",
        he_current_appointment = "all"
      )
      expect_no_error(output$he_current_trends_table)
    }

    for (d in sort(unique(env$k12sum$District))) {
      session$setInputs(district_trend = d, broad_category = sort(unique(env$k12sum$Broad_Category)),
                         k12_scroll = c(min(env$k12sum$Archive_Date), max(env$k12sum$Archive_Date)),
                         k12_detail_level_trends = "detail")
      expect_no_error(df_windowed())
      expect_no_error(output$k12_longitudinal_plot)
    }
    for (d in sort(unique(env$k12nowsum$District))) {
      session$setInputs(district_current = d, k12_detail_level_current = "detail")
      expect_no_error(output$k12_current_trends_table)
    }
  })
})

test_that("higher-ed appointment classifier separates adjunct, faculty, and other roles", {
  env <- load_app()

  expect_identical(
    env$classify_he_appointment(c(
      "Adjunct Mathematics Instructor",
      "Biology Professor",
      "Admissions Specialist"
    )),
    c(
      "Adjunct/part-time faculty",
      "Faculty/instructor (non-adjunct)",
      "Other / not faculty"
    )
  )
})

test_that("make_sparkline_svg draws a rising trend with a green endpoint and a falling trend with a red one", {
  env <- load_app()

  rising <- env$make_sparkline_svg(c(10, 15, 20))
  expect_match(rising, "^<svg")
  expect_match(rising, "#1baf7a", fixed = TRUE)

  falling <- env$make_sparkline_svg(c(20, 15, 10))
  expect_match(falling, "#e34948", fixed = TRUE)

  flat <- env$make_sparkline_svg(c(10, 10, 10))
  expect_match(flat, "#999999", fixed = TRUE)
})

test_that("make_sparkline_svg handles too little data without erroring", {
  env <- load_app()

  expect_equal(env$make_sparkline_svg(numeric(0)), "")
  expect_equal(env$make_sparkline_svg(c(5)), "")
  expect_equal(env$make_sparkline_svg(c(NA, NA)), "")
  # One real value plus NAs still isn't enough to draw a line between two points.
  expect_equal(env$make_sparkline_svg(c(5, NA, NA)), "")
})

test_that("compute_wow_delta skips an extra same-week snapshot instead of comparing against it", {
  env <- load_app()

  # A real week-old snapshot, plus an extra same-week re-run 1 day before
  # "latest" (the exact failure mode min_days_back guards against -- see
  # Archivek12_Data's 2026-08-03 entries for a real historical example).
  weekly <- data.frame(
    Archive_Date = as.Date(c("2026-07-30", "2026-08-05", "2026-08-06")),
    n = c(100, 108, 111)
  )
  # Without the gap guard this would return 111 - 108 = 3 (comparing
  # against yesterday's extra run); with it, it should skip the too-recent
  # 2026-08-05 snapshot and compare against the real week-old one.
  expect_equal(env$compute_wow_delta(weekly), 111 - 100)
})

test_that("compute_wow_delta returns NA when no snapshot is old enough to count as 'last week'", {
  env <- load_app()

  weekly <- data.frame(
    Archive_Date = as.Date(c("2026-08-05", "2026-08-06")),
    n = c(50, 55)
  )
  expect_true(is.na(env$compute_wow_delta(weekly)))
})

test_that("compute_vacancy_rate_domain falls back to a placeholder range instead of Inf/-Inf when every rate is NA", {
  env <- load_app()

  expect_equal(env$compute_vacancy_rate_domain(c(NA_real_, NA_real_, NA_real_)), c(0, 1))
  expect_equal(env$compute_vacancy_rate_domain(c(0.05, NA_real_, 0.20)), c(0.05, 0.20))
})

test_that("Sheridan/Gillette get a shared joint vacancy rate instead of NA", {
  # Regression for switching from "suppress both to NA" to "both show the
  # same combined rate" -- see map_he's Vacancy_Rate_Shared comment.
  env <- load_app()
  sg <- env$combined_map_data[env$combined_map_data$Name %in% c("Sheridan College", "Gillette College"), ]
  skip_if(nrow(sg) < 2, "Sheridan/Gillette rows not both present in committed data -- skipping")

  expect_true(all(sg$Vacancy_Rate_Shared))
  expect_false(any(is.na(sg$Vacancy_Rate)))
  # Same joint numerator/denominator/rate for both rows, not each one's own
  # independent count -- that's the whole point of "shared."
  expect_equal(sg$Vacancy_Numerator[1], sg$Vacancy_Numerator[2])
  expect_equal(sg$Vacancy_Denominator[1], sg$Vacancy_Denominator[2])
  expect_equal(sg$Vacancy_Rate[1], sg$Vacancy_Rate[2])
  # Internally consistent: the displayed rate really is numerator/denominator.
  expect_equal(sg$Vacancy_Rate, sg$Vacancy_Numerator / sg$Vacancy_Denominator)
})

test_that("an institution not flagged via Salary_Note gets its own independent vacancy rate", {
  env <- load_app()
  uw <- env$combined_map_data[env$combined_map_data$Name == "University of Wyoming", ]
  skip_if(nrow(uw) == 0, "University of Wyoming row not present in committed data -- skipping")

  expect_false(uw$Vacancy_Rate_Shared)
})

test_that("Data_Coverage is carried through to combined_map_data for both K-12 and HE", {
  # Regression for the disclosure fix: misc-district data completeness used
  # to live only in code comments, invisible to anyone using the dashboard.
  # Every HE institution is "Full" (no misc tier on that side); K-12 has a
  # real mix since misc_district_registry's 12 districts get a "Partial"
  # tier via salarymap2.csv's Data_Coverage column.
  env <- load_app()
  expect_true("Data_Coverage" %in% names(env$combined_map_data))
  expect_false(any(is.na(env$combined_map_data$Data_Coverage)))

  he <- env$combined_map_data[env$combined_map_data$Type == "Higher Ed Institution", ]
  skip_if(nrow(he) == 0, "no HE rows in committed data -- skipping")
  expect_true(all(he$Data_Coverage == "Full"))

  k12_partial <- env$combined_map_data[env$combined_map_data$Type == "K-12 District" &
                                          env$combined_map_data$Data_Coverage != "Full", ]
  skip_if(nrow(k12_partial) == 0, "no partial-coverage K-12 rows in committed data -- skipping")
  # Both partial tiers are legitimate here: misc_district_registry's 12
  # districts ("WSBA + own page"), plus WSBA_ONLY_ORGS entries like Snowy
  # Range Academy ("WSBA only", no own-page scraper at all).
  expect_true(all(k12_partial$Data_Coverage %in% c("Partial (WSBA + own page)", "Partial (WSBA only)")))
})

test_that("clean committed data produces no schema-guard issues", {
  env <- load_app()
  expect_equal(env$DATA_LOAD_ISSUES, character(0))
})

test_that("a dataset missing an expected column degrades instead of crashing app.R, and names the source", {
  # Regression for the "if something breaks, the dashboard should alert you
  # which data source is broken" fix -- before validate_and_pad_schema(),
  # this exact scenario (Teachers_Total_FTE silently dropped from
  # salarymap2.csv) crashed app.R at load time with a bare "object
  # 'Teachers_Total_FTE' not found", naming nothing.
  env <- load_app_with_corrupted_file("salarymap2.csv", function(path) {
    df <- read.csv(path)
    df$Teachers_Total_FTE <- NULL
    write.csv(df, path, row.names = FALSE)
  })

  expect_length(env$DATA_LOAD_ISSUES, 1)
  expect_match(env$DATA_LOAD_ISSUES, "salarymap2\\.csv is missing expected column\\(s\\): Teachers_Total_FTE")
  # The rest of the app still built successfully -- the missing column was
  # padded with NA rather than left absent, so downstream code that
  # references it by name (Vacancy_Rate's calculation) didn't hard-error.
  expect_true(is.data.frame(env$combined_map_data))
  expect_true(nrow(env$combined_map_data) > 0)
})

test_that("the Home-tab data-issue banner renders when there are load issues, and stays hidden when there aren't", {
  env <- load_app()
  shiny::testServer(env$server, {
    # req(FALSE) inside renderUI surfaces as a shiny.silent.error when the
    # output is accessed directly in testServer (rather than just quietly
    # returning NULL, as it would when rendered normally in a browser) --
    # that's the correct "nothing to show" signal here.
    expect_error(output$data_load_issues_banner, class = "shiny.silent.error")
  })

  broken_env <- load_app_with_corrupted_file("salarymap2.csv", function(path) {
    df <- read.csv(path)
    df$Teachers_Total_FTE <- NULL
    write.csv(df, path, row.names = FALSE)
  })
  shiny::testServer(broken_env$server, {
    expect_no_error(output$data_load_issues_banner)
    expect_false(is.null(output$data_load_issues_banner))
  })
})

test_that("Students_Per_Teacher is computed for both K-12 districts (vs. Teachers_Total_FTE) and HE institutions (vs. Faculty_Count)", {
  # Regression for the enrichment-data addition: Enrollment was already
  # fetched from CCD alongside Teachers_Total_FTE but never surfaced
  # anywhere -- Students_Per_Teacher is Enrollment / Teachers_Total_FTE,
  # zero additional scraping. HE gained its own real IPEDS fall-enrollment
  # pull 2026-08-06 (ipeds_enrollment_scraper.R), so both sides should now
  # have real, non-NA values (not just present-but-NA columns).
  env <- load_app()
  expect_true(all(c("Enrollment", "Students_Per_Teacher") %in% names(env$combined_map_data)))

  he <- env$combined_map_data[env$combined_map_data$Type == "Higher Ed Institution", ]
  skip_if(nrow(he) == 0, "no HE rows in committed data -- skipping")
  expect_true(any(!is.na(he$Students_Per_Teacher)))
  he_values <- he$Students_Per_Teacher[!is.na(he$Students_Per_Teacher)]
  # Same loose sanity range as the K-12 check below, not a tight real-
  # world one -- just catching a units/sign error, not asserting what a
  # plausible ratio looks like.
  expect_true(all(he_values > 0 & he_values < 100))

  k12_with_fte <- env$combined_map_data[env$combined_map_data$Type == "K-12 District" &
                                           !is.na(env$combined_map_data$Vacancy_Denominator) &
                                           env$combined_map_data$Vacancy_Denominator > 0, ]
  skip_if(nrow(k12_with_fte) == 0, "no K-12 rows with teacher FTE in committed data -- skipping")
  expect_true(any(!is.na(k12_with_fte$Students_Per_Teacher)))
  # Loose sanity range, not a tight real-world one -- most WY districts sit
  # roughly 10-16 students per teacher, but small/rural districts can be
  # real outliers on either side (confirmed: Weston County SD7 genuinely
  # reports ~36 in CCD's own data, 859 students against a reported 24
  # teacher FTE). This bound exists to catch a units/column mixup (e.g. a
  # negative value or one in the thousands), not to assert what a
  # plausible ratio looks like.
  real_values <- k12_with_fte$Students_Per_Teacher[!is.na(k12_with_fte$Students_Per_Teacher)]
  expect_true(all(real_values > 0 & real_values < 100))
})

test_that("Enrollment_Change_Pct is a real institution-level 5-year trend for HE, and NA (not missing) for K-12", {
  # Regression for the IPEDS enrollment-trend addition (2026-08-06):
  # distinct from Population_Change_Pct (county-level, real for K-12 via
  # the Census join, NA for HE) -- this is the institution-level mirror,
  # real for HE via ipeds_enrollment_scraper.R, NA for K-12 (no district-
  # level enrollment-trend pull exists). Both columns should coexist
  # without colliding.
  env <- load_app()
  expect_true("Enrollment_Change_Pct" %in% names(env$combined_map_data))

  he <- env$combined_map_data[env$combined_map_data$Type == "Higher Ed Institution", ]
  skip_if(nrow(he) == 0, "no HE rows in committed data -- skipping")
  expect_true(any(!is.na(he$Enrollment_Change_Pct)))
  # Loose sanity range -- a real 5-year FTE swing of more than 90% at any
  # WY public HE institution would be implausible and more likely a units
  # error than genuine enrollment collapse/boom.
  he_values <- he$Enrollment_Change_Pct[!is.na(he$Enrollment_Change_Pct)]
  expect_true(all(he_values > -0.9 & he_values < 0.9))
  # Not every institution moved the same direction (real data, not a
  # hardcoded sign) -- Central Wyoming College grew while most others
  # shrank over this window.
  expect_true(any(he_values > 0) && any(he_values < 0))

  k12 <- env$combined_map_data[env$combined_map_data$Type == "K-12 District", ]
  skip_if(nrow(k12) == 0, "no K-12 rows in committed data -- skipping")
  expect_true(all(is.na(k12$Enrollment_Change_Pct)))
})

test_that("Pell_Recipient_Share is real for HE (FSA) and NA (not missing) for K-12 (no SAIPE equivalent)", {
  # Regression for the FSA Pell Grant addition (2026-08-06): the HE
  # analogue of Child_Poverty_Rate (SAIPE, K-12-only) -- a different
  # federal program since SAIPE itself has no HE equivalent.
  env <- load_app()
  expect_true(all(c("Pell_Recipient_Share", "Pell_Year") %in% names(env$combined_map_data)))

  he <- env$combined_map_data[env$combined_map_data$Type == "Higher Ed Institution", ]
  skip_if(nrow(he) == 0, "no HE rows in committed data -- skipping")
  expect_true(any(!is.na(he$Pell_Recipient_Share)))
  he_values <- he$Pell_Recipient_Share[!is.na(he$Pell_Recipient_Share)]
  expect_true(all(he_values > 0 & he_values < 1))

  k12 <- env$combined_map_data[env$combined_map_data$Type == "K-12 District", ]
  skip_if(nrow(k12) == 0, "no K-12 rows in committed data -- skipping")
  expect_true(all(is.na(k12$Pell_Recipient_Share)))
})

test_that("county-level Census context is present for both K-12 and HE, and K-12 sibling districts share values", {
  # Regression for the Census ACS county-context enrichment: real per-
  # county figures on both K-12 and HE rows (HE joined via salarymap.csv's
  # County column, added 2026-08-06), and two K-12 districts in the same
  # county (e.g. Weston County SD1 and SD7) must show the EXACT same
  # county-level figures, not two different ones -- this is a county-level
  # join, not a district-level one. No equivalent sibling check for HE:
  # Wyoming's 9 public HE institutions each sit in a different county, so
  # there's no case of two HE rows legitimately sharing one county's figures.
  env <- load_app()
  census_cols <- c("Median_Household_Income", "Median_Gross_Rent", "Mining_Employment_Share", "Population_Change_Pct")
  expect_true(all(census_cols %in% names(env$combined_map_data)))

  he <- env$combined_map_data[env$combined_map_data$Type == "Higher Ed Institution", ]
  skip_if(nrow(he) == 0, "no HE rows in committed data -- skipping")
  expect_true(any(!is.na(he$Median_Household_Income)))
  # No two HE institutions share a county, so no two HE rows should share
  # an identical (County, Median_Household_Income) figure by coincidence.
  he_with_income <- he[!is.na(he$Median_Household_Income), ]
  expect_equal(length(unique(he_with_income$County)), nrow(he_with_income))

  k12 <- env$combined_map_data[env$combined_map_data$Type == "K-12 District", ]
  skip_if(nrow(k12) == 0, "no K-12 rows in committed data -- skipping")
  expect_true(any(!is.na(k12$Median_Household_Income)))

  siblings <- k12[k12$County == k12$County[which(!is.na(k12$Median_Household_Income))[1]] &
                    !is.na(k12$Median_Household_Income), ]
  skip_if(nrow(siblings) < 2, "no county with 2+ mapped K-12 districts in committed data -- skipping")
  expect_equal(length(unique(siblings$Median_Household_Income)), 1)
  expect_equal(length(unique(siblings$Mining_Employment_Share)), 1)
})

test_that("Child_Poverty_Rate is district-level (unlike the county-level Census columns), so sibling districts can genuinely differ", {
  # Regression for the SAIPE addition: unlike Median_Household_Income etc.
  # (real county-level joins, correctly identical for sibling districts),
  # Child_Poverty_Rate is a real DISTRICT-level figure and two districts in
  # the same county are NOT expected to share it -- confirmed with real
  # data (Fremont County SD2 and SD21 are both Fremont County but have very
  # different real child poverty rates). Also present (NA) on HE rows.
  env <- load_app()
  expect_true("Child_Poverty_Rate" %in% names(env$combined_map_data))

  he <- env$combined_map_data[env$combined_map_data$Type == "Higher Ed Institution", ]
  skip_if(nrow(he) == 0, "no HE rows in committed data -- skipping")
  expect_true(all(is.na(he$Child_Poverty_Rate)))

  k12 <- env$combined_map_data[env$combined_map_data$Type == "K-12 District", ]
  k12_with_rate <- k12[!is.na(k12$Child_Poverty_Rate), ]
  skip_if(nrow(k12_with_rate) == 0, "no K-12 rows with a child poverty rate in committed data -- skipping")
  expect_true(all(k12_with_rate$Child_Poverty_Rate >= 0 & k12_with_rate$Child_Poverty_Rate <= 1))

  fremont <- k12_with_rate[k12_with_rate$County == "Fremont County, Wyoming", ]
  skip_if(nrow(fremont) < 2, "fewer than 2 mapped Fremont County districts with a rate in committed data -- skipping")
  expect_true(length(unique(fremont$Child_Poverty_Rate)) > 1)
})

test_that("HE Vacancy_Numerator counts only full-time faculty postings, not adjunct/part-time ones", {
  # Regression: he_faculty_current_counts used to count every current HE
  # posting (both Job_Types) into the vacancy-rate numerator, but
  # Faculty_Count (the denominator) is IPEDS's full-time-only figure --
  # mixing in adjunct/part-time postings overstated the rate. Confirmed
  # live impact before the fix: Laramie County Community College's
  # numerator dropped from 50 (all types) to 3 (full-time only), Sheridan
  # College's from 41 to 1.
  env <- load_app()
  facultydata <- read.csv(here::here("Wy_Ed_Jobs", "facultydata.csv"), fileEncoding = "UTF-8")
  facultydata <- facultydata[facultydata$Job_Type %in%
    c("Instructor/Teacher/Faculty", "Adjunct/Part-Time Faculty"), ]
  latest <- max(as.Date(facultydata$Archive_Date))
  latest_rows <- facultydata[as.Date(facultydata$Archive_Date) == latest, ]

  institutions <- unique(latest_rows$Institution)
  ft_only_count <- function(inst) sum(latest_rows$Institution == inst &
                                         latest_rows$Job_Type == "Instructor/Teacher/Faculty")
  all_type_count <- function(inst) sum(latest_rows$Institution == inst)

  # Real, committed data: at least one institution must have more adjunct
  # postings than full-time ones for this test to actually exercise the
  # fix, not just trivially pass because the two counts always agreed.
  differing <- Filter(function(inst) ft_only_count(inst) != all_type_count(inst), institutions)
  skip_if(length(differing) == 0,
          "no institution in committed data currently has both full-time and adjunct postings -- skipping")

  he <- env$combined_map_data[env$combined_map_data$Type == "Higher Ed Institution", ]
  # Sheridan/Gillette share a joint vacancy rate (Vacancy_Rate_Shared) --
  # their numerator is a joint sum, not each institution's own count, so
  # they're checked separately rather than against their individual counts.
  shared_names <- he$Name[he$Vacancy_Rate_Shared %in% TRUE]
  standalone_differing <- setdiff(differing, shared_names)
  for (inst in standalone_differing) {
    row <- he[he$Name == inst, ]
    skip_if(nrow(row) == 0, paste(inst, "not present in committed map data -- skipping"))
    expect_equal(row$Vacancy_Numerator, ft_only_count(inst), info = paste("institution:", inst))
  }

  shared_differing <- intersect(differing, shared_names)
  if (length(shared_differing) > 0) {
    expected_joint <- sum(vapply(shared_names, ft_only_count, numeric(1)))
    shared_rows <- he[he$Name %in% shared_names, ]
    expect_true(all(shared_rows$Vacancy_Numerator == expected_joint))
  }
})

test_that("K-12 and HE comparator reactives and presets execute cleanly", {
  env <- load_app()
  shiny::testServer(env$server, {
    # Initial state tests
    session$setInputs(k12_compare_districts = c("Laramie County School District 1", "Natrona School District 1"))
    expect_no_error(output$k12_compare_cards)
    expect_no_error(output$k12_compare_salary_plot)
    expect_no_error(output$k12_compare_metrics_plot)
    expect_no_error(output$k12_compare_jobs_table)

    # Empty state handling
    session$setInputs(k12_compare_districts = character(0))
    expect_no_error(output$k12_compare_cards)
    expect_no_error(output$k12_compare_salary_plot)
    expect_no_error(output$k12_compare_metrics_plot)
    expect_no_error(output$k12_compare_jobs_table)

    # Preset button triggers
    session$setInputs(k12_comp_preset_top = 1)
    session$setInputs(k12_comp_preset_largest = 1)
    session$setInputs(k12_comp_preset_clear = 1)

    # HE comparator
    session$setInputs(he_compare_insts = c("University of Wyoming", "Laramie County Community College"))
    expect_no_error(output$he_compare_cards)
    expect_no_error(output$he_compare_salary_plot)
    expect_no_error(output$he_compare_openings_plot)
    expect_no_error(output$he_compare_jobs_table)

    session$setInputs(he_compare_insts = character(0))
    expect_no_error(output$he_compare_cards)
    expect_no_error(output$he_compare_salary_plot)
    expect_no_error(output$he_compare_openings_plot)
    expect_no_error(output$he_compare_jobs_table)

    session$setInputs(he_comp_preset_top = 1)
    session$setInputs(he_comp_preset_clear = 1)
  })
})

test_that("Map multi-dimensional filters and reset reactive controls function without error", {
  env <- load_app()
  shiny::testServer(env$server, {
    # Default state
    session$setInputs(
      map_types = c("K-12 District", "Higher Ed Institution"),
      map_county = "all",
      map_salary_tier = "all",
      map_urgency = "all"
    )
    initial_df <- map_filtered()
    expect_true(nrow(initial_df) > 0)
    expect_true(all(initial_df$CurrentCount > 0))
    expect_no_error(output$map_filter_count)

    # Employer Type filter: K-12 only
    session$setInputs(map_types = c("K-12 District"))
    k12_only <- map_filtered()
    expect_true(nrow(k12_only) > 0)
    expect_true(all(k12_only$Type == "K-12 District"))

    # Employer Type filter: Higher Ed only
    session$setInputs(map_types = c("Higher Ed Institution"))
    he_only <- map_filtered()
    expect_true(nrow(he_only) > 0)
    expect_true(all(he_only$Type == "Higher Ed Institution"))

    # County filter: Albany County
    albany_county <- grep("Albany", env$map_county_choices, value = TRUE)[1]
    session$setInputs(
      map_types = c("K-12 District", "Higher Ed Institution"),
      map_county = albany_county
    )
    albany_df <- map_filtered()
    expect_true(nrow(albany_df) > 0)
    expect_true(all(albany_df$County == albany_county))

    # Pay bands: each dot keeps only its postings in the band (compared in
    # their own unit), and its count is exactly those postings.
    for (band in names(env$PAY_BANDS)) {
      session$setInputs(map_county = "all", map_salary_tier = band)
      b <- map_filtered()
      expect_true(all(b$SearchActive & b$DisplayCount > 0 & b$DisplayCount <= b$CurrentCount), info = band)
      k12_dots <- b[b$Type == "K-12 District", ]
      in_band <- env$combineddata[env$in_pay_band(env$combineddata$.Pay_Value, env$combineddata$.Pay_Unit, band), ]
      expect_equal(sum(k12_dots$DisplayCount), sum(in_band$District %in% k12_dots$Name), info = band)
    }
    # The low annual band reaches paraprofessional postings -- the old filter's
    # one teacher figure per district never put them there.
    low_annual <- names(env$PAY_BANDS)[vapply(env$PAY_BANDS, function(b) b$unit == "year" && b$lo == 0, logical(1))]
    low <- env$combineddata[env$in_pay_band(env$combineddata$.Pay_Value, env$combineddata$.Pay_Unit, low_annual), ]
    expect_gt(sum(low$Position == "Paraprofessional"), 0)

    # Posting volume filters
    session$setInputs(map_salary_tier = "all", map_urgency = "5_plus")
    v5 <- map_filtered()
    expect_true(nrow(v5) > 0)
    expect_true(all(v5$CurrentCount >= 5))

    session$setInputs(map_urgency = "2_plus")
    v2 <- map_filtered()
    expect_true(nrow(v2) > 0)
    expect_true(all(v2$CurrentCount >= 2))

    session$setInputs(map_urgency = "has_new")
    v_new <- map_filtered()
    expect_true(nrow(v_new) > 0)
    expect_true(all(v_new$WeeklyNew > 0))

    # Empty match state
    session$setInputs(map_types = character(0))
    empty_df <- map_filtered()
    expect_equal(nrow(empty_df), 0)
    expect_no_error(output$map_filter_count)

    # Reset trigger
    session$setInputs(map_filter_reset = 1)

    # Keyword / Position / Subject search on map
    session$setInputs(
      map_types = c("K-12 District", "Higher Ed Institution"),
      map_county = "all",
      map_salary_tier = "all",
      map_urgency = "all",
      map_search_keyword = "special ed"
    )
    sped_df <- map_filtered()
    expect_true(nrow(sped_df) > 0)
    expect_true(all(sped_df$MatchingCount > 0))
    expect_true(all(sped_df$SearchActive))
    expect_equal(sped_df$SearchQuery[1], "special ed")
    expect_no_error(output$map_filter_count)

    # Search for mathematics
    session$setInputs(map_search_keyword = "mathematics")
    math_df <- map_filtered()
    expect_true(nrow(math_df) > 0)
    expect_true(all(math_df$MatchingCount > 0))
    expect_true(all(math_df$SearchActive))
    expect_no_error(output$map_filter_count)

    # Search suggestion chip triggers execute cleanly
    expect_no_error(session$setInputs(map_chip_math = 1))
    expect_no_error(session$setInputs(map_chip_sped = 1))
    expect_no_error(session$setInputs(map_chip_clear = 1))

    # Zero match search query
    session$setInputs(map_search_keyword = "astronaut nonesuch 999xyz")
    zero_match_df <- map_filtered()
    expect_equal(nrow(zero_match_df), 0)
    expect_no_error(output$map_filter_count)

    # Clear query restores all
    session$setInputs(map_search_keyword = "")
    restored_df <- map_filtered()
    expect_equal(nrow(restored_df), nrow(initial_df))
    expect_false(restored_df$SearchActive[1])
  })
})

test_that("K-12 and HE quick filter buttons and new postings tables evaluate cleanly", {
  env <- load_app()
  shiny::testServer(env$server, {
    # Default: All Active Postings
    session$setInputs(k12_quick_filter = "all")
    expect_no_error(output$k12_jobs)

    # Filter to New This Week
    session$setInputs(k12_quick_filter = "new")
    expect_no_error(output$k12_jobs)

    # HE Default: All Active Postings
    session$setInputs(he_quick_filter = "all", he_table_appointment = "all")
    expect_no_error(output$he_jobs)

    # HE Filter to New This Week
    session$setInputs(he_quick_filter = "new")
    expect_no_error(output$he_jobs)

    # Dedicated New This Week tables
    expect_no_error(output$k12_new_table)
    session$setInputs(he_new_appointment = "all")
    expect_no_error(output$he_new_table)
  })
})

test_that("map popup 'View matching jobs' carries the map search to the Jobs table", {
  env <- load_app()
  query <- "special ed"
  searchable <- paste(dplyr::coalesce(env$combineddata$Title, ""), dplyr::coalesce(env$combineddata$Position, ""))
  hits <- env$combineddata[env$match_job_query(searchable, query), , drop = FALSE]
  skip_if(nrow(hits) == 0, "no current postings match the test query")
  district <- names(sort(table(hits$District), decreasing = TRUE))[1]
  expected <- sum(hits$District == district)

  shiny::testServer(env$server, {
    session$setInputs(map_popup_view_jobs = list(entity = district, query = utils::URLencode(query, reserved = TRUE)))
    expect_equal(selected_district(), district)
    expect_equal(selected_k12_query(), query)
    df <- env$combineddata[env$combineddata$District == district, , drop = FALSE]
    shown <- df[env$match_job_query(paste(dplyr::coalesce(df$Title, ""), dplyr::coalesce(df$Position, "")), selected_k12_query()), ]
    expect_equal(nrow(shown), expected)

    # Any other route to the Jobs tab (home-page employer links) drops the map search.
    session$setInputs(popup_view_jobs = district)
    expect_null(selected_k12_query())
  })
})

test_that("weekly-totals downloads keep only the latest snapshot in each week", {
  env <- load_app()
  weekly <- data.frame(District = c("A", "B", "A", "B", "A"),
                       Archive_Date = c("2026-08-03", "2026-08-03", "2026-08-07", "2026-08-07", "2026-08-11"),
                       n = c(5, 2, 6, 3, 7))
  out <- env$latest_snapshot_per_week(weekly)
  expect_equal(out$Archive_Date, c("2026-08-07", "2026-08-07", "2026-08-11"))
  expect_equal(out$n, c(6, 3, 7))
})
