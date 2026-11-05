# Tier-2 data-quality invariant sweep -- ported from the Montana dashboard
# (PR #5), itself modelled on the LASSO project's tests/invariants.R. Runs
# against the *committed* pipeline-output CSVs (the same files app.R reads),
# and asserts properties that must hold no matter what the postings/salaries
# happen to be this week. Each block names the real failure it guards.
#
# These are cheap (plain CSV reads) and are also re-run in weekly CI right
# after verify_schema.R, as a blocking gate before the fresh data is
# committed: a broken classifier, a bad join, or a units error in a scraper
# shows up here as a hard failure instead of shipping to the live dashboard.
#
# Not duplicated here (already covered elsewhere):
#   - Total == sum of parts                -> test-data-integrity.R
#   - title encoding / mojibake            -> test-data-integrity.R
#     + test-k12-title-mojibake-repair.R
#   - schema / expected columns            -> test-schema-check.R
#   - reactive semantics, double-count     -> test-app-reactives.R
#   - incremental append == archive rebuild -> test-history-accumulator.R

suppressMessages({
  library(dplyr)
})

WY <- function(name) here::here("Wy_Ed_Jobs", name)
read_wy <- function(name, ...) {
  p <- WY(name)
  skip_if_not(file.exists(p), paste(name, "not found"))
  df <- utils::read.csv(p, stringsAsFactors = FALSE, ...)
  df[, setdiff(names(df), "X"), drop = FALSE]   # drop write.csv row-index col
}

numeric_cells <- function(df) {
  num <- df[vapply(df, is.numeric, logical(1))]
  unlist(num, use.names = FALSE)
}

in_band <- function(x, lo, hi) all(is.na(x) | (x >= lo & x <= hi))

ALL_OUTPUT_CSVS <- c(
  "combinedclean.csv", "k12jobanalysis.csv", "allsum.csv", "allnow.csv",
  "allsum_he.csv", "allnow_he.csv", "k12_district_weekly_totals.csv",
  "he_institution_weekly_totals.csv", "salarymap2.csv", "salarymap.csv",
  "facultydata.csv", "job_duration_spells.csv", "he_job_duration_spells.csv",
  "data_dictionary.csv"
)


# ---------------------------------------------------------------------------
# 1. No NaN / Inf anywhere. A units error, a divide-by-zero vacancy rate, or
#    a bad join leaks these into a rendered table/plot cell.
# ---------------------------------------------------------------------------
test_that("no committed CSV has a NaN or Inf in a numeric cell", {
  for (f in ALL_OUTPUT_CSVS) {
    v <- numeric_cells(read_wy(f))
    bad <- v[is.nan(v) | is.infinite(v)]
    expect_equal(length(bad), 0,
                 info = sprintf("%s has %d NaN/Inf numeric cell(s)", f, length(bad)))
  }
})

test_that("no committed CSV has a literal 'NaN' / 'Inf' / 'NA%' string in a character cell", {
  for (f in ALL_OUTPUT_CSVS) {
    df <- read_wy(f)
    chr <- df[vapply(df, is.character, logical(1))]
    hits <- vapply(chr, function(col) any(trimws(col) %in% c("NaN", "Inf", "-Inf", "NA%")), logical(1))
    expect_false(any(hits), info = sprintf("%s: string NaN/Inf in column(s) %s",
                                           f, paste(names(hits)[hits], collapse = ", ")))
  }
})


# ---------------------------------------------------------------------------
# 2. Range and sign -- a value out of these bounds is a units error or a bad
#    join, not real data.
# ---------------------------------------------------------------------------
test_that("weekly posting counts are non-negative integers", {
  for (f in c("k12_district_weekly_totals.csv", "he_institution_weekly_totals.csv")) {
    n <- read_wy(f)$n
    expect_true(all(n >= 0 & n == as.integer(n)), info = f)
  }
  for (f in c("allsum.csv", "allsum_he.csv")) {
    s <- read_wy(f)$sum
    expect_true(all(s >= 0), info = f)
  }
  for (f in c("allnow.csv", "allnow_he.csv")) {
    s <- read_wy(f)$Sum
    expect_true(all(s >= 0), info = f)
  }
})

test_that("salarymap2.csv (K-12): salary / staffing / context columns are in a plausible band", {
  d <- read_wy("salarymap2.csv")

  expect_true(in_band(d$Teacher_Base_Salary, 20000, 150000), info = "Teacher_Base_Salary")
  expect_true(in_band(d$Teacher_Base_Salary_Prior_Year, 20000, 150000), info = "Teacher_Base_Salary_Prior_Year")
  if ("Teacher_Avg_Salary" %in% names(d)) {
    expect_true(in_band(d$Teacher_Avg_Salary, 30000, 150000), info = "Teacher_Avg_Salary")
  }
  expect_true(in_band(d$Superintendent_Salary, 50000, 400000), info = "Superintendent_Salary")
  expect_true(in_band(d$Superintendent_Contract_Days, 180, 366), info = "Superintendent_Contract_Days")

  expect_true(in_band(d$Teachers_Total_FTE, 0, 6000), info = "Teachers_Total_FTE")
  expect_true(in_band(d$Enrollment, 0, 60000), info = "Enrollment")

  expect_true(in_band(d$Median_Household_Income, 20000, 250000), info = "Median_Household_Income")
  expect_true(in_band(d$Median_Gross_Rent, 200, 5000), info = "Median_Gross_Rent")
  expect_true(in_band(d$Population, 0, 700000), info = "Population (largest WY county ~ Laramie, 100k)")
})

test_that("salarymap.csv (HE): faculty salary / count columns are in a plausible band", {
  d <- read_wy("salarymap.csv")
  expect_true(in_band(d$Faculty_Avg_Salary, 15000, 250000), info = "Faculty_Avg_Salary")
  expect_true(in_band(d$Faculty_Avg_Salary_Professor, 20000, 300000), info = "Faculty_Avg_Salary_Professor")
  expect_true(in_band(d$Faculty_Count, 0, 5000), info = "Faculty_Count")
  expect_true(in_band(d$Enrollment, 0, 60000), info = "Enrollment")
})

test_that("percentage / rate columns are stored as proportions in [0, 1] (not 0-100)", {
  # These reach app.R as proportions and get scales::percent()'d there --
  # a value > 1 means the pipeline already multiplied by 100, which would
  # render as e.g. "8500%".
  d2 <- read_wy("salarymap2.csv")
  for (col in c("Mining_Employment_Share", "Child_Poverty_Rate")) {
    expect_true(in_band(d2[[col]], 0, 1), info = paste("salarymap2", col))
  }
  # a WY county gaining/losing >50% of its population in 5yr is a data error
  expect_true(all(is.na(d2$Population_Change_Pct) | abs(d2$Population_Change_Pct) <= 0.5),
              info = "salarymap2 Population_Change_Pct")

  dh <- read_wy("salarymap.csv")
  for (col in c("Mining_Employment_Share", "Pell_Recipient_Share")) {
    expect_true(in_band(dh[[col]], 0, 1), info = paste("salarymap", col))
  }
  expect_true(all(is.na(dh$Enrollment_Change_Pct) | abs(dh$Enrollment_Change_Pct) <= 1),
              info = "salarymap Enrollment_Change_Pct")
  expect_true(all(is.na(dh$Population_Change_Pct) | abs(dh$Population_Change_Pct) <= 0.5),
              info = "salarymap Population_Change_Pct")
})

test_that("Latitude / Longitude put every mapped entity inside Wyoming's bounding box", {
  # WY is a near-perfect rectangle: 41.0-45.0 N, 104.05-111.05 W. A marker
  # outside it is a bad geocode or a lat/long swap.
  for (f in c("salarymap2.csv", "salarymap.csv")) {
    d <- read_wy(f)
    lat <- d$Latitude; lon <- d$Longitude
    ok <- is.na(lat) | is.na(lon) | (lat >= 40.9 & lat <= 45.1 & lon >= -111.1 & lon <= -104.0)
    expect_true(all(ok), info = sprintf("%s: %d entity/entities outside the WY bbox",
                                        f, sum(!ok)))
  }
})


# ---------------------------------------------------------------------------
# 3. Referential integrity -- the joins app.R and the pipeline rely on.
# ---------------------------------------------------------------------------
test_that("combinedclean.csv has no byte-identical duplicate rows and posting_id is unique", {
  # Regression guard for the within-run duplicate-row inflation fixed in
  # Wy_ED_Jobs.Rmd (`combined %>% distinct()`) + scripts/
  # repair_combinedclean_row_duplicates.R: a modern direct-platform page
  # that lists one opening once per building emitted N identical rows, which
  # k12_district_weekly_totals.csv (a raw count()) and app.R's teacher
  # vacancy-rate numerator both over-counted.
  cc <- read_wy("combinedclean.csv")
  expect_equal(sum(duplicated(cc)), 0, info = "byte-identical duplicate rows in combinedclean.csv")
  dup_ids <- unique(cc$posting_id[duplicated(cc$posting_id)])
  expect_equal(length(dup_ids), 0, info = paste0(
    "duplicate posting_id in combinedclean.csv: ",
    paste(utils::head(dup_ids, 5), collapse = " | ")))
})

test_that("posting_id holds no control characters", {
  # A "\r" separator in fallback IDs (until 2026-11) put line breaks inside
  # the posting_id field of the research downloads.
  for (f in c("combinedclean.csv", "k12jobanalysis.csv")) {
    ids <- read_wy(f)$posting_id
    expect_equal(sum(grepl("[[:cntrl:]]", ids)), 0, info = f)
  }
})

test_that("the newest week's k12jobanalysis.csv / facultydata.csv have no byte-identical duplicate rows", {
  # The historical accumulated files still carry a known residue of
  # duplicate rows from older snapshots where the SchoolSpring scraper
  # dropped the per-building `position` value (tracked for a later
  # historical-dedup pass -- see repair_combinedclean_row_duplicates.R's
  # KNOWN LIMITATION note). The enforceable invariant is that a fresh
  # pipeline run does not add new ones.
  for (f in c("k12jobanalysis.csv", "facultydata.csv")) {
    d <- read_wy(f)
    latest <- d[d$Archive_Date == max(d$Archive_Date), , drop = FALSE]
    expect_equal(sum(duplicated(latest)), 0,
                 info = sprintf("%s: byte-identical duplicate rows in the newest week", f))
  }
})

test_that("every district in salarymap2.csv is a known WY K-12 entity", {
  # salarymap2 districts must join to a real scraped entity. WY keeps K-12
  # identity in four places: the platform registry, the heuristic misc
  # registry, WSBA-only orgs, and the Rmd's standalone charter-school block.
  skip_if_not(file.exists(here::here("misc_district_scrapers.R")))
  reg <- utils::read.csv(here::here("k12_district_registry.csv"), stringsAsFactors = FALSE)
  # misc_district_registry + WSBA_ONLY_ORGS are sourced by helper-setup.R
  known <- unique(c(
    reg$District,
    get0("misc_district_registry", ifnotfound = data.frame(District = character()))$District,
    get0("WSBA_ONLY_ORGS", ifnotfound = character()),
    "Laramie Montessori Charter School"  # Rmd charter-school block, own Paylocity source
  ))
  orphans <- setdiff(unique(read_wy("salarymap2.csv")$District), known)
  expect_equal(length(orphans), 0,
               info = paste("salarymap2 districts not in any registry:",
                            paste(orphans, collapse = "; ")))
})

test_that("this week's district totals tie out to combinedclean.csv's distinct rows", {
  cc <- read_wy("combinedclean.csv")
  wt <- read_wy("k12_district_weekly_totals.csv")
  latest <- max(as.Date(wt$Archive_Date))
  this_week <- wt %>% filter(as.Date(Archive_Date) == latest)

  from_cc <- cc %>%
    filter(District %in% this_week$District) %>%
    group_by(District) %>%
    summarize(n_cc = n(), .groups = "drop")

  merged <- this_week %>%
    select(District, n_wt = n) %>%
    full_join(from_cc, by = "District")
  # combinedclean.csv is "this week only" and now byte-deduplicated, so its
  # per-district row count must equal this week's k12_district_weekly_totals.
  mismatch <- merged %>% filter(is.na(n_wt) | is.na(n_cc) | n_wt != n_cc)
  expect_equal(nrow(mismatch), 0,
               info = paste(utils::capture.output(print(as.data.frame(mismatch))), collapse = "\n"))
})


# ---------------------------------------------------------------------------
# 4. Enum coverage -- a value app.R's switch/recode/ifelse logic doesn't
#    know about renders blank or falls through a case.
# ---------------------------------------------------------------------------
test_that("Data_Coverage is one of the documented values", {
  dc <- read_wy("salarymap2.csv")$Data_Coverage
  ok <- c("Full", "Partial (WSBA + own page)", "Partial (WSBA only)")
  expect_true(all(is.na(dc) | dc %in% ok),
              info = paste("unexpected Data_Coverage:",
                           paste(setdiff(unique(dc), c(NA, ok)), collapse = ", ")))
})

test_that("facultydata.csv / allsum_he.csv / allnow_he.csv are scoped to the two faculty Job_Type buckets", {
  # All three are filtered to these two upstream (see
  # rebuild_current_he_aggregates() and app.R's he_history). A third value
  # here means the filter regressed and the HE trend/summary counts now
  # include staff/coach/admin postings.
  faculty <- c("Instructor/Teacher/Faculty", "Adjunct/Part-Time Faculty")
  for (f in c("facultydata.csv", "allsum_he.csv", "allnow_he.csv")) {
    jt <- unique(read_wy(f)$Job_Type)
    jt <- jt[!is.na(jt)]
    expect_true(all(jt %in% faculty),
                info = sprintf("%s: unexpected Job_Type %s", f,
                               paste(setdiff(jt, faculty), collapse = ", ")))
  }
})

test_that("k12jobanalysis.csv holds only Teacher rows", {
  pos <- unique(read_wy("k12jobanalysis.csv")$position)
  expect_identical(pos, "Teacher", info = paste("unexpected position:", paste(pos, collapse = ", ")))
})

# Classification safeguards (claude-notes 2026-11-01 proposal), items 7 and 8:
# every committed bucket equals what today's classifier gives its title. A
# classifier change must ship with its reclassified data; otherwise history
# silently mixes old and new buckets (the 2026-11-01 audit found ~400 misrouted
# titles that no check caught). Also catches a K-12 position value the
# classifier can't produce (item 7). Fix by reclassifying the committed files.
classification_mismatches <- function(got, want, title) {
  same <- (is.na(got) & is.na(want)) | (!is.na(got) & !is.na(want) & got == want)
  bad <- !same
  if (!any(bad)) return("")
  x <- unique(data.frame(title = title[bad], committed = got[bad], classifier = want[bad]))
  paste(sum(bad), "rows differ, e.g.:", paste(utils::head(sprintf("'%s': %s -> %s", x$title, x$committed, x$classifier), 8),
                                         collapse = " | "))
}

test_that("committed K-12 position / Category / Broad_Category equal the current classifier", {
  cc <- read_wy("combinedclean.csv")
  k <- read_wy("k12jobanalysis.csv")
  expect_identical(classification_mismatches(cc$position, classify_k12_position(cc$title), cc$title), "",
                   label = "combinedclean.csv position")
  expect_identical(classification_mismatches(k$position, classify_k12_position(k$title), k$title), "",
                   label = "k12jobanalysis.csv position")
  expect_identical(classification_mismatches(k$Category, classify_k12_subject(k$title), k$title), "",
                   label = "k12jobanalysis.csv Category")
  expect_identical(classification_mismatches(k$Broad_Category, classify_k12_broad_category(k$Category), k$title), "",
                   label = "k12jobanalysis.csv Broad_Category")
})

test_that("committed HE Job_Type / Category equal the current classifier", {
  fd <- read_wy("facultydata.csv")
  expect_identical(classification_mismatches(fd$Job_Type, classify_he_job_type(fd$Title), fd$Title), "",
                   label = "facultydata.csv Job_Type")
  expect_identical(classification_mismatches(fd$Category, classify_he_faculty_category(fd$Title), fd$Title), "",
                   label = "facultydata.csv Category")
})

test_that("no placeholder board entry ships in any posting-level output (item 10)", {
  titles <- list(combinedclean = read_wy("combinedclean.csv")$title,
                 k12jobanalysis = read_wy("k12jobanalysis.csv")$title,
                 facultydata = read_wy("facultydata.csv")$Title)
  if (file.exists(WY("hedata.xlsx"))) titles$hedata <- readxl::read_excel(WY("hedata.xlsx"))$Title
  for (n in names(titles)) {
    hit <- unique(titles[[n]][is_placeholder_posting(titles[[n]])])
    expect(length(hit) == 0, paste(n, "ships placeholder titles:", paste(head(hit, 5), collapse = " | ")))
  }
})

test_that("duration-spell dummies agree with the position / job type they encode (item 10)", {
  # The 2026-11-01 audit found is_support_staff never set for Transportation
  # and Food Services.
  s <- read_wy("job_duration_spells.csv")
  support <- c("Support Services", "Paraprofessional", "Custodial/Maintenance", "Transportation", "Food Services")
  expect_equal(s$is_support_staff, as.integer(s$Position %in% support))
  expect_equal(s$is_admin, as.integer(s$Position == "Administration"))
  h <- read_wy("he_job_duration_spells.csv")
  expect_equal(h$is_faculty, as.integer(h$Job_Type == "Instructor/Teacher/Faculty"))
  expect_equal(h$is_adjunct, as.integer(h$Job_Type == "Adjunct/Part-Time Faculty"))
})

test_that("every Broad_Category / Category in the summary data has a colour AND collapse-map entry in app.R", {
  skip_if_not_installed("shiny")
  app_dir <- here::here("Wy_Ed_Jobs")
  skip_if_not(dir.exists(app_dir))
  old <- setwd(app_dir); on.exit(setwd(old), add = TRUE)
  env <- new.env()
  suppressMessages(suppressWarnings(sys.source("app.R", envir = env)))

  k12_detail <- setdiff(unique(env$k12sum$Broad_Category), NA)
  expect_true(all(k12_detail %in% names(env$K12_CATEGORY_COLORS_DETAIL)),
              info = paste("K-12 detail category with no colour:",
                           paste(setdiff(k12_detail, names(env$K12_CATEGORY_COLORS_DETAIL)), collapse = ", ")))
  k12_agg <- setdiff(unique(env$k12sum_agg$Broad_Category), NA)
  expect_true(all(k12_agg %in% names(env$K12_CATEGORY_COLORS_AGG)),
              info = paste("K-12 agg category with no colour:",
                           paste(setdiff(k12_agg, names(env$K12_CATEGORY_COLORS_AGG)), collapse = ", ")))

  he_detail <- setdiff(as.character(unique(env$hesum_he$Category)), NA)
  expect_true(all(he_detail %in% names(env$HE_CATEGORY_COLORS_DETAIL)),
              info = paste("HE detail category with no colour:",
                           paste(setdiff(he_detail, names(env$HE_CATEGORY_COLORS_DETAIL)), collapse = ", ")))
  he_agg <- setdiff(as.character(unique(env$hesum_he_agg$Category)), NA)
  expect_true(all(he_agg %in% names(env$HE_CATEGORY_COLORS_AGG)),
              info = paste("HE agg category with no colour:",
                           paste(setdiff(he_agg, names(env$HE_CATEGORY_COLORS_AGG)), collapse = ", ")))
})


# ---------------------------------------------------------------------------
# 5. Temporal sanity.
# ---------------------------------------------------------------------------
test_that("every Archive_Date parses and none is in the future", {
  for (f in c("combinedclean.csv", "k12jobanalysis.csv", "allsum.csv", "allsum_he.csv",
              "k12_district_weekly_totals.csv", "he_institution_weekly_totals.csv",
              "facultydata.csv")) {
    d <- read_wy(f)
    ad <- suppressWarnings(as.Date(d$Archive_Date))
    expect_false(any(is.na(ad)), info = paste(f, "has an unparseable Archive_Date"))
    expect_false(any(ad > Sys.Date() + 1), info = paste(f, "has a future Archive_Date"))
  }
})

test_that("the accumulated history only ever grows -- the newest week is not below half the prior week", {
  # A soft in-suite version of .github/scripts/sanity_check.R, so a local
  # run flags a systemic scrape failure too.
  wt <- read_wy("k12_district_weekly_totals.csv") %>%
    group_by(Archive_Date) %>% summarize(total = sum(n), .groups = "drop") %>%
    arrange(Archive_Date)
  skip_if(nrow(wt) < 2, "need >= 2 weeks of history")
  newest <- tail(wt$total, 1); prior <- tail(wt$total, 2)[1]
  expect_gt(newest, prior * 0.5)
})


# ---------------------------------------------------------------------------
# 6. Posted pay and pay estimates. Posted pay is read out of free text, so a
#    fee, a benefit's value or a units slip can look like a wage; the
#    estimates come from re-downloaded reference tables (BLS, IPEDS, UW).
# ---------------------------------------------------------------------------
read_hedata_wy <- function() {
  p <- here::here("Wy_Ed_Jobs", "hedata.xlsx")
  skip_if_not(file.exists(p), "hedata.xlsx not found")
  as.data.frame(readxl::read_xlsx(p))
}

test_that("no posted pay outside PAY_PLAUSIBLE_RANGE for its unit ships (drop_implausible_pay ran)", {
  for (d in list(K12 = read_wy("combinedclean.csv"), HE = read_hedata_wy())) {
    if (!"Pay_Min" %in% names(d)) next   # HE files before the first run with pay
    bad <- is_implausible_pay(d$Pay_Min, d$Pay_Max, d$Pay_Unit)
    expect_equal(sum(bad), 0, info = paste("implausible pay:", paste(head(d$Pay_Text[bad], 3), collapse = " | ")))
  }
})

test_that("every posted amount keeps the snippet it came from, and a unit never stands alone", {
  for (d in list(K12 = read_wy("combinedclean.csv"), HE = read_hedata_wy())) {
    if (!"Pay_Min" %in% names(d)) next
    has_pay <- !is.na(d$Pay_Min)
    expect_true(all(!is.na(d$Pay_Text[has_pay]) & nzchar(d$Pay_Text[has_pay])))
    expect_true(all(is.na(d$Pay_Unit[!has_pay])))
    expect_true(all(is.na(d$Pay_Max) == is.na(d$Pay_Min)))
  }
})

test_that("pay estimates are hourly or annual, in range, and say what they estimate", {
  for (d in list(K12 = read_wy("combinedclean.csv"), HE = read_hedata_wy())) {
    if (!"Est_Pay" %in% names(d)) next
    has <- !is.na(d$Est_Pay)
    expect_true(all(d$Est_Pay_Unit[has] %in% c("hour", "year")))
    expect_true(all(!is.na(d$Est_Occupation[has])))
    hourly <- has & d$Est_Pay_Unit %in% "hour"; annual <- has & d$Est_Pay_Unit %in% "year"
    # An hourly median in the annual column (or the reverse) is the units
    # slip these bounds catch; the top end allows UW's salary-book medians.
    expect_true(all(d$Est_Pay[hourly] >= 7 & d$Est_Pay[hourly] <= 150))
    expect_true(all(d$Est_Pay[annual] >= 15000 & d$Est_Pay[annual] <= 1500000))
  }
})

test_that("every reference table the estimates come from passes its own rules", {
  for (name in names(REFERENCE_TABLE_RULES)) {
    p <- here::here("Wy_Ed_Jobs", paste0(name, ".csv"))
    if (!file.exists(p)) next
    expect_null(reference_table_problem(read.csv(p, stringsAsFactors = FALSE), REFERENCE_TABLE_RULES[[name]]),
                info = name)
  }
})
