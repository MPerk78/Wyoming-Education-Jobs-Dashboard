# qcew_school_pay.R: BLS QCEW average pay of public school employees by
# county (map popup context). Fixtures: QCEW's real 2025 annual NAICS 6111
# file and area-titles table, trimmed to Montana and Wyoming rows.

qcew_fixture <- function(name) {
  utils::read.csv(test_path("fixtures", name), stringsAsFactors = FALSE, colClasses = "character")
}

test_that("parse_qcew_school_pay keeps public school districts' county rows, named", {
  industry <- qcew_fixture("qcew_6111_2025_mt_wy_trimmed.csv")
  titles <- qcew_fixture("qcew_area_titles_mt_wy_trimmed.csv")
  mt <- parse_qcew_school_pay(industry, titles, "30")
  expect_equal(nrow(mt), 56)
  expect_true(all(grepl(" County$", mt$County)))
  expect_false(any(duplicated(mt$County)))
  expect_true(all(mt$Avg_Annual_Pay > 15000 & mt$Avg_Annual_Pay < 150000))
  expect_equal(unique(mt$Year), "2025")
  src <- industry[industry$area_fips == "30111" & industry$own_code == "3", ]
  expect_equal(mt$Avg_Annual_Pay[mt$County == "Yellowstone County"], as.numeric(src$avg_annual_pay))
})

test_that("counties BLS withholds are left out, not shown as zero", {
  industry <- qcew_fixture("qcew_6111_2025_mt_wy_trimmed.csv")
  titles <- qcew_fixture("qcew_area_titles_mt_wy_trimmed.csv")
  wy <- parse_qcew_school_pay(industry, titles, "56")
  withheld <- industry$area_fips[startsWith(industry$area_fips, "56") & industry$own_code == "3" &
                                   industry$disclosure_code == "N"]
  expect_gt(length(withheld), 0)
  expect_false(any(wy$County_FIPS %in% withheld))
  expect_true(all(wy$Avg_Annual_Pay > 0))
})

test_that("qcew_county_key matches either way a county is written", {
  expect_equal(qcew_county_key(c("Albany County, Wyoming", "Yellowstone County ")), c("Albany County", "Yellowstone County"))
})

test_that("the committed county pay file joins to the districts' counties", {
  app_dir <- if (dir.exists(here::here("Mt_Ed_Jobs"))) "Mt_Ed_Jobs" else "Wy_Ed_Jobs"
  q <- read.csv(here::here(app_dir, "qcew_county_school_pay.csv"))
  sm <- read.csv(here::here(app_dir, "salarymap2.csv"))
  expect_gt(nrow(q), 0)
  expect_gt(sum(qcew_county_key(sm$County) %in% q$County), 0)
})
