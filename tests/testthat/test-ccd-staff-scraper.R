# Real fixture: the actual Wyoming (fips=56) response from the Urban
# Institute's Education Data Portal school-districts/ccd/directory/2024
# endpoint, captured 2026-08-04 -- all 62 WY-registered LEAs (48 real
# school districts plus BOCES-style service agencies, children's homes,
# and a state health department entry), not synthetic.

test_that("canonicalize_ccd_district_name handles the standard pattern, inconsistent '#' spacing, and the two exceptions", {
  expect_equal(canonicalize_ccd_district_name("Albany County School District #1"), "Albany County School District 1")
  expect_equal(canonicalize_ccd_district_name("Fremont County School District # 1"), "Fremont County School District 1")
  expect_equal(canonicalize_ccd_district_name("Fremont County School District #14"), "Fremont County School District 14")
  # Big Horn and Natrona don't take "County" in this project's canonical
  # naming, unlike CCD's own naming (which includes "County" for all 48,
  # more consistently than WSBA's source did).
  expect_equal(canonicalize_ccd_district_name("Big Horn County School District #4"), "Big Horn School District 4")
  expect_equal(canonicalize_ccd_district_name("Natrona County School District #1"), "Natrona School District 1")
})

test_that("canonicalize_ccd_district_name is vectorized", {
  result <- canonicalize_ccd_district_name(c("Albany County School District #1", "Big Horn County School District #2"))
  expect_equal(result, c("Albany County School District 1", "Big Horn School District 2"))
})

test_that("canonicalize_ccd_district_name returns NA for text that isn't a '<name> #<n>' district line", {
  expect_true(is.na(canonicalize_ccd_district_name("Wyoming Department of Health")))
})

test_that("parse_ccd_teacher_fte isolates exactly the 48 real school districts via agency_type == 1", {
  df <- read.csv(test_path("fixtures", "ccd_wy_directory_2024.csv"))
  result <- parse_ccd_teacher_fte(df)

  expect_equal(nrow(result), 48)
  expect_equal(length(unique(result$District)), 48)

  # Regression: BOCES-style service agencies, children's homes, and a state
  # health department entry are real rows in the raw CCD data (agency_type
  # != 1) that must not leak into the district-level FTE table.
  expect_false("Wyoming Department of Health" %in% result$District)
  expect_false(any(grepl("BOCES|Cathedral Home|Saint Joseph|Red Top Meadows", result$District)))
})

test_that("parse_ccd_teacher_fte extracts correct teacher FTE and enrollment values", {
  df <- read.csv(test_path("fixtures", "ccd_wy_directory_2024.csv"))
  result <- parse_ccd_teacher_fte(df)

  albany <- result[result$District == "Albany County School District 1", ]
  expect_equal(albany$Teachers_Total_FTE, 316)
  expect_equal(albany$Enrollment, 3752)

  laramie1 <- result[result$District == "Laramie County School District 1", ]
  expect_equal(laramie1$Teachers_Total_FTE, 1067)
})

test_that("parse_ccd_teacher_fte matches this project's canonical 48-district list exactly", {
  df <- read.csv(test_path("fixtures", "ccd_wy_directory_2024.csv"))
  result <- parse_ccd_teacher_fte(df)

  # salarymap2.csv now also carries a couple of charter schools (Snowy
  # Range Academy, Laramie Montessori Charter School) alongside the 48 real
  # school districts CCD's agency_type == 1 filter isolates -- charter
  # schools aren't LEAs in CCD's own data model, so they're correctly
  # absent from `result` and must be excluded here too, or this becomes a
  # false "canonicalization is broken" failure rather than a real one.
  # Every real district's name contains "School District"; the charter
  # schools' names don't.
  k12 <- read.csv(here::here("Wy_Ed_Jobs", "salarymap2.csv"))
  real_districts <- k12$District[grepl("School District", k12$District)]
  expect_setequal(result$District, real_districts)
})

test_that("parse_ccd_teacher_fte returns an empty, correctly-shaped frame when given no rows", {
  result <- parse_ccd_teacher_fte(data.frame())
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("District", "Teachers_Total_FTE", "Enrollment"))
})
