# Free/reduced-price lunch share, school staffing and the surrounding-district
# lookup (ccd_staff_scraper.R, 2026-11-09). Same file in the MT and WY repos.
# Real fixtures: WY's 2024 CCD school directory rows for Albany County SD 1,
# Fremont County SD 38, Niobrara County SD 1 and the 3 state-authorized
# charters (captured 2026-11-09), and the Census geocoder's answer for
# Prairie View Community School's coordinates (Chugwater).
schools <- read.csv(test_path("fixtures", "ccd_wy_school_directory_2024_subset.csv"),
                    colClasses = c(ncessch = "character", leaid = "character"))

test_that("a district's share sums its reporting schools (CCD's negative codes and empty schools left out)", {
  r <- parse_ccd_frl_share(schools, list("Albany County School District 1" = "5600730",
                                         "Fremont County School District 38" = "5600960"))
  expect_equal(round(r$Free_Reduced_Lunch_Share[1], 4), 0.3377)
  expect_equal(r$FRL_Schools_Reporting[1], 14)
  # Arapahoe Elementary 381 of 392 + Arapahoe Charter High School 54 of 54.
  expect_equal(r$Free_Reduced_Lunch_Share[2], (381 + 54) / (392 + 54))
})

test_that("a 12-digit id is one school (a charter inside its host district)", {
  r <- parse_ccd_frl_share(schools, list("Snowy Range Academy" = "560073000216"))
  expect_equal(r$Free_Reduced_Lunch_Share, 78 / 260)
  expect_equal(r$FRL_Schools_Reporting, 1)
})

test_that("no share when no school reports, or when reporting schools hold under 80% of students", {
  r <- parse_ccd_frl_share(schools, list("Niobrara County School District 1" = "5604230"))
  expect_true(is.na(r$Free_Reduced_Lunch_Share))
  part <- schools
  part$free_or_reduced_price_lunch[part$school_name %in% c("Laramie High School", "Laramie Middle School")] <- -1
  expect_true(is.na(parse_ccd_frl_share(part, list(A = "5600730"))$Free_Reduced_Lunch_Share))
})

test_that("fetch_ccd_frl_share takes each entity's newest reporting year", {
  older <- schools
  older$free_or_reduced_price_lunch[older$leaid == "5604230"] <- c(100, 50, 2, 60)
  home <- environment(fetch_ccd_frl_share)
  real <- get("fetch_ccd_school_directory", envir = home)
  assign("fetch_ccd_school_directory", function(fips, year) {
    if (year == 2024) schools else if (year == 2023) older else data.frame()
  }, envir = home)
  withr::defer(assign("fetch_ccd_school_directory", real, envir = home))
  r <- fetch_ccd_frl_share(56, list(Albany = "5600730", Niobrara = "5604230"), start_year = 2024, years_back = 2)
  expect_equal(r$FRL_Year[r$District == "Albany"], "2024")
  expect_equal(r$FRL_Year[r$District == "Niobrara"], "2023")
  expect_equal(r$Free_Reduced_Lunch_Share[r$District == "Niobrara"], 212 / (387 + 250 + 6 + 292))
})

test_that("parse_ccd_school_staffing sums a charter's school record", {
  r <- parse_ccd_school_staffing(schools, list("Prairie View Community School" = "568025900599",
                                               "Laramie Montessori Charter School" = "560073000542"))
  expect_equal(r$Teachers_Total_FTE, c(10, 8))
  expect_equal(r$Enrollment, c(90, 97))
})

test_that("geocode_school_district reads the district id from the Census geocoder's answer", {
  body <- paste(readLines(test_path("fixtures", "census_geocoder_prairie_view_2026-11-09.json"), warn = FALSE), collapse = "\n")
  httr2::local_mocked_responses(list(httr2::response(status_code = 200, headers = list("Content-Type" = "application/json"),
                                                     body = charToRaw(body))))
  expect_equal(geocode_school_district(41.76044, -104.823155), "5605090")  # Platte County School District 1
  expect_true(is.na(geocode_school_district(NA, NA)))
})
