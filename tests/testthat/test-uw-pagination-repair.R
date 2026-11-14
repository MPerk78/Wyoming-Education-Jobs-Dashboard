test_that("repair_uw_oracle_duplicates removes only verified repeated Oracle IDs", {
  postings <- data.frame(
    Title = c("Assistant Professor of Math", "Assistant Professor of Math",
              "Biology Instructor", "Assistant Professor of Math"),
    Location = "Laramie, WY",
    Posted_Date = "2026-08-11",
    Institution = c("University of Wyoming", "University of Wyoming",
                    "University of Wyoming", "Other College"),
    Link = c(
      "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/101",
      "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/101",
      "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/102",
      "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/101"
    ),
    Archive_Date = "2026-08-11",
    stringsAsFactors = FALSE
  )

  repaired <- repair_uw_oracle_duplicates(postings, "fixture")

  expect_equal(nrow(repaired$data), 3)
  expect_equal(repaired$report$posting_id, "101")
  expect_equal(repaired$report$duplicate_rows_removed, 1)
  expect_equal(
    sum(repaired$data$Institution == "Other College" & grepl("/job/101$", repaired$data$Link)),
    1
  )
})

test_that("repair_uw_oracle_duplicates fails closed for conflicting copies", {
  postings <- data.frame(
    Title = c("Assistant Professor of Math", "Assistant Professor of Science"),
    Location = "Laramie, WY",
    Posted_Date = "2026-08-11",
    Institution = "University of Wyoming",
    Link = "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/101",
    Archive_Date = "2026-08-11",
    stringsAsFactors = FALSE
  )

  expect_error(repair_uw_oracle_duplicates(postings, "fixture"), "conflicting copies")
})

test_that("rebuild_current_he_aggregates uses repaired UW postings", {
  postings <- data.frame(
    Title = c("Assistant Professor of Math", "Assistant Professor of Math",
              "Biology Instructor", "Admissions Specialist"),
    Location = "Laramie, WY",
    Posted_Date = "2026-08-11",
    Institution = "University of Wyoming",
    Link = c(
      "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/101",
      "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/101",
      "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/102",
      "https://eeik.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX_1/job/103"
    ),
    Archive_Date = "2026-08-11",
    stringsAsFactors = FALSE
  )

  repaired <- repair_uw_oracle_duplicates(postings, "fixture")
  current <- rebuild_current_he_aggregates(repaired$data)$allnow_he
  total <- current[current$Institution == "Total", , drop = FALSE]
  parts <- current[current$Institution != "Total", , drop = FALSE]

  expect_equal(sum(total$Sum), 2)
  expect_equal(sum(parts$Sum), 2)
})
