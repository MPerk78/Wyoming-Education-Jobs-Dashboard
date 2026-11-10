test_that("classify_k12_position buckets substitutes before teachers", {
  expect_equal(classify_k12_position("Substitute Teacher"), "Substitute")
  expect_equal(classify_k12_position("Sub Nurse"), "Substitute")
  expect_equal(
    classify_k12_position("Long Term Sub Kindergarten Dual Immersion Teacher-Spanish Instruction"),
    "Substitute"
  )
})

test_that("classify_k12_position separates part-time teaching from full-FTE Teacher", {
  # Regression: part-time/reduced-FTE teacher titles used to be counted
  # identically to full-time teacher postings in the FTE trend charts.
  expect_equal(
    classify_k12_position("Part-Time (1/7th) Agriculture Education Teacher Remainder of 25-26 School Year"),
    "Part-Time Teacher"
  )
  expect_equal(classify_k12_position("Head Start Part-Time Assistant Teacher"), "Part-Time Teacher")
  expect_equal(classify_k12_position("Elementary Teacher 1st Grade"), "Teacher")

  # "Part-Time" alone, with no teacher keyword, must NOT be swept into
  # Part-Time Teacher -- it should still classify by its own role.
  expect_equal(classify_k12_position("Part-Time Bus Driver (29 Hours/Week)"), "Transportation")
})

test_that("classify_k12_position covers the remaining coarse buckets", {
  expect_equal(classify_k12_position("Paraprofessional - Elementary"), "Paraprofessional")
  expect_equal(classify_k12_position("School Counselor"), "Support Services")
  expect_equal(classify_k12_position("Head Football Coach"), "Athletics")
  expect_equal(classify_k12_position("Custodian"), "Custodial/Maintenance")
  expect_equal(classify_k12_position("Bus Driver"), "Transportation")
  expect_equal(classify_k12_position("Principal"), "Administration")
  expect_equal(classify_k12_position("Secretary"), "Staff")
  expect_equal(classify_k12_position("Cafeteria Cook"), "Food Services")
  expect_equal(classify_k12_position("Daycare Provider"), "Child Care")
  expect_equal(classify_k12_position("Random Title With Nothing Matching"), "Other")
})

test_that("classify_k12_subject checks Special Education ahead of subject keywords", {
  expect_equal(classify_k12_subject("Special Education Math Teacher"), "Special Education - General")
})

test_that("classify_k12_subject covers representative subject areas", {
  expect_equal(classify_k12_subject("Welding Teacher"), "Technical Education")
  expect_equal(classify_k12_subject("Agriculture Teacher"), "Agriculture Education")
  expect_equal(classify_k12_subject("5th Grade Teacher"), "Elementary Education")
  expect_equal(classify_k12_subject("High School English Teacher"), "English Language Arts Education")
  expect_equal(classify_k12_subject("Spanish Teacher"), "Language Education")
  expect_equal(classify_k12_subject("Mathematics Teacher"), "Mathematics Education")
  expect_equal(classify_k12_subject("Band Director"), "Music Education")
  expect_equal(classify_k12_subject("Physical Education Teacher"), "Physical Education")
  expect_equal(classify_k12_subject("History Teacher"), "Social Studies Education")
  expect_equal(classify_k12_subject("Random Teacher Title"), "Uncategorized")
})

test_that("classify_k12_broad_category maps Language correctly (regression for 'Lanugage' typo)", {
  expect_equal(classify_k12_broad_category("Language Education"), "Language")
  expect_false(identical(classify_k12_broad_category("Language Education"), "Lanugage"))
})

test_that("classify_k12_broad_category covers other mappings and the catchall", {
  expect_equal(classify_k12_broad_category("Special Education - General"), "Special Education - General")
  expect_equal(classify_k12_broad_category("Special Education - Resource/Life Skills"), "Special Education - Resource/Life Skills")
  expect_equal(classify_k12_broad_category("Elementary Education"), "Elementary")
  expect_equal(classify_k12_broad_category("Uncategorized"), "Other")
  expect_equal(classify_k12_broad_category("Nonsense Category"), "Other")
})

test_that("canonicalize_k12_district fixes known typos and passes through everything else", {
  expect_equal(canonicalize_k12_district("Hot Springs School District 1"), "Hot Springs County School District 1")
  expect_equal(canonicalize_k12_district("Crook County school District 1"), "Crook County School District 1")
  expect_equal(canonicalize_k12_district("Albany Count School District 1"), "Albany County School District 1")
  expect_equal(canonicalize_k12_district("Campbell County Scholl District 1"), "Campbell County School District 1")
  expect_equal(canonicalize_k12_district("Converse County School Distrcit 2"), "Converse County School District 2")
  expect_equal(canonicalize_k12_district("Sheridan County School District 1"), "Sheridan County School District 1")
})

test_that("classify_k12_subject recognizes the literal 'Technical Education' phrase, not just 'Tech Ed'", {
  expect_equal(classify_k12_subject("Technical Education Teacher"), "Technical Education")
})

test_that("classify_k12_subject matches the plural 'Family and Consumer Sciences'", {
  expect_equal(classify_k12_subject("Family and Consumer Sciences Teacher"), "Family and Consumer Science")
})

test_that("classify_k12_subject recognizes 'Head Start' and hyphenated 'Pre-School' as Early Childhood", {
  expect_equal(
    classify_k12_subject(c("Head Start Teacher", "Pre-School Teacher")),
    c("Early Childhood Education", "Early Childhood Education")
  )
})

test_that("classify_k12_subject treats 'Guest Teacher' as substitute teaching", {
  expect_equal(classify_k12_subject("Guest Teacher"), "Substitute Teaching")
})

test_that("classify_k12_subject recognizes 'Vo/Ag' as Agriculture Education", {
  expect_equal(classify_k12_subject("Vo/Ag Teacher"), "Agriculture Education")
})

test_that("fix_title_encoding repairs a Windows-1252 byte without warning, and classification still works", {
  # Reconstruct the exact byte sequence that produced the original
  # 'unable to translate ... to a wide string' warning: a raw 0x96
  # (Windows-1252 en dash) inside an otherwise-plain title.
  bad_bytes <- as.raw(c(
    0x31, 0x20, 0x50, 0x61, 0x72, 0x74, 0x20, 0x54, 0x69, 0x6d, 0x65,
    0x20, 0x28, 0x31, 0x30, 0x3a, 0x30, 0x30, 0x20, 0x61, 0x6d, 0x20,
    0x96, 0x20, 0x32, 0x3a, 0x30, 0x30, 0x20, 0x70, 0x6d, 0x29, 0x20,
    0x41, 0x73, 0x73, 0x69, 0x73, 0x74, 0x61, 0x6e, 0x74, 0x20, 0x43,
    0x6f, 0x6f, 0x6b
  ))
  bad_title <- rawToChar(bad_bytes)
  Encoding(bad_title) <- "unknown"

  expect_no_warning(result <- classify_k12_position(bad_title))
  expect_equal(result, "Food Services")
  expect_no_warning(classify_k12_subject(bad_title))
})

test_that("fix_title_encoding does not corrupt an already-valid UTF-8 title", {
  # Regression: treating every title as Windows-1252 unconditionally used
  # to double-decode real UTF-8 characters into mojibake -- a genuine curly
  # apostrophe (U+2019) turned into "Teacherâ€™s Aide". Confirmed this was
  # live in committed data (combinedclean.csv) before the fix.
  title <- "Teacher’s Aide"
  expect_equal(fix_title_encoding(title), title)
})

test_that("classify_k12_position routes clerical staff with department names to Staff rather than Teacher", {
  expect_equal(classify_k12_position("General Secretary - Library"), "Staff")
  expect_equal(classify_k12_position("Secretary - Coordinator of Language Arts"), "Staff")
  expect_equal(classify_k12_position("Library Media Specialist"), "Teacher")
})

test_that("classify_k12_subject recognizes Indigenous languages and EL titles as Language Education", {
  expect_equal(
    classify_k12_subject(c(
      "Shoshone Language Teacher",
      "RMS Teacher ~ Salish and/or Kootenai Language ~ 2026-27",
      "Salish Teacher",
      "Cree Enhancement Program-Project Coordinator/Teacher",
      "EL Teacher - Desert View / Overland"
    )),
    rep("Language Education", 5)
  )
})

test_that("classify_k12_subject recognizes SE - TEACHER variants as Special Education", {
  expect_equal(classify_k12_subject("SE - TEACHER"), "Special Education - General")
  expect_equal(classify_k12_subject("SE - TEACHER LS"), "Special Education - Resource/Life Skills")
  expect_equal(classify_k12_subject("Disabilities Educator"), "Special Education - General")
  expect_equal(classify_k12_subject("Sp Ed Extended Resource Teacher (K-2)"), "Special Education - Resource/Life Skills")
})

test_that("classify_k12_subject recognizes Pre-K, Toddler, and SACC titles as Early Childhood Education", {
  expect_equal(
    classify_k12_subject(c("Pre -K Teacher", "Toddler Teacher", "SACC Lead Teacher", "Daycare Lead Teacher")),
    rep("Early Childhood Education", 4)
  )
})

test_that("classify_k12_subject recognizes Title I and literacy roles as English Language Arts", {
  expect_equal(
    classify_k12_subject(c("Title I Teacher", "Title One Teacher", "Reading Recovery Teacher", "Secondary Literacy Teacher")),
    rep("English Language Arts Education", 4)
  )
})

test_that("classify_k12_subject recognizes Drivers Ed, Woodworking, and CNA roles", {
  expect_equal(classify_k12_subject("Drivers Education Teacher"), "Technical Education")
  expect_equal(classify_k12_subject("9-12 Woodworking Teacher"), "Technical Education")
  expect_equal(classify_k12_subject("Certified Nursing Assistant (CNA) Instructor"), "Health Science")
})

test_that("classify_k12_position doesn't let the precedence overhaul swallow real roles", {
  # Bare "Intern" matched inside "Internal Applicants Only".
  expect_equal(classify_k12_position("Special Education Teacher - Internal Applicants"), "Teacher")
  expect_equal(classify_k12_position("Bus Driver (Internal Candidates Only)"), "Transportation")
  expect_equal(classify_k12_position("Technology Intern"), "Support Services")
  expect_equal(classify_k12_position("Casper Mountain Science Program Intern"), "Support Services")
  # "Technician" was checked ahead of Custodial/Transportation/Food Services.
  expect_equal(classify_k12_position("Dean Morgan Cafeteria Technician"), "Food Services")
  expect_equal(classify_k12_position("Maintenance- HVAC Technician"), "Custodial/Maintenance")
  expect_equal(classify_k12_position("Facility Technician - Carpenter [TS07]"), "Custodial/Maintenance")
  expect_equal(classify_k12_position("Transportation Parts Technician"), "Transportation")
  expect_equal(classify_k12_position("Seasonal Database Technician - Districtwide"), "Support Services")
  # Ensemble directors and Drivers Ed are teaching jobs.
  expect_equal(classify_k12_position("Middle School Band Teacher / Band Director"), "Teacher")
  expect_equal(classify_k12_position("Choir Director"), "Teacher")
  expect_equal(classify_k12_position("Drivers Education Teacher"), "Teacher")
  expect_equal(classify_k12_position("Athletic Director"), "Administration")
})

test_that("OPI-style subject-only titles are teaching jobs with the right subject", {
  titles <- c("Elementary Curriculum", "Special Education", "Music", "Physical Ed", "Social Studies (Broadfield)",
              "World Languages", "Health Enhancement", "Reading", "Psychology", "K - 12 Librarian")
  expect_equal(unname(classify_k12_position(titles)), rep("Teacher", length(titles)))
  expect_equal(unname(classify_k12_subject(c("Physical Ed", "Health Enhancement", "Reading", "World Languages", "Psychology"))),
               c("Physical Education", "Physical Education", "English Language Arts Education", "Language Education", "Social Studies Education"))
  # Subject words alone don't make an advisor/assistant/builder role a teacher.
  expect_false(classify_k12_position("FFA Advisor") == "Teacher")
  expect_false(classify_k12_position("Lego League/ Robotics Activity") == "Teacher")
  expect_false(classify_k12_position("GRHS Theater Set Design Builder") == "Teacher")
  expect_equal(classify_k12_position("Special Education Case Manager"), "Teacher")
})

test_that("CTE-prefixed subjects reach their specific subject, not Technical Education", {
  expect_equal(classify_k12_subject("CTE – Agricultural Education"), "Agriculture Education")
  expect_equal(classify_k12_subject("CTE – Business and Marketing Education"), "Business and Economics")
  expect_equal(classify_k12_subject("CTE – Family and Consumer Sciences"), "Family and Consumer Science")
  expect_equal(classify_k12_subject("CTE – Industrial Technology Education"), "Technical Education")
  expect_equal(classify_k12_subject("CTE Teacher"), "CTE Teacher")
  expect_equal(classify_k12_subject("Special Educaiton Teacher"), "Special Education - General")
})

test_that("classify_k12_position routes common non-teaching roles out of Other", {
  expect_equal(classify_k12_position("Special Programs Education Assistant"), "Paraprofessional")
  expect_equal(classify_k12_position("Crossing Guard"), "Support Services")
  expect_equal(classify_k12_position("School Nurse"), "Support Services")
  expect_equal(classify_k12_position("Nurse Educator"), "Teacher")
  expect_equal(classify_k12_position("General Secretary- Counseling"), "Staff")
  expect_equal(classify_k12_position("Seasonal Grounds Worker"), "Custodial/Maintenance")
  expect_equal(classify_k12_position("Superintendent"), "Administration")
  expect_equal(classify_k12_position("Dean Morgan Cafeteria Technician"), "Food Services")
  expect_equal(classify_k12_position("STIPEND - SWIMMING ASSISTANT JV"), "Athletics")
  expect_equal(classify_k12_position("Substiture Buffalo Bus Route Driver"), "Substitute")
  expect_equal(classify_k12_position("Substance Abuse/Mental Health Services Grant Coordinator"), "Administration")
  expect_equal(classify_k12_position("Test Preparation Teacher"), "Teacher")
})

test_that("is_placeholder_posting drops non-jobs but keeps standing applicant pools", {
  expect_true(all(is_placeholder_posting(c("Sample Certified Position", "Future Classified Positions",
    "Use This Template to Create An Account For Future Certified Openings", "Student Teaching Opportunities",
    "No open positions at this time", "Job Title", "No Jobs"))))
  expect_false(any(is_placeholder_posting(c("Applicant Pool - Substitute Teacher", "MS Science Teacher Job Description",
    "Elementary Teacher", NA))))
})

test_that("numeric HTML entities in titles are decoded, hex forms included", {
  # Albany's board switched from "Girls'" to "Girls&#x27;" in 2026, which broke
  # the posting's identity across runs (an ending plus a new start) and showed
  # the raw entity on the dashboard (WY spells audit, 2026-11-10).
  expect_equal(fix_title_encoding("Middle School Girls&#x27; Basketball Coach"), "Middle School Girls' Basketball Coach")
  expect_equal(fix_title_encoding("Teacher&#x2019;s Aide &#39;26"), "Teacher\u2019s Aide '26")
  expect_equal(fix_title_encoding("Para &#8211; SPED"), "Para \u2013 SPED")
  # Not a valid code point: left as written. Named entities still work.
  expect_equal(fix_title_encoding("Coach &#xD800; &amp; Aide"), "Coach &#xD800; & Aide")
  expect_equal(fix_title_encoding(c(NA, "Plain Title")), c(NA, "Plain Title"))
})
