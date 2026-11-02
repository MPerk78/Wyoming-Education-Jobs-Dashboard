test_that("classify_he_job_type separates Adjunct from full-time faculty", {
  # Regression: this is the main bug from the 2026-08-02 audit -- Adjunct
  # titles used to fall into the same "Instructor/Teacher/Faculty" bucket as
  # tenure-track/full-time postings.
  expect_equal(classify_he_job_type("Adjunct Instructor, Biology"), "Adjunct/Part-Time Faculty")
  expect_equal(classify_he_job_type("Adjunct- Spanish- Jackson"), "Adjunct/Part-Time Faculty")
  expect_equal(classify_he_job_type("Part-Time Mathematics Instructor"), "Adjunct/Part-Time Faculty")
  expect_equal(classify_he_job_type("Assistant Professor of Biology"), "Instructor/Teacher/Faculty")
  expect_equal(classify_he_job_type("Professor of History"), "Instructor/Teacher/Faculty")
})

test_that("a bare 'Part-Time' with no faculty-role word doesn't get counted as adjunct faculty", {
  # Regression: the Adjunct/Part-Time rule used to match on "Part-Time"
  # alone, so real non-faculty postings with that phrase in the title (a
  # part-time custodian, a part-time bookstore clerk) were miscounted into
  # the faculty pool.
  expect_equal(classify_he_job_type("Custodian 1-Part-Time"), "Maintenance & Service")
  expect_equal(classify_he_job_type("Part-Time Bookstore Sales Clerk"), "Support Services")
  expect_equal(classify_he_job_type("Part Time Faculty"), "Adjunct/Part-Time Faculty")
})

test_that("classify_he_job_type covers the remaining coarse buckets", {
  expect_equal(classify_he_job_type("Student Worker - Library"), "Student Positions")
  expect_equal(classify_he_job_type("Registered Nurse"), "Healthcare")
  expect_equal(classify_he_job_type("Temporary Pooled Position"), "Temporary Position")
  expect_equal(classify_he_job_type("Administrative Assistant"), "Staff")
  expect_equal(classify_he_job_type("Academic Advisor"), "Advising and Student Services")
  expect_equal(classify_he_job_type("Registrar"), "Professional")
  expect_equal(classify_he_job_type("Dean of Students"), "Administration")
  expect_equal(classify_he_job_type("Head Basketball Coach"), "Coaching or Athletics")
  expect_equal(classify_he_job_type("Staff Accountant"), "Accounting/Finance")
  expect_equal(classify_he_job_type("Research Associate"), "Research & Science")
  expect_equal(classify_he_job_type("Network Specialist"), "Technical")
  expect_equal(classify_he_job_type("HVAC Technician"), "Maintenance & Service")
  expect_equal(classify_he_job_type("Custodian"), "Maintenance & Service")
  expect_equal(classify_he_job_type("Executive Chef"), "Culinary")
  expect_equal(classify_he_job_type("Director of Marketing"), "Management")
  expect_equal(classify_he_job_type("Totally Unmatched Title Zyx"), "Other")
})

test_that("classify_he_faculty_category covers representative subject areas", {
  expect_equal(classify_he_faculty_category("Assistant Professor of Mathematics"), "Math")
  expect_equal(classify_he_faculty_category("Professor of English"), "Humanities")
  expect_equal(classify_he_faculty_category("Biology Instructor"), "Science")
  expect_equal(classify_he_faculty_category("History Instructor"), "History")
  expect_equal(classify_he_faculty_category("Welding Instructor"), "CTE - Trades & Engineering")
  expect_equal(classify_he_faculty_category("Spanish Instructor"), "Language")
  expect_equal(classify_he_faculty_category("Nursing Instructor"), "CTE - Health Sciences")
  expect_equal(classify_he_faculty_category("Art Instructor"), "The Arts")
  expect_equal(classify_he_faculty_category("Psychology Instructor"), "Social Science")
  expect_equal(classify_he_faculty_category("Librarian"), "Library")
  expect_equal(classify_he_faculty_category("Totally Unmatched Faculty Title"), "Uncategorized")
})

test_that("canonicalize_he_institution fixes the Eastern Wyoming typo and passes through everything else", {
  # Regression: "Commmunity" (triple-m) was corrected mid-history in the
  # scraper but old archives still carried it, silently splitting the
  # college's trend line in two on a full rebuild.
  expect_equal(canonicalize_he_institution("Eastern Wyoming Commmunity College"), "Eastern Wyoming Community College")
  expect_equal(canonicalize_he_institution("Casper College"), "Casper College")
})

test_that("classify_he_faculty_category prevents precedence-order misclassifications", {
  # 1. Political Science & Social Science must not be stolen by Science
  expect_equal(classify_he_faculty_category("Assistant Professor of Political Science"), "Social Science")
  expect_equal(classify_he_faculty_category("Gen Ed - Social Sciences Non-Tenure Track Faculty"), "Social Science")

  # 2. Health Sciences must not be stolen by Science
  expect_equal(classify_he_faculty_category("Gen Ed – Health Sciences Non-Tenure Track Faculty"), "CTE - Health Sciences")
  expect_equal(classify_he_faculty_category("Instructor, General Health Sciences"), "CTE - Health Sciences")

  # 3. Zoology must land in Science, not Business & Computing
  expect_equal(classify_he_faculty_category("Asst Professor - Sensory Biology/Zoology & Physiology"), "Science")
  expect_equal(classify_he_faculty_category("Zoology/Biology Instructor"), "Science")

  # 4. Agricultural Economics must land in Business & Computing, not Trades & Engineering
  expect_equal(
    classify_he_faculty_category("Agricultural Economics & Economics Non-Tenure Track Faculty"),
    "CTE - Business & Computing"
  )

  # 5. Hospitality in College of Business must land in Culinary/Hospitality
  expect_equal(
    classify_he_faculty_category("Hospitality Instructors for the Jake Jabs College of Business"),
    "Culinary/Hospitality"
  )

  # 6. Languages and Humanities
  expect_equal(classify_he_faculty_category("Japanese Non-Tenure Track Faculty"), "Language")
  expect_equal(classify_he_faculty_category("Arabic Non-Tenure Track Faculty"), "Language")
  expect_equal(classify_he_faculty_category("Chinese Non-Tenure Track Faculty"), "Language")
  expect_equal(classify_he_faculty_category("Philosophy Instructor"), "Humanities")
  expect_equal(classify_he_faculty_category("Texts and Critics Instructor"), "Humanities")

  # 7. Trades & Architecture
  expect_equal(classify_he_faculty_category("Instructor of Architecture Non-Tenure Track Faculty"), "CTE - Trades & Engineering")
  expect_equal(classify_he_faculty_category("Commercial Driver's License Instructor"), "CTE - Trades & Engineering")
})

test_that("HE faculty category substring misfires stay fixed", {
  # "latin" inside "Gallatin College"
  expect_equal(classify_he_faculty_category("Aviation Non-Tenure Track Faculty - Gallatin College (Applicant Pool)"), "CTE - Trades & Engineering")
  expect_equal(classify_he_faculty_category("Latin Instructor"), "Language")
  # "ems" inside "Systems"
  expect_equal(classify_he_faculty_category("Information Systems Instructor"), "CTE - Business & Computing")
  expect_equal(classify_he_faculty_category("Adjunct Faculty - EMS (Workforce)"), "CTE - Health Sciences")
  # Criminal Justice ahead of bare "law"
  expect_equal(classify_he_faculty_category("Law Enforcement Academy, Non-Tenure Track Faculty"), "Criminal Justice")
  expect_equal(classify_he_faculty_category("Assistant Professor - Zoology & Physiology"), "Science")
  expect_equal(classify_he_faculty_category("Instructional Design Librarian"), "Education")
})

test_that("HE job type keeps staff and child-center aides out of faculty", {
  expect_equal(classify_he_job_type("Classroom Aide/Substitute Teacher (Part-Time)"), "Support Services")
  expect_equal(classify_he_job_type("Academic Instructional Technologist"), "Professional")
  expect_equal(classify_he_job_type("Instructional Professor, Assistant - Organizational Behavior"), "Instructor/Teacher/Faculty")
  expect_equal(classify_he_job_type("Postdoctoral Researcher"), "Research & Science")
  expect_equal(classify_he_job_type("Livestock Judging Coach/Recruiter"), "Coaching or Athletics")
  expect_equal(classify_he_job_type("Academic Support Center (ASC) Assistant Director"), "Management")
  expect_equal(classify_he_job_type("Student Assistant, Music"), "Student Positions")
  expect_equal(classify_he_job_type("Barista, Café 1968 (Part-Time)"), "Culinary")
})
