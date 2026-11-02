# Misroutes found by the 2026-11-01 audit of every archived title in both
# states (identical file in the MT and WY repos). Every title here is a real
# scraped title.

test_that("activity sponsors, advisors and coaches get their own bucket", {
  activities <- c("FFA Advisor (9-12)", "STIPEND - BPA ADVISOR", "PHS HOSA Advisor 2026 27", "FBLA Co-Sponsor",
                  "DECA Advisor [CO58]", "SGMS Student Council Advisor 26 27", "HHS Assistant Key Club Advisor",
                  "Yearbook Sponsor", "Prom Sponsor", "Color Guard Advisor - Star Valley High School",
                  "Lego League Co-Sponsor", "Robotics Advisor - CS Porter Middle School",
                  "Assistant Speech and Debate Coach [CO46]", "HS Head Forensics (Speech/Debate) Coach",
                  "STIPEND - SPEECH ASSISTANT", "High School Head Robotics Coach", "WMS Drama Coach",
                  "2025-2026 Drama Club Sponsor", "Fall Play/State Drama Co-Sponsor",
                  # "Director" used to send these to Administration.
                  "Musical Director - Cokeville High School", "Play/Musical Director",
                  "Douglas Middle School Lego Robotics Coordinator", "Musical Costumer")
  expect_equal(unname(classify_k12_position(activities)), rep("Activities/Extracurricular", length(activities)))
})

test_that("the activities bucket leaves athletics, teachers and staff advisors alone", {
  expect_equal(classify_k12_position("Head Coach or Sponsor"), "Athletics")
  expect_equal(classify_k12_position("Boys Volleyball Coach (Club)"), "Athletics")
  expect_equal(classify_k12_position("CHEERLEADING ADVISOR (HS/JH)"), "Athletics")
  expect_equal(classify_k12_position("Cheerleading Sponsor - 7/8 Fall"), "Athletics")
  expect_equal(classify_k12_position("High School Science & Robotics Teacher"), "Teacher")
  expect_equal(classify_k12_position("Agricultural Education - FFA"), "Teacher")
  expect_equal(classify_k12_position("Choir Director"), "Teacher")
  expect_equal(classify_k12_position("Athletic/Activity Director"), "Administration")
  expect_equal(classify_k12_position("Speech Language Pathologist"), "Support Services")
  expect_equal(classify_k12_position("After School Program Club Leader- Sagebrush Elementary"), "Child Care")
})

test_that("every substitute posting is a Substitute, whatever role it covers", {
  subs <- c("Substitute Paraprofessional", "Sub Paraprofessional", "Substitute Teacher / Paraprofessional",
            "Special Education Para Educator/Substitutes", "Substitute Para/Custodian/Food Service",
            "Sustitute Bus Aide")
  expect_equal(unname(classify_k12_position(subs[-6])), rep("Substitute", 5))
  # "Sustitute" (sic) has no "Substit", so it is just a bus aide.
  expect_equal(classify_k12_position(subs[6]), "Transportation")
})

test_that("aides and monitors go to the bucket their role names", {
  expect_equal(classify_k12_position("CCA Football Aide"), "Athletics")
  expect_equal(classify_k12_position("CYMS Football Aid Position"), "Athletics")
  expect_equal(classify_k12_position("Dean Morgan Girls Basketball Aide"), "Athletics")
  expect_equal(classify_k12_position("General Kitchen Aide"), "Food Services")
  expect_equal(classify_k12_position("School Nutrition (SN) Aide Middle School MS2627_51"), "Food Services")
  expect_equal(classify_k12_position("Lincoln Education Support Personnel / Cafeteria Technician"), "Food Services")
  expect_equal(classify_k12_position("Bus Aide - Part-Time - Star Valley"), "Transportation")
  expect_equal(classify_k12_position("Bus Monitor (6 Openings)"), "Transportation")
  expect_equal(classify_k12_position("Teacher Aide - NMS"), "Paraprofessional")
  # Bare "Aid" matched inside "Paid".
  expect_equal(classify_k12_position("Bus Driver - No Experience Needed - Paid Training"), "Transportation")
  # Unchanged: generic aides are still support staff.
  expect_equal(classify_k12_position("Health Aide"), "Support Services")
})

test_that("\"Para\" no longer matches Paradise Valley", {
  expect_equal(classify_k12_position("Paradise Valley Kindergarten Teacher for the 2026-27 School Year Only"), "Teacher")
  expect_equal(classify_k12_position("Paradise Valley Elementary Custodian"), "Custodial/Maintenance")
  expect_equal(classify_k12_position("Paradise Valley Paraprofessional"), "Paraprofessional")
})

test_that("roles that sat in Other get their bucket", {
  expect_equal(classify_k12_position("Janitor"), "Custodial/Maintenance")
  expect_equal(classify_k12_position("LPN-District"), "Support Services")
  expect_equal(classify_k12_position("Mental Health Specialist I"), "Support Services")
  expect_equal(classify_k12_position("Accountant I"), "Staff")
  expect_equal(classify_k12_position("HR Benefits Specialist"), "Staff")
  expect_equal(classify_k12_position("Business Services Specialist (Finance & Accounting)"), "Staff")
  expect_equal(classify_k12_position("BusinessManager"), "Administration")
  expect_equal(classify_k12_position("District Technology Assistant"), "Support Services")
  expect_equal(classify_k12_position("Technology Specialist (Full-time)/TECH0008"), "Support Services")
  expect_equal(classify_k12_position("KWHS Student Information Technology Worker"), "Support Services")
  expect_equal(classify_k12_position("SUMMER TECH CREW"), "Support Services")
  # Technology leadership stays Administration.
  expect_equal(classify_k12_position("Technology Director"), "Administration")
  expect_equal(classify_k12_position("ASSISTANT TECHNOLOGY COORDINATOR"), "Administration")
  expect_true(is_placeholder_posting("None at this time"))
  # Grade band only, from Lodge Grass's Certified tab.
  expect_equal(classify_k12_position("Junior High 7-8"), "Teacher")
})

test_that("faculty applicant pools are the adjunct pipeline; staff and teacher pools are not", {
  expect_equal(classify_he_job_type("Spanish Non-Tenure Track Faculty (Applicant Pool)"), "Adjunct/Part-Time Faculty")
  expect_equal(classify_he_job_type("Welding Technology Non-Tenure Track Faculty - Gallatin College (Applicant Pool)"),
               "Adjunct/Part-Time Faculty")
  expect_equal(classify_he_job_type("Clinical Adjunct Instructor Nursing Pool"), "Adjunct/Part-Time Faculty")
  expect_equal(classify_he_job_type("Cook II (Applicant Pool)"), "Culinary")
  expect_equal(classify_he_job_type("Assistant Professor of Mathematics"), "Instructor/Teacher/Faculty")
  expect_equal(classify_he_faculty_category("Plant Sciences & Plant Pathology Non-Tenure Track Faculty (Applicant Pool)"),
               "Science")
})

test_that("postdocs and research scientists are research staff, not faculty", {
  expect_equal(classify_he_job_type("Postdoctoral Researcher - Lyme Disease Bacteriophage Biology"), "Research & Science")
  expect_equal(classify_he_job_type("Post Doc Res Assoc - Atmospheric Science"), "Research & Science")
  expect_equal(classify_he_job_type("Interdisciplinary Postdoctoral Opportunities (Open Pool)"), "Research & Science")
  expect_equal(classify_he_job_type("Research Scientist, Center for Translational Medicine"), "Research & Science")
  expect_equal(classify_he_job_type("Mellon Research Assistant Professor of Native American Studies"),
               "Instructor/Teacher/Faculty")
})

test_that("custodians are Maintenance & Service like custodial titles", {
  expect_equal(classify_he_job_type(c("Custodian I", "Custodian II - Housing", "Custodial Supervisor")),
               rep("Maintenance & Service", 3))
})

test_that("UW 'Hourly Pooled' postings are classified by the job after the prefix", {
  expect_equal(classify_he_job_type("Hourly Pooled - Part Time Custodian, Custodial Services"), "Maintenance & Service")
  expect_equal(classify_he_job_type("Hourly Pooled - Culinary Assistant - North Hall Dining Center"), "Culinary")
  expect_equal(classify_he_job_type("Hourly Pooled - Research Assistant, Ecosystem Science & Management"),
               "Research & Science")
  expect_equal(classify_he_job_type("Hourly Pooled - Branding Iron Editor-in-Chief, Student Media"), "Student Positions")
  expect_equal(classify_he_job_type("Hourly Pooled - Student Manager, Womens Volleyball"), "Student Positions")
  expect_equal(classify_he_job_type("Hourly Pooled - Box Office Manager (Fine Arts Outreach)"), "Staff")
  expect_equal(classify_he_job_type("Hourly Pooled - Basketball Table Crew"), "Staff")
  expect_equal(classify_he_job_type("Hourly Pooled - Part-Time Office Assistant, Senior"), "Staff")
})
