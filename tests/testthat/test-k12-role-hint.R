# k12_role_hint() / classify_k12_position(title, role_hint): OPI's title is
# only its subject field, so the description's opening refines the bucket.
# Every case below is a real OPI posting from the 2026-11-03 archive, its
# description trimmed to the opening the rule reads; each move was confirmed
# by reading the description (2026-11-09). Identical in the MT and WY repos.
role <- function(title, description) classify_k12_position(title, k12_role_hint(description))

test_that("descriptions move the OPI postings the title misfiles", {
  moves <- list(
    list("Special Education", "LONG TERM SUBSTITUTE POSITION - SPECIAL EDUCATION (March - June) Supervision: The Elementary Special Education-Resource Teacher falls under the primary direction of the Elementary School Principal", "Substitute"),
    list("Elementary Curriculum", "Position Title: Long-Term Substitute Kindergarten Teacher School: Marion School District Location: Marion, Montana", "Substitute"),
    list("Music", "The Richey School District is seeking a part time, fully certified Music teacher. We may also have coaching positions available as well.", "Part-Time Teacher"),
    list("Music", "Music Teacher: Absarokee Public Schools has an opening for a half time 6th-12th and 5th grade Music Teacher for the 2026-267 school year.", "Part-Time Teacher"),
    list("Science (Broadfield)", "Position Title: Science Teacher .5 FTE HS2627_13 Start Date: August 20, 2026 Location: High School Pay Type: Salaried-Exempt", "Part-Time Teacher"),
    list("Physical Ed", " Amsterdam School is looking for an outstanding .4 FTE PE Teacher to join our team!", "Part-Time Teacher"),
    list("Elementary Curriculum", " a caring, dependable, and student-centered paraprofessional. The successful candidate will work under the supervision of licensed teachers and administrators", "Paraprofessional"),
    list("Elementary Curriculum", "Broadview School District is currently advertising for an Title I paraeducator as well as SPED paraeducator for Kindergarten through 8th grades for the 2026-27 school year.", "Paraprofessional"),
    list("Administrative - Other", "High School Theology Teacher Ashland, Montana, United States | Full-time Apply by: No close date St. Labre Indian Catholic School", "Teacher"),
    list("Administrative - Other", "High School Career Counselor Ashland, Montana, United States | Full-time Apply by: No close date St. Labre Indian Catholic School", "Support Services"),
    list("Other", " NORTHERN CHEYENNE TRIBAL SCHOOL JOB ADVERTISEMENT POSITION: CULTURE TEACHER SALARY: CERTIFIED SALARY SCALE DEPARTMENT: NCTS ACCOUNTABLE TO:", "Teacher"))
  for (m in moves) expect_equal(role(m[[1]], m[[2]]), m[[3]], label = substr(m[[2]], 1, 50))
})

test_that("descriptions leave the title's bucket alone when the signal isn't clear", {
  keep <- list(
    # "Flexible FTE 0.5 FTE-1.0 FTE" may be full-time.
    list("Library", "New opening as the librarian hired did not fulfill the contract due to other committments! Here is your opportunity!!! Flexible FTE 0.5 FTE-1.0 FTE.", "Teacher"),
    list("Elementary Curriculum", "Elementary Teacher (Full-Time) (Part-Time) Vida Elementary School – 2026–2027 School Year", "Teacher"),
    # A hint never sends a posting to Administration, nor takes one out of it unless a non-admin role is named first.
    list("Administrative - Principal", "7-12 Principal | Box Elder Public Schools 2026–2027 School Year Box Elder Public Schools is seeking a dynamic 7-12 Principal", "Administration"),
    list("Administrative - Superintendent", "The Board of Trustees of Froid Public Schools is accepting applications for a motivated superintendent", "Administration"),
    list("Elementary Curriculum", NA, "Teacher"),
    list("Elementary Curriculum", "", "Teacher"),
    # A teacher posting that advertises para support is still a teacher posting.
    list("Elementary Curriculum", "Why Cottonwood? Small class sizes Personalized, student-centered approch to teaching and learning Highly experienced paraprofessional support in the classroom", "Teacher"))
  for (k in keep) expect_equal(role(k[[1]], k[[2]]), k[[3]], label = paste(k[[1]], substr(k[[2]], 1, 40)))
  # No hint at all: the title's bucket, as before.
  expect_equal(classify_k12_position(c("Music", "Custodian")), c("Teacher", "Custodial/Maintenance"))
  expect_equal(classify_k12_position(c("Music", "Custodian"), c(NA, NA)), c("Teacher", "Custodial/Maintenance"))
})
