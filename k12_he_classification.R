# Shared classification/canonicalization logic for the K-12 and Higher Ed
# job-posting pipelines.
#
# Sourced from Wy_ED_Jobs.Rmd (both K-12 munge chunks and the Higher Ed munge
# chunk) so the two K-12 chunks can't drift apart, and reused verbatim by any
# one-off script that rebuilds the summary CSVs from the archived data
# (Archivek12_Data/, Archived_HE_Data/) without re-scraping. Keeping this in
# one file means "what the notebook will produce next real run" and "what a
# rebuild-from-archive script produces now" can never silently diverge.

suppressMessages(library(dplyr))

# A handful of scraped K-12 titles carry raw Windows-1252 bytes (e.g. an en
# dash in a time range like "10:00 am - 2:00 pm") that aren't valid UTF-8.
# grepl() can't validate those as UTF-8, warns "unable to translate ... to a
# wide string", and returns NA -- which case_when() treats as "no match", so
# the row silently falls through to "Other"/"Uncategorized" instead of being
# classified. Re-encoding first fixes the bad bytes.
#
# Unconditionally treating every title as Windows-1252 (this function's
# original behavior) is itself a bug: it corrupts titles that are already
# valid UTF-8 -- a genuine curly apostrophe ("Teacher's Aide") gets
# double-decoded into mojibake ("Teacherâ€™s Aide"). Only strings that
# actually fail a UTF-8 round-trip are repaired as CP1252; everything else
# passes through unchanged. Stray CRs (some sources embed raw \r) are also
# stripped here so they don't survive into archived/displayed titles.
fix_title_encoding <- function(title) {
  title <- as.character(title)
  missing <- is.na(title)
  valid_utf8 <- iconv(title, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  needs_cp1252_repair <- !missing & is.na(valid_utf8)

  out <- title
  out[needs_cp1252_repair] <- iconv(
    title[needs_cp1252_repair],
    from = "WINDOWS-1252",
    to = "UTF-8",
    sub = "byte"
  )
  out <- enc2utf8(out)
  # Unescape common HTML entities that appear in scraped job titles / board text
  out <- gsub("&amp;", "&", out, fixed = TRUE)
  out <- gsub("&quot;", "\"", out, fixed = TRUE)
  out <- gsub("&#39;|&apos;", "'", out, perl = TRUE)
  out <- gsub("&lt;", "<", out, fixed = TRUE)
  out <- gsub("&gt;", ">", out, fixed = TRUE)
  out <- gsub("&nbsp;|\u00a0", " ", out, perl = TRUE)
  out <- gsub("[\r\n]+", " ", out)
  out <- trimws(out)
  out[missing] <- NA_character_
  out
}

# ---------------------------------------------------------------------------
# K-12
# ---------------------------------------------------------------------------

# Coarse position bucket from the raw job title.
classify_k12_position <- function(title) {
  title <- fix_title_encoding(title)
  dplyr::case_when(
    # Substitute -- checked first, so every substitute posting lands here
    # whatever role it covers ("Substitute Paraprofessional", "Sub Bus Driver").
    # \bSubs?\b / "Substit", not bare "Sub" -- matched inside "Subject"/
    # "Suburban"/"Substance Abuse" otherwise. "Substit" keeps typos like
    # "Substiture"; "subs\b" keeps run-together "Kitchensubs".
    # "Sub-Varsity" is a coaching level, not a substitute (golden set).
    grepl("\\bSubs?\\b(?!-Varsity)|subs\\b|Substit", title, ignore.case = TRUE, perl = TRUE) ~ "Substitute",

    # A "Bus Paraprofessional" rides the bus: Transportation, like bus aides
    # and monitors below (2026-11-01 audit). Checked before Paraprofessional.
    # Only "Bus Para..." itself: dual roles such as "Bus Driver/Classroom
    # Paraprofessional" stay Paraprofessional.
    grepl("\\bBus\\s+Para", title, ignore.case = TRUE) ~ "Transportation",

    # Paraprofessional-related positions
    # \bPara, not bare "Para" -- matched inside "Preparation"/"Comparative"
    # otherwise; and not "Paradise" (Paradise Valley Elementary, Casper).
    grepl("\\bPara(?!dise)|Paraeducator|Teacher Assistant|Teacher'?s? Aides?\\b|Teaching Assistant|Instructional Assistant|Instructional Aide|Classroom Aide|Library Assistant|Media Assistant|Media Center Assistant|Education Assistant|Special Needs Assistant|\\bSNA\\b|\\bSPEA\\b|Testing Assistant|Personal Care Attendant", title, ignore.case = TRUE, perl = TRUE) ~ "Paraprofessional",

    # Teaching roles whose titles also carry a word claimed by an earlier
    # non-teaching rule below: ensemble "Band/Choir Director" (Administration)
    # and "Drivers Ed" (Transportation).
    grepl("\\b(Band|Choir|Choral|Orchestra|Music)\\s+Director\\b|\\bDriver'?s?\\s*Ed(ucation)?\\b|\\bTraffic Ed",
          title, ignore.case = TRUE) ~ "Teacher",

    # Activity sponsors, advisors and coaches (FFA, FBLA, drama, speech &
    # debate, robotics, color guard, student council, the school musical):
    # season stipends, like coaching, but not athletics. Checked before
    # Support Services and Administration, which would otherwise take
    # "Musical Director" or a "Lego Robotics Coordinator". A named activity
    # counts even in a "Coach" title ("Speech and Debate Coach"); a bare
    # advisor/sponsor/club word counts only without one, so "Head Coach or
    # Sponsor" and "Volleyball Coach (Club)" stay in Athletics. Cheer stays in
    # Athletics; drama or robotics teachers stay teachers; academic, career
    # and wellness advisors are staff roles; after-school clubs are child care.
    ((grepl("Student Council|Yearbook|\\bProm\\b|Color Guard|Lego League|Knowledge Bowl|Mock Trial|Science Olympiad|\\bDebate\\b|Forensics|Speech (Coach|Assistant|Sponsor)|\\bDrama\\b|\\bMusical\\b|\\bPlay\\b.*(Director|Sponsor)|Theat(er|re) Set|Robotics|\\b(FFA|FBLA|DECA|HOSA|FCCLA|BPA)\\b.*(Advis|Sponsor)|Co-Curricular Student Activities",
            title, ignore.case = TRUE) |
        (grepl("Advis[eo]r|Sponsor|\\bClub\\b", title, ignore.case = TRUE) &
           !grepl("Coach", title, ignore.case = TRUE))) &
       !grepl("Cheer|Teacher|Instructor|Academic Advis|Career|College|Financial|Wellness|Benefit|After School|Before School|Kids Club|Boys (and|&) Girls Club|Paraprofessional|\\bAides?\\b",
              title, ignore.case = TRUE)) ~ "Activities/Extracurricular",

    # Aides and monitors whose role is named by a specific bucket: coach
    # aides, kitchen/nutrition aides, bus aides and monitors. Checked before
    # Support Services, whose bare "Aide"/"Monitor" would otherwise take them.
    (grepl("\\bAid(e|es|s)?\\b", title, ignore.case = TRUE) &
       grepl("\\bCoach|Football|Basketball|Volleyball|Wrestling|Soccer|\\bSwim|\\bGolf\\b|\\bTrack\\b|Cross Country|Nordic|\\bSki\\b|Baseball|Softball|Tennis|Athletic",
             title, ignore.case = TRUE)) ~ "Athletics",
    (grepl("\\bAid(e|es|s)?\\b|Technician|Support Personnel", title, ignore.case = TRUE) &
       grepl("Food|Kitchen|Nutrition|Cafeteria|\\bSN\\b|\\bCook", title, ignore.case = TRUE)) ~ "Food Services",
    grepl("\\bBus\\b.*\\b(Aides?|Monitors?)\\b|\\b(Aides?|Monitors?)\\b.*\\bBus\\b", title, ignore.case = TRUE) ~ "Transportation",

    # Support Services-related positions
    # \bIntern\b, not bare "Intern" -- matched inside "Internal Applicants Only"
    # otherwise, pulling teacher and bus-driver postings into this bucket.
    grepl("Counselor|Psychologist|Therapist|Pathologist|Occupational|Alternative Behavior Program|Audiologist|Lifeguard|Interpreter|Translator|Monitor|Safety Patrol|Social Worker|Technology Intern|Tutor|Instructional Facilitator|Resource Officer|Behavior|Occupational|\\bAides?\\b|\\bAids?\\b|Support|\\bLPN\\b|Mental Health (Specialist|Clinician|Therapist)|\\bIntern(ship)?s?\\b|Crossing Guard|Crosswalk Guard|Noon Duty|Recess|Lunchroom Supervis|General Duty Assistant|\\bGDA\\b|Liaison|Advocate|Ambassador|Orientation and Mobility|\\bSLP|Speech[- ]Language|Physical Therapy|Accompanist|\\bPsych\\b|Security|Assistive Technolog",
          title, ignore.case = TRUE) ~ "Support Services",

    # Staff-related positions (checked before Teacher so clerical titles like "General Secretary - Library" land here).
    # "Administrative Assistant" is clerical, not Administration (golden set, 2026-11-07).
    grepl("Administrative (Assistant|Secretary|Clerk)|Admin\\.? Asst|Secretary|Clerk|Office Manager|Clerical|Registrar|Payroll|Human Resources|Bookkeeper|Receptionist|Accounts (Payable|Receivable)|Purchasing|Accountant|Accounting|Benefits Specialist|Business (Services|Office) Specialist|Office Assistant|Records Assistant", title, ignore.case = TRUE) ~ "Staff",

    # Custodial/Maintenance-related positions
    grepl("Custodian|Custodial|Janitor|Maintenance|Cleaner|Snow Removal|Facility|Electrician|HVAC|Grounds|Lawn|Housekeep|Carpenter|Painter|Plumber|Laborer|Sweeper|Warehouse|Field Crew|Mower|Mowing",
          title, ignore.case = TRUE) ~ "Custodial/Maintenance",

    # Transportation-related positions
    # \bBus\b, not bare "Bus" -- matched inside "Business" otherwise.
    grepl("Bus Driver|Driver|Transportation|Route|\\bBus\\b|Mechanic|Bus Mechanic|Bus Monitor|Dispatch",
          title, ignore.case = TRUE) ~ "Transportation",

    # District IT and technology staff (help desk, technology specialists and
    # assistants, student tech workers). Directors and coordinators of
    # technology stay in Administration below; technology teachers stay
    # teachers.
    (grepl("Technology (Specialist|Assistant|Worker|Support|Summer|Job)|Information Technology|\\bIT\\b|Help ?Desk|Tech (Crew|Workers?)|Cyber Center|Information Systems Specialist|Data/Information Specialist",
           title, ignore.case = TRUE) &
       !grepl("Teacher|Instructor|Director|Coordinator", title, ignore.case = TRUE)) ~ "Support Services",

    # Technicians not already claimed by Custodial/Maintenance or Transportation
    # above; cafeteria technicians fall through to Food Services below.
    (grepl("Technician", title, ignore.case = TRUE) &
       !grepl("Food|Cook|Nutrition|Cafeteria|Dishwasher", title, ignore.case = TRUE)) ~ "Support Services",

    # Administration-related positions
    # "Dean of", not bare "Dean" -- Dean Morgan is a Casper school name.
    grepl("Principal|Director|Coordinator|Administrator|Supervisor|Administrative|Superintendent|Business ?Manager|\\bChief\\b|Dean of|General Counsel",
          title, ignore.case = TRUE) ~ "Administration",

    # Part-time / reduced-FTE teaching positions -- kept out of the full-FTE
    # "Teacher" bucket so they don't get counted equally with full teacher
    # postings in the subject-category trend charts.
    (grepl("Teacher|Instructor|Interventionist|English|Math|Library|Media|Language Arts|Science|Educator",
           title, ignore.case = TRUE) &
       grepl("Part[- ]?Time|Half[- ]?Time|\\b1/[2-9]th\\b|\\b0?\\.[1-9]\\s*FTE\\b",
             title, ignore.case = TRUE)) ~ "Part-Time Teacher",

    # Teacher-related positions
    grepl("Teacher|Instructor|Interventionist|English|Math|Library|Media|Language Arts|Science|Educator",
          title, ignore.case = TRUE) ~ "Teacher",

    # School nurses and counseling roles -- checked after Staff, Administration
    # and Teacher so "Nurse Educator", "General Secretary - Counseling" and
    # "Administrative Assistant - Counseling Office" keep those buckets.
    grepl("Nurse|Counseling", title, ignore.case = TRUE) ~ "Support Services",

    # Coach-related positions
    # "Skiing" not bare "Ski" -- bare "Ski" matches inside "Life SKIlls".
    grepl("Coach|Athletic|Sports|Track|Football|Basketball|Golf|Soccer|Wrestling|Cheer|BB|Volleyball|Aquatics|Skiing|Baseball|Softball|Swim|Cross Country|Tennis|Hockey|Rodeo|Intramural",
          title, ignore.case = TRUE) ~ "Athletics",

    # Food Services-related positions
    grepl("Food|Cook|Nutrition|Cafeteria|Dishwasher|Kitchen|Lunch|Meal|Feeding|Breakfast", title, ignore.case = TRUE) ~ "Food Services",

    # Student Teaching-related positions
    grepl("Daycare|After School|Head Start|Before School|Early Bird|Lights On|Child ?care", title, ignore.case = TRUE) ~ "Child Care",

    # Titles that are only a subject or grade ("Elementary Curriculum",
    # "Physics", "Art HS 7-12", "K - 12 Librarian") -- OPI Jobs for Teachers
    # publishes its subject-area field as the title -- are teaching jobs as
    # long as nothing marks them as an assistant/advisor-type role.
    (classify_k12_subject(title) != "Uncategorized" &
       !grepl("Assistant|Aide|Sponsor|Advisor|Stipend|Club|Worker|Volunteer|Student Teach|Opportunit|Activity|League|Builder|Information Systems|Business Office|Business Services|Accounting|(?<!Case )Manager|Compliance|Technologist",
              title, ignore.case = TRUE, perl = TRUE)) ~ "Teacher",

    # A title that is only a grade band ("Junior High 7-8", posted on Lodge
    # Grass's Certified tab) is a teaching job, like the subject-only titles
    # above.
    grepl("^(Junior High|Jr\\.? High|Middle School|Elementary|High School|HS)\\s+(Grades?\\s*)?(K|[0-9]{1,2})\\s*-\\s*[0-9]{1,2}$",
          trimws(title), ignore.case = TRUE) ~ "Teacher",

    # Other positions
    TRUE ~ "Other"
  )
}

# Fine-grained subject category, computed only for position == "Teacher" rows.
#
# 2026-08-04 audit: checked real title samples against the "Uncategorized"
# bucket (14.7% of all Teacher postings historically). Additions below are
# each backed by real observed titles, not speculative -- see the audit
# notes in the project's memory/commit history for the specific samples.
k12_category_keywords <- list(
  # Bare "CTE" / "Career and Technical" live in "CTE Teacher" below, checked
  # last, so "CTE - Agricultural Education" / "CTE - Business and Marketing" /
  # "CTE - Family and Consumer Sciences" reach their specific subject first.
  "Technical Education" = "\\b(Welding|Weld|WeldingTeacher|Automotive|Auto(?!nomy)|Wood|Woods|Woodworking|Industrial Arts|Industrial Tech(nology)?|Shop|Tech Ed|Technical Education|Trade and Technical|Drivers?\\s*Ed(ucation)?|Traffic Ed(ucation)?)\\b",
  "Agriculture Education" = "(\\b(Agricultur(e|al)|Ag Teacher|Ag Program|Agricultural teacher|FFA|Vo[-/]?Ag)\\b|\\(AG\\))",
  "Computer Science Education" = "\\b(Computer Science|Information Technology|\\bIT\\b|Technology Education(?! Teacher Assistant)|Technology Teacher)\\b",
  # Pre[- ]?school covers "Preschool"/"Pre-school"/"Pre school" in one
  # pattern; Head Start is a real, common federal early-childhood program
  # name that doesn't otherwise contain any of the other keywords here.
  "Early Childhood Education" = "\\b(Early Childhood|Pre\\s*[- ]?\\s*K(?!indergarten)|Pre\\s*[- ]?\\s*school|Birth to age|Ages 3-5|Head Start|Toddler|Daycare|Early Learning|\\bSACC\\b)\\b",
  "Elementary Education" = "\\b(Elementary|Kindergarten|1st|2nd|3rd|4th|5th|First|Second|Third|Fourth|Fifth|Grade [1-7]|Grades?\\s*K-[1-7]|K-[1-7]\\s*Teacher|Classroom Teacher|Colony Teacher|Sixth Grade|6th Grade|Seventh Grade|7th Grade|Primary Grade|Primary Teacher)\\b",
  # "Literacy/Reading Interventionist" specifically (not bare "Interventionist"
  # or "Academic Interventionist" -- those don't name a subject and are
  # genuinely ambiguous, left as Uncategorized rather than guessed).
  "English Language Arts Education" = "(?i)\\b(English(?!\\s+as\\s+a\\s+Second\\s+Language|\\s+Learner|\\s+Language\\s+Learner)|ELA|Language Arts(?!\\s*Coordinator)|(Literacy|Reading) Interventionist|Title\\s*(One|1|I)\\b|Reading Recovery|Literacy Teacher|Reading Teacher|Literacy Specialist|Reading and Writing|Intervention Teacher|Teacher Reading|Reading|Journalism)\\b",
  "Family and Consumer Science" = "\\b(Family and Consumer Sciences?|FCS|Home Economics|Culinary)\\b",
  "Language Education" = "\\b(ESL|ELL|English as a Second Language|English Learner|Bilingual|Spanish|French|Dual Language|World Languages?|Foreign Languages?|English Language Learner|Arapaho|Arapahoe|Shoshone|Salish|Kootenai|Cree|Crow|Blackfeet|Cheyenne|Assiniboine|Lakota|Dakota|Native Language|Indigenous Language|Culture Teacher|\\bEL Teacher\\b|\\bELL Teacher\\b|\\bESL Teacher\\b)\\b",
  "Mathematics Education" = "\\b(Mathematics|\\bMath\\b)\\b",
  "Music Education" = "\\b(Music|Orchestra|Band|Choir|Chorus|Vocal)\\b",
  "Health Science" = "(?i)\\b(Nurse Educator|Health Science(s)? Teacher|Health Science(s)?|Certified Nursing Assistant|Nursing Assistant|CNA)\\b",
  "Science Education" = "\\b(Science|Biology|Chemistry|Physics|Earth Science)\\b",
  "Social Studies Education" = "\\b(Social Studies|History|Geography|Civics|Political Science|Psychology|Sociology|\\bSS Teacher\\b)\\b",
  # Special Education was the single largest category by a wide margin
  # (3,239 -- more than the next two categories combined), so it's split
  # into two here rather than left as one monolithic bucket: program types
  # that are specifically resource-room/life-skills/functional programs,
  # vs. general/core special education. Checked in that order (more
  # specific first) since a title can plausibly contain both kinds of
  # words.
  "Special Education - Resource/Life Skills" = "\\b(Resource (Teacher|Room)|Life Skills|Affective Needs|Daily Living Skills|Special Needs|\\bPALS\\b|\\bSE - TEACHER LS\\b|\\bACP\\b|Alternative Communication(s)? Program)\\b",
  "Special Education - General" = "\\b(Special[- ]Educ[a-z]*|Special Ed\\.?\\b|SPED|Exceptional Children|Deaf|Visually Impaired|Autism|\\bASD\\b|\\bSE - TEACHER\\b|\\bSE TEACHER\\b|\\bSp\\.?\\s*Ed\\b|Disabilities Educator)\\b",
  "Physical Education" = "(?i)\\b(Physical Education|P\\.?E\\.?|PE\\s*/\\s*Health|Health\\s*/\\s*PE|Health and Physical Education|Phys\\.?\\s*Ed\\.?|Physical Ed|Health Enhancement|Water Safety|Pickleball|Yoga|Aquatics|Lifeguard Instructor)\\b",
  "Business and Economics" = "\\b(Business|Economics|Econ|Financial Literacy)\\b",
  # "Guest Teacher" is a common euphemism for substitute teacher used by
  # some districts/staffing agencies (e.g. Kelly Education), not a distinct
  # role.
  "Substitute Teaching" = "(?i)\\b(Substitute Teacher|Sub Teacher|Guest Teacher(s)?)\\b",
  "Virtual Education" = "\\b(Virtual|Remote|Online)\\b",
  "Art Teacher" = "\\b(Art Teacher|\\bArt\\b|Visual Art|Performing Arts|Photography|Pottery|Sculpture|Drawing|Ceramics|Theater|Theatre|Drama)\\b",
  "STEM Teacher" = "\\b(STEM|STEAM|Robotics)\\b",
  # JROTC/military-science instructors fold into CTE, matching how many
  # state frameworks classify JROTC.
  "CTE Teacher" = "\\b(Career and Technic(i)?al|CTE|CTE Teacher|JROTC|ROTC|Military Instructor|Aviation Teacher)\\b",
  "Library Media" = "\\b(Library|Librarian|Media Specialist|Library Media)\\b",
  "Gifted and Talented" = "\\b(Gifted|Talented|GATE|Advanced Learning)\\b"
)

classify_k12_subject <- function(title) {
  title <- fix_title_encoding(title)
  kw <- k12_category_keywords
  dplyr::case_when(
    stringr::str_detect(title, stringr::regex(kw[["Special Education - Resource/Life Skills"]], ignore_case = TRUE)) ~ "Special Education - Resource/Life Skills",
    stringr::str_detect(title, stringr::regex(kw[["Special Education - General"]], ignore_case = TRUE)) ~ "Special Education - General",
    stringr::str_detect(title, stringr::regex(kw[["Technical Education"]], ignore_case = TRUE)) ~ "Technical Education",
    stringr::str_detect(title, stringr::regex(kw[["Agriculture Education"]], ignore_case = TRUE)) ~ "Agriculture Education",
    stringr::str_detect(title, stringr::regex(kw[["Computer Science Education"]], ignore_case = TRUE)) ~ "Computer Science Education",
    stringr::str_detect(title, stringr::regex(kw[["Early Childhood Education"]], ignore_case = TRUE)) ~ "Early Childhood Education",
    stringr::str_detect(title, stringr::regex(kw[["Elementary Education"]], ignore_case = TRUE)) ~ "Elementary Education",
    stringr::str_detect(title, stringr::regex(kw[["English Language Arts Education"]], ignore_case = TRUE)) ~ "English Language Arts Education",
    stringr::str_detect(title, stringr::regex(kw[["Family and Consumer Science"]], ignore_case = TRUE)) ~ "Family and Consumer Science",
    stringr::str_detect(title, stringr::regex(kw[["Language Education"]], ignore_case = TRUE)) ~ "Language Education",
    stringr::str_detect(title, stringr::regex(kw[["Mathematics Education"]], ignore_case = TRUE)) ~ "Mathematics Education",
    stringr::str_detect(title, stringr::regex(kw[["Music Education"]], ignore_case = TRUE)) ~ "Music Education",
    stringr::str_detect(title, stringr::regex(kw[["Physical Education"]], ignore_case = TRUE)) ~ "Physical Education",
    stringr::str_detect(title, stringr::regex(kw[["Health Science"]], ignore_case = TRUE)) ~ "Health Science",
    stringr::str_detect(title, stringr::regex(kw[["Science Education"]], ignore_case = TRUE)) ~ "Science Education",
    stringr::str_detect(title, stringr::regex(kw[["Social Studies Education"]], ignore_case = TRUE)) ~ "Social Studies Education",
    stringr::str_detect(title, stringr::regex(kw[["Business and Economics"]], ignore_case = TRUE)) ~ "Business and Economics",
    stringr::str_detect(title, stringr::regex(kw[["Substitute Teaching"]], ignore_case = TRUE)) ~ "Substitute Teaching",
    stringr::str_detect(title, stringr::regex(kw[["Virtual Education"]], ignore_case = TRUE)) ~ "Virtual Education",
    stringr::str_detect(title, stringr::regex(kw[["Art Teacher"]], ignore_case = TRUE)) ~ "Art Teacher",
    stringr::str_detect(title, stringr::regex(kw[["STEM Teacher"]], ignore_case = TRUE)) ~ "STEM Teacher",
    stringr::str_detect(title, stringr::regex(kw[["CTE Teacher"]], ignore_case = TRUE)) ~ "CTE Teacher",
    stringr::str_detect(title, stringr::regex(kw[["Library Media"]], ignore_case = TRUE)) ~ "Library Media",
    stringr::str_detect(title, stringr::regex(kw[["Gifted and Talented"]], ignore_case = TRUE)) ~ "Gifted and Talented",
    TRUE ~ "Uncategorized"
  )
}

# 2026-08-04 audit: rebalanced two oversized, artificially-merged buckets --
# "STEM" (Math + Science + generic STEM Teacher combined, 1,506 postings)
# splits into real Math/Science departments; "CTE" (7 unrelated subjects
# combined, 772 postings) splits into a trades/agriculture/technical group
# and a business/family-and-health-sciences group. Special Education's own
# split happens earlier, at the subject-category level (see
# k12_category_keywords above), and passes through unchanged here.
classify_k12_broad_category <- function(category) {
  dplyr::case_when(
    category == "Agriculture Education" ~ "CTE - Trades, Ag & Technical",
    category == "Art Teacher" ~ "Art",
    category == "Business and Economics" ~ "CTE - Business & Family Sciences",
    category == "Computer Science Education" ~ "CTE - Business & Family Sciences",
    category == "Early Childhood Education" ~ "Early Childhood",
    category == "Elementary Education" ~ "Elementary",
    category == "English Language Arts Education" ~ "English Language Arts Secondary",
    category == "Family and Consumer Science" ~ "CTE - Business & Family Sciences",
    category == "Health Science" ~ "CTE - Business & Family Sciences",
    category == "Language Education" ~ "Language",
    category == "Mathematics Education" ~ "Math",
    category == "Music Education" ~ "Music",
    category == "Physical Education" ~ "Physical Education",
    category == "STEM Teacher" ~ "Science",
    category == "Science Education" ~ "Science",
    category == "Social Studies Education" ~ "Secondary Social Studies",
    category == "Special Education - General" ~ "Special Education - General",
    category == "Special Education - Resource/Life Skills" ~ "Special Education - Resource/Life Skills",
    category == "Technical Education" ~ "CTE - Trades, Ag & Technical",
    category == "CTE Teacher" ~ "CTE - Trades, Ag & Technical",
    category == "Uncategorized" ~ "Other",
    category == "Virtual Education" ~ "Other",
    category == "Library Media" ~ "Library Media",
    category == "Gifted and Talented" ~ "Gifted and Talented",
    TRUE ~ "Other"
  )
}

# Fixes known misspellings of district names at the source so the same
# district doesn't get silently split into two "different" districts in the
# dashboard's dropdowns/trend lines.
canonicalize_k12_district <- function(district) {
  dplyr::case_when(
    district == "Hot Springs School District 1" ~ "Hot Springs County School District 1",
    district == "Crook County school District 1" ~ "Crook County School District 1",
    district == "Albany Count School District 1" ~ "Albany County School District 1",
    district == "Campbell County Scholl District 1" ~ "Campbell County School District 1",
    district == "Converse County School Distrcit 2" ~ "Converse County School District 2",
    district == "Fremont County School District #21" ~ "Fremont County School District 21",
    district == "Washakie County School District No.1" ~ "Washakie County School District 1",
    district == "Cabon County School District 2" ~ "Carbon County School District 2",   # WSBA, 2026-10-30
    TRUE ~ district
  )
}

# Returns a snapshot-stable K-12 posting identity. Platforms that expose a
# durable source ID supply it directly; older sources fall back to a
# normalized fingerprint of the information they do expose. Archive_Date is
# deliberately excluded so one posting keeps the same identity across weeks.
# Fallback parts are joined with " | ": the "\r" used until 2026-11 landed in
# the CSVs as a line break inside the field.
build_k12_posting_id <- function(source_id = NULL, title, location,
                                 date_posted, url, district) {
  values <- list(source_id, title, location, date_posted, url, district)
  lengths <- lengths(values)
  n <- max(c(lengths, 1L))

  recycle <- function(x) {
    if (length(x) == 0) return(rep(NA_character_, n))
    if (length(x) == 1) return(rep(as.character(x), n))
    if (length(x) != n) {
      stop("build_k12_posting_id(): inputs must have length 1 or a shared length")
    }
    as.character(x)
  }

  normalize <- function(x) {
    x <- trimws(tolower(fix_title_encoding(recycle(x))))
    x[is.na(x) | !nzchar(x)] <- "<missing>"
    x
  }

  source_id <- normalize(source_id)
  # A rebuild from the raw archive passes each week's stored ID back in as
  # source_id. A stored fallback fingerprint is rebuilt from the fields, so
  # a rebuild gives the same ID the weekly run did.
  source_id[startsWith(source_id, "fallback")] <- "<missing>"
  fallback <- paste(
    "fallback",
    normalize(district),
    normalize(url),
    normalize(title),
    normalize(location),
    normalize(date_posted),
    sep = " | "
  )

  ifelse(source_id == "<missing>", fallback, source_id)
}

# Keeps the first row per posting_id. Runs right after the byte-identical
# distinct(), to catch rows that build_k12_posting_id() already treats as
# one posting but distinct() doesn't: the same source ID emitted twice with
# differing text, or fallback-keyed rows differing only in case, surrounding
# whitespace, a mis-encoded byte, or NA vs "" (all normalized away in the
# ID). Left in, those inflate k12_district_weekly_totals (a raw count()) and
# fail test-weekly-data-quality.R's posting_id-uniqueness gate, which
# blocked the 2026-09-25 weekly run. The dropped rows are written to stderr
# so the CI log shows which source produced them.
collapse_duplicate_posting_ids <- function(df, label = "K-12 postings") {
  dup <- duplicated(df$posting_id)
  if (any(dup)) {
    ids <- unique(df$posting_id[dup])
    cat(sprintf("%s: collapsing %d row(s) that share a posting_id with an earlier row:\n",
                label, sum(dup)), file = stderr())
    cat(utils::capture.output(print(df[df$posting_id %in% ids, , drop = FALSE])),
        sep = "\n", file = stderr())
  }
  df[!dup, , drop = FALSE]
}

# Board entries that are not job openings: account-creation templates,
# "future/sample position" placeholders, student-teaching notices, and a
# scraped table header ("Job Title"). Dropped before any count, in both the
# weekly pipeline and the rebuild-from-archive scripts. Standing applicant
# pools (e.g. "Applicant Pool - Substitute Teacher") are real, ongoing
# postings and are kept.
is_placeholder_posting <- function(title) {
  t <- trimws(as.character(title))
  !is.na(t) & grepl(
    paste0("^(Sample|Future) (Certified|Classified) Positions?$|",
           "Use This Template to Create An Account|",
           "^Student Teaching Opportunit|",
           "^Extra-Curricular Positions for|",
           "^No (open |current )?(positions|openings|vacancies)|^None at this time$|",
           "^Volunteers?$|",
           "^No Jobs|H-?1 ?B Hiring Notification|",
           "^Job Title$"),
    t, ignore.case = TRUE
  )
}

# ---------------------------------------------------------------------------
# Posted pay
# ---------------------------------------------------------------------------

# Pulls the advertised pay out of a posting's free text: the amount (or
# range), its unit, and the snippet it came from, so every figure can be
# traced back to the posting's own words. Returns one row per input text;
# all-NA when the posting states no dollar amount. Amounts tied to bonuses,
# fees, reimbursements or benefits are ignored, and an amount preceded by a
# pay word ("Salary:", "Wage", "Stipend") wins over an earlier bare one.
extract_posted_pay <- function(text) {
  n <- length(text)
  out <- data.frame(Pay_Min = rep(NA_real_, n), Pay_Max = NA_real_,
                    Pay_Unit = NA_character_, Pay_Text = NA_character_,
                    stringsAsFactors = FALSE)
  # "\\*{0,2}" tolerates footnote marks ("$20.60* - $21.95*/hr").
  # Up to three decimals: Montana University System classified rates are
  # posted to the tenth of a cent ("$17.820 to $22.00 hourly").
  amount <- "\\$\\s?((?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d{1,3})?)\\s?([kK](?![a-z]))?\\*{0,2}"
  # A range may repeat the unit after its low end ("$23.684/hr - $29.604/hr").
  pattern <- paste0(amount, "(?:(?:\\s?/\\s?(?:hr|hour))?\\s*(?:-|\u2013|\u2014|to)(?:\\s+(?:a\\s+)?max(?:imum)?(?:\\s+of)?)?\\s*\\$?\\s?((?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d{1,3})?)\\s?([kK](?![a-z]))?\\*{0,2})?")
  to_num <- function(x, k) {
    v <- suppressWarnings(as.numeric(gsub(",", "", x)))
    ifelse(!is.na(k) & nzchar(k), v * 1000, v)
  }
  for (i in seq_len(n)) {
    t <- text[i]
    if (is.na(t) || !grepl("$", t, fixed = TRUE)) next
    t <- gsub("[[:space:] ]+", " ", t)
    # Boards that drop the line break after a field run the unit into the
    # next label ("PAY RATE: $145/dayBENEFITS: None", "$146.09 per dayIf you").
    t <- gsub("\\b(hourly|hour|hr|daily|day|monthly|month|yearly|year|yr|annually|annual|annum)(?=[A-Z])", "\\1 ", t, perl = TRUE)
    m <- gregexpr(pattern, t, perl = TRUE)[[1]]
    if (m[1] == -1) next
    starts <- as.integer(m); lens <- attr(m, "match.length")
    cs <- attr(m, "capture.start"); cl <- attr(m, "capture.length")
    best <- NULL; best_score <- -Inf; skipped_end <- NA_integer_
    for (j in seq_along(starts)) {
      before <- tolower(substr(t, max(1, starts[j] - 60), starts[j] - 1))
      after <- tolower(substr(t, starts[j] + lens[j], starts[j] + lens[j] + 45))
      near_before <- substr(before, max(1, nchar(before) - 30), nchar(before))
      # Only the amount's own sentence decides whether it's a bonus/fee, so a
      # "signing bonus." just before "Starting salary: $X" doesn't veto $X.
      near_before <- sub("^.*[.;!]\\s", "", near_before)
      # Nor does an earlier amount's own wording: in "Signing bonus $1,000,
      # $42,000 annual salary" the bonus word belongs to $1,000.
      near_before <- sub("^.*\\$\\s?\\d[0-9,.]*[kK]?", "", near_before)
      not_pay <- grepl("bonus|fee|\\bcosts?\\b|reimburs|insurance|retire|deductible|allowance|incentive|signing|relocation|tuition|premium|contribut|match|donat|scholarship|award|grant of|differential|repayment|forgiveness|\\bloan|assistance", paste(near_before, substr(after, 1, 25))) ||
        # "up to $3,000 of loan repayment": a ceiling on a benefit, not pay --
        # unless a pay word leads it ("Salary: up to $60,000").
        (grepl("up to\\s*$", near_before) && !grepl("salary|wage|pay|rate|compensation", near_before)) ||
        # An amount said to go toward something else ("$800/month toward
        # health, dental and vision", "$200 monthly for housing").
        # Also money paid into a benefit ("$11,446 into a flex benefits package").
        grepl("^[^.;]{0,15}\\b(for|toward|towards|into)\\b[^.;]{0,12}(housing|rent|utilit|health|dental|vision|medical|insurance|benefit)", after) ||
        # A benefits package's stated value ("Benefits at FVCC (Worth ~$25K)")
        # isn't pay. Only value wording counts, so "benefits eligible, $45,000"
        # and "$52,000 plus benefits" are still read as pay.
        grepl("\\bworth\\b|valued at|benefits? (package )?value", near_before) ||
        # District housing offered with the job ("3 houses and 7 apartments
        # starting at $350 per month") is a cost, not pay.
        grepl("apartment|housing|\\bhouses?\\b|\\brent\\b|\\brental|\\blease|teacherage|utilities", near_before)
      # An amount continuing a list that starts with a skipped one ("Loan
      # Assistance of up to $3,000, $4,000 & $5,000") is skipped with it,
      # unless it carries its own pay unit ("bonus $1,000, $42,000 annual salary").
      chained <- !is.na(skipped_end) && grepl("^\\s*(,|&|and|or)?\\s*$", substr(t, skipped_end + 1, starts[j] - 1)) &&
        !grepl("^[^a-z]{0,3}(/|per|an|a)?\\s*(hour|hr\\b|day\\b|daily|month|year|yr\\b|annual|salary|wage)", after)
      if (not_pay || chained) {
        skipped_end <- starts[j] + lens[j] - 1
        next
      }
      cap <- function(k) if (cl[j, k] > 0) substr(t, cs[j, k], cs[j, k] + cl[j, k] - 1) else NA_character_
      lo <- to_num(cap(1), cap(2)); hi <- if (!is.na(cap(3))) to_num(cap(3), cap(4)) else lo
      # "$0.00" is a board's blank pay field (NEOGOV prints "Adjunct - $0.00
      # Hourly" when none is entered), not a wage.
      if (is.na(lo) || max(lo, hi, na.rm = TRUE) <= 0) next
      unit <- dplyr::case_when(
        grepl("^[^a-z]{0,3}(/|per|an|a)?\\s*(hour|hr\\b)|^[^a-z]{0,3}hourly", after) ~ "hour",
        grepl("^[^a-z]{0,3}(/|per|a)?\\s*((whole|half|full) )?(day\\b|daily)|^[^a-z]{0,3}per diem", after) |
          grepl("daily rate[^.]{0,60}$", before) ~ "day",
        grepl("^[^a-z]{0,3}(/|per|a)?\\s*(month|mo\\b)|^[^a-z]{0,3}monthly", after) ~ "month",
        grepl("^[^a-z]{0,3}(/|per|a)?\\s*(year|yr\\b|annum)|^[^a-z]{0,3}annual", after) ~ "year",
        grepl("^[^a-z]{0,3}(/|per|a)?\\s*(1\\s+)?(semester\\s+)?credit", after) ~ "credit hour",
        grepl("^[^a-z]{0,3}(/|per|a)?\\s*(season|unit|event|game|stipend)", after) |
          grepl("stipend[^.$]{0,20}$", before) ~ "stipend",
        grepl("hourly[^.$]{0,25}$|hour(ly)? (rate|wage)[^.$]{0,15}$|per hour[^.$]{0,10}$", before) ~ "hour",
        grepl("(annual|yearly) (salary|pay)[^.$]{0,20}$", before) ~ "year",
        hi >= 15000 ~ "year",
        # No unit next to the amount: infer one only when the rest of the
        # posting backs it up -- a classified pay-grade figure ("Salary:
        # SEC-II, $22.45") in a posting that describes hours per day/week is
        # hourly; a few-thousand-dollar amount on a coaching/advisor posting
        # is a season stipend.
        hi >= 7 & hi <= 100 & grepl("\\bhours?\\b|\\bhrs?\\b", tolower(t)) ~ "hour",
        hi >= 250 & hi < 15000 & grepl("coach|advisor|sponsor|stipend|season|extra[- ]?duty|extracurricular", tolower(t)) ~ "stipend",
        TRUE ~ NA_character_
      )
      # An "hourly" figure in the thousands, or a "yearly" one under $1,000,
      # is a misread unit -- keep the amount, drop the unit.
      if (!is.na(unit) && ((unit == "hour" && hi > 500) || (unit == "year" && hi < 1000))) unit <- NA_character_
      score <- (if (grepl("salary|wage|pay|rate|stipend|compensation|starting|step|lane|schedule", near_before)) 2 else 0) +
        (if (!is.na(unit)) 1 else 0)
      if (score > best_score) {
        best_score <- score
        snippet <- trimws(substr(t, max(1, starts[j] - 50), starts[j] + lens[j] + 35))
        best <- list(lo = min(lo, hi), hi = max(lo, hi), unit = unit, text = snippet)
      }
    }
    if (!is.null(best)) {
      out$Pay_Min[i] <- best$lo; out$Pay_Max[i] <- best$hi
      out$Pay_Unit[i] <- best$unit; out$Pay_Text[i] <- best$text
    }
  }
  out
}

POSTED_PAY_COLUMNS <- c("Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text")

# Guarantees the posted-pay columns exist (NA where a source states no pay),
# so combinedclean.csv keeps one schema whether or not this run's sources --
# or a rebuild from archives written before pay was captured -- carry them.
ensure_posted_pay_columns <- function(df) {
  for (col in POSTED_PAY_COLUMNS) {
    if (!col %in% names(df)) df[[col]] <- rep(if (col %in% c("Pay_Min", "Pay_Max")) NA_real_ else NA_character_, nrow(df))
  }
  df
}

# The range a posted amount can plausibly take in each unit. An amount
# outside it is a misread (a fee, a benefit value, a phone number), not
# pay. Generous on purpose: real postings seen so far run $11-$100 an hour,
# $68-$350 a day, $35,780-$175,000 a year, $60-$5,670 a stipend and $775 a
# credit hour. "unknown" covers amounts with no unit.
PAY_PLAUSIBLE_RANGE <- list(
  hour = c(7, 200), day = c(30, 1500), month = c(300, 30000),
  year = c(10000, 500000), `credit hour` = c(50, 5000), stipend = c(10, 60000),
  unknown = c(1, 500000)
)

# TRUE where a row's posted pay is out of range for its unit, has an
# unknown unit name, or runs low > high. Rows without pay are FALSE.
is_implausible_pay <- function(pay_min, pay_max, pay_unit) {
  unit <- ifelse(is.na(pay_unit), "unknown", pay_unit)
  lo <- vapply(unit, function(u) if (u %in% names(PAY_PLAUSIBLE_RANGE)) PAY_PLAUSIBLE_RANGE[[u]][1] else NA_real_, numeric(1), USE.NAMES = FALSE)
  hi <- vapply(unit, function(u) if (u %in% names(PAY_PLAUSIBLE_RANGE)) PAY_PLAUSIBLE_RANGE[[u]][2] else NA_real_, numeric(1), USE.NAMES = FALSE)
  has_pay <- !is.na(pay_min) | !is.na(pay_max)
  pmin_ <- dplyr::coalesce(pay_min, pay_max); pmax_ <- dplyr::coalesce(pay_max, pay_min)
  has_pay & (is.na(lo) | pmin_ < lo | pmax_ > hi | pmin_ > pmax_)
}

# Blanks the Pay_* columns of rows whose posted pay is implausible, so a
# misread never reaches the dashboard. The raw archive keeps them as
# scraped; check_pay_coverage.R lists them in the weekly drift report.
drop_implausible_pay <- function(df) {
  bad <- is_implausible_pay(df$Pay_Min, df$Pay_Max, df$Pay_Unit)
  if (any(bad)) {
    message("Posted pay: blanked ", sum(bad), " implausible amount(s)")
    df$Pay_Min[bad] <- NA_real_; df$Pay_Max[bad] <- NA_real_
    df$Pay_Unit[bad] <- NA_character_; df$Pay_Text[bad] <- NA_character_
  }
  df
}

# ---------------------------------------------------------------------------
# Higher Ed
# ---------------------------------------------------------------------------

# Coarse job-type bucket from the raw posting title. Adjunct/pooled titles
# get their own bucket, separate from "Instructor/Teacher/Faculty", so
# standing adjunct-pool postings don't get counted the same as full-time
# faculty hiring in the faculty trend charts.
classify_he_job_type <- function(title) {
  # UW posts every hourly job as "Hourly Pooled - <job>"; the job after the
  # prefix decides the bucket (a student cook is Culinary, a custodian is
  # Maintenance & Service). Hourly student, intern and peer-mentor jobs are
  # Student Positions, whatever their title says ("Branding Iron
  # Editor-in-Chief", "Student Manager, Womens Volleyball"); an hourly job
  # can't be an administrator or manager, and hourly jobs matching nothing
  # else stay office Staff, as before.
  title <- as.character(title)
  hourly_pooled <- grepl("^\\s*Hourly\\s*Pooled", title, ignore.case = TRUE)
  stripped <- sub("^\\s*Hourly\\s*Pooled\\s*[-\u2013:]*\\s*:?\\s*", "", title, ignore.case = TRUE)
  type <- classify_he_job_type_rules(ifelse(hourly_pooled, stripped, title))
  dplyr::case_when(
    !hourly_pooled ~ type,
    grepl("\\bStudents?\\b|\\bIntern(ship)?s?\\b|Peer Mentor|Branding Iron|Student Media|Work[- ]?Study", stripped, ignore.case = TRUE) ~ "Student Positions",
    type %in% c("Other", "Administration", "Management") ~ "Staff",
    TRUE ~ type
  )
}

classify_he_job_type_rules <- function(title) {
  dplyr::case_when(
    # Campus child-center / classroom aides and instructional support staff
    # aren't faculty. Checked first: "Classroom Aide/Substitute Teacher
    # (Part-Time)" otherwise hits the Part-Time + Teacher adjunct rule, and
    # bare "Instructional" (Technologist, Designer, Lab Coordinator) used to
    # land these in the faculty bucket.
    grepl("Classroom Aide|Instructional Aide|Instructional Assistant|Teacher Aide|Teacher Assistant|Assistant Teacher|Associate Teacher|Children.s (Discovery )?Center|\\bECLC\\b",
          title, ignore.case = TRUE) ~ "Support Services",
    grepl("Instructional (Technologist|Designer|Design|Technology|Specialist|Lab)", title, ignore.case = TRUE) ~ "Professional",

    # Adjunct / part-time pool positions. A bare "Part-Time" is not enough on
    # its own -- plenty of real non-faculty postings ("Custodian 1-Part-Time",
    # "Major Gift Officer - Part-time", "Part-Time Clinical Resource
    # Registered Nurse") also contain that phrase, so it only counts here
    # alongside an actual faculty-role word.
    # Standing applicant pools for faculty ("Spanish Non-Tenure Track Faculty
    # (Applicant Pool)", "Part-time Faculty Pool - Accounting") are the
    # adjunct pipeline too. Pools of teachers (campus child centers) and of
    # staff ("Cook II (Applicant Pool)") are not.
    (grepl("Adjunct", title, ignore.case = TRUE) |
       (grepl("Part[- ]?Time", title, ignore.case = TRUE) &
          grepl("Faculty|Instructor|Professor|Lecturer|Teacher", title, ignore.case = TRUE)) |
       (grepl("\\bPool\\b", title, ignore.case = TRUE) &
          grepl("Faculty|Instructor|Lecturer", title, ignore.case = TRUE))) &
      !grepl("Post ?Doc|Postdoc|Post[- ]?Doctoral", title, ignore.case = TRUE) ~
      "Adjunct/Part-Time Faculty",

    # Postdocs and research scientists/associates are research staff, not
    # faculty (checked before the faculty rule's "Post Doc" and before
    # Healthcare's "Medicine"). "Research Associate Professor" stays faculty.
    grepl("Post ?Doc|Postdoc|Post[- ]?Doctoral|Research (Scientist|Associate)", title, ignore.case = TRUE) &
      !grepl("Professor", title, ignore.case = TRUE) ~ "Research & Science",

    # Administration & Faculty Affairs (checked before teaching faculty so administrative Provost roles don't land in faculty)
    grepl("Faculty Affairs|Faculty Development|Provost Office|Office of the Provost", title, ignore.case = TRUE) ~ "Administration",

    # Instructor/Teacher/Faculty
    grepl("Instructor|Instructional Professor|Teacher|Faculty|Professor|Lecturer|Post Doc|Postdoc|Subject Matter Expert|Librarian|Educator", title, ignore.case = TRUE) ~ "Instructor/Teacher/Faculty",

    # Student Positions
    grepl("Student Worker|Student Position|Work[- ]?Study|Students Only|Student Library Aid|Student Employment|Student Assistant|Student Staff|Student Lab Assistant|Student Ambassador|\\bFWS\\b", title, ignore.case = TRUE) ~ "Student Positions",

    # Healthcare
    grepl("Nurse|Wellbeing|Health|Medicine|Medical Support Assistant|\\bRN\\b|Dietitian|Psychiatric|Practice Provider", title, ignore.case = TRUE) ~ "Healthcare",

    # Temporary Position
    grepl("Pooled Position|Temporary|Monthly Pooled", title, ignore.case = TRUE) ~ "Temporary Position",

    # Staff (consolidated all office assistant/associate roles)
    grepl("Hourly|Administrative Assistant|Administrative Associate|Secretary|Library Assistant|Office Assistant|Office Associate",
          title, ignore.case = TRUE) ~ "Staff",

    # Advising
    # Academic, success and retention coaches advise students; they aren't
    # athletics (golden set, 2026-11-07: "Student Success Coach", "GEAR UP
    # Success Coach", "Academic Coach Talent Search").
    grepl("(Academic|Success|Retention|Career|Completion|Enrollment|College|Life|Navigation)\\s+Coach|GEAR UP|Advisor|Advising|Enrollment Counselor|Financial Aid|Tutor|Career Peer|Tribal Education Assistant|Night ESL Class Aide|Writing Consultant|Learning Commons|Testing Center|Camp Counselor", title, ignore.case = TRUE) ~ "Advising and Student Services",

    # Culinary -- checked before Maintenance & Service so "Cook"/"Baker"
    # land here instead of being swallowed by Maintenance & Service's own
    # copy of the same words.
    grepl("Chef|Cook|Baker|Dining|Culinary|Barista|Caf[eé]|Dishwasher|Kitchen|Catering|Food Service|Line Server", title, ignore.case = TRUE) ~ "Culinary",

    # Professional
    grepl("Admissions|Coordinator|\\bCoord\\b|Registrar|Executive Assistant|Professional|Statistician|Proctor|Exec Assistant|Grant Writer|Graphic Designer|Negotiator|Botanist|College Relations|Controller|Database|Officer|Auditor|Web Site|Special Assistant|Retention Mentor|Buyer Assistant|Buyer, Textbooks|GEAR UP Event & College Coordination|Ombudsperson|Data Analyst|Employee Relations Specialist|Finance Technician|Grant Support Specialist|Marketing & Comm Spec.|Program Specialist",
          title, ignore.case = TRUE) ~ "Professional",

    # Administration
    grepl("\\bDean\\b|Vice President|Chancellor|Provost|Chief|Business Manager|Enrollment Specialist|Finance Technician|HR Records Specialist", title, ignore.case = TRUE) ~ "Administration",

    # Coaching or Athletics
    grepl("coach|Athletic|Athletics", title, ignore.case = TRUE) ~ "Coaching or Athletics",

    # Accounting/Finance
    grepl("Accountant|Receivable|Accounts|Payroll|Bursar|Accounting|Cashier|Financial Associate|Storekeeper", title, ignore.case = TRUE) ~ "Accounting/Finance",

    # Research & Science (only actual research/scientific roles)
    grepl("Research Associate|Laboratory Technician|Research Scientist|Research Assistant|Lab Assistant|Laboratory Assistant|Lab/Clinical|Farm M|Researcher|ologist\\b|Research Pilot", title, ignore.case = TRUE) ~ "Research & Science",

    # Technical -- bare "Specialist" excluded (it shadows later, more
    # specific buckets like Support Services' "Library Information
    # Specialist"; only qualified "<Thing> Specialist" phrases stay here).
    grepl("Control Specialist|Engineer|IT Support Technician|Information Security Analyst|Programmer Analyst|Network Specialist|Data Operation Engineer|Design Engineer|GIS Specialist|Systems? Administrator|Network Administrator|Software|Information Technology|\\bI\\. ?T\\.|Computer|Linux|Developer|Programmer|\\bIT\\b", title, ignore.case = TRUE) ~ "Technical",

    # Maintenance & Service ("Cook"/"Baker" excluded -- see Culinary above)
    grepl("Maintenance|Technician|Facilities|Grounds|Custodian|HVAC|Plumber|Carpenter|Logistics Handler|Glazier|Painter|Operator|Mechanic|Flooring|Electrician|Locksmith|Custodial|Warehouse|Mail|Postal|Shipping|Receiving|Foreman|Stage Tech|Scene Shop", title, ignore.case = TRUE) ~ "Maintenance & Service",

    # Support Services
    grepl("Safety|Security|Police|Dispatch|Fire Marshal|Interpreter|Custodian|Laborer|Driver|Cleaner|Facilities|Grounds|Library Information Specialist|Museum Exhibit Specialist|Specialist, Student Services|Lifeguard|Resident Assistant|Campus Services I|Outdoor Recreation Lab Room|Bookstore Sales Clerk|Student Service Assistant|Aide|Monitor|Attendant|Scorekeeper|Referee|Accompanist|Stall Assistant",
          title, ignore.case = TRUE) ~ "Support Services",

    # Management ("Coordinator" excluded -- Professional's own copy of that
    # keyword is checked earlier and always wins, making a copy here dead
    # code).
    grepl("Director|\\bDirector\\b|Manager|Supervisor|Assoc Director|Assoc Dir|Assist Dir|Asst Dir",
          title, ignore.case = TRUE) ~ "Management",

    # Student-facing counseling/mentoring roles, checked after Management so
    # "Assistant Director, Academic Support Center" stays Management.
    grepl("Counselor|Mentor|Navigator|Student Success|\\bTRIO\\b|Student Engagement|Academic Support", title, ignore.case = TRUE) ~ "Advising and Student Services",

    # Generic professional titles, checked last so the specific
    # "<Thing> Specialist"/"Analyst" phrases above keep their own buckets.
    grepl("Specialist|Analyst|Marketing|Communications|Writer|Editor|Producer|Curator|Archivist|Buyer|Consultant|Generalist|Extension Agent|Counsel\\b|Attorney|Investigator|Reporter|Journalist|Announcer|Photographer|Designer|Planner|Program Lead|Partner\\b|Recruiter",
          title, ignore.case = TRUE) ~ "Professional",

    # Remaining generic assistant/associate/clerk titles are office staff.
    grepl("Assistant|Associate|Clerk|\\bAid\\b", title, ignore.case = TRUE) ~ "Staff",

    TRUE ~ "Other"
  )
}

# Subject category, computed only for Job_Type == "Instructor/Teacher/Faculty" rows.
classify_he_faculty_category <- function(title) {
  dplyr::case_when(
    # 1. Legal & Criminal Justice
    grepl("criminal justice|criminology|corrections|law enforcement|police|police science|forensic|homeland security|justice studies", title, ignore.case = TRUE) ~ "Criminal Justice",
    grepl("\\blaw\\b|legal|college of law|attorney|paralegal|juris doctor|\\bjd\\b|crimin(al)? law", title, ignore.case = TRUE) ~ "Legal",

    # 2. Extension / Outreach
    grepl("extension educator|extension specialist|\\bextension\\b|4-h", title, ignore.case = TRUE) ~ "Extension/Outreach",

    # 3. Culinary & Hospitality (before Business so Hospitality in College of Business lands here)
    grepl("culinary|hospitality|tourism|baking|food service|restaurant|hotel|brewing|event planning", title, ignore.case = TRUE) ~ "Culinary/Hospitality",

    # 4. Human Services / Social Work / Counseling (before Education / Health)
    grepl("human services?|social work|\\bmsw\\b|counseling|counselor|mental health|substance abuse|addiction|chemical dependency|\\bhdch\\b|child development|youth", title, ignore.case = TRUE) ~ "Human Services",

    # Plant/animal/range sciences are Science; "Plant Pathology" would
    # otherwise hit Health Sciences' "pathology".
    grepl("plant scien|plant patholog|animal (and range )?scien|range scien", title, ignore.case = TRUE) ~ "Science",

    # 5. CTE - Health Sciences (before general Science so "Health Sciences" lands here)
    grepl("health technology|nursing|\\brn\\b|\\blpn\\b|dental|\\bems\\b|paramedic|emergency medical|emergency care|\\becp\\b|medical|medicine|nutritional|nutrition|dietetics|pharmacy|pharmacist|global health|public health|allied health|health sciences?|physician assistant|physical therapy|occupational therapy|speech-language|pathology|audiol|respiratory therapy|surgical tech|phlebotom|radiolog|radiograph|x-ray|sonograph|ultrasound|biomed|clinical|medical assistant|massage|respiratory|cardiovascular|\\bdms\\b|\\bcna\\b|nurs(e|ing) assistant|physical therapist|\\bemt\\b|\\bcpr\\b|first aid|\\bfwwson\\b", title, ignore.case = TRUE) ~ "CTE - Health Sciences",

    # 6. Social Science (before general Science so "Political Science", "Social Sciences", "Native American Studies" land here)
    grepl("social sciences?|political science|politics|government|public policy|public administration|tribal governance|native american studies|indigenous studies|ethnic studies|african american studies|chicano|latinx|gender|sexuality|women|international relations|global studies|sociology|sociological|anthropology|psychology|psychological|behavior|geography|urban planning", title, ignore.case = TRUE) ~ "Social Science",

    # 7. Languages (including Indigenous and non-European)
    grepl("spanish|foreign language|world language|french|german|japanese|arabic|chinese|russian|italian|\\blatin\\b|greek|salish|kootenai|cree|crow|blackfeet|cheyenne|assiniboine|lakota|dakota|shoshone|arapaho|linguistics|\\basl\\b|sign language|\\besl\\b|\\bell\\b|tesol|language instructor", title, ignore.case = TRUE) ~ "Language",

    # 8. History
    grepl("history|historical|heritage|museum studies", title, ignore.case = TRUE) ~ "History",

    # 9. Physical Education
    grepl("physical education|\\bpe\\b|health and human performance|yoga|outdoor program|outdoor activity|outdoor recreation|recreation|athletic train|kinesiology|strength|sports management|exercise science|fitness|wellness|activity class|coach", title, ignore.case = TRUE) ~ "Physical Education",

    # Instructional design/technology is Education, checked ahead of The
    # Arts' bare "design".
    grepl("instructional design|instructional technolog", title, ignore.case = TRUE) ~ "Education",

    # 10. The Arts
    grepl("\\bart\\b|visual art|fine arts?|studio art|graphic design|design|theater|theatre|drama|\\bstage\\b|\\bacting\\b|music|musical|vocal|choir|choral|instrumental|\\bband\\b|orchestra|piano|adobe|creative cloud|ceramics|sculpture|painting|drawing|photography|photo|film|cinema|animation|media arts?|digital media|\\bdance|jewelry|metalsmithing", title, ignore.case = TRUE) ~ "The Arts",

    # 11. Education (including instructional technology)
    grepl("instructional technology|instructional design|early childhood|preschool|daycare|child care|asum child care|elementary education|secondary education|special education|\\bsped\\b|teacher education|teacher prep|reading specialist|stem education|learning center|college success|pedagogy|curriculum|leadership fellows|educational leadership|education|educational|\\babe\\b|\\bged\\b|hiset|adult education|\\bae instructor|\\bstem\\b|makerspace|upward bound|gear up|disabilit|career readiness", title, ignore.case = TRUE) ~ "Education",

    # 12. Library
    grepl("library|librarian|library media|archivist|archives|information literacy", title, ignore.case = TRUE) ~ "Library",

    # 13. Math
    grepl("math|mathematics|calculus|algebra|geometry|quantitative|statistics|biostatistics|actuarial|data analytics", title, ignore.case = TRUE) ~ "Math",

    # 14. Humanities
    grepl("philosophy|english|communication|literature|mass communication|humanities|liberal studies|liberal arts|texts and critics|public speaking|rhetoric|writing|creative writing|journalism|religious studies|religion|ethics|folklore|classics|american studies|cultural studies|cls seminar|theology|broadcasting|media studies|speech|comp\\.?\\s*/\\s*lit|composition", title, ignore.case = TRUE) ~ "Humanities",

    # 15. CTE - Business & Computing (includes Agricultural Economics BEFORE general Agricultural!)
    grepl("agricultural economics|agricultural business|computer science|computer|information technology|computing|cybersecurity|data science|software|informatics|information systems|\\bcis\\b|network|web development|artificial intelligence|\\bai\\b|machine learning|business|accounting|finance|financial planning|family financial|economics|\\becon\\b|management|marketing|commerce|entrepreneurship|\\bmba\\b|business administration|real estate|banking|\\biv&v\\b|verification & validation", title, ignore.case = TRUE) ~ "CTE - Business & Computing",

    # 16. CTE - Trades & Engineering
    grepl("agricultural communications|\\bagricultural\\b|\\bag mechanics?\\b|ag instructor|college of ag|heavy equipment|\\bheo\\b|mechanic|welding|construction|hvac|electrical|powerline|lineman|substation|diesel|automotive|auto body|\\bcdl\\b|commercial driver|truck driving|transportation|drafting|architecture|architect|aviation|pilot|flight|\\buas\\b|drone|unmanned aerial|surveying|geomatics|mining|machine tool|manufacturing|industrial|printmaking|supply chain|logistics|carpentry|plumbing|engineering|engineer|energy|equine|commercial motor vehicle|\\bcmv\\b|driver training|driving|\\bmsha\\b|metallurg|gunsmith|pipe ?fitt|electronic|ag technology|\\bcad\\b|technical skills|cosmetology|\\bcnc\\b|machin", title, ignore.case = TRUE) ~ "CTE - Trades & Engineering",

    # 17. Science (Zoology now here; Political Science & Health Sciences safely filtered above)
    grepl("chemistry|biology|physics|astronomy|\\blab\\b|laboratory|geology|geoscience|earth science|environmental science|environmental studies|ecology|ecological|agronom|agriculture|animal science|veterinary|vet tech|plant science|soils?|hydrology|water resources|forestry|wildlife|natural resources|conservation|fisheries|range(land)? management|microbiology|biochem|neuroscience|genetics|genomic|anatomy|physiology|botany|zoology|meteorology|atmospheric|\\bmsse\\b|science", title, ignore.case = TRUE) ~ "Science",

    TRUE ~ "Uncategorized"
  )
}

# Fixes the "Commmunity" (triple-m) misspelling that Eastern Wyoming
# Community College's scrape block used until it was corrected at the
# source; without this, the college's pre-2026-01-09 history and its
# current postings silently split into two different "institutions" the
# next time the full archive gets rebuilt.
canonicalize_he_institution <- function(institution) {
  dplyr::case_when(
    institution == "Eastern Wyoming Commmunity College" ~ "Eastern Wyoming Community College",
    TRUE ~ institution
  )
}

# ---------------------------------------------------------------------------
# Detail riding in a title (classification safeguards item 5, the title-shape
# lint's findings of 2026-11-05). Some boards put more than the job's name in
# the title: a requirement ("Bus Drivers- Must have a CDL with a school bus
# passenger endorsement.", Huntley Project), the pay ("Secretary - $14 per
# hour, up to 32 hours per week", Turner), a status note ("Special Education
# Teachers - Open Until Filled.", Fremont #21), or an uploaded file's name
# ("Dish_Server_Cashier 26 - Copy", Livingston). split_title_detail() cuts the
# title at the first separator (" - ", "- ", " (", ", ") whose remainder opens
# with such detail and returns the job's name and the detail. Titles with no
# such remainder come back unchanged.
# ---------------------------------------------------------------------------
TITLE_DETAIL_OPENING <- paste0(
  "^(?i)(must\\b|required\\b|requires\\b|requirements?\\b|open until filled|until filled|ouf\\b|",
  "\\$\\s?\\d)")

split_title_detail <- function(title) {
  t <- as.character(title)
  detail <- rep(NA_character_, length(t))
  ok <- !is.na(t)
  # An uploaded file's name: underscores for spaces, a trailing " - Copy" and
  # a school year ("Dish_Server_Cashier 26 - Copy", "Baker_Cook 2026 27").
  year_suffix <- "\\s+(\\d{4}([ -]\\d{2,4})?|\\d{2})$"
  bare <- sub(year_suffix, "", sub("\\s-\\sCopy$", "", t))
  file_name <- ok & grepl("[A-Za-z]{2,}_[A-Za-z]{2,}", t) & !grepl("\\s", bare)
  t[file_name] <- trimws(gsub("_", " ", bare[file_name]))
  t[ok] <- sub("\\s-\\sCopy$", "", t[ok])
  sep <- "(\\s[-–—]\\s|[-–—]\\s|\\s\\(|,\\s)"
  for (i in which(ok)) {
    hits <- gregexpr(sep, t[i], perl = TRUE)[[1]]
    if (hits[1] == -1) next
    lens <- attr(hits, "match.length")
    for (k in seq_along(hits)) {
      head <- trimws(substr(t[i], 1, hits[k] - 1))
      tail <- trimws(substr(t[i], hits[k] + lens[k], nchar(t[i])))
      if (nchar(head) >= 3 && grepl(TITLE_DETAIL_OPENING, tail, perl = TRUE)) {
        detail[i] <- sub("\\)$", "", sub("\\.$", "", tail))
        t[i] <- sub("[-–—,\\s]+$", "", head, perl = TRUE)
        break
      }
    }
  }
  data.frame(title = t, detail = detail, stringsAsFactors = FALSE)
}

# Applies split_title_detail() to a K-12 frame's `title`, and fills Posted Pay
# from the detail where the row carries none ("$14 per hour" in Turner's
# title). Run at ingestion, before the raw archive is written, so archives
# keep the cleaned title and a rebuild of older archives is unchanged.
move_title_detail <- function(df) {
  if (nrow(df) == 0) return(df)
  s <- split_title_detail(df$title)
  df$title <- s$title
  # Pay columns are added only when a title carried pay: WY's archives have
  # none when no source stated pay that week.
  need <- !is.na(s$detail) & grepl("\\$\\s?\\d", s$detail) &
    (if ("Pay_Min" %in% names(df)) is.na(df$Pay_Min) else TRUE)
  if (any(need)) {
    df <- ensure_posted_pay_columns(df)
    pay <- extract_posted_pay(s$detail[need])
    for (col in POSTED_PAY_COLUMNS) df[[col]][need] <- pay[[col]]
  }
  df
}

# ---------------------------------------------------------------------------
# Per-posting job IDs (posting_spells.R; remove_wsba_direct_duplicates()).
# ---------------------------------------------------------------------------
# A per-posting ID from the link, keyed by platform and number, or NA.
k12_link_posting_id <- function(url) {
  if (is.null(url)) return(character(0))
  u <- tolower(trimws(fix_title_encoding(as.character(url)))); u[is.na(u)] <- ""
  out <- rep(NA_character_, length(u))
  pick <- function(pattern, fmt) {
    hit <- is.na(out) & grepl(pattern, u, perl = TRUE)
    out[hit] <<- sub(paste0("^.*?", pattern, ".*$"), fmt, u[hit], perl = TRUE)
  }
  pick("applitrack\\.com/([^/]+)/.*applitrackjobid=(\\d+)", "applitrack:\\1:\\2")
  pick("([a-z0-9-]+)\\.schoolspring\\.com/jobs/(\\d+)", "schoolspring:\\2")
  pick("redroverk12\\.com/org/([^/]+)/opening/(\\d+)", "redrover:\\1:\\2")
  pick("([a-z0-9-]+)\\.tedk12\\.com/hire/viewjob\\.aspx\\?jobid=(\\d+)", "tedk12:\\1:\\2")
  out
}

# A source posting_id the archive stored ("applitrack:bighorn1:1534",
# "schoolspring:5569023"), normalized to the same form as the link ID.
k12_source_posting_id <- function(posting_id) {
  p <- tolower(trimws(as.character(posting_id)))
  p[is.na(p) | !nzchar(p) | startsWith(p, "fallback")] <- NA_character_
  p <- sub("^schoolspring:[^0-9]*?(\\d+)$", "schoolspring:\\1", p)
  p
}
