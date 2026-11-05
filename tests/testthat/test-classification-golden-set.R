# Golden labeled set (classification safeguards item 3; identical file and
# fixture in the MT and WY repos). fixtures/classification_golden_set.csv holds
# real archived titles from both states (since 2025-10-01, sampled across
# every bucket, 2026-11-05), each with its hand-checked bucket: `label` is
# position (classify_k12_position) for kind "k12" and Job_Type
# (classify_he_job_type) for kind "he". Titles whose right bucket was a
# judgment call were left out rather than guessed.
#
# known_miss = TRUE marks a title the classifier still gets wrong today; `note`
# says why. The test fails when
#   - any other title disagrees with its label (a classifier change broke
#     something it used to get right), or
#   - a known miss now agrees (a fix: set known_miss to FALSE and clear the
#     note, so the gain is kept).
# Every disagreement is listed. When you fix a misroute, add its title here
# (known_miss FALSE); when you find one you won't fix yet, add it with
# known_miss TRUE.

golden_set <- function() {
  g <- read.csv(test_path("fixtures", "classification_golden_set.csv"), stringsAsFactors = FALSE)
  g$predicted <- ifelse(g$kind == "k12", classify_k12_position(g$title), classify_he_job_type(g$title))
  g
}

describe_rows <- function(rows) {
  paste(sprintf("  [%s %s] \"%s\": classifier says %s, label %s%s", rows$kind, rows$state,
                gsub("\\s+", " ", trimws(rows$title)), rows$predicted, rows$label,
                ifelse(nzchar(rows$note), paste0(" (", rows$note, ")"), "")),
        collapse = "\n")
}

test_that("the golden set is well formed", {
  g <- golden_set()
  expect_setequal(unique(g$kind), c("k12", "he"))
  expect_false(any(duplicated(g[, c("kind", "title")])))
  expect_true(all(nzchar(g$note[g$known_miss])))
  # Every label is a bucket the classifier can produce.
  k12_buckets <- c("Substitute", "Paraprofessional", "Activities/Extracurricular", "Transportation", "Food Services",
                   "Athletics", "Support Services", "Staff", "Custodial/Maintenance", "Administration",
                   "Part-Time Teacher", "Teacher", "Child Care", "Other")
  expect_true(all(g$label[g$kind == "k12"] %in% k12_buckets))
  he_buckets <- c("Accounting/Finance", "Adjunct/Part-Time Faculty", "Administration", "Advising and Student Services",
                  "Coaching or Athletics", "Culinary", "Healthcare", "Instructor/Teacher/Faculty",
                  "Maintenance & Service", "Management", "Other", "Professional", "Research & Science", "Staff",
                  "Student Positions", "Support Services", "Technical", "Temporary Position")
  expect_true(all(g$label[g$kind == "he"] %in% he_buckets))
})

test_that("the classifier agrees with every golden label it got right before", {
  g <- golden_set()
  agree <- g$predicted == g$label
  for (k in c("k12", "he")) {
    s <- g$kind == k
    message(sprintf("Golden set, %s: %d of %d agree (%.1f%%); %d known misses.", k, sum(agree[s]), sum(s),
                    100 * mean(agree[s]), sum(g$known_miss[s])))
  }
  regressions <- g[!agree & !g$known_miss, ]
  expect(nrow(regressions) == 0,
         paste0(nrow(regressions), " golden title(s) now classified differently from their label:\n",
                describe_rows(regressions)))
  fixed <- g[agree & g$known_miss, ]
  expect(nrow(fixed) == 0,
         paste0(nrow(fixed), " known miss(es) now classified correctly -- set known_miss to FALSE and clear the note ",
                "in fixtures/classification_golden_set.csv:\n", describe_rows(fixed)))
})
