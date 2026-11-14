# State-only tests left here when the shared classifier and posted-pay tests moved to edjobs
# (phase 2, 2026-11-11): these call this state's own code (name corrections or scrapers).


test_that("canonicalize_he_institution fixes the Eastern Wyoming typo and passes through everything else", {
  # Regression: "Commmunity" (triple-m) was corrected mid-history in the
  # scraper but old archives still carried it, silently splitting the
  # college's trend line in two on a full rebuild.
  expect_equal(canonicalize_he_institution("Eastern Wyoming Commmunity College"), "Eastern Wyoming Community College")
  expect_equal(canonicalize_he_institution("Casper College"), "Casper College")
})
