# State-only tests left here when the shared classifier and posted-pay tests moved to edjobs
# (phase 2, 2026-11-11): these call this state's own code (name corrections or scrapers).


test_that("WSBA's 'Cabon County School District 2' is Carbon #2, in the WSBA parser too", {
  expect_equal(canonicalize_k12_district("Cabon County School District 2"), "Carbon County School District 2")
  expect_equal(canonicalize_wsba_district("Cabon County School District #2"), "Carbon County School District 2")
})
