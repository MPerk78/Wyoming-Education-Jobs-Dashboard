# Each state's committed reference tables against their rules
# (REFERENCE_TABLE_RULES and reference_table_problem() are in edjobs, with the
# rest of the reference-table tests). A rule for a table this state doesn't
# write is skipped.

test_that("every committed reference table passes its own rules", {
  app_dir <- if (dir.exists(here::here("Mt_Ed_Jobs"))) "Mt_Ed_Jobs" else "Wy_Ed_Jobs"
  for (name in names(edjobs::REFERENCE_TABLE_RULES)) {
    p <- here::here(app_dir, paste0(name, ".csv"))
    if (!file.exists(p)) next
    expect_null(edjobs::reference_table_problem(read.csv(p, stringsAsFactors = FALSE), edjobs::REFERENCE_TABLE_RULES[[name]]),
                info = name)
  }
})
