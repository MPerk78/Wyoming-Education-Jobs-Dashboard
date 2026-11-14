# Each state's committed reference tables against their rules
# (reference_table_rules(): edjobs's shared REFERENCE_TABLE_RULES plus this
# state's own from state_config.R; the rest of the reference-table tests are in
# edjobs). A rule for a table this state doesn't write is skipped.

test_that("every committed reference table passes its own rules", {
  app_dir <- if (dir.exists(here::here("Mt_Ed_Jobs"))) "Mt_Ed_Jobs" else "Wy_Ed_Jobs"
  rules <- edjobs::reference_table_rules()
  for (name in names(rules)) {
    p <- here::here(app_dir, paste0(name, ".csv"))
    if (!file.exists(p)) next
    expect_null(edjobs::reference_table_problem(read.csv(p, stringsAsFactors = FALSE), rules[[name]]),
                info = name)
  }
})

test_that("this state's config is set, and its own tables have rules", {
  cfg <- edjobs::current_state_config()
  expect_false(is.null(cfg))
  expect_equal(cfg$app_dir, "Wy_Ed_Jobs")
  expect_true(all(names(cfg$reference_tables) %in% names(edjobs::reference_table_rules())))
  # Every table this state refreshes through write_reference_file() has rules.
  for (f in c("uw_salary_book"))
    expect_false(is.null(edjobs::reference_table_rules()[[f]]), info = f)
})
