# Tier-1 boot smoke -- ported from the Montana dashboard (PR #5), modelled
# on the LASSO project's tests/smoke.R.
#
# test-app-reactives.R drives the server() function with shiny::testServer(),
# which (per Mastering Shiny) "ignores the UI entirely" -- inputs start NULL,
# no JavaScript, no htmlwidgets. This file boots the REAL Wy_Ed_Jobs/app.R
# in a headless browser against the real committed CSVs and checks it renders
# end to end: every tab, no Shiny error banner anywhere, and the leaflet /
# plotly / DT widgets actually came back.
#
# It answers "does the app run on this data shape without falling over", not
# "are the numbers right" (that's test-app-reactives.R + test-weekly-data-
# quality.R). It exists so a refactor of app.R doesn't have to start from a
# blank manual click-through.
#
# Needs shinytest2 (pulls chromote / headless Chrome). ~1-2 min. Skips
# unless NOT_CRAN=true and a headless Chrome is resolvable.

skip_on_cran()
skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
        "no headless Chrome available")

`%||%` <- function(a, b) if (is.null(a)) b else a

app_dir <- here::here("Wy_Ed_Jobs")
skip_if_not(dir.exists(app_dir), "Wy_Ed_Jobs/ not found")
skip_if_not(file.exists(file.path(app_dir, "combinedclean.csv")),
            "committed CSVs not found -- run the pipeline first")

# every leaf tab a user can actually land on (k12_root / he_root are
# menuItem containers, not destinations)
LEAF_TABS <- c(
  "intro", "map_tab", "data_tab",
  "k12_table", "k12_summary", "k12_trends", "k12_spells",
  "he_table", "he_summary", "he_trends", "he_spells"
)

# One AppDriver for the whole file (booting bslib + leaflet +
# plotly + DT takes ~15s; don't pay it per test).
app <- NULL
withr::defer(if (!is.null(app)) app$stop(), teardown_env())

test_that("app.R boots against the committed data without an error", {
  app <<- shinytest2::AppDriver$new(
    app_dir,
    name = "wy-ed-jobs-boot",
    load_timeout = 60 * 1000,
    timeout = 30 * 1000,
    seed = 1
  )
  app$wait_for_idle(duration = 1000, timeout = 30 * 1000)

  expect_null(app$get_html(".shiny-output-error"),
              info = "a Shiny output errored on the initial Home tab")

  logs <- app$get_logs()
  js_errors <- logs[!is.na(logs$level) & logs$level %in% c("error", "severe"), ]
  hard <- js_errors[grepl("Uncaught|ReferenceError|TypeError|shiny.*error",
                          js_errors$message, ignore.case = TRUE), ]
  expect_equal(nrow(hard), 0,
               info = paste(utils::capture.output(print(hard[, c("level", "message")])),
                            collapse = "\n"))
})

test_that("every tab renders with no Shiny output error", {
  skip_if(is.null(app), "app failed to boot")

  for (tab in LEAF_TABS) {
    app$set_inputs(sidebar_tabs = tab)
    app$wait_for_idle(duration = 800, timeout = 20 * 1000)

    err <- app$get_html(".shiny-output-error")
    expect_null(err, info = sprintf("tab '%s' rendered a Shiny output error:\n%s",
                                    tab, if (is.null(err)) "" else substr(err, 1, 800)))
  }
})

test_that("the map, a longitudinal plot, and a jobs table actually render their widgets", {
  skip_if(is.null(app), "app failed to boot")

  app$set_inputs(sidebar_tabs = "map_tab")
  app$wait_for_idle(duration = 1500, timeout = 25 * 1000)
  map_html <- app$get_html("#combined_map")
  expect_false(is.null(map_html), info = "leaflet map container missing")
  expect_true(grepl("leaflet-container|leaflet-tile|leaflet-marker|leaflet-pane",
                    map_html %||% ""),
              info = "leaflet map rendered no tiles/markers/panes")

  app$set_inputs(sidebar_tabs = "k12_trends")
  app$wait_for_idle(duration = 1500, timeout = 25 * 1000)
  plot_html <- app$get_html("#k12_longitudinal_plot")
  expect_false(is.null(plot_html), info = "k12 longitudinal plotly container missing")
  expect_true(grepl("plotly|svg|js-plotly-plot", plot_html %||% ""),
              info = "k12 longitudinal plot rendered no plotly/svg")

  app$set_inputs(sidebar_tabs = "k12_table")
  app$wait_for_idle(duration = 1500, timeout = 25 * 1000)
  dt_html <- app$get_html("#k12_jobs")
  expect_false(is.null(dt_html), info = "k12 jobs DT container missing")
  expect_true(grepl("dataTable|dataTables_wrapper|<td", dt_html %||% ""),
              info = "k12 jobs table rendered no rows")
})

test_that("HE trends and HE jobs table render (the merged-entity / Vacancy_Rate_Shared path)", {
  skip_if(is.null(app), "app failed to boot")

  app$set_inputs(sidebar_tabs = "he_trends")
  app$wait_for_idle(duration = 1500, timeout = 25 * 1000)
  he_plot <- app$get_html("#he_longitudinal_plot")
  expect_false(is.null(he_plot), info = "he longitudinal plotly container missing")
  expect_true(grepl("plotly|svg|js-plotly-plot", he_plot %||% ""),
              info = "he longitudinal plot rendered no plotly/svg")

  app$set_inputs(sidebar_tabs = "he_table")
  app$wait_for_idle(duration = 1500, timeout = 25 * 1000)
  expect_null(app$get_html(".shiny-output-error"),
              info = "HE Jobs Table errored")
})

test_that("clicking 'View all jobs' from a map popup jumps to the filtered K-12 Jobs Table", {
  skip_if(is.null(app), "app failed to boot")

  # simulate the popup link's Shiny.setInputValue('popup_view_jobs', <name>).
  # Use a district that's actually on the map (has lat/long via salarymap2).
  sm <- utils::read.csv(here::here("Wy_Ed_Jobs", "salarymap2.csv"), stringsAsFactors = FALSE)
  a_district <- sm$District[!is.na(sm$Latitude)][1]
  app$set_inputs(popup_view_jobs = a_district, allow_no_input_binding_ = TRUE)
  app$wait_for_idle(duration = 1000, timeout = 20 * 1000)

  expect_equal(app$get_value(input = "sidebar_tabs"), "k12_table",
               info = "popup_view_jobs did not switch to the K-12 Jobs Table")
  expect_null(app$get_html(".shiny-output-error"),
              info = "the filtered Jobs Table errored after a map-popup jump")
})
