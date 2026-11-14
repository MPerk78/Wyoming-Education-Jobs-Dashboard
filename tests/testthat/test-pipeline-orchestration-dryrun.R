# Pipeline Orchestration Dry-Run Test
#
# Validates the execution of the main K-12 scraping and combination chunks in Wy_ED_Jobs.Rmd
# by executing chunk expressions against an isolated mock environment.
#
# Catches:
# 1. Pipeline syntax / operator typos (e.g., %% vs %>%)
# 2. Scope errors (e.g. object 'District' not found)
# 3. Join / transmute errors on empty or partial data frames
# 4. Incompatible types in bind_rows (e.g. <date> vs <character> date_posted)
# 5. Missing columns in direct / other before combination

extract_chunk_code <- function(rmd_lines, chunk_name) {
  start_pat <- paste0("^```[{]r[[:space:]]+", chunk_name)
  chunk_start <- which(grepl(start_pat, rmd_lines))
  if (length(chunk_start) == 0) stop("Chunk not found: ", chunk_name)
  chunk_ends <- which(grepl("^```$", rmd_lines))
  chunk_end <- chunk_ends[chunk_ends > chunk_start[1]][1]
  paste(rmd_lines[(chunk_start[1] + 1):(chunk_end - 1)], collapse = "\n")
}

test_that("Wy_ED_Jobs.Rmd K-12 scraping and combination chunks execute cleanly with mock data", {
  rmd_path <- here::here("Wy_ED_Jobs.Rmd")
  expect_true(file.exists(rmd_path))
  rmd_lines <- readLines(rmd_path, warn = FALSE)

  k12_scrape_code <- extract_chunk_code(rmd_lines, "loop with districts on a direct scraping platform")
  combine_code_full <- extract_chunk_code(rmd_lines, "combine all the data into one dataframe")
  combine_lines <- strsplit(combine_code_full, "\n")[[1]]
  combine_start_bind <- which(grepl("^combined\\s*<-\\s*(dplyr::)?bind_rows\\(", combine_lines))
  expect_true(length(combine_start_bind) > 0)
  # Statement ends at the mutate line
  combine_end_line <- combine_start_bind[1] + 1
  combine_code <- paste(combine_lines[1:combine_end_line], collapse = "\n")

  # Create mock environment
  env <- new.env(parent = globalenv())
  env$registry <- read.csv(here::here("k12_district_registry.csv"), stringsAsFactors = FALSE)
  env$safe_scrape <- function(name, scrape_fn, expected_cols = NULL, log_path = NULL) {
    if (is.null(expected_cols)) expected_cols <- c("title", "date_posted", "position", "location", "url", "District")
    vals <- as.list(rep("mock_val", length(expected_cols)))
    names(vals) <- expected_cols
    if ("date_posted" %in% expected_cols) vals$date_posted <- "2026-10-27"
    if ("Posted_Date" %in% expected_cols) vals$Posted_Date <- "2026-10-27"
    df <- as.data.frame(vals, stringsAsFactors = FALSE)
    if ("Posted_Date" %in% names(df)) df$Posted_Date <- as.character(df$Posted_Date)
    if ("date_posted" %in% names(df)) df$date_posted <- as.character(df$date_posted)
    df
  }
  env$WSBA_VACANCIES_URL <- "https://wsba-wy.org/vacancies/"
  env$remove_wsba_direct_duplicates <- function(wsba, direct, report_path = NULL) wsba

  withr::with_dir(here::here(), {
    expect_no_error(eval(parse(text = k12_scrape_code), envir = env))
    expect_no_error(eval(parse(text = combine_code), envir = env))
  })

  expect_true(exists("combined", envir = env))
  expect_true(nrow(env$combined) > 0)
  expect_s3_class(env$combined$date_posted, "Date")
})
