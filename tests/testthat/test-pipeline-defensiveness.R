test_that("all chunk-level fetch_* calls in Wy_ED_Jobs.Rmd are wrapped in safe_scrape or tryCatch", {
  rmd_path <- here::here("Wy_ED_Jobs.Rmd")
  expect_true(file.exists(rmd_path))

  lines <- readLines(rmd_path, warn = FALSE)
  chunk_starts <- which(grepl("^```[{]r", lines))
  chunk_ends <- which(grepl("^```$", lines))

  naked_calls <- list()

  for (start_idx in chunk_starts) {
    end_candidates <- chunk_ends[chunk_ends > start_idx]
    if (length(end_candidates) == 0) next
    end_idx <- end_candidates[1]

    header <- lines[start_idx]
    chunk_name <- sub("^```[{]r[[:space:]]*([^,}]+).*", "\\1", header)
    chunk_code <- lines[(start_idx + 1):(end_idx - 1)]
    code_text <- paste(chunk_code, collapse = "\n")

    parsed <- tryCatch(parse(text = code_text, keep.source = FALSE), error = function(e) NULL)
    if (is.null(parsed)) next

    walk_ast <- function(node, inside_guard = FALSE) {
      if (rlang::is_missing(node) || is.null(node)) return()
      if (is.call(node)) {
        fn_name <- tryCatch(deparse(node[[1]])[1], error = function(e) "")
        is_guard <- fn_name %in% c("safe_scrape", "tryCatch")
        now_inside <- inside_guard || is_guard

        bare_fn <- sub(".*::", "", fn_name)
        if (grepl("^fetch_", bare_fn) && !now_inside) {
          naked_calls[[length(naked_calls) + 1]] <<- list(
            chunk = chunk_name,
            call = bare_fn
          )
        }

        for (i in seq_along(node)) {
          if (i == 1) next
          arg <- node[[i]]
          if (!rlang::is_missing(arg) && !is.null(arg)) {
            walk_ast(arg, inside_guard = now_inside)
          }
        }
      } else if (is.expression(node) || (is.list(node) && !is.data.frame(node))) {
        for (i in seq_along(node)) {
          item <- node[[i]]
          if (!rlang::is_missing(item) && !is.null(item)) {
            walk_ast(item, inside_guard = inside_guard)
          }
        }
      }
    }

    walk_ast(parsed)
  }

  expect_equal(length(naked_calls), 0,
               info = paste(vapply(naked_calls, function(n) paste0("[", n$chunk, "]: ", n$call), character(1)), collapse = ", "))
})

test_that("Wy_ED_Jobs.Rmd does not contain typo %% modulo operator instead of %>% pipe", {
  rmd_path <- here::here("Wy_ED_Jobs.Rmd")
  lines <- readLines(rmd_path, warn = FALSE)
  typo_lines <- which(grepl("%%[[:space:]]*$", lines) | grepl("\\w+_df[[:space:]]*%%", lines))
  expect_equal(length(typo_lines), 0,
               info = paste("Typo %% operator found on line(s):", paste(typo_lines, collapse = ", ")))
})
