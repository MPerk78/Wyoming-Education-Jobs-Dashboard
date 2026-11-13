# Renders the weekly Rmd; if the workflow's `timeout -s INT` interrupts it
# (a step stuck long enough to threaten the job's time limit), prints R's call
# stack to the log first, so the run says where it was stuck instead of just
# "The operation was canceled" (WY, 2026-11-13).
# Usage: Rscript .github/scripts/render_with_stack.R <file.Rmd>
rmd <- commandArgs(TRUE)[1]
withCallingHandlers(
  rmarkdown::render(rmd),
  interrupt = function(e) {
    cat("\n=== Render interrupted at ", format(Sys.time(), "%H:%M:%S"),
        " (time budget). R call stack, innermost last:\n", sep = "", file = stderr())
    for (cl in utils::tail(sys.calls(), 40)) {
      cat("  ", paste(deparse(cl, nlines = 1L, width.cutoff = 200L), collapse = ""), "\n", file = stderr())
    }
  }
)
