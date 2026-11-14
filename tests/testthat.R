# Test runner. From the repo root:
#   Rscript tests/testthat.R
# or, from an R session with the working directory anywhere inside the repo:
#   testthat::test_dir(here::here("tests", "testthat"))

library(testthat)
library(here)

test_dir(here::here("tests", "testthat"))
