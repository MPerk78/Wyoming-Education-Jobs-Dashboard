# Guards against safe_scrape()'s per-source error handling masking a
# systemic failure (e.g. the runner's IP getting blocked, a shared
# upstream outage). safe_scrape() deliberately lets individual sources
# fail without crashing the render, which is correct for one bad
# district but would be silently disastrous for an unattended weekly
# job if EVERY source failed at once -- that would render "successfully"
# while overwriting real data with a near-empty dataset. Fails loudly
# (non-zero exit) instead of letting the workflow proceed to commit.

old_k12 <- nrow(read.csv("/tmp/baseline_combinedclean.csv"))
new_k12 <- nrow(read.csv("Wy_Ed_Jobs/combinedclean.csv"))

old_he <- nrow(readxl::read_xlsx("/tmp/baseline_hedata.xlsx"))
new_he <- nrow(readxl::read_xlsx("Wy_Ed_Jobs/hedata.xlsx"))

cat(sprintf("K-12 rows: %d -> %d\n", old_k12, new_k12))
cat(sprintf("HE rows:   %d -> %d\n", old_he, new_he))

THRESHOLD <- 0.5

if (new_k12 < old_k12 * THRESHOLD) {
  stop(sprintf(
    "K-12 row count dropped from %d to %d (more than 50%%) -- looks like a systemic failure, not normal week-to-week churn. Refusing to commit.",
    old_k12, new_k12
  ))
}

if (new_he < old_he * THRESHOLD) {
  stop(sprintf(
    "HE row count dropped from %d to %d (more than 50%%) -- looks like a systemic failure, not normal week-to-week churn. Refusing to commit.",
    old_he, new_he
  ))
}

cat("Sanity check passed.\n")
