#!/usr/bin/env Rscript
# scripts/generate_job_duration_spells.R
#
# Generates or refreshes Wy_Ed_Jobs/job_duration_spells.csv from accumulated
# k12jobanalysis.csv longitudinal postings data, linking federal NCES_LEAID
# and deriving vacancy durations, time-to-fill, and shortage indicators.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

root_dir <- if (file.exists("Wy_Ed_Jobs/k12jobanalysis.csv")) {
  "."
} else if (file.exists("k12jobanalysis.csv")) {
  ".."
} else {
  stop("Run from the repo root (or its app folder): Rscript scripts/generate_job_duration_spells.R")
}

k12_path <- file.path(root_dir, "Wy_Ed_Jobs", "k12jobanalysis.csv")
smap_path <- file.path(root_dir, "Wy_Ed_Jobs", "salarymap2.csv")
out_path <- file.path(root_dir, "Wy_Ed_Jobs", "job_duration_spells.csv")

if (!file.exists(k12_path)) {
  stop("k12jobanalysis.csv not found at: ", k12_path)
}

k12 <- read.csv(k12_path, stringsAsFactors = FALSE)
smap <- read.csv(smap_path, stringsAsFactors = FALSE)

# Lookup table from salarymap2
dist_lookup <- unique(smap[!is.na(smap$NCES_LEAID), c("District", "NCES_LEAID")])
if (file.exists(out_path)) {
  prior_spells <- read.csv(out_path, stringsAsFactors = FALSE)
  prior_lookup <- unique(prior_spells[!is.na(prior_spells$NCES_LEAID), c("District", "NCES_LEAID")])
  dist_lookup <- bind_rows(prior_lookup, dist_lookup) %>% distinct(District, .keep_all = TRUE)
}

max_scrape_date <- max(k12$Archive_Date, na.rm = TRUE)

# A posting that drops off the board for more than SPELL_GAP_DAYS (i.e. it
# missed two or more weekly scrapes) and later reappears under the same title
# starts a new spell, rather than being merged into one multi-month "spell"
# with the earlier posting.
SPELL_GAP_DAYS <- 21

spells <- k12 %>%
  mutate(obs_date = as.Date(Archive_Date)) %>%
  arrange(District, title, obs_date) %>%
  group_by(District, title) %>%
  mutate(spell_no = cumsum(c(TRUE, diff(as.numeric(obs_date)) > SPELL_GAP_DAYS))) %>%
  group_by(District, Title = title, spell_no) %>%
  summarise(
    Category = first(Category),
    Broad_Category = first(Broad_Category),
    Position = first(position),
    First_Seen_Date = min(Archive_Date),
    Last_Seen_Date = max(Archive_Date),
    # Calendar weeks, not snapshots: some weeks had more than one scrape.
    Weeks_Observed = n_distinct(format(obs_date, "%G-%V")),
    Estimated_Days_Open = as.integer(as.Date(max(Archive_Date)) - as.Date(min(Archive_Date)) + 7),
    Status = ifelse(max(Archive_Date) == max_scrape_date, "Still Active", "Closed / Filled"),
    .groups = "drop"
  ) %>%
  select(-spell_no)

# Join NCES_LEAID
spells <- spells %>%
  left_join(dist_lookup, by = "District")

# Fallback for name variants
missing_idx <- which(is.na(spells$NCES_LEAID))
for (i in missing_idx) {
  d <- spells$District[i]
  m <- smap$NCES_LEAID[grep(paste0("^", d), smap$District)]
  if (length(m) > 0) spells$NCES_LEAID[i] <- m[1]
}

# Add shortage dummy flags
spells <- spells %>%
  mutate(
    is_stem = ifelse(Broad_Category %in% c("Math", "Science", "CTE - Business & Family Sciences") |
                       grepl("Math|Science|STEM", Category, ignore.case = TRUE), 1L, 0L),
    is_sped = ifelse(grepl("Special Education", Broad_Category) |
                       grepl("Special Education", Category, ignore.case = TRUE), 1L, 0L),
    is_elementary = ifelse(grepl("Elementary", Broad_Category) |
                             grepl("Elementary", Category, ignore.case = TRUE), 1L, 0L),
    is_secondary = ifelse(grepl("Secondary", Broad_Category) |
                            grepl("Secondary|High School|Middle School", Category, ignore.case = TRUE), 1L, 0L),
    is_admin = ifelse(grepl("Admin|Principal|Superintendent", Position, ignore.case = TRUE), 1L, 0L),
    is_support_staff = ifelse(grepl("Support|Paraprofessional|Aide|Custodial|Transportation|Food Services", Position, ignore.case = TRUE), 1L, 0L)
  )

# Exact schema order
spells <- spells %>%
  select(District, NCES_LEAID, Title, Category, Broad_Category, Position,
         First_Seen_Date, Last_Seen_Date, Weeks_Observed, Estimated_Days_Open, Status,
         is_stem, is_sped, is_elementary, is_secondary, is_admin, is_support_staff)

write.csv(spells, out_path, row.names = FALSE, na = "")
message("Successfully generated ", nrow(spells), " vacancy spells to: ", out_path)
