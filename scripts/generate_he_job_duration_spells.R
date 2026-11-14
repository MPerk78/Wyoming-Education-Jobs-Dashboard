#!/usr/bin/env Rscript
# scripts/generate_he_job_duration_spells.R
#
# Generates or refreshes Wy_Ed_Jobs/he_job_duration_spells.csv from accumulated
# facultydata.csv longitudinal higher education postings data, linking federal IPEDS_UNITID
# and deriving vacancy durations, time-to-fill, and academic discipline indicators.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

root_dir <- if (file.exists("Wy_Ed_Jobs/facultydata.csv")) {
  "."
} else if (file.exists("facultydata.csv")) {
  ".."
} else {
  stop("Run from the repo root (or its app folder): Rscript scripts/generate_he_job_duration_spells.R")
}

he_path <- file.path(root_dir, "Wy_Ed_Jobs", "facultydata.csv")
smap_path <- file.path(root_dir, "Wy_Ed_Jobs", "salarymap.csv")
out_path <- file.path(root_dir, "Wy_Ed_Jobs", "he_job_duration_spells.csv")

if (!file.exists(he_path)) {
  stop("facultydata.csv not found at: ", he_path)
}

fac <- read.csv(he_path, stringsAsFactors = FALSE)
smap <- read.csv(smap_path, stringsAsFactors = FALSE)

# Lookup table combining salarymap and prior mappings
inst_lookup <- unique(smap[!is.na(smap$IPEDS_UNITID), c("Name", "IPEDS_UNITID")])
names(inst_lookup)[1] <- "Institution"

if (file.exists(out_path)) {
  prior_spells <- read.csv(out_path, stringsAsFactors = FALSE)
  prior_lookup <- unique(prior_spells[!is.na(prior_spells$IPEDS_UNITID), c("Institution", "IPEDS_UNITID")])
  inst_lookup <- bind_rows(prior_lookup, inst_lookup) %>% distinct(Institution, .keep_all = TRUE)
}

max_scrape_date <- max(fac$Archive_Date, na.rm = TRUE)

# A posting that drops off the board for more than SPELL_GAP_DAYS (i.e. it
# missed two or more weekly scrapes) and later reappears under the same title
# starts a new spell, rather than being merged into one multi-month "spell"
# with the earlier posting.
SPELL_GAP_DAYS <- 21

spells <- fac %>%
  mutate(obs_date = as.Date(Archive_Date)) %>%
  arrange(Institution, Title, obs_date) %>%
  group_by(Institution, Title) %>%
  mutate(spell_no = cumsum(c(TRUE, diff(as.numeric(obs_date)) > SPELL_GAP_DAYS))) %>%
  group_by(Institution, Title, spell_no) %>%
  summarise(
    Category = first(Category),
    Job_Type = first(Job_Type),
    First_Seen_Date = min(Archive_Date),
    Last_Seen_Date = max(Archive_Date),
    # Calendar weeks, not snapshots: some weeks had more than one scrape.
    Weeks_Observed = n_distinct(format(obs_date, "%G-%V")),
    Estimated_Days_Open = as.integer(as.Date(max(Archive_Date)) - as.Date(min(Archive_Date)) + 7),
    Status = ifelse(max(Archive_Date) == max_scrape_date, "Still Active", "Closed / Filled"),
    .groups = "drop"
  ) %>%
  select(-spell_no)

# Join IPEDS_UNITID
spells <- spells %>%
  left_join(inst_lookup, by = "Institution")

# Add taxonomy dummy flags
spells <- spells %>%
  mutate(
    is_stem = ifelse(grepl("Math|Science|Engineering|Computing|STEM", Category, ignore.case = TRUE) |
                       Category %in% c("CTE - Trades & Engineering", "CTE - Business & Computing"), 1L, 0L),
    is_health = ifelse(grepl("Health|Nursing", Category, ignore.case = TRUE) |
                         Category == "CTE - Health Sciences", 1L, 0L),
    is_cte = ifelse(grepl("^CTE", Category), 1L, 0L),
    is_adjunct = ifelse(Job_Type == "Adjunct/Part-Time Faculty", 1L, 0L),
    is_faculty = ifelse(Job_Type == "Instructor/Teacher/Faculty", 1L, 0L)
  )

# Exact schema order
spells <- spells %>%
  select(Institution, IPEDS_UNITID, Title, Category, Job_Type,
         First_Seen_Date, Last_Seen_Date, Weeks_Observed, Estimated_Days_Open, Status,
         is_stem, is_health, is_cte, is_adjunct, is_faculty)

write.csv(spells, out_path, row.names = FALSE, na = "")
message("Successfully generated ", nrow(spells), " collegiate vacancy spells to: ", out_path)
