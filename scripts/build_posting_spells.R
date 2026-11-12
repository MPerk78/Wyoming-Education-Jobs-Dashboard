#!/usr/bin/env Rscript
# scripts/build_posting_spells.R
#
# Rebuilds the posting-level spells (edjobs::build_posting_spells() over the
# panels in posting_spells.R, under WY_SPELL_RULES) from the raw archives,
# coverage_calendar.csv (from 2026-11-06) and schoolspring_endings.csv, and
# writes at the repo root:
#   posting_spells_k12.csv     one row per K-12 posting: start, end, how it ended, flags
#   posting_durations_k12.csv  Kaplan-Meier median days listed: all, by position,
#                              by school year, teachers by school year
#   posting_spells_he.csv      the same for higher ed (Archived_HE_Data)
#   posting_durations_he.csv   all, all except UW, by HE job type, by institution
#   frozen_boards.csv          every frozen stretch found (K-12 and HE)
#   advertised_windows.csv     median days from start to the stated closing
#                              date, where postings state one (K-12, HE)
# Research tables only: the dashboard's Listing Duration tabs stay paused
# (2026-11-05 spells trust audit) until it's decided what they show.
# Rerunnable from scratch any time.

suppressPackageStartupMessages({ library(dplyr); library(readxl) })
source("k12_he_classification.R")
source("posting_spells.R")

calendar <- if (file.exists("coverage_calendar.csv")) read.csv("coverage_calendar.csv", colClasses = "character") else NULL
endings <- if (file.exists("schoolspring_endings.csv")) read.csv("schoolspring_endings.csv", colClasses = "character") else NULL
statuses <- if (file.exists("posting_status.csv")) read.csv("posting_status.csv", colClasses = "character") else NULL

# ---- K-12 ----
panel <- use_earliest_posted_date(bridge_posting_ids(k12_posting_panel("Archivek12_Data"), WY_SPELL_RULES))
k12_cal <- if (is.null(calendar)) NULL else calendar[calendar$Segment %in% c("K-12", "Feed"), , drop = FALSE]
frozen_k12 <- frozen_board_runs(panel)
spells <- build_posting_spells(panel, k12_cal, endings, frozen = frozen_k12, statuses = statuses, rules = WY_SPELL_RULES)
teacher <- spells[spells$Position == "Teacher", ]
durations <- bind_rows(summarize_posting_durations(spells),
                       summarize_posting_durations(spells, "Position"),
                       summarize_posting_durations(spells, "School_Year") %>% mutate(Group = paste("School year:", Group)),
                       summarize_posting_durations(teacher, "School_Year") %>% mutate(Group = paste("Teacher, school year:", Group))) %>%
  mutate(As_Of = max(spells$Last_Seen))
write.csv(spells, "posting_spells_k12.csv", row.names = FALSE)
write.csv(durations, "posting_durations_k12.csv", row.names = FALSE)
cat("posting_spells_k12.csv:", nrow(spells), "postings,", sum(spells$In_Durations), "in durations,",
    sum(spells$Event & spells$In_Durations), "endings. Median days listed (all):",
    durations$Median_Days[durations$Group == "All"], "\n")

# ---- Higher ed: each institution is its own source in the calendar's HE rows ----
he_cal <- if (is.null(calendar)) NULL else calendar[calendar$Segment == "HE", , drop = FALSE]
he_panel <- use_earliest_posted_date(bridge_posting_ids(he_posting_panel("Archived_HE_Data"), WY_SPELL_RULES))
frozen_he <- frozen_board_runs(he_panel)
he_spells <- build_posting_spells(he_panel, he_cal, frozen = frozen_he, statuses = statuses, rules = WY_SPELL_RULES)
he_durations <- bind_rows(summarize_posting_durations(he_spells),
                          summarize_posting_durations(he_spells[he_spells$District != "University of Wyoming", ]) %>%
                            mutate(Group = "All except University of Wyoming"),
                          summarize_posting_durations(he_spells, "Position"),
                          summarize_posting_durations(he_spells, "District") %>% mutate(Group = paste("Institution:", Group))) %>%
  mutate(As_Of = max(he_spells$Last_Seen))
write.csv(he_spells, "posting_spells_he.csv", row.names = FALSE)
# "Posted / listed" falls back to the first run that found a posting when its
# board gives no date (edjobs::first_seen_lookup()).
first_seen <- rbind(first_seen_lookup(panel, spells, "K-12"), first_seen_lookup(he_panel, he_spells, "HE"))
write.csv(first_seen, file.path("Wy_Ed_Jobs", "posting_first_seen.csv"), row.names = FALSE)
cat("Wy_Ed_Jobs/posting_first_seen.csv:", nrow(first_seen), "current postings\n")
write.csv(he_durations, "posting_durations_he.csv", row.names = FALSE)
cat("posting_spells_he.csv:", nrow(he_spells), "postings,", sum(he_spells$In_Durations), "in durations,",
    sum(he_spells$Event & he_spells$In_Durations), "endings. Median days listed (all):",
    he_durations$Median_Days[he_durations$Group == "All"], "\n")

write.csv(bind_rows(mutate(frozen_k12, Segment = "K-12"), mutate(frozen_he, Segment = "HE")), "frozen_boards.csv",
          row.names = FALSE)
cat("frozen_boards.csv:", nrow(frozen_k12), "K-12 and", nrow(frozen_he), "HE frozen stretches\n")

# Advertised application window: start to the stated closing date, where the
# posting states one. A measure of its own, needing no ending.
stated <- bind_rows(`K-12` = spells, HE = he_spells, .id = "Segment") %>%
  filter(!is.na(Advertised_Days), !Evergreen, !Pool)
windows <- bind_rows(stated %>% mutate(Position = "All"), stated) %>%
  group_by(Segment, Position) %>%
  summarize(Postings = n(), Median_Days = median(Advertised_Days), .groups = "drop") %>%
  arrange(Segment, Position != "All", desc(Postings))
write.csv(windows, "advertised_windows.csv", row.names = FALSE)
cat("advertised_windows.csv:", sum(windows$Postings[windows$Position == "All"]), "postings with a stated closing date\n")
