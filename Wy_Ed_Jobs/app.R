library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(data.table)
library(leaflet)
library(plotly)
library(scales)
library(stringr)
library(readxl)
library(shinycssloaders)

#--------------------------------------------------
# Schema guard -- if a pipeline chunk ever silently drops or renames a
# column app.R references by name, the failure mode used to be a bare
# "object 'Teachers_Total_FTE' not found" crash for the first user who
# opened the dashboard after a bad deploy, with nothing identifying which
# upstream FILE was actually responsible. The real prevention lives in
# schema_check.R + .github/scripts/verify_schema.R at the repo root, which
# blocks a schema-broken commit from ever reaching this deployed app in the
# first place -- but this app folder is deployed standalone (Connect Cloud
# only bundles Wy_Ed_Jobs/, not the repo root), so it can't source that
# file directly. validate_and_pad_schema() is the deployed app's own last
# line of defense: any dataset actually missing an expected column gets
# that column backfilled with NA (so downstream mutate/select references
# don't hard-crash the whole app for every feature) and the problem is
# recorded in DATA_LOAD_ISSUES for a visible Home-tab banner instead of a
# silent blank field or an opaque stack trace.
#--------------------------------------------------
DATA_LOAD_ISSUES <- character(0)

validate_and_pad_schema <- function(df, required_cols, source_name) {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    DATA_LOAD_ISSUES <<- c(DATA_LOAD_ISSUES, sprintf(
      "%s is missing expected column(s): %s. That data will show blank until the pipeline is fixed.",
      source_name, paste(missing, collapse = ", ")
    ))
    for (col in missing) df[[col]] <- NA
  }
  df
}

# The repository pipeline classifies these same title patterns in
# k12_he_classification.R. Keep the deployed app self-contained while the
# dashboard distinguishes appointment type without inferring FTE where a
# source does not explicitly provide it.
classify_he_appointment <- function(titles) {
  titles <- as.character(titles)
  dplyr::case_when(
    grepl("Adjunct|Part[- ]?Time", titles, ignore.case = TRUE) ~ "Adjunct/part-time faculty",
    grepl("Instructor|Instructional|Teacher|Faculty|Professor|Lecturer|Post Doc|Subject Matter Expert|Librarian|Educator",
          titles, ignore.case = TRUE) ~ "Faculty/instructor (non-adjunct)",
    TRUE ~ "Other / not faculty"
  )
}

summarize_he_faculty_postings <- function(titles) {
  appointment <- classify_he_appointment(titles)
  list(
    faculty_instructor = sum(appointment == "Faculty/instructor (non-adjunct)", na.rm = TRUE),
    adjunct_part_time = sum(appointment == "Adjunct/part-time faculty", na.rm = TRUE)
  )
}

# Sources that keep evergreen pools active may retain an original posting
# date for years. Make that distinction visible without discarding a still-
# open opportunity or inventing a newer date the source does not provide.
format_posted_or_listed_date <- function(dates, stale_after_days = 180) {
  values <- as.character(dates)
  parsed <- as.Date(lubridate::parse_date_time(
    values,
    orders = c("ymd", "mdy", "dmy", "b d Y h:M p", "B d Y h:M p", "Y-m-d H:M:S"),
    tz = "UTC",
    quiet = TRUE
  ))
  stale <- !is.na(parsed) & parsed < Sys.Date() - stale_after_days
  values[stale] <- paste("Listed since", values[stale])
  values
}

# Pay as the posting itself states it (Pay_* from extract_posted_pay() in the
# pipeline): "$21.46–$22.81/hr", "$46,500–$71,803/yr", "$3,691 stipend".
# Units are never converted -- an hourly wage isn't turned into an annual
# figure, since hours per year aren't known.
format_posted_pay <- function(pay_min, pay_max, pay_unit) {
  fmt <- function(x) ifelse(x >= 1000, scales::dollar(x, accuracy = 1), scales::dollar(x, accuracy = 0.01))
  amount <- ifelse(is.na(pay_min), NA_character_,
                   ifelse(is.na(pay_max) | pay_min == pay_max, fmt(pay_min), paste0(fmt(pay_min), "\u2013", fmt(pay_max))))
  suffix <- unname(c(hour = "/hr", day = "/day", month = "/mo", year = "/yr",
                     "credit hour" = "/credit hr", stipend = " stipend")[pay_unit])
  ifelse(is.na(amount), "\u2014", paste0(amount, ifelse(is.na(suffix), "", suffix)))
}

# BLS OEWS regional median for the posting's role (Est_* from
# attach_oews_estimates() in the pipeline), shown only where the posting
# states no pay: "$34,750/yr", with the role and area in a hover tooltip
# ("BLS median for teaching assistants, West Montana nonmetro").
format_bls_estimate <- function(est_pay, est_unit, occupation, area, has_posted_pay) {
  format_pay_estimate(est_pay, est_unit, paste0("BLS median for ", tolower(occupation), ", ", area), has_posted_pay)
}

# Same for higher-ed staff postings: the institution's IPEDS average for the
# posting's staff category ("IPEDS average for full-time management staff,
# Montana State University").
format_ipeds_estimate <- function(est_pay, est_unit, category, institution, has_posted_pay) {
  format_pay_estimate(est_pay, est_unit, paste0("IPEDS average for full-time ", category, " staff, ", institution),
                      has_posted_pay)
}

format_pay_estimate <- function(est_pay, est_unit, tooltip, has_posted_pay) {
  show <- !has_posted_pay & !is.na(est_pay)
  tip <- htmltools::htmlEscape(tooltip, attribute = TRUE)
  ifelse(show,
         paste0("<span title=\"", tip, "\" style=\"border-bottom: 1px dotted #888; cursor: help;\">",
                format_posted_pay(est_pay, est_pay, est_unit), "</span>"),
         "\u2014")
}

# Map pay bands. Each posting is compared in its own unit -- an hourly wage
# is never turned into an annual one -- so day rates, stipends and per-credit
# pay appear only under "All".
PAY_BANDS <- list(
  hour_lt18  = list(unit = "hour", lo = 0,     hi = 18,    label = "under $18/hr"),
  hour_18_25 = list(unit = "hour", lo = 18,    hi = 25,    label = "$18\u201325/hr"),
  hour_25p   = list(unit = "hour", lo = 25,    hi = Inf,   label = "$25+/hr"),
  year_lt60  = list(unit = "year", lo = 0,     hi = 60000, label = "under $60k/yr"),
  year_60_70 = list(unit = "year", lo = 60000, hi = 70000, label = "$60\u201370k/yr"),
  year_70p   = list(unit = "year", lo = 70000, hi = Inf,   label = "$70k+/yr")
)
in_pay_band <- function(value, unit, band) {
  if (is.null(band) || !band %in% names(PAY_BANDS)) return(rep(TRUE, length(value)))
  b <- PAY_BANDS[[band]]
  !is.na(value) & !is.na(unit) & unit == b$unit & value >= b$lo & value < b$hi
}

# The figure a posting is filtered on: its posted pay (midpoint of a range)
# when stated by the hour or year; nothing when stated in another unit (a
# day rate or stipend is the posting's own figure, so no estimate replaces
# it); otherwise the fallback the caller supplies (district role figure, then
# the BLS estimate).
posting_pay_value <- function(pay_min, pay_max, pay_unit, fallback_value, fallback_unit) {
  posted <- !is.na(pay_min)
  filterable <- posted & pay_unit %in% c("hour", "year")
  data.frame(
    value = ifelse(filterable, (pay_min + coalesce(pay_max, pay_min)) / 2, ifelse(posted, NA_real_, fallback_value)),
    unit = ifelse(filterable, pay_unit, ifelse(posted, NA_character_, fallback_unit)),
    stringsAsFactors = FALSE
  )
}

#--------------------------------------------------
# Load K-12 data
#--------------------------------------------------
combineddata <- read.csv("combinedclean.csv", fileEncoding = "UTF-8")
# Posted-pay columns are optional (absent from files written before the
# pipeline captured pay) -- padded here rather than in validate_and_pad_schema(),
# which would flag their absence as a schema problem on the Home tab.
# The BLS estimate columns (Est_*) are optional the same way.
for (col in c("Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text",
              "Est_Pay", "Est_Pay_Unit", "Est_Occupation", "Est_Area", "Est_Year")) {
  if (!col %in% names(combineddata)) combineddata[[col]] <- NA
}
oews_year <- na.omit(combineddata$Est_Year)[1]
combineddata <- combineddata %>%
  validate_and_pad_schema(c("District", "title", "position", "location", "date_posted", "url"), "combinedclean.csv") %>%
  mutate(`Posted Pay` = format_posted_pay(Pay_Min, Pay_Max, Pay_Unit),
         `Typical pay (BLS est.)` = format_bls_estimate(Est_Pay, Est_Pay_Unit, Est_Occupation, Est_Area, !is.na(Pay_Min))) %>%
  select(District, title, position, location, `Posted Pay`, `Typical pay (BLS est.)`, date_posted, url,
         Pay_Min, Pay_Max, Pay_Unit, Est_Pay, Est_Pay_Unit) %>%
  mutate(District = str_squish(as.character(District))) %>%
  mutate(date_posted = format_posted_or_listed_date(date_posted)) %>%
  arrange(District, title) %>%
  mutate(url = paste0('<a href="', url, '" target="_blank">', url, '</a>')) %>%
  rename(Title = title, Position = position, Location = location,
         `Posted / listed` = date_posted, Link = url)

# BLS QCEW average pay of public school employees by county, all roles (the
# pipeline's qcew_county_school_pay.csv); county context in the map popup.
# Optional: counties BLS withholds, or a missing file, show nothing.
qcew_school_pay <- if (file.exists("qcew_county_school_pay.csv")) {
  read.csv("qcew_county_school_pay.csv", stringsAsFactors = FALSE, colClasses = c(Year = "character"))
} else {
  data.frame(County = character(0), Avg_Annual_Pay = numeric(0), Year = character(0))
}

mapdata2_k12 <- read.csv("salarymap2.csv", fileEncoding = "UTF-8") %>%
  validate_and_pad_schema(c("District", "County", "Latitude", "Longitude", "Job_Link",
                             "Teacher_Base_Salary", "Teacher_Base_Salary_Prior_Year", "Salary_Year",
                             "Superintendent_Salary", "Superintendent_Contract_Days", "Salary_Source",
                             "Teachers_Total_FTE", "Data_Coverage", "Enrollment",
                             "Median_Household_Income", "Median_Gross_Rent", "Mining_Employment_Share",
                             "Population_Change_Pct", "ACS_Year", "Child_Poverty_Rate", "SAIPE_Year",
                             "Teacher_Avg_Salary"),
                           "salarymap2.csv") %>%
  rename(Name = District)

# District salary lookup for the Jobs Table
k12_salary_lookup <- mapdata2_k12 %>%
  select(Name, Teacher_Base_Salary, Teacher_Base_Salary_Prior_Year, Teacher_Avg_Salary) %>%
  mutate(
    `District Starting Base` = ifelse(
      is.na(Teacher_Base_Salary),
      "—",
      scales::dollar(Teacher_Base_Salary, accuracy = 1)
    )
  )

# Role-specific WDE salary lookup for K-12 Jobs Table
# Maps job position/title to its specific assignment-category benchmark in WDE602
wde_staff_file <- "wde602_district_staff_salaries.csv"
if (file.exists(wde_staff_file)) {
  wde_staff <- read.csv(wde_staff_file, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  canonicalize_wde_dist <- function(name) {
    name <- trimws(name)
    m <- regmatches(name, regexec("^(.+?)\\s*#(\\d+)$", name))
    vapply(m, function(parts) {
      if (length(parts) != 3) return(parts[1])
      county <- parts[2]
      num <- parts[3]
      if (county %in% c("Big Horn", "Natrona")) {
        paste0(county, " School District ", num)
      } else {
        paste0(county, " County School District ", num)
      }
    }, character(1))
  }
  wde_staff$canon_district <- canonicalize_wde_dist(wde_staff$District_Name)
  wde_staff$Salary_Num <- suppressWarnings(as.numeric(wde_staff$Average_Total_Salary))

  map_pos_to_wde_group <- function(pos, title) {
    pos_l <- tolower(pos)
    title_l <- tolower(title)
    case_when(
      pos_l %in% c("teacher", "part-time teacher") ~ "SUBTOTAL Instructional Staff Teachers",
      pos_l == "administration" ~ "School Building Administrators",
      pos_l == "paraprofessional" ~ "Instructional Aides",
      pos_l == "transportation" ~ "School Bus Drivers",
      pos_l == "custodial/maintenance" ~ "Operation and Maintenance",
      pos_l == "food services" ~ "Food Service Staff",
      pos_l == "staff" ~ "School Secretary/Clerical Support",
      pos_l == "support services" & grepl("counselor", title_l) ~ "TOTAL Counselors",
      pos_l == "support services" ~ "Student Support Staff -- Professional",
      # Substitutes (daily rate), coaches (stipends), child care and
      # unclassified postings have no WDE602 counterpart -- leave them blank
      # rather than showing the teacher average.
      TRUE ~ NA_character_
    )
  }

  combineddata <- combineddata %>%
    mutate(WDE_Group = map_pos_to_wde_group(Position, Title)) %>%
    left_join(
      wde_staff %>%
        filter(!is.na(canon_district) & !is.na(Salary_Num) & Salary_Num > 0) %>%
        select(canon_district, Group_Name, Salary_Num),
      by = c("District" = "canon_district", "WDE_Group" = "Group_Name")
    ) %>%
    left_join(k12_salary_lookup, by = c("District" = "Name")) %>%
    mutate(
      Role_Suffix = case_when(
        WDE_Group == "School Building Administrators" ~ " (Admin)",
        WDE_Group == "Instructional Aides" ~ " (Aide)",
        WDE_Group == "School Bus Drivers" ~ " (Bus)",
        WDE_Group == "Operation and Maintenance" ~ " (Maint)",
        WDE_Group == "Food Service Staff" ~ " (Food)",
        WDE_Group == "School Secretary/Clerical Support" ~ " (Clerical)",
        WDE_Group == "TOTAL Counselors" ~ " (Counselor)",
        WDE_Group == "Student Support Staff -- Professional" ~ " (Support)",
        TRUE ~ ""
      ),
      is_teacher_row = Position %in% c("Teacher", "Part-Time Teacher"),
      # Only a teacher row may fall back to the district teacher average; any
      # other role without its own WDE602 figure stays blank.
      Final_Salary = ifelse(is_teacher_row, coalesce(Salary_Num, Teacher_Avg_Salary), Salary_Num),
      `Teacher Starting Base` = ifelse(is_teacher_row, `District Starting Base`, "—"),
      `Role Avg Salary` = ifelse(
        is.na(Final_Salary),
        "—",
        paste0(scales::dollar(round(Final_Salary), accuracy = 1), ifelse(is.na(Salary_Num), "", Role_Suffix))
      ),
      # Figure for the map's pay bands: posted pay, else the WDE602 role
      # figure (the district teacher average on a teaching posting), else
      # the BLS estimate.
      .Pay_Value = posting_pay_value(Pay_Min, Pay_Max, Pay_Unit, coalesce(Final_Salary, Est_Pay),
                                     ifelse(!is.na(Final_Salary), "year", Est_Pay_Unit))$value,
      .Pay_Unit = posting_pay_value(Pay_Min, Pay_Max, Pay_Unit, coalesce(Final_Salary, Est_Pay),
                                    ifelse(!is.na(Final_Salary), "year", Est_Pay_Unit))$unit
    ) %>%
    select(District, Title, Position, Location, `Posted Pay`, `Typical pay (BLS est.)`, `Teacher Starting Base`,
           `Role Avg Salary`, `Posted / listed`, Link, .Pay_Value, .Pay_Unit)
} else {
  combineddata <- combineddata %>%
    left_join(k12_salary_lookup, by = c("District" = "Name")) %>%
    mutate(
      is_teacher_row = Position %in% c("Teacher", "Part-Time Teacher"),
      `Role Avg Salary` = ifelse(
        !is_teacher_row | is.na(Teacher_Avg_Salary),
        "—",
        scales::dollar(Teacher_Avg_Salary, accuracy = 1)
      ),
      `Teacher Starting Base` = ifelse(is_teacher_row, `District Starting Base`, "—"),
      .fallback_value = ifelse(is_teacher_row & !is.na(Teacher_Avg_Salary), Teacher_Avg_Salary, Est_Pay),
      .fallback_unit = ifelse(is_teacher_row & !is.na(Teacher_Avg_Salary), "year", Est_Pay_Unit),
      .Pay_Value = posting_pay_value(Pay_Min, Pay_Max, Pay_Unit, .fallback_value, .fallback_unit)$value,
      .Pay_Unit = posting_pay_value(Pay_Min, Pay_Max, Pay_Unit, .fallback_value, .fallback_unit)$unit
    ) %>%
    select(District, Title, Position, Location, `Posted Pay`, `Typical pay (BLS est.)`, `Teacher Starting Base`,
           `Role Avg Salary`, `Posted / listed`, Link, .Pay_Value, .Pay_Unit)
}

# Weekly ALL-category posting totals per district/institution (not scoped
# to Teacher/Faculty like k12_history/he_history below) -- powers the
# sparkline trend next to each entity's raw count on the Top Hiring
# tables, matching that same all-category count rather than a
# teacher/faculty-only proxy that wouldn't line up with the number shown.
k12_district_weekly_totals <- read.csv("k12_district_weekly_totals.csv", fileEncoding = "UTF-8") %>%
  validate_and_pad_schema(c("District", "Archive_Date", "n"), "k12_district_weekly_totals.csv") %>%
  mutate(Archive_Date = as.Date(Archive_Date))
he_institution_weekly_totals <- read.csv("he_institution_weekly_totals.csv", fileEncoding = "UTF-8") %>%
  validate_and_pad_schema(c("Institution", "Archive_Date", "n"), "he_institution_weekly_totals.csv") %>%
  mutate(Archive_Date = as.Date(Archive_Date))

data_dictionary <- if (file.exists("data_dictionary.csv")) {
  read.csv("data_dictionary.csv", stringsAsFactors = FALSE)
} else {
  data.frame(Dataset = character(0), File_Name = character(0), Column_Name = character(0),
             Data_Type = character(0), Primary_Authority = character(0), Definition = character(0))
}

job_duration_spells <- if (file.exists("job_duration_spells.csv")) {
  read.csv("job_duration_spells.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
} else {
  data.frame()
}

he_job_duration_spells <- if (file.exists("he_job_duration_spells.csv")) {
  read.csv("he_job_duration_spells.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
} else {
  data.frame()
}

# Tiny inline trend chart (no axes/legend, just the shape of a trend) for
# the Top Hiring tables -- lets "31 openings" read differently depending
# on whether that's a district climbing steadily or one that just posted a
# batch this week, without a separate chart. Plain SVG string templating
# rather than a real ggplot/plotly render, since this needs to be cheap
# enough to generate per table row.
make_sparkline_svg <- function(series, accent = "#2a78d6") {
  series <- series[!is.na(series)]
  if (length(series) < 2) return("")

  w <- 96; h <- 28; pad <- 3
  rng <- range(series)
  span <- diff(rng)
  if (span == 0) span <- 1

  n <- length(series)
  step_x <- (w - pad * 2) / (n - 1)
  xs <- pad + (seq_len(n) - 1) * step_x
  ys <- pad + (1 - (series - rng[1]) / span) * (h - pad * 2)

  line_path <- paste0(ifelse(seq_along(xs) == 1, "M", "L"), round(xs, 1), ",", round(ys, 1), collapse = " ")
  area_path <- paste0(line_path, " L", round(xs[n], 1), ",", h - pad, " L", round(xs[1], 1), ",", h - pad, " Z")

  dot_color <- if (series[n] > series[1]) "#1baf7a" else if (series[n] < series[1]) "#e34948" else "#999999"

  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 %d %d" style="vertical-align:middle;"><path d="%s" fill="%s22" stroke="none"></path><path d="%s" fill="none" stroke="%s" stroke-width="1.75" stroke-linejoin="round" stroke-linecap="round"></path><circle cx="%s" cy="%s" r="2.5" fill="%s"></circle></svg>',
    w, h, w, h, area_path, accent, line_path, accent, round(xs[n], 1), round(ys[n], 1), dot_color
  )
}

# Larger, monochrome-white variant for the KPI tiles' icon slot -- replaces
# the plain school/university icon with the same last-12-weeks trend
# shown on the Top Hiring tables, since a colored valueBox background
# doesn't have room for a second color scheme to read clearly.
make_sparkline_svg_light <- function(series) {
  series <- series[!is.na(series)]
  if (length(series) < 2) return("")

  w <- 160; h <- 80; pad <- 6
  rng <- range(series)
  span <- diff(rng)
  if (span == 0) span <- 1

  n <- length(series)
  step_x <- (w - pad * 2) / (n - 1)
  xs <- pad + (seq_len(n) - 1) * step_x
  ys <- pad + (1 - (series - rng[1]) / span) * (h - pad * 2)

  line_path <- paste0(ifelse(seq_along(xs) == 1, "M", "L"), round(xs, 1), ",", round(ys, 1), collapse = " ")
  area_path <- paste0(line_path, " L", round(xs[n], 1), ",", h - pad, " L", round(xs[1], 1), ",", h - pad, " Z")

  sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 %d %d"><path d="%s" fill="rgba(255,255,255,0.18)" stroke="none"></path><path d="%s" fill="none" stroke="rgba(255,255,255,0.85)" stroke-width="2.5" stroke-linejoin="round" stroke-linecap="round"></path><circle cx="%s" cy="%s" r="3.5" fill="#ffffff"></circle></svg>',
    w, h, w, h, area_path, line_path, round(xs[n], 1), round(ys[n], 1)
  )
}

# Current Trends table -- replaces what used to be a horizontal bar chart
# plus a separate sparkline table. Once every bar was already labeled
# with its exact count and a table below repeated those same numbers,
# the bar chart wasn't earning its space (user feedback 2026-08-05).
# One row per category: current count, a 12-week sparkline, and delta
# vs month/quarter/year ago. Week-over-week is deliberately not included
# here -- confirmed against real data it's pure noise at the category
# level (median absolute delta 0), the same finding that shaped
# find_biggest_mover()'s window above. Month is weak but real signal
# (median 1); quarter is strong (median 4) but conflates the actual
# within-year seasonal hiring cycle (e.g. Elementary postings peak in
# spring, trough in late summer) with real change; year isolates
# genuine year-over-year movement by comparing two points at the same
# place in that cycle (confirmed distinct from quarter with real data:
# Elementary was -31 quarter-over-quarter but only -17 year-over-year
# for the same "now").
#
# history: data.frame(Category, Archive_Date, n) already filtered to
# one district/institution and already summed across any other
# dimension (e.g. HE's Job_Type, so this shows category totals, not an
# FT/PT breakdown -- the FT/PT split isn't lost, just no longer show
# here since the bar chart carrying it is gone; Jobs Table / New This
# Week still show individual postings).
build_current_trends_table <- function(history, accent) {
  dates <- sort(unique(history$Archive_Date))
  if (length(dates) == 0) return(NULL)
  latest <- max(dates)

  nearest_on_or_before <- function(target) {
    candidates <- dates[dates <= target]
    if (length(candidates) == 0) return(NA)
    max(candidates)
  }
  month_ago <- nearest_on_or_before(latest - 28)
  quarter_ago <- nearest_on_or_before(latest - 90)
  year_ago <- nearest_on_or_before(latest - 365)

  current <- history %>%
    filter(Archive_Date == latest) %>%
    group_by(Category) %>%
    summarize(`Current Postings` = sum(n), .groups = "drop") %>%
    arrange(desc(`Current Postings`))
  if (nrow(current) == 0) return(NULL)

  value_at <- function(cat, date) {
    if (is.na(date)) return(NA_integer_)
    v <- history$n[history$Category == cat & history$Archive_Date == date]
    if (length(v) == 0) 0L else sum(v)
  }
  fmt_delta <- function(now, then) {
    if (is.na(then)) return("<span style='color:#999;'>—</span>")
    d <- now - then
    color <- if (d > 0) "#1baf7a" else if (d < 0) "#e34948" else "#999999"
    arrow <- if (d >= 0) "▲" else "▼"
    sprintf("<span style='color:%s;'>%s %d</span>", color, arrow, abs(d))
  }

  current$`Last 12 Weeks` <- vapply(current$Category, function(cat) {
    series <- history %>%
      filter(Category == cat) %>%
      group_by(Archive_Date) %>%
      summarize(n = sum(n), .groups = "drop") %>%
      arrange(Archive_Date) %>%
      tail(12) %>%
      pull(n)
    make_sparkline_svg(series, accent = accent)
  }, character(1))

  current$`vs Month Ago` <- mapply(function(cat, now) fmt_delta(now, value_at(cat, month_ago)),
                                    current$Category, current$`Current Postings`)
  current$`vs Quarter Ago` <- mapply(function(cat, now) fmt_delta(now, value_at(cat, quarter_ago)),
                                      current$Category, current$`Current Postings`)
  current$`vs Year Ago` <- mapply(function(cat, now) fmt_delta(now, value_at(cat, year_ago)),
                                   current$Category, current$`Current Postings`)

  current
}

# Statewide week-over-week delta for the KPI tiles -- aggregation smooths
# individual-entity noise into a real signal (confirmed against real data
# 2026-08-05: statewide K-12 totals move ~20-50/week, HE climbed steadily
# 439->487 over 4 weeks, but per-district week-over-week deltas have a
# median absolute value of 0 -- see find_biggest_mover()'s longer window
# below for why that metric uses a different timeframe).
#
# Uses the same min_days_back-eligible-snapshot pattern as
# find_biggest_mover() below rather than the literal last two distinct
# dates -- archive cadence isn't perfectly regular (multiple same-week CI
# test runs exist historically, see Archivek12_Data's 2026-08-03 entries),
# and without this guard an extra same-week run would make "vs last week"
# compare today against a snapshot from hours or a day earlier instead of a
# real week ago, understating or fabricating the KPI tile's headline delta.
compute_wow_delta <- function(weekly_totals, min_days_back = 5) {
  totals <- weekly_totals %>% group_by(Archive_Date) %>% summarize(n = sum(n), .groups = "drop") %>% arrange(Archive_Date)
  if (nrow(totals) < 2) return(NA_integer_)
  latest <- max(totals$Archive_Date)
  candidates <- totals$Archive_Date[totals$Archive_Date <= latest - min_days_back]
  if (length(candidates) == 0) return(NA_integer_)
  prior <- max(candidates)
  totals$n[totals$Archive_Date == latest] - totals$n[totals$Archive_Date == prior]
}

# "Biggest mover" needs a longer window than a week to mean anything --
# individual districts/institutions are mostly flat week-to-week (median
# absolute change 0 in real data), but real movement shows up over ~3+
# weeks (Natrona SD1 +22 postings, University of Wyoming +52, both over
# a 4-week span). Picks the nearest available snapshot at least
# min_days_back before the latest date rather than a fixed index, since
# archive cadence isn't perfectly regular (multiple same-week CI test
# runs exist historically -- see Archivek12_Data's 2026-08-03 entries).
find_biggest_mover <- function(weekly_totals, name_col, min_days_back = 21) {
  dates <- sort(unique(weekly_totals$Archive_Date))
  if (length(dates) < 2) return(NULL)
  latest <- max(dates)
  candidates <- dates[dates <= latest - min_days_back]
  if (length(candidates) == 0) return(NULL)
  prior <- max(candidates)

  wide <- weekly_totals %>%
    filter(Archive_Date %in% c(latest, prior)) %>%
    tidyr::pivot_wider(names_from = Archive_Date, values_from = n, values_fill = 0)
  names(wide)[names(wide) == as.character(prior)] <- "Prior"
  names(wide)[names(wide) == as.character(latest)] <- "Latest"
  wide$Delta <- wide$Latest - wide$Prior

  top <- wide %>% arrange(desc(abs(Delta))) %>% slice(1)
  list(name = top[[name_col]], prior = top$Prior, latest = top$Latest, delta = top$Delta,
       prior_date = prior, latest_date = latest)
}

render_mover_box <- function(mover, label) {
  if (is.null(mover)) return(NULL)
  arrow <- if (mover$delta >= 0) "▲" else "▼"
  delta_color <- if (mover$delta >= 0) "#1baf7a" else "#e34948"
  box(width = 12, status = "info",
      div(
        tags$span(style = "font-weight:bold;", paste0(label, ": ")),
        tags$span(mover$name),
        tags$span(style = paste0("color:", delta_color, "; font-weight:bold; margin-left:8px;"),
                   paste0(arrow, " ", abs(mover$delta))),
        tags$span(style = "color:#999; font-size:0.85em; margin-left:6px;",
                   paste0("(", mover$prior, " → ", mover$latest,
                          " since ", format(mover$prior_date, "%b %d"), ")"))
      )
  )
}

k12sum <- read.csv("allsum.csv", fileEncoding = "UTF-8") %>%
  validate_and_pad_schema(c("Broad_Category", "Archive_Date", "District", "sum"), "allsum.csv") %>%
  mutate(District = str_squish(as.character(District)),
         Broad_Category = dplyr::recode(Broad_Category,
                                        "English Language Arts Secondary" = "Engl. LA",
                                        "Secondary Social Studies" = "Soc. St.",
                                        "Special Education - General" = "SpEd - General",
                                        "Special Education - Resource/Life Skills" = "SpEd - Resource/LS",
                                        "CTE - Trades, Ag & Technical" = "CTE - Trades/Ag",
                                        "CTE - Business & Family Sciences" = "CTE - Biz/Family")) %>%
  filter(Broad_Category != "Other")

k12sum$Archive_Date <- as.Date(k12sum$Archive_Date)

# The source aggregation (group_by + summarize) only produces a row for a
# Broad_Category/Archive_Date/District combination that actually had at
# least one posting -- a week with zero postings for a category is simply
# absent, not an explicit 0. geom_line() then draws straight through that
# gap, silently connecting the surrounding weeks and making it look like
# the count never dropped. tidyr::complete() fills every combination that
# doesn't exist with a real 0 so the line actually dips.
k12sum <- k12sum %>%
  tidyr::complete(Broad_Category, Archive_Date, District, fill = list(sum = 0))


k12nowsum <- read.csv("allnow.csv", fileEncoding = "UTF-8") %>%
  validate_and_pad_schema(c("Broad_Category", "Sum", "District"), "allnow.csv") %>%
  mutate(Broad_Category = dplyr::recode(Broad_Category,
                                        "English Language Arts Secondary" = "Engl. LA",
                                        "Secondary Social Studies" = "Soc. St.",
                                        "Special Education - General" = "SpEd - General",
                                        "Special Education - Resource/Life Skills" = "SpEd - Resource/LS",
                                        "CTE - Trades, Ag & Technical" = "CTE - Trades/Ag",
                                        "CTE - Business & Family Sciences" = "CTE - Biz/Family"),
         District = str_squish(iconv(District, from = "", to = "UTF-8"))) %>%
  filter(Broad_Category != "Other")

#--------------------------------------------------
# Load Higher Ed data
#--------------------------------------------------
ccdata <- read_xlsx("hedata.xlsx")
# Optional posted-pay columns, padded the same way as combinedclean.csv's.
for (col in c("Pay_Min", "Pay_Max", "Pay_Unit", "Pay_Text",
              "Est_Pay", "Est_Pay_Unit", "Est_Occupation", "Est_Area", "Est_Year")) {
  if (!col %in% names(ccdata)) ccdata[[col]] <- NA
}
ccdata <- ccdata %>%
  validate_and_pad_schema(c("Institution", "Title", "Location", "Posted_Date", "Link"), "hedata.xlsx") %>%
  mutate(`Posted Pay` = format_posted_pay(Pay_Min, Pay_Max, Pay_Unit),
         `Typical pay (IPEDS est.)` = format_ipeds_estimate(Est_Pay, Est_Pay_Unit, Est_Occupation, Est_Area, !is.na(Pay_Min))) %>%
  select(Institution, Title, Location, `Posted Pay`, `Typical pay (IPEDS est.)`, Posted_Date, Link,
         Pay_Min, Pay_Max, Pay_Unit, Est_Pay, Est_Pay_Unit) %>%
  arrange(Institution, Title) %>%
  mutate(
    Appointment = classify_he_appointment(Title),
    Posted_Date = format_posted_or_listed_date(Posted_Date)
  ) %>%
  rename(`Posted / listed` = Posted_Date)
ccdata$Link <- paste0('<a href="', ccdata$Link, '" target="_blank">', ccdata$Link, '</a>')
he_faculty_counts <- summarize_he_faculty_postings(ccdata$Title)

mapdata2_he <- read.csv("salarymap.csv") %>%
  validate_and_pad_schema(c("Name", "Longitude", "Latitude", "Link", "Faculty_Avg_Salary",
                             "Faculty_Avg_Salary_Professor", "Faculty_Count", "Salary_Year",
                             "Salary_Note", "Salary_Source", "Faculty_Avg_Salary_Y1Ago", "Faculty_Avg_Salary_Y2Ago",
                             "County", "Median_Household_Income", "Median_Gross_Rent",
                             "Mining_Employment_Share", "Population_Change_Pct", "ACS_Year",
                             "Enrollment", "Enrollment_Change_Pct", "Pell_Recipient_Share", "Pell_Year"),
                           "salarymap.csv") %>%
  mutate(Salary_Year = as.character(Salary_Year), Pell_Year = as.character(Pell_Year))

# Institution faculty salary lookup for the Higher Ed Jobs Table
he_salary_lookup <- mapdata2_he %>%
  select(Name, Faculty_Avg_Salary) %>%
  mutate(
    `Avg Faculty Salary` = ifelse(
      is.na(Faculty_Avg_Salary),
      "—",
      scales::dollar(Faculty_Avg_Salary, accuracy = 1)
    )
  )

ccdata <- ccdata %>%
  left_join(he_salary_lookup, by = c("Institution" = "Name")) %>%
  # Figure for the map's pay bands: posted pay, else the institution's IPEDS
  # faculty average on a full-time faculty posting, else its IPEDS average
  # for the posting's staff category.
  mutate(
    .faculty_avg = Appointment == "Faculty/instructor (non-adjunct)" & !is.na(Faculty_Avg_Salary),
    .fallback_value = ifelse(.faculty_avg, Faculty_Avg_Salary, Est_Pay),
    .fallback_unit = ifelse(.faculty_avg, "year", Est_Pay_Unit),
    .Pay_Value = posting_pay_value(Pay_Min, Pay_Max, Pay_Unit, .fallback_value, .fallback_unit)$value,
    .Pay_Unit = posting_pay_value(Pay_Min, Pay_Max, Pay_Unit, .fallback_value, .fallback_unit)$unit
  ) %>%
  select(Institution, Title, Location, Appointment, `Posted Pay`, `Typical pay (IPEDS est.)`, `Avg Faculty Salary`,
         `Posted / listed`, Link, .Pay_Value, .Pay_Unit)

hesum_he <- read.csv("allsum_he.csv") %>%
  validate_and_pad_schema(c("Category", "Archive_Date", "Institution", "Job_Type", "sum"), "allsum_he.csv") %>%
  filter(Category != "Uncategorized")

hesum_he$Archive_Date <- as.Date(hesum_he$Archive_Date)

# Same gap-filling as k12sum above -- a Category/Institution/Job_Type
# combination with zero postings in a given week is simply missing from
# the source aggregation, not an explicit 0.
hesum_he <- hesum_he %>%
  tidyr::complete(Category, Archive_Date, Institution, Job_Type, fill = list(sum = 0))
he_dates <- sort(unique(hesum_he$Archive_Date))
WINDOW_WEEKS <- 52
hesum_he$Category<- as.factor(hesum_he$Category)

henowsum_he <- read.csv("allnow_he.csv") %>%
  validate_and_pad_schema(c("Category", "Job_Type", "Sum", "Institution"), "allnow_he.csv") %>%
  filter(Category != "Uncategorized")

last_refreshed_date <- format(max(k12sum$Archive_Date, hesum_he$Archive_Date, na.rm = TRUE), "%B %d, %Y")

#--------------------------------------------------
# Simple rollups -- a 2026-08-04 audit found several badly-oversized,
# artificially-merged categories (K-12 "Special Education" alone was 3,239
# postings; HE "CTE" was 5,834, three times the next-largest real
# category) and split them into coherent sub-groups. That's more accurate,
# but too many categories at once makes the longitudinal line charts an
# unreadable tangle -- and a first attempt at an "Aggregated" view that
# just undid those specific splits (13-16 categories) turned out to still
# be nearly as busy as "Detailed" (16-18 categories), not a real second
# option. "Simple" instead collapses every detailed category into ~5 broad
# groups chosen from real posting volume (see git history for the specific
# percentages), matching how school/college staffing is actually organized
# -- e.g. K-12 elementary-generalist vs special-ed vs secondary-subject vs
# CTE vs enrichment, HE by division. Both levels stay available via a
# toggle rather than picking one and losing the other.
#--------------------------------------------------
k12_collapse_map <- c(
  "Elementary" = "Elementary & Early Childhood",
  "Early Childhood" = "Elementary & Early Childhood",
  "SpEd - General" = "Special Education",
  "SpEd - Resource/LS" = "Special Education",
  "Math" = "Core Academic",
  "Science" = "Core Academic",
  "Engl. LA" = "Core Academic",
  "Soc. St." = "Core Academic",
  "Language" = "Core Academic",
  "CTE - Trades/Ag" = "CTE",
  "CTE - Biz/Family" = "CTE",
  "Music" = "Arts & Enrichment",
  "Art" = "Arts & Enrichment",
  "Physical Education" = "Arts & Enrichment",
  "Library Media" = "Arts & Enrichment",
  "Gifted and Talented" = "Arts & Enrichment"
)

k12sum_agg <- k12sum %>%
  mutate(Broad_Category = dplyr::recode(Broad_Category, !!!k12_collapse_map)) %>%
  group_by(Broad_Category, Archive_Date, District) %>%
  summarize(sum = sum(sum), .groups = "drop")

k12nowsum_agg <- k12nowsum %>%
  mutate(Broad_Category = dplyr::recode(Broad_Category, !!!k12_collapse_map)) %>%
  group_by(Broad_Category, District) %>%
  summarize(Sum = sum(Sum), .groups = "drop")

he_collapse_map <- c(
  "CTE - Trades & Engineering" = "CTE / Career-Technical",
  "CTE - Health Sciences" = "CTE / Career-Technical",
  "CTE - Business & Computing" = "CTE / Career-Technical",
  "Culinary/Hospitality" = "CTE / Career-Technical",
  "Science" = "STEM",
  "Math" = "STEM",
  "Humanities" = "Humanities & Social Sciences",
  "Social Science" = "Humanities & Social Sciences",
  "History" = "Humanities & Social Sciences",
  "Language" = "Humanities & Social Sciences",
  "Criminal Justice" = "Humanities & Social Sciences",
  "Legal" = "Humanities & Social Sciences",
  "Human Services" = "Humanities & Social Sciences",
  "Education" = "Humanities & Social Sciences",
  "The Arts" = "Arts & Physical Education",
  "Physical Education" = "Arts & Physical Education",
  "Extension/Outreach" = "Extension/Outreach & Library",
  "Library" = "Extension/Outreach & Library"
)

hesum_he_agg <- hesum_he %>%
  mutate(Category = dplyr::recode(as.character(Category), !!!he_collapse_map)) %>%
  group_by(Category, Archive_Date, Institution, Job_Type) %>%
  summarize(sum = sum(sum), .groups = "drop")

henowsum_he_agg <- henowsum_he %>%
  mutate(Category = dplyr::recode(Category, !!!he_collapse_map)) %>%
  group_by(Category, Institution, Job_Type) %>%
  summarize(Sum = sum(Sum), .groups = "drop")

#--------------------------------------------------
# Category colors -- one fixed hex per category, shared by every chart in
# its section, so a category is never a different color on the current-
# snapshot chart than on the longitudinal chart. First 8 slots (by
# all-time volume) are the validated categorical palette from the dataviz
# skill (fixed order, CVD-checked); slots past 8 extend it with additional
# well-separated hues for the lower-volume categories rather than
# reusing/cycling a slot. Separate palettes for the Simple and Detailed
# views since they're different category sets, not just different labels.
#--------------------------------------------------
EXT_HUES <- c("#8B5E34", "#5C7A99", "#7A7A3D", "#767671", "#2E8B87",
              "#C77DA8", "#B8860B", "#6B5B95", "#4A6670", "#D97757")
BASE8 <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7", "#e34948")

# Ordered by real posting volume: Special Education 28%, Core Academic 27%,
# Elementary & Early Childhood 22%, Arts & Enrichment 15%, CTE 9%.
K12_CATEGORY_COLORS_AGG <- setNames(BASE8[1:5], c(
  "Special Education", "Core Academic", "Elementary & Early Childhood",
  "Arts & Enrichment", "CTE"
))

K12_CATEGORY_COLORS_DETAIL <- setNames(c(BASE8, EXT_HUES[1:8]), c(
  "Elementary", "SpEd - General", "SpEd - Resource/LS", "Music", "Math",
  "Engl. LA", "Science", "CTE - Trades/Ag", "Language", "CTE - Biz/Family",
  "Soc. St.", "Physical Education", "Early Childhood", "Art", "Library Media",
  "Gifted and Talented"
))

# Ordered by real posting volume: CTE/Career-Technical 45%, Humanities &
# Social Sciences 21%, STEM 19%, Arts & PE 9%, Extension/Outreach & Library 6%.
HE_CATEGORY_COLORS_AGG <- setNames(BASE8[1:5], c(
  "CTE / Career-Technical", "Humanities & Social Sciences", "STEM",
  "Arts & Physical Education", "Extension/Outreach & Library"
))

HE_CATEGORY_COLORS_DETAIL <- setNames(c(BASE8, EXT_HUES), c(
  "CTE - Trades & Engineering", "Science", "CTE - Health Sciences", "CTE - Business & Computing",
  "The Arts", "Humanities", "Social Science", "Extension/Outreach", "Math",
  "History", "Education", "Culinary/Hospitality", "Physical Education",
  "Library", "Language", "Criminal Justice", "Legal", "Human Services"
))

# Appointment colors retained for any future split view. The current
# faculty-trends table now filters its underlying Job_Type directly.
HE_JOB_TYPE_COLORS <- c(
  "Faculty/instructor (non-adjunct)" = "#2a78d6",
  "Adjunct/Part-Time"  = "#eb6834"
)

#--------------------------------------------------
# "New this week" -- row-level history, diffed against the previous
# archived week by (identity columns), scoped to Teacher/Faculty postings
# same as the rest of the trend tabs (k12jobanalysis.csv/facultydata.csv
# are already filtered that way upstream).
#--------------------------------------------------
k12_history <- read.csv("k12jobanalysis.csv", fileEncoding = "UTF-8") %>%
  validate_and_pad_schema(c("title", "Archive_Date", "location", "District"), "k12jobanalysis.csv") %>%
  mutate(Archive_Date = as.Date(Archive_Date))

k12_new_this_week <- {
  dates <- sort(unique(k12_history$Archive_Date))
  if (length(dates) >= 2) {
    latest <- dates[length(dates)]
    previous <- dates[length(dates) - 1]
    k12_history %>%
      filter(Archive_Date == latest) %>%
      anti_join(k12_history %>% filter(Archive_Date == previous), by = c("title", "location", "District"))
  } else {
    k12_history[0, ]
  }
}

he_history <- read.csv("facultydata.csv", fileEncoding = "UTF-8") %>%
  validate_and_pad_schema(c("Title", "Location", "Institution", "Link", "Archive_Date", "Job_Type", "Category"), "facultydata.csv") %>%
  mutate(Archive_Date = as.Date(Archive_Date)) %>%
  filter(Job_Type %in% c("Instructor/Teacher/Faculty", "Adjunct/Part-Time Faculty")) %>%
  mutate(Appointment = dplyr::recode(
    Job_Type,
    "Instructor/Teacher/Faculty" = "Faculty/instructor (non-adjunct)",
    "Adjunct/Part-Time Faculty" = "Adjunct/part-time faculty"
  ))

he_new_this_week <- {
  dates <- sort(unique(he_history$Archive_Date))
  if (length(dates) >= 2) {
    latest <- dates[length(dates)]
    previous <- dates[length(dates) - 1]
    he_history %>%
      filter(Archive_Date == latest) %>%
      anti_join(he_history %>% filter(Archive_Date == previous), by = c("Title", "Location", "Institution"))
  } else {
    he_history[0, ]
  }
}

#--------------------------------------------------
# Combined map dataset -- one row per K-12 district or HE institution,
# merging each entity's existing location/salary reference data with a
# live current-openings count, a couple of sample current titles, and a
# new-this-week count (reusing k12_new_this_week/he_new_this_week above,
# so it's the same Teacher/Faculty-scoped definition of "new" as the New
# This Week tabs, not a separate metric).
#--------------------------------------------------
k12_current_counts <- combineddata %>% count(District, name = "CurrentCount")
k12_sample_titles <- combineddata %>%
  group_by(District) %>%
  summarize(SampleTitles = paste(head(Title, 3), collapse = "; "), .groups = "drop")
k12_weekly_new <- k12_new_this_week %>% count(District, name = "WeeklyNew")

# Teacher-only current postings (k12_history is already scoped to
# position == "Teacher", same as the Teacher Trends tabs), for a vacancy
# rate scoped consistently with its denominator (Teachers_Total_FTE from
# CCD, also teacher-only) -- dividing ALL open postings (bus drivers,
# coaches, custodians, etc. included) by teacher FTE would overstate the
# rate by mixing categories that don't correspond to each other.
# Vacancy rate needs a minimum FTE to be a meaningful comparison -- 1
# opening out of 1 teacher is a "100%" rate that isn't really comparable
# to Laramie County's ~5% on a base of 1,067 teachers. Currently a no-op
# against real data (every WY district/institution has 15+ FTE), but
# keeps a future small entity from producing a noisy, misleading rate.
VACANCY_RATE_MIN_FTE <- 10

# Live links for the Summary tables' source footnotes -- see
# salary_scrapers.R / ipeds_salary_scraper.R's own header comments for why
# these are the canonical pages to check if a fetch ever starts failing.
WSBA_SALARY_SOURCE_URL <- "https://sites.google.com/wsba-wy.org/my-wsba/wy-education-salaries"
IPEDS_SALARY_SOURCE_URL <- "https://educationdata.urban.org"

# WSBA's Salary_Year is a "2025-2026"-style school-year range -- the prior
# year is that same range shifted back by one, not a separate value we
# fetch. Used to put the actual year in the K-12 summary table's column
# headers instead of a generic "Prior Year" label.
prior_school_year_label <- function(year_label) {
  parts <- regmatches(year_label, gregexpr("[0-9]{4}", year_label))[[1]]
  if (length(parts) != 2) return(NA_character_)
  paste0(as.integer(parts[1]) - 1, "-", as.integer(parts[2]) - 1)
}

k12_teacher_current_counts <- k12_history %>%
  filter(Archive_Date == max(Archive_Date)) %>%
  count(District, name = "TeacherCurrentCount")

map_k12 <- mapdata2_k12 %>%
  left_join(k12_current_counts, by = c("Name" = "District")) %>%
  left_join(k12_sample_titles, by = c("Name" = "District")) %>%
  left_join(k12_weekly_new, by = c("Name" = "District")) %>%
  left_join(k12_teacher_current_counts, by = c("Name" = "District")) %>%
  mutate(
    CurrentCount = coalesce(CurrentCount, 0L),
    WeeklyNew = coalesce(WeeklyNew, 0L),
    SampleTitles = coalesce(SampleTitles, ""),
    Type = "K-12 District",
    TeacherCurrentCount = coalesce(TeacherCurrentCount, 0L),
    Vacancy_Rate = ifelse(!is.na(Teachers_Total_FTE) & Teachers_Total_FTE >= VACANCY_RATE_MIN_FTE,
                           TeacherCurrentCount / Teachers_Total_FTE, NA_real_),
    Vacancy_Numerator = TeacherCurrentCount, Vacancy_Denominator = Teachers_Total_FTE,
    Vacancy_Rate_Shared = FALSE,
    Faculty_Avg_Salary = NA_real_, Faculty_Avg_Salary_Professor = NA_real_, Faculty_Count = NA_real_,
    Faculty_Avg_Salary_Y1Ago = NA_real_, Faculty_Avg_Salary_Y2Ago = NA_real_,
    Salary_Note = NA_character_,
    # Already fetched from CCD alongside Teachers_Total_FTE (same API call
    # in ccd_staff_scraper.R) but never surfaced anywhere in the app until
    # now -- Students_Per_Teacher is a real class-size-ish proxy a
    # prospective teacher would want, at zero additional scraping cost.
    Students_Per_Teacher = ifelse(!is.na(Teachers_Total_FTE) & Teachers_Total_FTE > 0,
                                   Enrollment / Teachers_Total_FTE, NA_real_),
    # No district-level enrollment-trend equivalent on the K-12 side (CCD's
    # own multi-year history isn't pulled here) -- HE's Enrollment_Change_
    # Pct (institution-level, from ipeds_enrollment_scraper.R) is a
    # different, institution-specific signal from Population_Change_Pct
    # (county-level, already real here via the Census join above), not a
    # duplicate of it.
    Enrollment_Change_Pct = NA_real_,
    # No K-12 equivalent -- Pell Grant recipient share is an HE-specific
    # federal program (FSA), the closest HE analogue to Child_Poverty_Rate
    # below (which itself has no HE equivalent -- see that column's own
    # comment on this K-12 side further down).
    Pell_Recipient_Share = NA_real_, Pell_Year = NA_character_
  ) %>%
  select(Name, Longitude, Latitude, Type, CurrentCount, WeeklyNew, SampleTitles,
         Link = Job_Link, Teacher_Base_Salary, Teacher_Base_Salary_Prior_Year, Salary_Year,
         Teacher_Avg_Salary, Teacher_Avg_Base_Salary, WDE_Salary_Year, WDE_Salary_Source,
         Superintendent_Salary, Superintendent_Contract_Days,
         Faculty_Avg_Salary, Faculty_Avg_Salary_Professor, Faculty_Count,
         Faculty_Avg_Salary_Y1Ago, Faculty_Avg_Salary_Y2Ago, Salary_Note,
         Vacancy_Rate, Vacancy_Numerator, Vacancy_Denominator, Vacancy_Rate_Shared, Salary_Source, County,
         Data_Coverage, Enrollment, Students_Per_Teacher, Enrollment_Change_Pct, Pell_Recipient_Share, Pell_Year,
         Median_Household_Income, Median_Gross_Rent, Mining_Employment_Share, Population_Change_Pct, ACS_Year,
         Child_Poverty_Rate, SAIPE_Year)

he_current_counts <- ccdata %>% count(Institution, name = "CurrentCount")
he_sample_titles <- ccdata %>%
  group_by(Institution) %>%
  summarize(SampleTitles = paste(head(Title, 3), collapse = "; "), .groups = "drop")
he_weekly_new <- he_new_this_week %>% count(Institution, name = "WeeklyNew")

# Scoped to full-time faculty only, matching Faculty_Count's own scope --
# IPEDS's salaries-instructional-staff survey (the source of Faculty_Count)
# only covers full-time instructional staff, so mixing Adjunct/Part-Time
# postings into this numerator would overstate the rate the same way using
# CurrentCount (all K-12 job categories) would on the K-12 side.
#
# Regression: he_history itself was later widened to include BOTH Job_Types
# (for the FT/PT appointment toggles added to the trend/current/new-this-
# week tabs), but this consumer wasn't updated at the same time -- it
# silently started counting adjunct postings into a full-time-only
# denominator. This filter makes the scope explicit again instead of
# assuming the shared he_history object's scope.
he_faculty_current_counts <- he_history %>%
  filter(Archive_Date == max(Archive_Date), Job_Type == "Instructor/Teacher/Faculty") %>%
  count(Institution, name = "FacultyCurrentCount")

map_he <- mapdata2_he %>%
  left_join(he_current_counts, by = c("Name" = "Institution")) %>%
  left_join(he_sample_titles, by = c("Name" = "Institution")) %>%
  left_join(he_weekly_new, by = c("Name" = "Institution")) %>%
  left_join(he_faculty_current_counts, by = c("Name" = "Institution")) %>%
  mutate(
    CurrentCount = coalesce(CurrentCount, 0L),
    WeeklyNew = coalesce(WeeklyNew, 0L),
    SampleTitles = coalesce(SampleTitles, ""),
    Type = "Higher Ed Institution",
    FacultyCurrentCount = coalesce(FacultyCurrentCount, 0L),
    # Sheridan College and Gillette College (flagged via Salary_Note) share
    # one IPEDS-reported Faculty_Count -- dividing either institution's own
    # posting count by that joint total isn't a real per-campus rate (it
    # inflated Sheridan to 66% in testing, since its numerator is compared
    # against a denominator that's really Sheridan+Gillette combined).
    # Rather than suppress both to NA, both show the same JOINT rate --
    # their combined current postings over the one shared faculty count --
    # since that's still a real, meaningful "how tight is staffing at this
    # combined institution" figure, just not splittable by campus. Vacancy_
    # Rate_Shared flags this so the UI can label it rather than presenting
    # it as if it were campus-specific. Once IPEDS starts reporting them
    # separately (watched by fetch_sheridan_gillette_split_check() in
    # ipeds_salary_scraper.R), Faculty_Count will stop being identical for
    # both rows and this collapses back to each institution's own rate with
    # no code change needed here.
    Vacancy_Rate_Shared = !is.na(Salary_Note) & nzchar(trimws(Salary_Note)),
    Vacancy_Numerator = ifelse(Vacancy_Rate_Shared,
                                sum(FacultyCurrentCount[!is.na(Salary_Note) & nzchar(trimws(Salary_Note))]),
                                FacultyCurrentCount),
    Vacancy_Rate = ifelse(!is.na(Faculty_Count) & Faculty_Count >= VACANCY_RATE_MIN_FTE,
                           Vacancy_Numerator / Faculty_Count, NA_real_),
    Vacancy_Denominator = Faculty_Count,
    Teacher_Base_Salary = NA_real_, Teacher_Base_Salary_Prior_Year = NA_real_,
    Teacher_Avg_Salary = NA_real_, Teacher_Avg_Base_Salary = NA_real_,
    WDE_Salary_Year = NA_character_, WDE_Salary_Source = NA_character_,
    Superintendent_Salary = NA_real_, Superintendent_Contract_Days = NA_real_,
    # Every HE institution is scraped from a genuine structured job-board
    # platform (NEOGOV/PeopleAdmin/Oracle) -- there's no "misc" tier on the
    # HE side, unlike K-12's Data_Coverage from misc_district_registry.
    Data_Coverage = "Full",
    # Enrollment comes straight from salarymap.csv (mapdata2_he, joined via
    # ipeds_enrollment_scraper.R's fetch_ipeds_he_enrollment() -- added
    # 2026-08-06). Students_Per_Teacher is computed here the same way
    # map_k12 computes it (Enrollment / Teachers_Total_FTE there), just
    # against Faculty_Count instead -- IPEDS's own instructional-staff
    # headcount, already fetched for the vacancy-rate denominator.
    Students_Per_Teacher = ifelse(!is.na(Faculty_Count) & Faculty_Count > 0,
                                   Enrollment / Faculty_Count, NA_real_),
    # Enrollment_Change_Pct also comes straight through from salarymap.csv
    # (ipeds_enrollment_scraper.R's fetch_ipeds_he_enrollment_trend() -- a
    # 5-year institution-level trend, HE's analogue of Population_Change_
    # Pct below but at the institution rather than county level).
    #
    # County/Median_Household_Income/Median_Gross_Rent/Mining_Employment_
    # Share/Population_Change_Pct/ACS_Year come straight from salarymap.csv
    # (mapdata2_he, joined via census_acs_scraper.R's fetch_census_county_
    # context() the same way K-12 gets it -- added 2026-08-06, each HE
    # institution's real home county is now hand-maintained the same way
    # Latitude/Longitude already are) -- no override needed here, unlike
    # the K-12-only fields above.
    #
    # SAIPE child poverty rate has no HE equivalent -- it's a K-12 school-
    # district-specific federal program (child poverty, not overall
    # poverty), and doesn't map onto a mostly-adult college student body.
    # Pell_Recipient_Share/Pell_Year (fsa_pell_scraper.R, added 2026-08-06)
    # come straight through from salarymap.csv instead -- the real HE
    # analogue: share of students who are low-income, from a different
    # federal program (FSA, not SAIPE) since SAIPE itself has no HE
    # equivalent.
    Child_Poverty_Rate = NA_real_, SAIPE_Year = NA_integer_
  ) %>%
  select(Name, Longitude, Latitude, Type, CurrentCount, WeeklyNew, SampleTitles,
         Link, Teacher_Base_Salary, Teacher_Base_Salary_Prior_Year, Salary_Year,
         Teacher_Avg_Salary, Teacher_Avg_Base_Salary, WDE_Salary_Year, WDE_Salary_Source,
         Superintendent_Salary, Superintendent_Contract_Days,
         Faculty_Avg_Salary, Faculty_Avg_Salary_Professor, Faculty_Count,
         Faculty_Avg_Salary_Y1Ago, Faculty_Avg_Salary_Y2Ago, Salary_Note,
         Vacancy_Rate, Vacancy_Numerator, Vacancy_Denominator, Vacancy_Rate_Shared, Salary_Source, County,
         Data_Coverage, Enrollment, Students_Per_Teacher, Enrollment_Change_Pct, Pell_Recipient_Share, Pell_Year,
         Median_Household_Income, Median_Gross_Rent, Mining_Employment_Share, Population_Change_Pct, ACS_Year,
         Child_Poverty_Rate, SAIPE_Year)

combined_map_data <- bind_rows(map_k12, map_he)

map_county_choices <- c("All Counties" = "all", setNames(
  sort(unique(na.omit(combined_map_data$County))),
  sub(", Wyoming$", "", sort(unique(na.omit(combined_map_data$County))))
))

# Map marker size/color -- radius scaled by sqrt(CurrentCount), not
# CurrentCount directly, since a circle's perceived size is its area, not
# its radius (linear scaling would visually overstate the gap between a
# 10-opening and a 100-opening district). Color is a single-hue sequential
# ramp on Vacancy_Rate (a bounded 0-100% "severity" dimension that pairs
# naturally with size-as-volume) rather than salary, which would need two
# separate ramps for K-12 vs. HE's different pay scales. Domain is fixed
# from the full unfiltered dataset so the color scale (and legend) don't
# shift as the K-12/HE checkbox filter changes what's on screen.
MAP_MARKER_MIN_RADIUS <- 6
MAP_MARKER_MAX_RADIUS <- 24
map_marker_radius <- function(current_count) {
  pmin(MAP_MARKER_MAX_RADIUS, MAP_MARKER_MIN_RADIUS + 1.2 * sqrt(current_count))
}
# Flexible query matching for map and job searches. Supports keywords,
# abbreviations (e.g. "special ed", "sped", "math", "cte"), and multi-word queries.
match_job_query <- function(title_vec, query) {
  if (is.null(query) || !nzchar(trimws(query))) return(rep(TRUE, length(title_vec)))
  q <- trimws(query)
  q_lower <- tolower(q)
  pattern <- if (q_lower %in% c("special ed", "sped", "special education")) {
    "special[[:space:]]*ed|sped|special[[:space:]]*educ"
  } else if (q_lower %in% c("math", "mathematics")) {
    "math"
  } else if (q_lower %in% c("pe", "phys ed", "physical education")) {
    "physical[[:space:]]*ed|phys[[:space:]]*ed|\\bpe\\b"
  } else if (q_lower %in% c("cte", "career and technical", "vocational")) {
    "cte|career[[:space:]]*and[[:space:]]*tech|vocational|industrial[[:space:]]*arts"
  } else if (q_lower %in% c("bus driver", "driver", "transportation")) {
    "driver|transportation"
  } else {
    words <- strsplit(q, "[[:space:]]+")[[1]]
    words <- words[nzchar(words)]
    if (length(words) > 1) {
      paste0("(?=.*", words, ")", collapse = "")
    } else {
      words
    }
  }
  !is.na(title_vec) & grepl(pattern, title_vec, ignore.case = TRUE, perl = TRUE)
}

# If every Vacancy_Rate is NA (both the CCD and IPEDS staffing fetches
# failing the same week, or a fresh checkout before either has ever
# succeeded once), range(..., na.rm = TRUE) on an all-NA vector returns
# c(Inf, -Inf) with a warning -- colorNumeric() then gets an invalid
# domain and the map's color legend breaks for every user, not just the
# vacancy-rate feature. Falls back to a placeholder 0-1 domain (never
# actually used to color a marker, since every Vacancy_Rate would be NA
# too, rendering na.color everywhere) so the legend itself stays valid.
# Kept as its own function (rather than inlined) so the all-NA branch is
# unit-testable without constructing a full combined_map_data.
compute_vacancy_rate_domain <- function(vacancy_rate) {
  if (all(is.na(vacancy_rate))) c(0, 1) else range(vacancy_rate, na.rm = TRUE)
}
vacancy_rate_domain <- compute_vacancy_rate_domain(combined_map_data$Vacancy_Rate)
vacancy_rate_palette <- colorNumeric(palette = "YlOrRd", domain = vacancy_rate_domain, na.color = "#9e9e9e")

#--------------------------------------------------
# UI
#--------------------------------------------------
ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = "Wyo Edu Jobs", titleWidth = "180px"),
  dashboardSidebar(
    width = "180px",
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Home", tabName = "intro", icon = icon("house")),
      menuItem("Map", tabName = "map_tab", icon = icon("map-location-dot")),
      tags$li(class = "header", "K-12"),
      menuItem("Jobs", tabName = "k12_table", icon = icon("table")),
      menuItem("Districts", tabName = "k12_summary", icon = icon("school")),
      menuItem("Trends", tabName = "k12_trends", icon = icon("chart-line")),
      menuItem("Time-to-Fill", tabName = "k12_spells", icon = icon("stopwatch")),
      tags$li(class = "header", "HIGHER ED"),
      menuItem("Jobs", tabName = "he_table", icon = icon("table")),
      menuItem("Colleges", tabName = "he_summary", icon = icon("building-columns")),
      menuItem("Trends", tabName = "he_trends", icon = icon("chart-line")),
      menuItem("Time-to-Fill", tabName = "he_spells", icon = icon("stopwatch")),
      tags$li(class = "header", "COMMUNITY"),
      menuItem("Data", tabName = "data_tab", icon = icon("database")),
      menuItem("Feedback", icon = icon("flag"),
               href = "https://github.com/bostickmike/Wyoming-Education-Jobs-Dashboard/issues/new?template=data_issue.yml",
               newtab = TRUE)
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* AdminLTE header logo styling */
    .main-header .logo {
      font-size: 18px;
      font-weight: 600;
      padding: 0 8px;
    }
    .leaflet-tooltip {
      max-width: min(800px, 92vw) !important;
      min-width: min(320px, 85vw) !important;
      white-space: normal !important;
      background-color: rgba(255,255,255,0.95);
      padding: 6px 10px;
      border-radius: 6px;
      border: 1px solid gray;
      font-size: 14px;
      display: inline-block;
      box-sizing: border-box;
    }
    /* Flat modern section headers */
    .sidebar-menu .header {
      font-size: 11px !important;
      font-weight: 700 !important;
      letter-spacing: 0.08em;
      color: #8aa4af !important;
      background: transparent !important;
      text-transform: uppercase;
      padding: 14px 15px 4px 15px !important;
    }
    /* Rounded pill navigation styling */
    .sidebar-menu > li > a {
      border-left: none !important;
      border-radius: 6px;
      margin: 1px 8px;
      padding: 8px 12px;
      transition: background-color 0.15s ease-in-out;
    }
    .sidebar-menu > li.active > a {
      background-color: #2b3b4c !important;
      color: #ffffff !important;
      font-weight: 600;
    }
    @media (max-width: 767px) {
      .leaflet-container {
        height: 480px !important;
      }
    }
    /* Clean tabBox styling */
    .nav-tabs-custom {
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
      border: 1px solid #e5e9ec;
      border-radius: 4px;
    }
    .nav-tabs-custom > .nav-tabs > li.active {
      border-top-color: #2a78d6;
    }
    .nav-tabs-custom > .nav-tabs > li.active > a {
      font-weight: 600;
      color: #2a78d6;
    }
    /* Table toolbar styling */
    .table-toolbar {
      display: flex;
      align-items: center;
      justify-content: space-between;
      flex-wrap: wrap;
      gap: 10px;
      margin-bottom: 12px;
      background: #f8fafc;
      padding: 10px 14px;
      border-radius: 6px;
      border: 1px solid #e2e8f0;
    }
    /* Clean table styling so tables do not overfill space */
    .trends-table-container {
      width: 100%;
      overflow-x: auto;
      margin-top: 10px;
    }
    .trends-table-container table {
      width: 100% !important;
      max-width: 980px !important;
      margin: 0 auto;
      border-collapse: separate;
      border-spacing: 0;
      border-radius: 6px;
      overflow: hidden;
      border: 1px solid #e2e8f0;
      background: #ffffff;
      font-size: 13.5px;
    }
    .trends-table-container th {
      background-color: #f8fafc !important;
      color: #334155;
      font-weight: 600;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      padding: 10px 14px !important;
      border-bottom: 2px solid #e2e8f0 !important;
    }
    .trends-table-container td {
      padding: 8px 14px !important;
      border-bottom: 1px solid #f1f5f9;
      color: #1e293b;
      vertical-align: middle !important;
    }
    .trends-table-container tr:last-child td {
      border-bottom: none;
    }
    .trends-table-container tr:hover td {
      background-color: #f8fafc;
    }
  ")),
      tags$script(HTML("
    $(document).on('click', '.sidebar-menu a[data-toggle=\"tab\"]', function() {
      if ($(window).width() < 768) {
        $('body').removeClass('sidebar-open');
      }
    });
  "))
    )
    ,
    tabItems(
      # ------------------ Global Introduction ------------------
      tabItem(
        tabName = "intro",
        h1("Education Jobs in Wyoming"),
        uiOutput("data_load_issues_banner"),
        fluidRow(
          valueBoxOutput("kpi_k12_total", width = 4),
          valueBoxOutput("kpi_he_total", width = 4),
          valueBoxOutput("kpi_last_refreshed", width = 4)
        ),
        fluidRow(
          column(width = 6, uiOutput("k12_biggest_mover")),
          column(width = 6, uiOutput("he_biggest_mover"))
        ),
        fluidRow(
          box(title = "Top K-12 Hiring Districts This Week", width = 6, status = "primary",
              div(style = "overflow-x: auto;", tableOutput("top_k12_districts"))),
          box(title = "Top Higher Ed Hiring Institutions This Week", width = 6, status = "primary",
              div(style = "overflow-x: auto;", tableOutput("top_he_institutions")))
        ),
        fluidRow(
          box(title = "Highest Teacher Vacancy Rate", width = 6, status = "primary",
              withSpinner(plotlyOutput("k12_vacancy_leaderboard", height = 320)),
              helpText("Districts with at least", VACANCY_RATE_MIN_FTE, "full-time teachers (FTE).")),
          box(title = "Highest Faculty Vacancy Rate", width = 6, status = "primary",
              withSpinner(plotlyOutput("he_vacancy_leaderboard", height = 320)),
              helpText("Institutions with at least", VACANCY_RATE_MIN_FTE, "full-time faculty.",
                        "* Sheridan College and Gillette College are reported jointly by IPEDS and share one combined rate."))
        ),
        div(style = "color:#999; font-size:0.8em; padding: 0 5px 5px;",
            "K-12 vacancy rate is current teacher postings ÷ full-time teacher staffing (NCES CCD); Higher Ed vacancy rate is current instructor/faculty postings ÷ full-time instructional staff (NCES IPEDS). ",
            "The two use different staffing sources and reporting years — compare rates within a type (district vs. district, institution vs. institution), not across K-12 and Higher Ed."),
        div(style = "text-align:center; color:#999; font-size:0.85em; padding:15px;",
            paste0("Refreshed on: ", last_refreshed_date, " · Developed by Mike Bostick and Mark Perkins"))
      ),

      tabItem(
        tabName = "map_tab",
        h1("Where the Openings Are"),
        box(
          width = 12, title = "Wyoming Education Job Openings Map", status = "primary",
          fluidRow(
            column(
              6,
              textInput(
                "map_search_keyword",
                label = tagList(icon("magnifying-glass"), " Search Open Positions by Keyword or Role:"),
                value = "",
                placeholder = "e.g., special ed, mathematics, science, music, coach, bus driver..."
              ),
              div(
                style = "margin-top: -5px; margin-bottom: 8px; font-size: 11.5px; color: #64748b;",
                span("Popular: "),
                actionLink("map_chip_sped", "Special Ed", style = "margin-right: 8px; font-weight: 500;"),
                actionLink("map_chip_math", "Mathematics", style = "margin-right: 8px; font-weight: 500;"),
                actionLink("map_chip_elem", "Elementary", style = "margin-right: 8px; font-weight: 500;"),
                actionLink("map_chip_science", "Science", style = "margin-right: 8px; font-weight: 500;"),
                actionLink("map_chip_music", "Music / Art", style = "margin-right: 8px; font-weight: 500;"),
                actionLink("map_chip_driver", "Bus Driver", style = "margin-right: 8px; font-weight: 500;"),
                actionLink("map_chip_clear", "✕ Clear", style = "color: #d9534f; margin-left: 4px; font-weight: 600;")
              )
            ),
            column(
              3,
              selectInput(
                "map_county", "Filter by County:",
                choices = map_county_choices,
                selected = "all"
              )
            ),
            column(
              3,
              checkboxGroupInput(
                "map_types", "Employer Type:",
                choices = c("K-12 Districts" = "K-12 District", "Higher Ed" = "Higher Ed Institution"),
                selected = c("K-12 District", "Higher Ed Institution"),
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              4,
              selectInput(
                "map_salary_tier", "Pay:",
                # Built from PAY_BANDS. A posting counts by its posted pay,
                # else its district/institution role figure, else the BLS
                # estimate -- each in its own unit.
                choices = c("All pay" = "all", setNames(names(PAY_BANDS), vapply(PAY_BANDS, function(b) {
                  paste0(if (b$unit == "hour") "Hourly: " else "Annual: ", b$label)
                }, character(1)))),
                selected = "all"
              )
            ),
            column(
              4,
              selectInput(
                "map_urgency", "Posting Volume:",
                choices = c(
                  "All Active Locations" = "all",
                  "High Volume (5+ openings)" = "5_plus",
                  "Multiple Openings (2+)" = "2_plus",
                  "New Postings This Week" = "has_new"
                ),
                selected = "all"
              )
            ),
            column(
              4,
              div(
                style = "padding-top: 25px;",
                actionButton("map_filter_reset", "Reset All Filters", icon = icon("arrow-rotate-left"), class = "btn-sm btn-default btn-block")
              )
            )
          ),
          div(
            style = "margin: 2px 0 12px 2px;",
            uiOutput("map_filter_count")
          ),
          withSpinner(leafletOutput("combined_map", height = 650)),
          helpText("Only locations with current openings are shown. Circle size reflects current openings; color reflects teacher/faculty vacancy rate where available. Click a marker to jump to its filtered Jobs Table. K-12 and Higher Ed vacancy rates use different staffing sources (CCD vs. IPEDS) and years -- the shared color scale is for a rough at-a-glance read, not a precise cross-type comparison. A red 'Partial' badge means that district's postings come from Wyoming School Boards Association's statewide feed (which reliably captures only ~25-40% of real openings) instead of the district's own job board -- its current-openings count is a floor, not a complete count.")
        )
      ),

      tabItem(
        tabName = "data_tab",
        h1("Data Sources & Open Research Downloads"),
        p(class = "lead", "Transparent data provenance, methodology documentation, and downloadable datasets for researchers, policy analysts, school administrators, and the public."),
        
        fluidRow(
          box(
            width = 12, title = "Open Research Downloads", status = "primary", solidHeader = TRUE,
            p("Every weekly snapshot is preserved. Download current or multi-year longitudinal datasets in standard CSV format:"),
            fluidRow(
              column(3,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("briefcase"), " Active K-12 Jobs", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "All current public school job openings in Wyoming with standardized titles, positions, locations, links, and district/role salary benchmarks."),
                  downloadButton("download_active_k12", "Active Jobs (CSV)", class = "btn-default btn-xs btn-block")
                )
              ),
              column(3,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("clock-rotate-left"), " K-12 History Archive", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "Multi-year historical record of weekly K-12 job postings across Wyoming (76,000+ records) for hiring seasonality and longitudinal demand studies."),
                  downloadButton("download_history_k12", "Historical K-12 (CSV)", class = "btn-default btn-xs btn-block")
                )
              ),
              column(3,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("stopwatch"), " K-12 Time-to-Fill", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "Unique job postings with NCES LEAID, estimated days open (time-to-fill), and high-need subject areas (STEM, SPED, etc.) across weekly snapshots."),
                  downloadButton("download_spells", "Job Duration Data (CSV)", class = "btn-default btn-xs btn-block")
                )
              ),
              column(3,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("school"), " District Benchmarks", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "District profiles: NCES LEAID, WSBA base salaries, WDE avg salaries, NCES teacher FTE & enrollment, student-to-teacher ratios, and Census poverty/rent."),
                  downloadButton("download_district_benchmarks", "District Profiles (CSV)", class = "btn-default btn-xs btn-block")
                )
              )
            ),
            fluidRow(
              column(3,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("users"), " WDE602 Staff Matrix", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "Wyoming Dept of Education staffing matrix: average total & base salaries and FTE counts for 51 districts across 30+ assignment categories."),
                  downloadButton("download_wde_staff", "WDE602 Matrix (CSV)", class = "btn-default btn-xs btn-block")
                )
              ),
              column(3,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("building-columns"), " Higher Ed Archive", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "Longitudinal postings for instructional faculty, adjuncts, and staff across Wyoming's 9 public community colleges and UW (29,000+ records)."),
                  downloadButton("download_history_he", "Higher Ed Archive (CSV)", class = "btn-default btn-xs btn-block")
                )
              ),
              column(3,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("stopwatch"), " Higher Ed Time-to-Fill", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "Posting duration dataset tracking time-to-fill, active/closed search status, and discipline indicators for collegiate faculty."),
                  downloadButton("download_he_spells", "Higher Ed Time-to-Fill (CSV)", class = "btn-default btn-xs btn-block")
                )
              ),
              column(3,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("graduation-cap"), " Higher Ed Profiles", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "Institution-level profiles: IPEDS UNITID, instructional faculty salaries (all ranks and professor), enrollment, and federal Pell shares."),
                  downloadButton("download_he_benchmarks", "Higher Ed Profiles (CSV)", class = "btn-default btn-xs btn-block")
                )
              )
            ),
            fluidRow(
              column(4,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("chart-line"), " K-12 Weekly Vacancies", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "Weekly longitudinal vacancy totals across Wyoming school districts. Ideal for seasonal hiring cycles and supply/demand modeling."),
                  downloadButton("download_weekly_totals", "K-12 Weekly Totals (CSV)", class = "btn-default btn-xs btn-block")
                )
              ),
              column(4,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("chart-line"), " Higher Ed Weekly Vacancies", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "Weekly longitudinal vacancy totals across Wyoming community colleges and UW. Ideal for tracking higher ed hiring seasonality."),
                  downloadButton("download_he_weekly_totals", "Higher Ed Weekly Totals (CSV)", class = "btn-default btn-xs btn-block")
                )
              ),
              column(4,
                div(style = "border: 1px solid #e0e0e0; border-radius: 6px; padding: 15px; margin-bottom: 15px; background: #fafafa;",
                  h4(icon("book"), " Data Dictionary", style = "margin-top: 0; font-size: 15px; font-weight: 600;"),
                  p(style = "font-size: 12px; color: #555; min-height: 48px;", "Comprehensive machine-readable codebook documenting variable names, data types, definitions, and source authorities across all datasets."),
                  downloadButton("download_dictionary", "Codebook (CSV)", class = "btn-default btn-xs btn-block")
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12, title = "Interactive Data Dictionary & Variable Codebook", status = "primary",
            p("Search, sort, and filter variable definitions, data types, and primary source authorities across all datasets:"),
            div(style = "overflow-x: auto;", withSpinner(DTOutput("data_dictionary_table")))
          )
        ),
        
        fluidRow(
          box(
            width = 12, title = "Data Provenance & Methodology", status = "info",
            p("This project integrates 7 primary public data layers, updated either on a continuous weekly automated cadence or an annual federal/state survey release cycle:"),
            tags$div(class = "table-responsive",
              tags$table(class = "table table-striped table-hover", style = "font-size: 13px;",
                tags$thead(
                  tags$tr(
                    tags$th("Data Layer"),
                    tags$th("Source Organization"),
                    tags$th("Update Cadence"),
                    tags$th("Scope & Description")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(strong("Job Openings")),
                    tags$td("District Hiring Portals & Statewide Feeds"),
                    tags$td(span(class = "label label-success", "Weekly Refresh")),
                    tags$td("Directly compiled from online district hiring portals (Frontline/AppliTrack, SchoolSpring, Red Rover, NEOGOV, Oracle Cloud) and statewide aggregators. Cleaned to remove duplicate listings and standardized into consistent role categories.")
                  ),
                  tags$tr(
                    tags$td(strong("Starting Base Salaries")),
                    tags$td("Wyoming School Boards Association (WSBA)"),
                    tags$td(span(class = "label label-info", "Annual")),
                    tags$td("Published BA Step 1 teacher base settlements and superintendent compensation across all 48 Wyoming public school districts.")
                  ),
                  tags$tr(
                    tags$td(strong("Average Salaries by Role")),
                    tags$td("Wyoming Department of Education (WDE602)"),
                    tags$td(span(class = "label label-info", "Annual")),
                    tags$td("Official state collection reporting actual average total compensation and FTE for teachers, administrators, counselors, aides, and support staff across all districts.")
                  ),
                  tags$tr(
                    tags$td(strong("Staffing Headcounts & FTE")),
                    tags$td("NCES Common Core of Data (CCD) via Urban Institute"),
                    tags$td(span(class = "label label-info", "Annual")),
                    tags$td("Federal National Center for Education Statistics district-level teacher FTE and student enrollment counts. Powers vacancy-rate calculations and student-to-teacher ratios.")
                  ),
                  tags$tr(
                    tags$td(strong("Higher Ed Faculty Compensation")),
                    tags$td("NCES IPEDS Instructional Staff Salary Survey"),
                    tags$td(span(class = "label label-info", "Annual")),
                    tags$td("Average full-time instructional faculty salaries (all combined and full professor rank) for Wyoming's 7 community colleges, Gillette College, and the University of Wyoming.")
                  ),
                  tags$tr(
                    tags$td(strong("County Economic Context")),
                    tags$td("US Census Bureau (ACS 5-Year Estimates)"),
                    tags$td(span(class = "label label-info", "Annual")),
                    tags$td("County median household income, median gross rent, share of civilian labor force in mining/energy extraction, and 5-year population trajectory.")
                  ),
                  tags$tr(
                    tags$td(strong("Student Need Context")),
                    tags$td("Census SAIPE & US Dept of Education FSA"),
                    tags$td(span(class = "label label-info", "Annual")),
                    tags$td("Small Area Income and Poverty Estimates (SAIPE) district-level child poverty rates for K-12, and Federal Student Aid (FSA) undergraduate Pell Grant recipient shares for colleges.")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12, title = "Open Research Citation & Code", status = "primary",
            p("This project is open-source and maintained to promote labor market transparency in public education. If you use these datasets in academic research, policy analysis, or journalism, please cite:"),
            tags$blockquote(
              style = "font-size: 13px; background: #f9f9f9; padding: 10px 15px; border-left: 4px solid #2a78d6;",
              "Bostick, M., & Perkins, M. (2026). Wyoming Education Jobs Dashboard: Longitudinal Staffing Demand, Compensation, and Socioeconomic Context. ",
              tags$a(href = "https://github.com/bostickmike/Wyoming-Education-Jobs-Dashboard", target = "_blank", "https://github.com/bostickmike/Wyoming-Education-Jobs-Dashboard")
            ),
            p(style = "margin-bottom: 0; font-size: 12px; color: #777;",
              "To report discrepancies, missing districts, or data issues, please submit an issue on the ",
              tags$a(href = "https://github.com/bostickmike/Wyoming-Education-Jobs-Dashboard/issues", target = "_blank", "GitHub Issue Tracker"), ".")
          )
        )
      ),

      tabItem(
        tabName = "k12_table",
        div(
          class = "table-toolbar",
          div(
            style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
            span(style = "font-weight: 600; font-size: 13.5px; color: #334155;", "Filter Postings:"),
            radioGroupButtons(
              inputId = "k12_quick_filter",
              label = NULL,
              choices = c(
                "All Active Postings" = "all",
                "🟢 New This Week" = "new"
              ),
              selected = "all",
              status = "primary",
              size = "sm",
              checkIcon = list(yes = icon("check"))
            )
          ),
          uiOutput("k12_filter_status")
        ),
        p(class = "help-block", style = "margin-bottom: 8px;", paste0("Posted Pay is the pay the job posting itself states (hourly, daily, annual, or a season stipend, as written); blank where the posting gives no figure. ",
          "Typical pay (BLS est.) fills in where the posting states none: the Bureau of Labor Statistics' ",
          if (!is.na(oews_year)) paste0("May ", oews_year, " ") else "",
          "median pay for that role in the posting's region (hover for the role and area; OEWS, all employers, not just schools), so treat it as a regional estimate, not this job's offer. "),
          "Teacher Starting Base is the district's published starting base salary for teachers (BA Step 1, Source: WSBA), shown on teaching postings only. Role Avg Salary is the average total compensation the Wyoming Department of Education reports (Source: WDE602) for that job category in that district (e.g. Teacher, Admin, Aide, Bus Driver, Custodial). It is blank where WDE602 has no matching category, such as substitutes and coaches."),
        DTOutput("k12_jobs")
      ),
      tabItem(
        tabName = "k12_summary",
        tabBox(
          id = "k12_summary_tabs",
          width = 12,
          tabPanel(
            title = tagList(icon("table-list"), " District Directory"),
            value = "k12_summary_dir",
            h4("One row per district -- current openings, teacher vacancy rate, and salary data"),
            DTOutput("k12_summary_table"),
            uiOutput("k12_summary_footnote")
          ),
          tabPanel(
            title = tagList(icon("code-compare"), " Head-to-Head Comparator"),
            value = "k12_summary_comp",
            p(class = "help-block", style = "margin-top: 6px;",
              "Select up to four school districts to evaluate staffing demand, compensation benchmarks, and community demographics side-by-side."),
            fluidRow(
              column(
                width = 8,
                selectizeInput(
                  "k12_compare_districts",
                  label = "Select School Districts (up to 4):",
                  choices = sort(unique(mapdata2_k12$Name)),
                  selected = c("Laramie County School District 1", "Natrona School District 1"),
                  multiple = TRUE,
                  options = list(maxItems = 4, placeholder = "Type or select up to 4 districts...")
                )
              ),
              column(
                width = 4,
                tags$label("Quick Presets:", style = "display: block; margin-bottom: 5px;"),
                div(
                  class = "btn-group",
                  actionButton("k12_comp_preset_top", "Top Hiring", class = "btn-sm btn-default", icon = icon("fire")),
                  actionButton("k12_comp_preset_largest", "Largest", class = "btn-sm btn-default", icon = icon("users")),
                  actionButton("k12_comp_preset_clear", "Clear", class = "btn-sm btn-default", icon = icon("xmark"))
                )
              )
            ),
            uiOutput("k12_compare_cards"),
            fluidRow(
              box(
                width = 6, title = "Starting Base & Average Salary", status = "primary", solidHeader = FALSE,
                withSpinner(plotlyOutput("k12_compare_salary_plot", height = "360px"))
              ),
              box(
                width = 6, title = "Student Enrollment & Active Openings", status = "primary", solidHeader = FALSE,
                withSpinner(plotlyOutput("k12_compare_metrics_plot", height = "360px"))
              )
            ),
            box(
              width = 12, title = "Active Openings in Selected Districts", status = "info", solidHeader = FALSE,
              DTOutput("k12_compare_jobs_table")
            )
          )
        )
      ),

      tabItem(
        tabName = "k12_trends",
        tabBox(
          id = "k12_trends_tabs",
          width = 12,
          tabPanel(
            title = tagList(icon("chart-column"), " Current Trends"),
            value = "k12_trends_current",
            p(class = "text-muted", style = "margin-top: 5px; margin-bottom: 12px;",
              "Current subject distribution across active K-12 postings with 12-week vacancy sparklines."),
            fluidRow(
              column(
                width = 6,
                radioButtons("k12_detail_level_current", "View:",
                             choices = c("Individual subjects" = "detail",
                                         "Major subject groups" = "agg"),
                             selected = "detail", inline = TRUE)
              ),
              column(
                width = 6,
                selectInput(
                  "district_current",
                  "Choose District:",
                  choices = sort(unique(k12nowsum$District)),
                  selected = "Total",
                  width = "100%"
                )
              )
            ),
            div(class = "trends-table-container",
                withSpinner(tableOutput("k12_current_trends_table")))
          ),
          tabPanel(
            title = tagList(icon("chart-line"), " Multi-Year Trends"),
            value = "k12_trends_historical",
            p(class = "text-muted", style = "margin-top: 5px; margin-bottom: 12px;",
              "Multi-year vacancy volume time series tracking hiring trends across weekly snapshots."),
            radioButtons("k12_detail_level_trends", "View:",
                         choices = c("Individual subjects" = "detail",
                                     "Major subject groups" = "agg"),
                         selected = "detail", inline = TRUE),
            fluidRow(
              column(
                width = 6,
                pickerInput(
                  "broad_category",
                  "Choose Teacher Category:",
                  choices  = sort(unique(k12sum$Broad_Category)),
                  selected = sort(unique(k12sum$Broad_Category)),
                  multiple = TRUE,
                  width = "100%",
                  options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3")
                )
              ),
              column(
                width = 6,
                selectInput(
                  "district_trend",
                  "Choose District:",
                  choices = sort(unique(k12sum$District)),
                  selected = "Total",
                  width = "100%"
                )
              )
            ),
            sliderInput(
              "k12_scroll",
              "Scroll timeline:",
              min = min(k12sum$Archive_Date),
              max = max(k12sum$Archive_Date),
              value = c(max(k12sum$Archive_Date) - 365,
                        max(k12sum$Archive_Date)),
              timeFormat = "%Y-%m-%d",
              width = "100%"
            ),
            withSpinner(plotlyOutput("k12_longitudinal_plot"))
          )
        )
      ),

      tabItem(
        tabName = "k12_spells",
        h3(style = "margin-top: 5px; font-weight: 600;", tagList(icon("stopwatch"), " K-12 Job Duration & Time-to-Fill")),
        p(class = "help-block",
          "Weekly tracking of posting duration and hiring speed. Measures time-to-fill and persistent vacancies across Wyoming school districts."),
        fluidRow(
          valueBoxOutput("k12_spell_median_box", width = 3),
          valueBoxOutput("k12_spell_active_box", width = 3),
          valueBoxOutput("k12_spell_persistent_box", width = 3),
          valueBoxOutput("k12_spell_hardest_box", width = 3)
        ),
        box(
          width = 12, status = "primary", solidHeader = FALSE,
          fluidRow(
            column(
              width = 3,
              selectInput("k12_spell_status", "Vacancy Status:",
                          choices = c("All Postings" = "all",
                                      "Still Active (Open)" = "Still Active",
                                      "Filled / Closed" = "Closed / Filled"),
                          selected = "all")
            ),
            column(
              width = 3,
              selectInput("k12_spell_position", "Position Group:",
                          choices = c("All Roles" = "all",
                                      "Teacher" = "Teacher",
                                      "Administration" = "Administration",
                                      "Support Staff / Classified" = "Support"),
                          selected = "all")
            ),
            column(
              width = 3,
              selectInput("k12_spell_discipline", "Discipline / Shortage Area:",
                          choices = c("All Disciplines" = "all",
                                      "STEM (Math & Science)" = "stem",
                                      "Special Education" = "sped",
                                      "Elementary Education" = "elementary",
                                      "Secondary Education" = "secondary",
                                      "Administration" = "admin",
                                      "Support / Classified" = "support"),
                          selected = "all")
            ),
            column(
              width = 3,
              selectInput("k12_spell_district", "Filter District:",
                          choices = c("All Districts" = "all"),
                          selected = "all")
            )
          )
        ),
        fluidRow(
          box(
            width = 7, title = "Median Days Open by Subject Category", status = "info",
            withSpinner(plotlyOutput("k12_spell_duration_plot", height = "360px"))
          ),
          box(
            width = 5, title = "Vacancy Duration Distribution", status = "info",
            withSpinner(plotlyOutput("k12_spell_distrib_plot", height = "360px"))
          )
        ),
        box(
          width = 12, title = "Job Duration & Time-to-Fill Explorer", status = "primary",
          p(style = "font-size: 12px; color: #777; margin-bottom: 12px;",
            "Estimated Days Open represents the calendar duration between when a posting was first captured and when it disappeared from weekly updates (or weeks observed × 7)."),
          DTOutput("k12_spell_table")
        )
      ),

      # ------------------ Higher Ed ------------------
      tabItem(
        tabName = "he_table",
        div(
          class = "table-toolbar",
          div(
            style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
            span(style = "font-weight: 600; font-size: 13.5px; color: #334155;", "Filter Postings:"),
            radioGroupButtons(
              inputId = "he_quick_filter",
              label = NULL,
              choices = c(
                "All Active Postings" = "all",
                "🟢 New This Week" = "new"
              ),
              selected = "all",
              status = "primary",
              size = "sm",
              checkIcon = list(yes = icon("check"))
            ),
            radioButtons(
              "he_table_appointment", "Appointment:",
              choices = c(
                "All roles" = "all",
                "Faculty/instructor (non-adjunct)" = "Faculty/instructor (non-adjunct)",
                "Adjunct/part-time faculty" = "Adjunct/part-time faculty",
                "Other / not faculty" = "Other / not faculty"
              ),
              selected = "all", inline = TRUE
            )
          ),
          uiOutput("he_filter_status")
        ),
        p(class = "help-block", style = "margin-bottom: 8px;", "Posted Pay is the pay the job posting itself states (hourly, annual, or per credit hour, as written); blank where the posting gives no figure. Typical pay (IPEDS est.) fills in for staff postings that state none: the college's average pay for its full-time staff in that job category (IPEDS; hover for the category), not this job's offer. Avg Faculty Salary reflects full-time instructional staff compensation from IPEDS via Urban Institute."),
        DTOutput("he_jobs")
      ),
      tabItem(
        tabName = "he_summary",
        tabBox(
          id = "he_summary_tabs",
          width = 12,
          tabPanel(
            title = tagList(icon("building-columns"), " Institution Directory"),
            value = "he_summary_dir",
            h4("One row per institution -- current openings, faculty vacancy rate, and salary data"),
            DTOutput("he_summary_table"),
            uiOutput("he_summary_footnote")
          ),
          tabPanel(
            title = tagList(icon("code-compare"), " Head-to-Head Comparator"),
            value = "he_summary_comp",
            p(class = "help-block", style = "margin-top: 6px;",
              "Select up to four higher education institutions to evaluate faculty demand, IPEDS salary benchmarks, and institutional enrollment side-by-side."),
            fluidRow(
              column(
                width = 8,
                selectizeInput(
                  "he_compare_insts",
                  label = "Select Institutions (up to 4):",
                  choices = sort(unique(mapdata2_he$Name)),
                  selected = c("University of Wyoming", "Laramie County Community College"),
                  multiple = TRUE,
                  options = list(maxItems = 4, placeholder = "Type or select up to 4 institutions...")
                )
              ),
              column(
                width = 4,
                tags$label("Quick Presets:", style = "display: block; margin-bottom: 5px;"),
                div(
                  class = "btn-group",
                  actionButton("he_comp_preset_top", "Top Hiring", class = "btn-sm btn-default", icon = icon("fire")),
                  actionButton("he_comp_preset_clear", "Clear", class = "btn-sm btn-default", icon = icon("xmark"))
                )
              )
            ),
            uiOutput("he_compare_cards"),
            fluidRow(
              box(
                width = 6, title = "Faculty Average Salary (IPEDS)", status = "primary", solidHeader = FALSE,
                withSpinner(plotlyOutput("he_compare_salary_plot", height = "360px"))
              ),
              box(
                width = 6, title = "Openings by Appointment Type", status = "primary", solidHeader = FALSE,
                withSpinner(plotlyOutput("he_compare_openings_plot", height = "360px"))
              )
            ),
            box(
              width = 12, title = "Active Openings in Selected Institutions", status = "info", solidHeader = FALSE,
              DTOutput("he_compare_jobs_table")
            )
          )
        )
      ),
      tabItem(
        tabName = "he_trends",
        tabBox(
          id = "he_trends_tabs",
          width = 12,
          tabPanel(
            title = tagList(icon("chart-column"), " Current Trends"),
            value = "he_trends_current",
            p(class = "text-muted", style = "margin-top: 5px; margin-bottom: 12px;",
              "Current institution and discipline breakdown across active higher education postings with 12-week sparklines."),
            fluidRow(
              column(
                width = 4,
                radioButtons("he_detail_level_current", "View:",
                             choices = c("Individual fields" = "detail", "Major academic areas" = "agg"),
                             selected = "detail", inline = TRUE)
              ),
              column(
                width = 4,
                radioButtons("he_current_appointment", "Appointment:",
                             choices = c(
                               "All faculty postings" = "all",
                               "Faculty/instructor (non-adjunct)" = "Instructor/Teacher/Faculty",
                               "Adjunct/part-time faculty" = "Adjunct/Part-Time Faculty"
                             ),
                             selected = "all", inline = TRUE)
              ),
              column(
                width = 4,
                selectInput("inst_current", "Select Institution:",
                            choices = sort(unique(henowsum_he$Institution)), selected = "Total", width = "100%")
              )
            ),
            div(class = "trends-table-container",
                withSpinner(tableOutput("he_current_trends_table")))
          ),
          tabPanel(
            title = tagList(icon("chart-line"), " Multi-Year Trends"),
            value = "he_trends_historical",
            p(class = "text-muted", style = "margin-top: 5px; margin-bottom: 12px;",
              "Multi-year vacancy volume time series tracking higher education hiring trends across weekly snapshots."),
            radioButtons("he_detail_level_trends", "View:",
                         choices = c("Individual fields" = "detail", "Major academic areas" = "agg"),
                         selected = "detail", inline = TRUE),
            fluidRow(
              column(
                width = 6,
                pickerInput(
                  "he_category",
                  "Choose Category:",
                  choices  = sort(unique(hesum_he$Category)),
                  selected = sort(unique(hesum_he$Category)),
                  multiple = TRUE,
                  width = "100%",
                  options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3")
                )
              ),
              column(
                width = 6,
                selectInput(
                  "inst_trend",
                  "Select Institution:",
                  choices = sort(unique(hesum_he$Institution)),
                  selected = "Total",
                  width = "100%"
                )
              )
            ),
            textOutput("he_slider_label"),
            sliderInput(
              "he_scroll",
              "Scroll timeline:",
              min = min(hesum_he$Archive_Date),
              max = max(hesum_he$Archive_Date),
              value = c(
                max(hesum_he$Archive_Date) - 365,
                max(hesum_he$Archive_Date)
              ),
              timeFormat = "%Y-%m-%d",
              width = "100%"
            ),
            radioButtons("he_chart_type", NULL, choices = c(
              "All Jobs" = "all",
              "Faculty/instructor (non-adjunct)" = "Instructor/Teacher/Faculty",
              "Adjunct/part-time faculty" = "Adjunct/Part-Time Faculty"
            ), selected = "all", inline = TRUE),
            withSpinner(plotlyOutput("he_longitudinal_plot"))
          )
        )
      ),

      tabItem(
        tabName = "he_spells",
        h3(style = "margin-top: 5px; font-weight: 600;", tagList(icon("stopwatch"), " Higher Ed Job Duration & Time-to-Fill")),
        p(class = "help-block",
          "Weekly tracking of collegiate faculty vacancy duration across snapshots. Measures time-to-fill and search persistence across academic disciplines and institutions."),
        fluidRow(
          valueBoxOutput("he_spell_median_box", width = 3),
          valueBoxOutput("he_spell_active_box", width = 3),
          valueBoxOutput("he_spell_persistent_box", width = 3),
          valueBoxOutput("he_spell_hardest_box", width = 3)
        ),
        box(
          width = 12, status = "primary", solidHeader = FALSE,
          fluidRow(
            column(
              width = 3,
              selectInput("he_spell_status", "Vacancy Status:",
                          choices = c("All Postings" = "all",
                                      "Still Active (Open)" = "Still Active",
                                      "Filled / Closed" = "Closed / Filled"),
                          selected = "all")
            ),
            column(
              width = 3,
              selectInput("he_spell_job_type", "Appointment Type:",
                          choices = c("All Appointments" = "all",
                                      "Full-Time / Regular Faculty" = "Instructor/Teacher/Faculty",
                                      "Adjunct / Part-Time Faculty" = "Adjunct/Part-Time Faculty"),
                          selected = "all")
            ),
            column(
              width = 3,
              selectInput("he_spell_discipline", "Academic Focus:",
                          choices = c("All Disciplines" = "all",
                                      "STEM Fields" = "stem",
                                      "Health Sciences & Nursing" = "health",
                                      "Career & Technical (CTE)" = "cte"),
                          selected = "all")
            ),
            column(
              width = 3,
              selectInput("he_spell_institution", "Filter Institution:",
                          choices = c("All Institutions" = "all"),
                          selected = "all")
            )
          )
        ),
        fluidRow(
          box(
            width = 7, title = "Median Days Open by Academic Discipline", status = "info",
            withSpinner(plotlyOutput("he_spell_duration_plot", height = "360px"))
          ),
          box(
            width = 5, title = "Collegiate Vacancy Duration Distribution", status = "info",
            withSpinner(plotlyOutput("he_spell_distrib_plot", height = "360px"))
          )
        ),
        box(
          width = 12, title = "Collegiate Job Duration & Time-to-Fill Explorer", status = "primary",
          p(style = "font-size: 12px; color: #777; margin-bottom: 12px;",
            "Estimated Days Open represents the calendar duration between when a posting was first captured and when it disappeared from weekly updates (or weeks observed × 7)."),
          DTOutput("he_spell_table")
        )
      )
    )
  )
)


#--------------------------------------------------
# Server
#--------------------------------------------------
server <- function(input, output, session) {

  # Set by clicking a marker on the combined map; cleared by the "Show
  # All" button on each Jobs Table tab.
  selected_district <- reactiveVal(NULL)
  selected_institution <- reactiveVal(NULL)
  # Map search carried over from a map popup's "View matching jobs" link;
  # NULL whenever the Jobs tab is reached any other way.
  selected_k12_query <- reactiveVal(NULL)
  selected_he_query <- reactiveVal(NULL)
  # Pay band carried over from a map popup (PAY_BANDS name), or NULL.
  selected_pay_band <- reactiveVal(NULL)

  # DATA_LOAD_ISSUES is populated once at app startup (see
  # validate_and_pad_schema() above) -- not reactive, but wrapped in
  # renderUI/uiOutput anyway rather than inlined in the UI definition, so
  # the exact same "which source is broken" list a maintainer would see in
  # the R console is also visible to anyone using the deployed dashboard,
  # instead of a blank field or a crash being the only symptom.
  output$data_load_issues_banner <- renderUI({
    req(length(DATA_LOAD_ISSUES) > 0)
    box(width = 12, status = "danger", title = "Data issue detected",
        tags$ul(lapply(DATA_LOAD_ISSUES, tags$li)),
        helpText("This is a schema problem in the underlying data pipeline, not something wrong with your view of the dashboard -- the missing field(s) above need a pipeline fix."))
  })

  # -------- Intro KPIs --------
  # Week-over-week delta as a subtitle line -- compute_wow_delta() works at
  # the statewide-total level specifically because that's where the signal
  # is real (see its definition above for why individual-entity deltas
  # aren't used here).
  wow_delta_ui <- function(delta) {
    if (is.na(delta)) return(NULL)
    arrow <- if (delta >= 0) "▲" else "▼"
    tags$div(style = "font-size: 0.85em; margin-top: 2px;", paste0(arrow, " ", abs(delta), " vs last week"))
  }

  # Statewide last-12-week trend, for the KPI tiles' icon slot -- replaces
  # the plain school/university icon (previously a static hero-photo
  # stand-in) with the same information the sparkline redesign already
  # added everywhere else, since that space was otherwise just decorative.
  statewide_weekly_series <- function(weekly_totals) {
    weekly_totals %>%
      group_by(Archive_Date) %>%
      summarize(n = sum(n), .groups = "drop") %>%
      arrange(Archive_Date) %>%
      tail(12) %>%
      pull(n)
  }

  output$kpi_k12_total <- renderValueBox({
    valueBox(format(nrow(combineddata), big.mark = ","),
             tagList("Open K-12 Postings", wow_delta_ui(compute_wow_delta(k12_district_weekly_totals))),
             icon = tags$i(HTML(make_sparkline_svg_light(statewide_weekly_series(k12_district_weekly_totals)))),
             color = "blue")
  })
  output$kpi_he_total <- renderValueBox({
    valueBox(format(nrow(ccdata), big.mark = ","),
             tagList(
               "Open Higher Ed Postings",
               tags$small(
                 style = "display: block; font-size: 11px; opacity: 0.85;",
                 sprintf(
                   "%s faculty/instructor (non-adjunct) | %s adjunct/part-time faculty",
                   format(he_faculty_counts$faculty_instructor, big.mark = ","),
                   format(he_faculty_counts$adjunct_part_time, big.mark = ",")
                 )
               ),
               wow_delta_ui(compute_wow_delta(he_institution_weekly_totals))
             ),
             icon = tags$i(HTML(make_sparkline_svg_light(statewide_weekly_series(he_institution_weekly_totals)))),
             color = "purple")
  })
  output$kpi_last_refreshed <- renderValueBox({
    valueBox(last_refreshed_date, "Last Refreshed", icon = icon("calendar"), color = "green")
  })

  output$k12_biggest_mover <- renderUI({
    render_mover_box(find_biggest_mover(k12_district_weekly_totals, "District"), "Biggest K-12 mover")
  })
  output$he_biggest_mover <- renderUI({
    render_mover_box(find_biggest_mover(he_institution_weekly_totals, "Institution"), "Biggest Higher Ed mover")
  })

  output$top_k12_districts <- renderTable({
    top <- combineddata %>% count(District, sort = TRUE, name = "Open Postings") %>% head(5)
    top$`Last 12 Weeks` <- vapply(top$District, function(d) {
      series <- k12_district_weekly_totals %>%
        filter(District == d) %>%
        arrange(Archive_Date) %>%
        tail(12) %>%
        pull(n)
      make_sparkline_svg(series, accent = "#2a78d6")
    }, character(1))
    top$District <- paste0(
      '<a href="#" onclick="Shiny.setInputValue(\'popup_view_jobs\', \'',
      gsub("'", "&#39;", top$District, fixed = TRUE),
      '\', {priority: \'event\'}); return false;">',
      top$District,
      '</a>'
    )
    top
  }, sanitize.text.function = function(x) x)

  output$top_he_institutions <- renderTable({
    top <- ccdata %>% count(Institution, sort = TRUE, name = "Open Postings") %>% head(5)
    top$`Last 12 Weeks` <- vapply(top$Institution, function(inst) {
      series <- he_institution_weekly_totals %>%
        filter(Institution == inst) %>%
        arrange(Archive_Date) %>%
        tail(12) %>%
        pull(n)
      make_sparkline_svg(series, accent = "#4a3aa7")
    }, character(1))
    top$Institution <- paste0(
      '<a href="#" onclick="Shiny.setInputValue(\'popup_view_jobs\', \'',
      gsub("'", "&#39;", top$Institution, fixed = TRUE),
      '\', {priority: \'event\'}); return false;">',
      top$Institution,
      '</a>'
    )
    top
  }, sanitize.text.function = function(x) x)

  # Vacancy-rate leaderboards -- a different question from the raw-count
  # tables above ("who's short-staffed relative to their size" rather than
  # "who's hiring the most"), so they sit alongside rather than replacing
  # them. Same horizontal-bar, ranked-descending pattern as the Current
  # Trends charts. combined_map_data's Vacancy_Rate is already NA below
  # VACANCY_RATE_MIN_FTE, so no separate floor check is needed here.
  #
  # validate(need()), not a bare req() -- req() would leave the box showing
  # only its spinner forever if nothing qualifies (e.g. every staffing
  # source failed this week), with no indication that's a real state and
  # not just slow loading. Matches the friendly-message pattern the
  # longitudinal charts already use below.
  output$k12_vacancy_leaderboard <- renderPlotly({
    df <- combined_map_data %>%
      filter(Type == "K-12 District", !is.na(Vacancy_Rate)) %>%
      arrange(desc(Vacancy_Rate)) %>%
      head(8)
    validate(need(nrow(df) > 0, "No districts currently meet the minimum-FTE threshold for a vacancy rate."))

    plot <- ggplot(df, aes(x = reorder(Name, Vacancy_Rate), y = Vacancy_Rate,
                            text = paste0(Name, ": ", Vacancy_Numerator, " / ", Vacancy_Denominator,
                                          " = ", scales::percent(Vacancy_Rate, accuracy = 0.1)))) +
      geom_bar(stat = "identity", fill = "#2a78d6") +
      geom_text(aes(label = scales::percent(Vacancy_Rate, accuracy = 0.1)), hjust = -0.15, size = 3) +
      labs(x = NULL, y = "Teacher vacancy rate") +
      scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      theme_minimal()

    ggplotly(plot, tooltip = "text")
  })

  output$he_vacancy_leaderboard <- renderPlotly({
    df <- combined_map_data %>%
      filter(Type == "Higher Ed Institution", !is.na(Vacancy_Rate)) %>%
      arrange(desc(Vacancy_Rate)) %>%
      head(8) %>%
      # Sheridan/Gillette carry a marked *-suffixed label rather than their
      # plain name, since this is the one place their joint rate sits next
      # to genuinely campus-specific rates with nothing else distinguishing
      # them -- see Vacancy_Rate_Shared's definition in map_he above.
      mutate(Name = ifelse(Vacancy_Rate_Shared, paste0(Name, " *"), Name))
    validate(need(nrow(df) > 0, "No institutions currently meet the minimum-FTE threshold for a vacancy rate."))

    plot <- ggplot(df, aes(x = reorder(Name, Vacancy_Rate), y = Vacancy_Rate,
                            text = paste0(Name, ": ", Vacancy_Numerator, " / ", Vacancy_Denominator,
                                          " = ", scales::percent(Vacancy_Rate, accuracy = 0.1)))) +
      geom_bar(stat = "identity", fill = "#4a3aa7") +
      geom_text(aes(label = scales::percent(Vacancy_Rate, accuracy = 0.1)), hjust = -0.15, size = 3) +
      labs(x = NULL, y = "Faculty vacancy rate") +
      scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      theme_minimal()

    ggplotly(plot, tooltip = "text")
  })

  # -------- New This Week --------
  output$k12_new_table <- renderDT({
    datatable(
      k12_new_this_week %>% select(title, District, location, Broad_Category, url),
      options = list(scrollX = TRUE)
    )
  })
  output$he_new_table <- renderDT({
    df <- he_new_this_week
    if (!identical(input$he_new_appointment, "all")) {
      df <- df %>% filter(Appointment == input$he_new_appointment)
    }
    datatable(
      df %>% select(Title, Institution, Location, Appointment, Category, Link),
      options = list(scrollX = TRUE)
    )
  })

  # ------------------ Open Research Downloads ------------------
  output$download_active_k12 <- downloadHandler(
    filename = function() { paste0("wyoming_active_k12_jobs_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(combineddata %>% select(-any_of(c(".Pay_Value", ".Pay_Unit"))), file, row.names = FALSE, na = "")
    }
  )

  output$download_history_k12 <- downloadHandler(
    filename = function() { paste0("wyoming_k12_historical_archive_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("k12jobanalysis.csv", file)
    }
  )

  output$download_district_benchmarks <- downloadHandler(
    filename = function() { paste0("wyoming_district_profiles_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("salarymap2.csv", file)
    }
  )

  output$download_history_he <- downloadHandler(
    filename = function() { paste0("wyoming_higher_ed_historical_archive_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("facultydata.csv", file)
    }
  )

  output$download_he_benchmarks <- downloadHandler(
    filename = function() { paste0("wyoming_higher_ed_profiles_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("salarymap.csv", file)
    }
  )

  output$download_wde_staff <- downloadHandler(
    filename = function() { paste0("wyoming_wde602_staff_salaries_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("wde602_district_staff_salaries.csv", file)
    }
  )

  output$download_spells <- downloadHandler(
    filename = function() { paste0("wyoming_job_duration_spells_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("job_duration_spells.csv", file)
    }
  )

  output$download_he_spells <- downloadHandler(
    filename = function() { paste0("wyoming_he_job_duration_spells_", Sys.Date(), ".csv") },
    content = function(file) {
      if (file.exists("he_job_duration_spells.csv")) {
        file.copy("he_job_duration_spells.csv", file)
      } else {
        write.csv(data.frame(), file)
      }
    }
  )

  output$download_weekly_totals <- downloadHandler(
    filename = function() { paste0("wyoming_k12_weekly_vacancy_series_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("k12_district_weekly_totals.csv", file)
    }
  )

  output$download_he_weekly_totals <- downloadHandler(
    filename = function() { paste0("wyoming_he_weekly_vacancy_series_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("he_institution_weekly_totals.csv", file)
    }
  )

  output$download_dictionary <- downloadHandler(
    filename = function() { paste0("wyoming_data_dictionary_", Sys.Date(), ".csv") },
    content = function(file) {
      file.copy("data_dictionary.csv", file)
    }
  )

  output$data_dictionary_table <- renderDT({
    req(data_dictionary)
    datatable(
      data_dictionary,
      filter = "top",
      escape = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE)
    )
  })

  # -------- K-12 --------
  output$k12_jobs <- renderDT({
    df <- combineddata
    if (!is.null(selected_district())) df <- df %>% filter(District == selected_district())
    # Same matcher and searchable text as map_filtered(), so the count here
    # equals the popup's "N matching openings".
    if (!is.null(selected_k12_query())) {
      df <- df[match_job_query(paste(coalesce(df$Title, ""), coalesce(df$Position, "")), selected_k12_query()), , drop = FALSE]
    }
    if (!is.null(selected_pay_band())) df <- df[in_pay_band(df$.Pay_Value, df$.Pay_Unit, selected_pay_band()), , drop = FALSE]
    df <- df %>% select(-any_of(c(".Pay_Value", ".Pay_Unit")))
    if (isTRUE(identical(input$k12_quick_filter, "new"))) {
      new_keys <- paste(k12_new_this_week$title, k12_new_this_week$District, sep = "@@@")
      current_keys <- paste(df$Title, df$District, sep = "@@@")
      df <- df[current_keys %in% new_keys, ]
    }
    datatable(df, filter = "top", escape = FALSE, extensions = "Buttons",
              options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print")))
  })

  output$k12_filter_status <- renderUI({
    if (is.null(selected_district())) return(NULL)
    div(style = "margin-bottom: 10px;",
        strong(paste0("Showing: ", selected_district(),
                      if (!is.null(selected_k12_query())) paste0(" \u2014 jobs matching \u201c", selected_k12_query(), "\u201d") else "",
                      if (!is.null(selected_pay_band())) paste0(" \u2014 pay ", PAY_BANDS[[selected_pay_band()]]$label) else "")),
        actionButton("clear_k12_filter", "Show All Districts", class = "btn-xs", style = "margin-left: 10px;")
    )
  })
  observeEvent(input$clear_k12_filter, { selected_district(NULL); selected_k12_query(NULL); selected_pay_band(NULL) })

  # District Summary -- everything the map popups show (vacancy rate,
  # salary figures, source/date), but as a real exportable table instead
  # of something only visible one marker at a time.
  output$k12_summary_table <- renderDT({
    k12_rows <- combined_map_data %>% filter(Type == "K-12 District")
    year <- unique(na.omit(k12_rows$Salary_Year))
    current_label <- if (length(year) > 0) year[1] else "current year"
    prior_label <- if (length(year) > 0) prior_school_year_label(year[1]) else "prior year"
    wde_years <- unique(na.omit(k12_rows$WDE_Salary_Year))
    wde_label <- if (length(wde_years) > 0) wde_years[1] else "2025-26"

    df <- k12_rows %>%
      arrange(desc(CurrentCount)) %>%
      transmute(
        District = Name,
        County,
        `Current Openings` = CurrentCount,
        # Real, sortable/filterable column -- not just a map badge or a
        # buried code comment -- so "why does this district look quiet"
        # is answerable directly from the exportable table, not just the
        # map. "Full" is the overwhelming majority (every district on a
        # real job-board platform); see the Map tab's helpText for what
        # the partial tiers mean.
        `Posting Data` = Data_Coverage,
        `New This Week` = WeeklyNew,
        `Teacher Vacancy Rate` = ifelse(is.na(Vacancy_Rate), NA_character_, scales::percent(Vacancy_Rate, accuracy = 0.1)),
        Enrollment,
        `Students per Teacher` = ifelse(is.na(Students_Per_Teacher), NA_character_, sprintf("%.1f", Students_Per_Teacher)),
        TeacherBaseSalary = ifelse(is.na(Teacher_Base_Salary), NA_character_, scales::dollar(Teacher_Base_Salary)),
        TeacherBaseSalaryPrior = ifelse(is.na(Teacher_Base_Salary_Prior_Year), NA_character_, scales::dollar(Teacher_Base_Salary_Prior_Year)),
        TeacherAvgSalary = ifelse(is.na(Teacher_Avg_Salary), NA_character_, scales::dollar(Teacher_Avg_Salary)),
        `Superintendent Salary` = ifelse(is.na(Superintendent_Salary), NA_character_, scales::dollar(Superintendent_Salary)),
        # County-level context (Census ACS 5-Year) -- constant across
        # sibling districts in the same county, same as any other county-
        # level figure, and deliberately alongside the salary columns
        # rather than off in a separate tab: "$46k pay in a county where
        # median rent is $650/mo" is the actual comparison a prospective
        # teacher is making, not two numbers looked up separately.
        `County Median Income` = ifelse(is.na(Median_Household_Income), NA_character_, scales::dollar(Median_Household_Income)),
        `County Median Rent` = ifelse(is.na(Median_Gross_Rent), NA_character_, paste0(scales::dollar(Median_Gross_Rent), "/mo")),
        # Share of the county's civilian workforce in mining/oil & gas --
        # Wyoming's energy-producing counties pay teachers measurably more
        # specifically to compete with these wages for the same local
        # labor pool, so this is the one column here that helps explain
        # WHY salaries vary across WY districts, not just what things cost.
        `County Mining/Energy Jobs` = ifelse(is.na(Mining_Employment_Share), NA_character_, scales::percent(Mining_Employment_Share, accuracy = 0.1)),
        `County Population Trend (5yr)` = ifelse(is.na(Population_Change_Pct), NA_character_,
                                                   paste0(ifelse(Population_Change_Pct >= 0, "+", ""), scales::percent(Population_Change_Pct, accuracy = 0.1))),
        # District-level, not county-level, unlike the four columns above --
        # the closest free proxy to free/reduced-lunch eligibility at the
        # actual district a posting is in (Census SAIPE doesn't publish
        # free/reduced-lunch counts directly). A prospective teacher sizing
        # up a district's student population, not just its town.
        `District Child Poverty Rate` = ifelse(is.na(Child_Poverty_Rate), NA_character_, scales::percent(Child_Poverty_Rate, accuracy = 0.1))
      )
    # Real year in the header instead of a generic "Prior Year" label --
    # cleaner than a separate Salary Year column repeating the same value
    # in every row.
    names(df)[names(df) == "TeacherBaseSalary"] <- paste0("Teacher Base Salary (", current_label, ")")
    names(df)[names(df) == "TeacherBaseSalaryPrior"] <- paste0("Teacher Base Salary (", prior_label, ")")
    names(df)[names(df) == "TeacherAvgSalary"] <- paste0("Teacher Avg Salary (", wde_label, " WDE)")

    datatable(df, filter = "top", extensions = "Buttons",
              options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print"), pageLength = 48))
  })

  # Salary_Year and Salary_Source are the same for every district (one
  # WSBA document covers all 48 for one school year) -- a column repeating
  # an identical value 48 times isn't information, so it's a single
  # footnote with a live link instead.
  output$k12_summary_footnote <- renderUI({
    year <- unique(na.omit(combined_map_data$Salary_Year[combined_map_data$Type == "K-12 District"]))
    source <- unique(na.omit(combined_map_data$Salary_Source[combined_map_data$Type == "K-12 District"]))
    wde_year <- unique(na.omit(combined_map_data$WDE_Salary_Year[combined_map_data$Type == "K-12 District"]))
    wde_source <- unique(na.omit(combined_map_data$WDE_Salary_Source[combined_map_data$Type == "K-12 District"]))
    acs_year <- unique(na.omit(combined_map_data$ACS_Year[combined_map_data$Type == "K-12 District"]))
    saipe_year <- unique(na.omit(combined_map_data$SAIPE_Year[combined_map_data$Type == "K-12 District"]))
    req(length(year) > 0, length(source) > 0)
    tagList(
      helpText(
        "Base salary data:", source[1], paste0("(", year[1], " school year)"), "—",
        tags$a(href = WSBA_SALARY_SOURCE_URL, target = "_blank", "wsba-wy.org")
      ),
      if (length(wde_source) > 0) {
        helpText(
          "Average teacher salary data:", wde_source[1],
          if (length(wde_year) > 0) paste0("(", wde_year[1], " school year)") else "", "—",
          tags$a(href = "https://edu.wyoming.gov/data/staff-member-collection/", target = "_blank", "edu.wyoming.gov")
        )
      },
      if (length(acs_year) > 0) {
        helpText(
          "County context (income, rent, mining/energy employment share, population trend): US Census Bureau, American Community Survey 5-Year Estimates",
          paste0("(", acs_year[1], ")"), "—",
          tags$a(href = "https://www.census.gov/data/developers/data-sets/acs-5year.html", target = "_blank", "census.gov"),
          ". County-level, not district-level -- sibling districts in the same county share the same figures."
        )
      },
      if (length(saipe_year) > 0) {
        helpText(
          "District child poverty rate: US Census Bureau, Small Area Income and Poverty Estimates (SAIPE)",
          paste0("(", saipe_year[1], ")"), "—",
          tags$a(href = "https://www.census.gov/programs-surveys/saipe/data/datasets.html", target = "_blank", "census.gov/saipe"),
          ". District-level (unlike the county context above)."
        )
      }
    )
  })

  #--------------------------------------------------
  # K-12 Head-to-Head Comparator
  #--------------------------------------------------
  observeEvent(input$k12_comp_preset_top, {
    top_districts <- combined_map_data %>%
      filter(Type == "K-12 District") %>%
      arrange(desc(CurrentCount)) %>%
      pull(Name) %>%
      head(3)
    updateSelectizeInput(session, "k12_compare_districts", selected = top_districts)
  })

  observeEvent(input$k12_comp_preset_largest, {
    largest_districts <- combined_map_data %>%
      filter(Type == "K-12 District") %>%
      arrange(desc(Enrollment)) %>%
      pull(Name) %>%
      head(3)
    updateSelectizeInput(session, "k12_compare_districts", selected = largest_districts)
  })

  observeEvent(input$k12_comp_preset_clear, {
    updateSelectizeInput(session, "k12_compare_districts", selected = character(0))
  })

  output$k12_compare_cards <- renderUI({
    selected <- input$k12_compare_districts
    if (is.null(selected) || length(selected) == 0) {
      return(div(class = "alert alert-info", style = "margin-top: 15px;",
                 icon("circle-info"), " Please select one to four school districts above to display comparative benchmarks."))
    }
    
    col_width <- case_when(
      length(selected) == 1 ~ 12,
      length(selected) == 2 ~ 6,
      length(selected) == 3 ~ 4,
      TRUE ~ 3
    )

    k12_rows <- combined_map_data %>%
      filter(Type == "K-12 District", Name %in% selected)

    cards <- lapply(selected, function(dname) {
      row <- k12_rows %>% filter(Name == dname)
      if (nrow(row) == 0) return(NULL)

      base_sal <- if (!is.na(row$Teacher_Base_Salary[1])) scales::dollar(row$Teacher_Base_Salary[1]) else "N/A"
      avg_sal <- if (!is.na(row$Teacher_Avg_Salary[1])) scales::dollar(row$Teacher_Avg_Salary[1]) else "N/A"
      vac_rate <- if (!is.na(row$Vacancy_Rate[1])) scales::percent(row$Vacancy_Rate[1], accuracy = 0.1) else "N/A"
      enroll <- if (!is.na(row$Enrollment[1])) scales::comma(row$Enrollment[1]) else "N/A"
      spt <- if (!is.na(row$Students_Per_Teacher[1])) sprintf("%.1f", row$Students_Per_Teacher[1]) else "N/A"
      rent <- if (!is.na(row$Median_Gross_Rent[1])) paste0(scales::dollar(row$Median_Gross_Rent[1]), "/mo") else "N/A"
      poverty <- if (!is.na(row$Child_Poverty_Rate[1])) scales::percent(row$Child_Poverty_Rate[1], accuracy = 0.1) else "N/A"

      column(
        width = col_width,
        div(
          class = "box box-solid box-primary",
          style = "border-top: 3px solid #2a78d6; box-shadow: 0 1px 3px rgba(0,0,0,0.08); margin-bottom: 20px;",
          div(
            class = "box-header with-border", style = "background: #f7f9fa; padding: 10px 15px;",
            h4(style = "margin: 0; font-size: 15px; font-weight: 600; color: #222;", dname),
            tags$small(class = "text-muted", paste0(row$County[1], " County"))
          ),
          div(
            class = "box-body", style = "padding: 15px;",
            fluidRow(
              column(6, div(style = "text-align: center; border-right: 1px solid #eee;",
                           div(style = "font-size: 22px; font-weight: 700; color: #2a78d6;", row$CurrentCount[1]),
                           div(style = "font-size: 11px; text-transform: uppercase; color: #777;", "Active Openings"))),
              column(6, div(style = "text-align: center;",
                           div(style = "font-size: 22px; font-weight: 700; color: #e67e22;", vac_rate),
                           div(style = "font-size: 11px; text-transform: uppercase; color: #777;", "Vacancy Rate")))
            ),
            tags$hr(style = "margin: 12px 0;"),
            tags$table(
              style = "width: 100%; font-size: 13px; line-height: 1.8;",
              tags$tr(tags$td(class = "text-muted", "Base Starting Salary:"), tags$td(style = "text-align: right; font-weight: 600;", base_sal)),
              tags$tr(tags$td(class = "text-muted", "Avg Salary (WDE):"), tags$td(style = "text-align: right; font-weight: 600;", avg_sal)),
              tags$tr(tags$td(class = "text-muted", "Enrollment:"), tags$td(style = "text-align: right; font-weight: 600;", enroll)),
              tags$tr(tags$td(class = "text-muted", "Students / Teacher:"), tags$td(style = "text-align: right; font-weight: 600;", spt)),
              tags$tr(tags$td(class = "text-muted", "County Median Rent:"), tags$td(style = "text-align: right; font-weight: 600;", rent)),
              tags$tr(tags$td(class = "text-muted", "Child Poverty Rate:"), tags$td(style = "text-align: right; font-weight: 600;", poverty))
            )
          )
        )
      )
    })
    fluidRow(cards)
  })

  output$k12_compare_salary_plot <- renderPlotly({
    selected <- input$k12_compare_districts
    if (is.null(selected) || length(selected) == 0) {
      return(plot_ly(type = "scatter", mode = "none") %>% layout(
        annotations = list(list(text = "Select one or more districts to display salary comparison",
                                showarrow = FALSE, font = list(size = 14, color = "#666"))),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ))
    }
    df <- combined_map_data %>%
      filter(Type == "K-12 District", Name %in% selected) %>%
      select(Name, Teacher_Base_Salary, Teacher_Avg_Salary)
    
    plot_ly(df, x = ~Name) %>%
      add_bars(y = ~Teacher_Base_Salary, name = "Base Starting Salary", marker = list(color = "#4a90e2")) %>%
      add_bars(y = ~Teacher_Avg_Salary, name = "Average Salary (WDE)", marker = list(color = "#50e3c2")) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = "Annual Salary ($)", tickformat = "$,.0f"),
        legend = list(orientation = "h", x = 0, y = 1.15),
        margin = list(b = 60, l = 60, r = 20, t = 40)
      )
  })

  output$k12_compare_metrics_plot <- renderPlotly({
    selected <- input$k12_compare_districts
    if (is.null(selected) || length(selected) == 0) {
      return(plot_ly(type = "scatter", mode = "none") %>% layout(
        annotations = list(list(text = "Select one or more districts to display staffing metrics",
                                showarrow = FALSE, font = list(size = 14, color = "#666"))),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ))
    }
    df <- combined_map_data %>%
      filter(Type == "K-12 District", Name %in% selected) %>%
      select(Name, Enrollment, CurrentCount)
    
    plot_ly(df, x = ~Name) %>%
      add_bars(y = ~Enrollment, name = "Enrollment", yaxis = "y1", marker = list(color = "#34495e")) %>%
      add_lines(y = ~CurrentCount, name = "Active Openings", yaxis = "y2",
                line = list(color = "#e74c3c", width = 3), marker = list(size = 8, color = "#e74c3c")) %>%
      layout(
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = "Student Enrollment", tickformat = ",.0f"),
        yaxis2 = list(title = "Active Openings", overlaying = "y", side = "right", showgrid = FALSE),
        legend = list(orientation = "h", x = 0, y = 1.15),
        margin = list(b = 60, l = 60, r = 50, t = 40)
      )
  })

  output$k12_compare_jobs_table <- renderDT({
    selected <- input$k12_compare_districts
    df <- combineddata
    if (!is.null(selected) && length(selected) > 0) {
      df <- df %>% filter(District %in% selected)
    } else {
      df <- df[0, ]
    }
    datatable(df %>% select(-any_of(c(".Pay_Value", ".Pay_Unit"))), filter = "top", escape = FALSE, extensions = "Buttons",
              options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print"), pageLength = 10))
  })

  # -------- Combined map (Introduction tab) --------
  output$combined_map <- renderLeaflet({
    leaflet() %>%
      # Esri World Light Gray, not CARTO Positron: CARTO's basemaps now require
      # an API key and serve an "API KEY REQUIRED" image tile without one.
      # The reference layer adds place names on top of the label-free base.
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Clean (Default)") %>%
      addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}",
        options = tileOptions(maxZoom = 16),
        group = "Clean (Default)"
      ) %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
      setView(lng = -107.5, lat = 43, zoom = 7) %>%
      addLayersControl(
        baseGroups = c("Clean (Default)", "OpenStreetMap", "Topographic"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(position = "bottomright", pal = vacancy_rate_palette, values = vacancy_rate_domain,
                title = "Vacancy rate", labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x),
                na.label = "N/A")
  })
  # The map now lives on its own "Map" tab, not the default active one --
  # without this, the marker-update observe() below fires (and calls
  # leafletProxy()) before the browser has ever rendered/registered the
  # widget, since shinydashboard doesn't render hidden tabs' outputs by
  # default. Confirmed live 2026-08-05: markers silently never appeared,
  # "Couldn't find map with id combined_map" in the browser console.
  outputOptions(output, "combined_map", suspendWhenHidden = FALSE)

  # Quick suggestion chips for position search
  observeEvent(input$map_chip_sped, {
    updateTextInput(session, "map_search_keyword", value = "special ed")
  })
  observeEvent(input$map_chip_math, {
    updateTextInput(session, "map_search_keyword", value = "mathematics")
  })
  observeEvent(input$map_chip_elem, {
    updateTextInput(session, "map_search_keyword", value = "elementary")
  })
  observeEvent(input$map_chip_science, {
    updateTextInput(session, "map_search_keyword", value = "science")
  })
  observeEvent(input$map_chip_music, {
    updateTextInput(session, "map_search_keyword", value = "music")
  })
  observeEvent(input$map_chip_driver, {
    updateTextInput(session, "map_search_keyword", value = "bus driver")
  })
  observeEvent(input$map_chip_clear, {
    updateTextInput(session, "map_search_keyword", value = "")
  })

  map_filtered <- reactive({
    df <- combined_map_data %>% filter(Type %in% input$map_types, CurrentCount > 0)

    # Search keyword / role / subject query filter, and the pay band. Both
    # narrow each dot to its matching postings (count, samples, popup).
    kw <- if (!is.null(input$map_search_keyword)) trimws(input$map_search_keyword) else ""
    band <- input$map_salary_tier
    band_on <- !is.null(band) && band %in% names(PAY_BANDS)
    if (nzchar(kw) || band_on) {
      searchable_k12 <- paste(coalesce(combineddata$Title, ""), coalesce(combineddata$Position, ""))
      k12_matches <- combineddata[match_job_query(searchable_k12, kw) &
                                    in_pay_band(combineddata$.Pay_Value, combineddata$.Pay_Unit, if (band_on) band), , drop = FALSE]

      searchable_he <- paste(coalesce(ccdata$Title, ""), coalesce(ccdata$Appointment, ""))
      he_matches <- ccdata[match_job_query(searchable_he, kw) &
                             in_pay_band(ccdata$.Pay_Value, ccdata$.Pay_Unit, if (band_on) band), , drop = FALSE]

      k12_counts <- if (nrow(k12_matches) > 0) {
        as.data.frame(table(Name = k12_matches$District), stringsAsFactors = FALSE)
      } else {
        data.frame(Name = character(0), Freq = integer(0))
      }
      names(k12_counts) <- c("Name", "MatchingCount")

      k12_samples <- if (nrow(k12_matches) > 0) {
        k12_matches %>%
          group_by(District) %>%
          summarize(MatchingSample = paste(head(unique(Title), 3), collapse = "; "), .groups = "drop") %>%
          rename(Name = District)
      } else {
        data.frame(Name = character(0), MatchingSample = character(0))
      }

      he_counts <- if (nrow(he_matches) > 0) {
        as.data.frame(table(Name = he_matches$Institution), stringsAsFactors = FALSE)
      } else {
        data.frame(Name = character(0), Freq = integer(0))
      }
      names(he_counts) <- c("Name", "MatchingCount")

      he_samples <- if (nrow(he_matches) > 0) {
        he_matches %>%
          group_by(Institution) %>%
          summarize(MatchingSample = paste(head(unique(Title), 3), collapse = "; "), .groups = "drop") %>%
          rename(Name = Institution)
      } else {
        data.frame(Name = character(0), MatchingSample = character(0))
      }

      all_counts <- bind_rows(k12_counts, he_counts)
      all_samples <- bind_rows(k12_samples, he_samples)

      df <- df %>%
        inner_join(all_counts, by = "Name") %>%
        left_join(all_samples, by = "Name") %>%
        mutate(
          MatchingCount = coalesce(MatchingCount, 0L),
          MatchingSample = coalesce(MatchingSample, ""),
          DisplayCount = MatchingCount,
          SearchActive = TRUE,
          SearchQuery = paste(c(if (nzchar(kw)) kw, if (band_on) PAY_BANDS[[band]]$label), collapse = " \u00b7 "),
          SearchKeyword = kw,
          SearchBand = if (band_on) band else ""
        ) %>%
        filter(MatchingCount > 0)
    } else {
      df <- df %>%
        mutate(
          MatchingCount = CurrentCount,
          MatchingSample = SampleTitles,
          DisplayCount = CurrentCount,
          SearchActive = FALSE,
          SearchQuery = "",
          SearchKeyword = "",
          SearchBand = ""
        )
    }

    # Filter by county
    if (!is.null(input$map_county) && input$map_county != "all") {
      df <- df %>% filter(County == input$map_county)
    }

    # Filter by posting volume / freshness
    if (!is.null(input$map_urgency) && input$map_urgency != "all") {
      if (input$map_urgency == "5_plus") {
        df <- df %>% filter(DisplayCount >= 5)
      } else if (input$map_urgency == "2_plus") {
        df <- df %>% filter(DisplayCount >= 2)
      } else if (input$map_urgency == "has_new") {
        df <- df %>% filter(WeeklyNew > 0)
      }
    }

    df
  })

  output$map_filter_count <- renderUI({
    filtered_df <- map_filtered()
    filtered_n <- nrow(filtered_df)
    kw <- if (!is.null(input$map_search_keyword)) trimws(input$map_search_keyword) else ""
    total_active <- nrow(combined_map_data %>% filter(CurrentCount > 0))
    total_postings <- sum(filtered_df$DisplayCount, na.rm = TRUE)

    if (filtered_n == 0) {
      if (nzchar(kw)) {
        tags$span(
          class = "badge",
          style = "background-color: #d9534f; font-size: 0.9em; padding: 6px 12px; font-weight: 500; border-radius: 4px;",
          sprintf('No locations currently have open positions matching "%s" under selected filters — try broadening your search or click Reset.', kw)
        )
      } else {
        tags$span(
          class = "badge",
          style = "background-color: #d9534f; font-size: 0.9em; padding: 6px 12px; font-weight: 500; border-radius: 4px;",
          "No locations match all selected filters — broaden criteria or click Reset Filters."
        )
      }
    } else {
      if (nzchar(kw)) {
        tags$span(
          class = "badge",
          style = "background-color: #198754; font-size: 0.9em; padding: 6px 12px; font-weight: 500; border-radius: 4px;",
          sprintf('Showing %d locations with %s matching openings for "%s"',
                  filtered_n, format(total_postings, big.mark = ","), kw)
        )
      } else {
        tags$span(
          class = "badge",
          style = "background-color: #3c8dbc; font-size: 0.9em; padding: 6px 12px; font-weight: 500; border-radius: 4px;",
          sprintf("Showing %d of %d active locations (%s current openings across filtered locations)",
                  filtered_n, total_active, format(total_postings, big.mark = ","))
        )
      }
    }
  })

  observeEvent(input$map_filter_reset, {
    updateTextInput(session, "map_search_keyword", value = "")
    updateCheckboxGroupInput(session, "map_types", selected = c("K-12 District", "Higher Ed Institution"))
    updateSelectInput(session, "map_county", selected = "all")
    updateSelectInput(session, "map_salary_tier", selected = "all")
    updateSelectInput(session, "map_urgency", selected = "all")
  })

  observeEvent(input$map_county, {
    if (is.null(input$map_county) || input$map_county == "all") {
      leafletProxy("combined_map") %>%
        setView(lng = -107.5, lat = 43, zoom = 7)
    } else {
      county_data <- combined_map_data %>%
        filter(County == input$map_county, !is.na(Latitude), !is.na(Longitude))
      if (nrow(county_data) > 0) {
        if (nrow(county_data) == 1) {
          leafletProxy("combined_map") %>%
            setView(lng = county_data$Longitude[1], lat = county_data$Latitude[1], zoom = 9)
        } else {
          leafletProxy("combined_map") %>%
            fitBounds(
              lng1 = min(county_data$Longitude) - 0.15,
              lat1 = min(county_data$Latitude) - 0.15,
              lng2 = max(county_data$Longitude) + 0.15,
              lat2 = max(county_data$Latitude) + 0.15
            )
        }
      }
    }
  }, ignoreInit = TRUE)

  observe({
    df <- map_filtered()

    # Clear unconditionally so unchecking every filter or having 0 matches
    # empties the map safely without erroring on 0-row popups.
    proxy <- leafletProxy("combined_map", data = df) %>% clearMarkers()
    qcew_row <- match(sub(",.*$", "", trimws(as.character(df$County))), qcew_school_pay$County)
    df$School_Pay_County <- ifelse(df$Type == "K-12 District", qcew_school_pay$Avg_Annual_Pay[qcew_row], NA_real_)
    df$School_Pay_Year <- qcew_school_pay$Year[qcew_row]
    if (nrow(df) == 0) return()

    # Informative hover tooltip labels
    labels <- with(df, sprintf(
      "<strong>%s</strong><br/>%s%s%s",
      Name,
      ifelse(SearchActive,
             sprintf("<span style='color:#198754;font-weight:600;'>🎯 %d matching opening%s for \"%s\"</span><br/>",
                     DisplayCount, ifelse(DisplayCount == 1, "", "s"), SearchQuery),
             sprintf("%d active opening%s<br/>", CurrentCount, ifelse(CurrentCount == 1, "", "s"))),
      ifelse(!is.na(Vacancy_Rate), sprintf("Vacancy rate: %.1f%%<br/>", Vacancy_Rate * 100), ""),
      "<span style='color:#666;font-size:11px;'>Click marker for details & jobs</span>"
    )) %>% lapply(HTML)

    # A marker click always just opens its popup (leaflet's default) --
    # navigating away from the map has to be a second, deliberate action,
    # so the "View all jobs" link below fires a Shiny input from inside
    # the popup itself rather than reacting to the marker click event,
    # which used to fire at the same instant as the popup opened and
    # immediately navigated away before it could be read.
    popups <- with(df, paste0(
      "<div><strong>", Name, "</strong><br/>", Type, "</div>",
      ifelse(SearchActive,
             paste0("<div style='margin:4px 0;background:#e8f5e9;color:#2e7d32;padding:3px 8px;border-radius:4px;font-weight:600;font-size:0.9em;'>",
                    "🎯 ", DisplayCount, " matching opening", ifelse(DisplayCount == 1, "", "s"),
                    " for &ldquo;", SearchQuery, "&rdquo; (out of ", CurrentCount, " total)</div>",
                    ifelse(nzchar(MatchingSample), paste0("<div style='margin-bottom:4px;font-size:0.85em;'><strong>Matching:</strong> ", MatchingSample, "</div>"), "")),
             ""),
      # Visible badge, not just a footnote or a code comment -- Data_Coverage
      # is "Full" for every district on a real structured job-board platform;
      # the ~25-40% (own page) / ~25-40% only (WSBA-only) partial tiers come
      # from misc_district_coverage_tiers() in misc_district_scrapers.R.
      ifelse(!SearchActive & !is.na(Data_Coverage) & Data_Coverage != "Full",
             paste0("<div style='margin:2px 0;'><span style='background:#fdecea;color:#a92f1e;",
                    "font-size:0.78em;font-weight:bold;padding:1px 6px;border-radius:8px;'>",
                    Data_Coverage, "</span></div>"),
             ""),
      "<div>Current openings: <strong>", CurrentCount, "</strong></div>",
      "<div>New this week: ", WeeklyNew, "</div>",
      ifelse(!is.na(Vacancy_Rate),
             paste0("<div>", ifelse(Type == "K-12 District", "Teacher", "Faculty"),
                    " vacancy rate: ", scales::percent(Vacancy_Rate, accuracy = 0.1),
                    ifelse(Vacancy_Rate_Shared,
                           " <span style='color:#666;'>(combined Sheridan + Gillette figure -- see note below)</span>",
                           ""),
                    "</div>"),
             ""),
      ifelse(!SearchActive & nzchar(SampleTitles), paste0("<div>Recent postings: ", SampleTitles, "</div>"), ""),
      ifelse(!is.na(Students_Per_Teacher),
             paste0("<div>Students per ", ifelse(Type == "K-12 District", "teacher", "faculty member"), ": ",
                    sprintf("%.1f", Students_Per_Teacher),
                    " (", format(Enrollment, big.mark = ","), " students)</div>"),
             ""),
      # Institution-level, HE only -- K-12's equivalent trend is county-
      # level (Population_Change_Pct, shown further down alongside income/
      # rent/mining share) since no district-level enrollment trend is
      # pulled here.
      ifelse(!is.na(Enrollment_Change_Pct),
             paste0("<div>Enrollment trend (5yr): ", ifelse(Enrollment_Change_Pct >= 0, "+", ""),
                    scales::percent(Enrollment_Change_Pct, accuracy = 0.1), "</div>"),
             ""),
      # HE only -- see Pell_Recipient_Share's map_he comment for why
      # there's no K-12 equivalent.
      ifelse(!is.na(Pell_Recipient_Share),
             paste0("<div>Pell Grant recipients: ", scales::percent(Pell_Recipient_Share, accuracy = 0.1),
                    " of students (", Pell_Year, ")</div>"),
             ""),
      ifelse(!is.na(Teacher_Base_Salary),
             paste0("<div>Teacher base salary: ", scales::dollar(Teacher_Base_Salary),
                    ifelse(!is.na(Teacher_Base_Salary_Prior_Year),
                           paste0(" (", scales::dollar(Teacher_Base_Salary_Prior_Year), " prior year)"),
                           ""),
                    " &middot; ", Salary_Year, "</div>"),
             ""),
      ifelse(!is.na(Teacher_Avg_Salary),
             paste0("<div>Average teacher salary: ", scales::dollar(Teacher_Avg_Salary),
                    ifelse(!is.na(WDE_Salary_Year), paste0(" &middot; ", WDE_Salary_Year), ""),
                    " (WDE)</div>"),
             ""),
      ifelse(!is.na(Superintendent_Salary),
             paste0("<div>Superintendent salary: ", scales::dollar(Superintendent_Salary), "</div>"),
             ""),
      ifelse(!is.na(Faculty_Avg_Salary),
             paste0("<div>Avg. faculty salary: ", scales::dollar(Faculty_Avg_Salary),
                    ifelse(!is.na(Faculty_Avg_Salary_Professor),
                           paste0(" (Professor rank: ", scales::dollar(Faculty_Avg_Salary_Professor), ")"),
                           ""),
                    " &middot; ", Salary_Year, "</div>",
                    # IPEDS's "all combined" figure blends every reported
                    # contract length -- full-time 9-12 month faculty
                    # alongside lower-paid part-time/adjunct instructional
                    # staff -- into one average. No reliable full-time-only
                    # breakout is available from this source (IPEDS's
                    # contract-length codes aren't consistently documented
                    # enough to split this safely), so this is a disclosure
                    # rather than a fix. Ported from Montana's copy of this
                    # same fix, motivated by a real Montana example
                    # (Blackfeet Community College's $25,621 reflecting a
                    # $54,317 full-time cohort blended with lower-paid
                    # part-time staff) but the underlying IPEDS blending
                    # issue applies equally to Wyoming's own HE salary data.
                    "<div style='font-size:0.8em;color:#666;'>Blends full-time and part-time/adjunct instructional staff pay into one average.</div>"),
             ""),
      ifelse(!is.na(Salary_Note),
             paste0("<div style='font-size:0.85em;color:#666;'>", Salary_Note, "</div>"),
             ""),
      ifelse(!is.na(Salary_Source),
             paste0("<div style='font-size:0.85em;color:#666;'>Salary source: ", Salary_Source, "</div>"),
             ""),
      ifelse(!is.na(Median_Household_Income),
             paste0("<div style='font-size:0.85em;color:#666;margin-top:4px;'>County: median income ",
                    scales::dollar(Median_Household_Income), ", median rent ", scales::dollar(Median_Gross_Rent), "/mo",
                    ifelse(!is.na(Mining_Employment_Share) & Mining_Employment_Share >= 0.05,
                           paste0(", ", scales::percent(Mining_Employment_Share, accuracy = 1), " of jobs in mining/energy"),
                           ""),
                    "</div>"),
             ""),
      ifelse(!is.na(Child_Poverty_Rate),
             paste0("<div style='font-size:0.85em;color:#666;'>District: ",
                    scales::percent(Child_Poverty_Rate, accuracy = 0.1), " child poverty rate</div>"),
             ""),
      ifelse(!is.na(School_Pay_County),
             paste0("<div style='font-size:0.85em;color:#666;'>", sub(",.*$", "", County),
                    " public school employees averaged ", scales::dollar(School_Pay_County, accuracy = 1),
                    " in ", School_Pay_Year, " (all roles; BLS QCEW)</div>"),
             ""),
      "<div style='margin-top:6px;'>",
      "<a href='", Link, "' target='_blank'>Careers page</a>",
      " &nbsp;|&nbsp; ",
      # Carries the active map search with it, so the Jobs tab opens on just
      # the postings this popup counted. URL-encoded so quotes in the query
      # can't break out of the JS string.
      "<a href='#' onclick=\"Shiny.setInputValue('map_popup_view_jobs', {entity: '",
      gsub("'", "&#39;", Name, fixed = TRUE),
      "', query: '",
      ifelse(SearchActive,
             vapply(coalesce(SearchKeyword, ""), utils::URLencode, character(1), reserved = TRUE, USE.NAMES = FALSE),
             ""),
      "', band: '", ifelse(SearchActive, SearchBand, ""),
      "'}, {priority: 'event'}); return false;\">",
      ifelse(SearchActive,
             paste0("View ", DisplayCount, " matching job", ifelse(DisplayCount == 1, "", "s")),
             "View all jobs"),
      " &rarr;</a>",
      "</div>"
    ))

    proxy %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~map_marker_radius(DisplayCount),
        fillColor = ~vacancy_rate_palette(Vacancy_Rate),
        color = "#333333", weight = 1,
        fillOpacity = 0.85,
        layerId = ~Name,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "4px 8px"),
          textsize = "12px",
          direction = "auto"
        ),
        popup = popups
      )
  })

  observeEvent(input$popup_view_jobs, {
    navigate_to_entity_jobs(input$popup_view_jobs)
  })

  observeEvent(input$map_popup_view_jobs, {
    ev <- input$map_popup_view_jobs
    query <- trimws(utils::URLdecode(if (is.null(ev$query)) "" else ev$query))
    band <- if (!is.null(ev$band) && ev$band %in% names(PAY_BANDS)) ev$band else NULL
    navigate_to_entity_jobs(ev$entity, if (nzchar(query)) query else NULL, band)
  })

  navigate_to_entity_jobs <- function(entity, query = NULL, band = NULL) {
    selected_k12_query(query)
    selected_he_query(query)
    selected_pay_band(band)
    row <- combined_map_data %>% filter(Name == entity)
    if (nrow(row) > 0) {
      if (row$Type[1] == "K-12 District") {
        selected_district(entity)
        updateTabItems(session, "sidebar_tabs", "k12_table")
      } else {
        selected_institution(entity)
        updateTabItems(session, "sidebar_tabs", "he_table")
      }
    } else if (entity %in% combineddata$District) {
      selected_district(entity)
      updateTabItems(session, "sidebar_tabs", "k12_table")
    } else if (entity %in% ccdata$Institution) {
      selected_institution(entity)
      updateTabItems(session, "sidebar_tabs", "he_table")
    }
  }


  # ---- DATA SCOPE: District + Broad Category ----------------------------
  
  #------------Filter for longitudinal plot--------
  
  # Toggling detail level swaps both the underlying dataset and the
  # picker's own choices (Simple and Detailed are different category
  # name sets, not just different labels on the same data).
  observeEvent(input$k12_detail_level_trends, {
    cats <- if (identical(input$k12_detail_level_trends, "detail")) {
      sort(unique(k12sum$Broad_Category))
    } else {
      sort(unique(k12sum_agg$Broad_Category))
    }
    updatePickerInput(session, "broad_category", choices = cats, selected = cats)
  }, ignoreInit = TRUE)

  filtered_k12sum <- reactive({
    req(input$district_trend, input$broad_category, input$k12_detail_level_trends)

    base <- if (identical(input$k12_detail_level_trends, "detail")) k12sum else k12sum_agg

    base %>%
      dplyr::filter(
        # District filter
        input$district_trend == "Total" |
          District == input$district_trend,
        
        # Broad category filter (MULTI-SELECT SAFE)
        Broad_Category %in% input$broad_category
      )
  })
  
 
  # K-12 longitudinal plot
  df_windowed <- reactive({
    df <- filtered_k12sum()
    req(nrow(df) > 0, input$k12_scroll)
    
    df <- df %>%
      filter(
        Archive_Date >= input$k12_scroll[1],
        Archive_Date <= input$k12_scroll[2]
      )
    
    # Aggregate to ensure one row per Broad_Category x Archive_Date
    df <- df %>%
      filter(District != "Total") %>%  # remove total row to prevent double-counting
      group_by(Broad_Category, Archive_Date) %>%
      summarise(sum = sum(sum), .groups = "drop") %>%
      arrange(Broad_Category, Archive_Date)
    
    df
  })
  

  # ---- OUTPUT: LONGITUDINAL PLOT ----------------------------------------
  
  
  output$k12_longitudinal_plot <- renderPlotly({
    df <- df_windowed()
    
    validate(
      need(nrow(df) > 0, "No data for selected districts.")
    )
    
    # Get the exact dates in the filtered data
    all_dates <- sort(unique(df$Archive_Date))

    p <- ggplot(df, aes(
      x = Archive_Date,
      y = sum,
      color = Broad_Category,
      group = Broad_Category,
      text = paste0(
        "Date: ", Archive_Date, "<br>",
        "Category: ", Broad_Category, "<br>",
        "Postings: ", sum
      )
    )) +
      geom_line() +
      geom_point(size = 1) +
      scale_color_manual(values = if (identical(input$k12_detail_level_trends, "detail")) K12_CATEGORY_COLORS_DETAIL else K12_CATEGORY_COLORS_AGG) +
      labs(x = "Archive Date", y = "Number of Postings") +
      scale_x_date(
        breaks = all_dates,      # show every date exactly
        labels = scales::date_format("%b %d")
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)
      )

    ggplotly(p, height = 500, tooltip = "text")
  })
  

  
  output$k12_current_trends_table <- renderTable({
    req(input$k12_detail_level_current)
    hist_src <- if (identical(input$k12_detail_level_current, "detail")) k12sum else k12sum_agg
    history <- hist_src %>%
      filter(District == input$district_current) %>%
      transmute(Category = Broad_Category, Archive_Date, n = sum)

    result <- build_current_trends_table(history, accent = "#2a78d6")
    req(!is.null(result))
    result
  }, sanitize.text.function = function(x) x)

  # -------- Higher Ed --------
  output$he_jobs <- renderDT({
    df <- ccdata
    if (!is.null(selected_institution())) df <- df %>% filter(Institution == selected_institution())
    if (!is.null(selected_he_query())) {
      df <- df[match_job_query(paste(coalesce(df$Title, ""), coalesce(df$Appointment, "")), selected_he_query()), , drop = FALSE]
    }
    if (!is.null(selected_pay_band())) df <- df[in_pay_band(df$.Pay_Value, df$.Pay_Unit, selected_pay_band()), , drop = FALSE]
    df <- df %>% select(-any_of(c(".Pay_Value", ".Pay_Unit")))
    if (!identical(input$he_table_appointment, "all")) {
      df <- df %>% filter(Appointment == input$he_table_appointment)
    }
    if (isTRUE(identical(input$he_quick_filter, "new"))) {
      new_keys <- paste(he_new_this_week$Title, he_new_this_week$Institution, sep = "@@@")
      current_keys <- paste(df$Title, df$Institution, sep = "@@@")
      df <- df[current_keys %in% new_keys, ]
    }
    datatable(df, filter = "top", escape = FALSE, extensions = "Buttons",
              options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print")))
  })

  output$he_filter_status <- renderUI({
    if (is.null(selected_institution())) return(NULL)
    div(style = "margin-bottom: 10px;",
        strong(paste0("Showing: ", selected_institution(),
                      if (!is.null(selected_he_query())) paste0(" \u2014 jobs matching \u201c", selected_he_query(), "\u201d") else "",
                      if (!is.null(selected_pay_band())) paste0(" \u2014 pay ", PAY_BANDS[[selected_pay_band()]]$label) else "")),
        actionButton("clear_he_filter", "Show All Institutions", class = "btn-xs", style = "margin-left: 10px;")
    )
  })
  observeEvent(input$clear_he_filter, { selected_institution(NULL); selected_he_query(NULL); selected_pay_band(NULL) })

  # Institution Summary -- same purpose as k12_summary_table above: the
  # map popups' data as a real exportable table, not one marker at a time.
  output$he_summary_table <- renderDT({
    he_rows <- combined_map_data %>% filter(Type == "Higher Ed Institution")
    appointment_counts <- ccdata %>%
      count(Institution, Appointment, name = "Openings") %>%
      tidyr::complete(
        Institution,
        Appointment = c("Faculty/instructor (non-adjunct)", "Adjunct/part-time faculty"),
        fill = list(Openings = 0L)
      ) %>%
      tidyr::pivot_wider(names_from = Appointment, values_from = Openings, values_fill = 0)
    year <- unique(na.omit(he_rows$Salary_Year))
    year_label <- if (length(year) > 0) year[1] else "current"
    year_int <- suppressWarnings(as.integer(year_label))
    y1_label <- if (!is.na(year_int)) as.character(year_int - 1) else "1 year ago"
    y2_label <- if (!is.na(year_int)) as.character(year_int - 2) else "2 years ago"

    df <- he_rows %>%
      left_join(appointment_counts, by = c("Name" = "Institution")) %>%
      arrange(desc(CurrentCount)) %>%
      transmute(
        Institution = Name,
        County,
        `Current Openings` = CurrentCount,
        `Faculty/instructor (non-adjunct)` = coalesce(`Faculty/instructor (non-adjunct)`, 0L),
        `Adjunct/part-time faculty` = coalesce(`Adjunct/part-time faculty`, 0L),
        `New This Week` = WeeklyNew,
        `Faculty Vacancy Rate` = ifelse(is.na(Vacancy_Rate), NA_character_, scales::percent(Vacancy_Rate, accuracy = 0.1)),
        AvgFacultySalary = ifelse(is.na(Faculty_Avg_Salary), NA_character_, scales::dollar(Faculty_Avg_Salary)),
        AvgFacultySalaryY1 = ifelse(is.na(Faculty_Avg_Salary_Y1Ago), NA_character_, scales::dollar(Faculty_Avg_Salary_Y1Ago)),
        AvgFacultySalaryY2 = ifelse(is.na(Faculty_Avg_Salary_Y2Ago), NA_character_, scales::dollar(Faculty_Avg_Salary_Y2Ago)),
        ProfessorAvgSalary = ifelse(is.na(Faculty_Avg_Salary_Professor), NA_character_, scales::dollar(Faculty_Avg_Salary_Professor)),
        `Faculty Count` = Faculty_Count,
        # IPEDS fall enrollment (FTE) -- HE's analogue of the K-12 District
        # Summary table's Enrollment/Students per Teacher columns, added
        # 2026-08-06 via ipeds_enrollment_scraper.R.
        Enrollment,
        `Students per Faculty` = ifelse(is.na(Students_Per_Teacher), NA_character_, sprintf("%.1f", Students_Per_Teacher)),
        # 5-year enrollment trend, institution-level (IPEDS) -- the HE
        # analogue of the K-12 District Summary table's county-level
        # "County Population Trend (5yr)" column, but arguably a more
        # direct signal here since it's the actual institution, not just
        # its county.
        `Enrollment Trend (5yr)` = ifelse(is.na(Enrollment_Change_Pct), NA_character_,
                                           paste0(ifelse(Enrollment_Change_Pct >= 0, "+", ""), scales::percent(Enrollment_Change_Pct, accuracy = 0.1))),
        # Share of students receiving a Pell Grant (FSA) -- the HE
        # analogue of the K-12 District Summary table's "District Child
        # Poverty Rate" column, a different federal program since SAIPE
        # has no HE equivalent. Pell_Year usually trails Salary_Year/
        # Enrollment's year (FSA's own data lags IPEDS's), so it's called
        # out by name here rather than assumed to match.
        `Pell Grant Recipient Share` = ifelse(is.na(Pell_Recipient_Share), NA_character_,
                                               paste0(scales::percent(Pell_Recipient_Share, accuracy = 0.1), " (", Pell_Year, ")")),
        # County-level context (Census ACS 5-Year) -- same reasoning and
        # same source as the K-12 District Summary table's equivalent
        # columns, added 2026-08-06 once salarymap.csv gained a County
        # column to join through. No child-poverty-rate column here (see
        # map_he's Child_Poverty_Rate comment for why SAIPE has no HE
        # equivalent).
        `County Median Income` = ifelse(is.na(Median_Household_Income), NA_character_, scales::dollar(Median_Household_Income)),
        `County Median Rent` = ifelse(is.na(Median_Gross_Rent), NA_character_, paste0(scales::dollar(Median_Gross_Rent), "/mo")),
        `County Mining/Energy Jobs` = ifelse(is.na(Mining_Employment_Share), NA_character_, scales::percent(Mining_Employment_Share, accuracy = 0.1)),
        `County Population Trend (5yr)` = ifelse(is.na(Population_Change_Pct), NA_character_,
                                                   paste0(ifelse(Population_Change_Pct >= 0, "+", ""), scales::percent(Population_Change_Pct, accuracy = 0.1))),
        # Short plain-text label, not the full Salary_Note sentence --
        # that blew out Sheridan/Gillette's row height next to every other
        # institution's single-line rows (confirmed visually 2026-08-05).
        # Kept plain text rather than an HTML tooltip so the CSV/copy
        # export buttons still produce clean text, not raw markup. Now
        # mentions the vacancy rate too, since that's a combined figure
        # here as well (see Vacancy_Rate_Shared), not just salary.
        Note = ifelse(is.na(Salary_Note), "", "Shared salary & vacancy-rate reporting w/ Sheridan & Gillette")
      )
    names(df)[names(df) == "AvgFacultySalary"] <- paste0("Avg Faculty Salary (", year_label, ")")
    names(df)[names(df) == "AvgFacultySalaryY1"] <- paste0("Avg Faculty Salary (", y1_label, ")")
    names(df)[names(df) == "AvgFacultySalaryY2"] <- paste0("Avg Faculty Salary (", y2_label, ")")
    names(df)[names(df) == "ProfessorAvgSalary"] <- paste0("Professor Avg Salary (", year_label, ")")

    datatable(df, filter = "top", extensions = "Buttons",
              options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print"), pageLength = 9))
  })

  # Salary_Year and Salary_Source are the same for every institution (one
  # IPEDS survey year covers all 9) -- same reasoning as
  # k12_summary_footnote above.
  output$he_summary_footnote <- renderUI({
    year <- unique(na.omit(combined_map_data$Salary_Year[combined_map_data$Type == "Higher Ed Institution"]))
    source <- unique(na.omit(combined_map_data$Salary_Source[combined_map_data$Type == "Higher Ed Institution"]))
    acs_year <- unique(na.omit(combined_map_data$ACS_Year[combined_map_data$Type == "Higher Ed Institution"]))
    pell_year <- unique(na.omit(combined_map_data$Pell_Year[combined_map_data$Type == "Higher Ed Institution"]))
    req(length(year) > 0, length(source) > 0)
    tagList(
      helpText(
        "Salary data:", source[1], paste0("(", year[1], " data)"), "—",
        tags$a(href = IPEDS_SALARY_SOURCE_URL, target = "_blank", "educationdata.urban.org"),
        ". Blends full-time and part-time/adjunct instructional staff pay into one average",
        "— a small institution leaning heavily on adjuncts can report a much lower blended",
        "figure than its own full-time faculty actually earn."
      ),
      if (length(acs_year) > 0) {
        helpText(
          "County context (income, rent, mining/energy employment share, population trend): US Census Bureau, American Community Survey 5-Year Estimates",
          paste0("(", acs_year[1], ")"), "—",
          tags$a(href = "https://www.census.gov/data/developers/data-sets/acs-5year.html", target = "_blank", "census.gov")
        )
      },
      if (length(pell_year) > 0) {
        helpText(
          "Pell Grant recipient share: US Dept. of Education, Federal Student Aid, via Urban Institute Education Data Portal",
          paste0("(", pell_year[1], " data — usually a year or more older than the salary/enrollment figures above, since FSA's own data lags IPEDS's)"), "—",
          tags$a(href = "https://educationdata.urban.org/documentation/colleges.html", target = "_blank", "educationdata.urban.org"),
          ". Recipients ÷ that same year's IPEDS FTE enrollment — a headcount-over-FTE ratio, not two directly comparable counts."
        )
      }
    )
  })

  #--------------------------------------------------
  # Higher Ed Head-to-Head Comparator
  #--------------------------------------------------
  observeEvent(input$he_comp_preset_top, {
    top_insts <- combined_map_data %>%
      filter(Type == "Higher Ed Institution") %>%
      arrange(desc(CurrentCount)) %>%
      pull(Name) %>%
      head(3)
    updateSelectizeInput(session, "he_compare_insts", selected = top_insts)
  })

  observeEvent(input$he_comp_preset_clear, {
    updateSelectizeInput(session, "he_compare_insts", selected = character(0))
  })

  output$he_compare_cards <- renderUI({
    selected <- input$he_compare_insts
    if (is.null(selected) || length(selected) == 0) {
      return(div(class = "alert alert-info", style = "margin-top: 15px;",
                 icon("circle-info"), " Please select one to four institutions above to display comparative benchmarks."))
    }
    
    col_width <- case_when(
      length(selected) == 1 ~ 12,
      length(selected) == 2 ~ 6,
      length(selected) == 3 ~ 4,
      TRUE ~ 3
    )

    he_rows <- combined_map_data %>%
      filter(Type == "Higher Ed Institution", Name %in% selected)

    cards <- lapply(selected, function(iname) {
      row <- he_rows %>% filter(Name == iname)
      if (nrow(row) == 0) return(NULL)

      avg_sal <- if (!is.na(row$Faculty_Avg_Salary[1])) scales::dollar(row$Faculty_Avg_Salary[1]) else "N/A"
      prof_sal <- if (!is.na(row$Faculty_Avg_Salary_Professor[1])) scales::dollar(row$Faculty_Avg_Salary_Professor[1]) else "N/A"
      vac_rate <- if (!is.na(row$Vacancy_Rate[1])) scales::percent(row$Vacancy_Rate[1], accuracy = 0.1) else "N/A"
      enroll <- if (!is.na(row$Enrollment[1])) scales::comma(row$Enrollment[1]) else "N/A"
      spf <- if (!is.na(row$Students_Per_Teacher[1])) sprintf("%.1f", row$Students_Per_Teacher[1]) else "N/A"
      enroll_trend <- if (!is.na(row$Enrollment_Change_Pct[1])) paste0(ifelse(row$Enrollment_Change_Pct[1] >= 0, "+", ""), scales::percent(row$Enrollment_Change_Pct[1], accuracy = 0.1)) else "N/A"
      pell <- if (!is.na(row$Pell_Recipient_Share[1])) scales::percent(row$Pell_Recipient_Share[1], accuracy = 0.1) else "N/A"

      column(
        width = col_width,
        div(
          class = "box box-solid box-primary",
          style = "border-top: 3px solid #2a78d6; box-shadow: 0 1px 3px rgba(0,0,0,0.08); margin-bottom: 20px;",
          div(
            class = "box-header with-border", style = "background: #f7f9fa; padding: 10px 15px;",
            h4(style = "margin: 0; font-size: 15px; font-weight: 600; color: #222;", iname),
            tags$small(class = "text-muted", paste0(row$County[1], " County"))
          ),
          div(
            class = "box-body", style = "padding: 15px;",
            fluidRow(
              column(6, div(style = "text-align: center; border-right: 1px solid #eee;",
                           div(style = "font-size: 22px; font-weight: 700; color: #2a78d6;", row$CurrentCount[1]),
                           div(style = "font-size: 11px; text-transform: uppercase; color: #777;", "Active Openings"))),
              column(6, div(style = "text-align: center;",
                           div(style = "font-size: 22px; font-weight: 700; color: #e67e22;", vac_rate),
                           div(style = "font-size: 11px; text-transform: uppercase; color: #777;", "Vacancy Rate")))
            ),
            tags$hr(style = "margin: 12px 0;"),
            tags$table(
              style = "width: 100%; font-size: 13px; line-height: 1.8;",
              tags$tr(tags$td(class = "text-muted", "Avg Faculty Salary:"), tags$td(style = "text-align: right; font-weight: 600;", avg_sal)),
              tags$tr(tags$td(class = "text-muted", "Full Professor Avg:"), tags$td(style = "text-align: right; font-weight: 600;", prof_sal)),
              tags$tr(tags$td(class = "text-muted", "FTE Enrollment:"), tags$td(style = "text-align: right; font-weight: 600;", enroll)),
              tags$tr(tags$td(class = "text-muted", "Students / Faculty:"), tags$td(style = "text-align: right; font-weight: 600;", spf)),
              tags$tr(tags$td(class = "text-muted", "5-Yr Enrollment Trend:"), tags$td(style = "text-align: right; font-weight: 600;", enroll_trend)),
              tags$tr(tags$td(class = "text-muted", "Pell Grant Share:"), tags$td(style = "text-align: right; font-weight: 600;", pell))
            )
          )
        )
      )
    })
    fluidRow(cards)
  })

  output$he_compare_salary_plot <- renderPlotly({
    selected <- input$he_compare_insts
    if (is.null(selected) || length(selected) == 0) {
      return(plot_ly(type = "scatter", mode = "none") %>% layout(
        annotations = list(list(text = "Select one or more institutions to display salary comparison",
                                showarrow = FALSE, font = list(size = 14, color = "#666"))),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ))
    }
    df <- combined_map_data %>%
      filter(Type == "Higher Ed Institution", Name %in% selected) %>%
      select(Name, Faculty_Avg_Salary, Faculty_Avg_Salary_Y1Ago)
    
    plot_ly(df, x = ~Name) %>%
      add_bars(y = ~Faculty_Avg_Salary, name = "Current Avg Faculty Salary", marker = list(color = "#2b5876")) %>%
      add_bars(y = ~Faculty_Avg_Salary_Y1Ago, name = "Prior Year Avg Faculty Salary", marker = list(color = "#4e4376")) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = "Annual Compensation ($)", tickformat = "$,.0f"),
        legend = list(orientation = "h", x = 0, y = 1.15),
        margin = list(b = 60, l = 60, r = 20, t = 40)
      )
  })

  output$he_compare_openings_plot <- renderPlotly({
    selected <- input$he_compare_insts
    if (is.null(selected) || length(selected) == 0) {
      return(plot_ly(type = "scatter", mode = "none") %>% layout(
        annotations = list(list(text = "Select one or more institutions to display openings breakdown",
                                showarrow = FALSE, font = list(size = 14, color = "#666"))),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ))
    }
    df <- ccdata %>%
      filter(Institution %in% selected) %>%
      count(Institution, Appointment, name = "Count")
    
    if (nrow(df) == 0) {
      return(plot_ly(type = "scatter", mode = "none") %>% layout(
        annotations = list(list(text = "No active postings for selected institutions",
                                showarrow = FALSE, font = list(size = 14, color = "#666"))),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      ))
    }

    plot_ly(df, x = ~Institution, y = ~Count, color = ~Appointment, type = "bar") %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = "Active Openings"),
        legend = list(orientation = "h", x = 0, y = 1.15),
        margin = list(b = 60, l = 50, r = 20, t = 40)
      )
  })

  output$he_compare_jobs_table <- renderDT({
    selected <- input$he_compare_insts
    df <- ccdata
    if (!is.null(selected) && length(selected) > 0) {
      df <- df %>% filter(Institution %in% selected)
    } else {
      df <- df[0, ]
    }
    datatable(df %>% select(-any_of(c(".Pay_Value", ".Pay_Unit"))), filter = "top", escape = FALSE, extensions = "Buttons",
              options = list(scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "print"), pageLength = 10))
  })

  observeEvent(input$he_detail_level_trends, {
    cats <- if (identical(input$he_detail_level_trends, "detail")) {
      sort(unique(hesum_he$Category))
    } else {
      sort(unique(hesum_he_agg$Category))
    }
    updatePickerInput(session, "he_category", choices = cats, selected = cats)
  }, ignoreInit = TRUE)

  # ---- Reactive filtered dataset by institution + Category ----
  filtered_hesum <- reactive({
    req(input$inst_trend, input$he_category, input$he_chart_type, input$he_detail_level_trends)

    base <- if (identical(input$he_detail_level_trends, "detail")) hesum_he else hesum_he_agg

    # Filter by institution
    df <- if (input$inst_trend == "Total") {
      base
    } else {
      base %>% filter(Institution == input$inst_trend)
    }

    # Filter by Category (vector match)
    df <- df %>% filter(Category %in% input$he_category)

    # Filter by Job_Type -- "all" sums Full Time + Part Time together
    # (the group_by/summarize below combines whatever rows are present),
    # otherwise scope down to just the selected Job_Type.
    if (!identical(input$he_chart_type, "all")) {
      df <- df %>% filter(Job_Type == input$he_chart_type)
    }

    df
  })
  
  # ---- Update slider based on filtered data ----
  observe({
    df <- filtered_hesum()
    req(nrow(df) > 0)
    
    df$Archive_Date <- as.Date(df$Archive_Date)
    he_dates <- sort(unique(df$Archive_Date))
    
    # Set slider min/max as actual dates
    updateSliderInput(
      session,
      "he_scroll",
      min = min(he_dates),
      max = max(he_dates),
      value = c(max(he_dates) - WINDOW_WEEKS*7, max(he_dates)),
      timeFormat = "%Y-%m-%d"
    )
    
    # Store for reactive filtering
    session$userData$he_dates <- he_dates
  })
  
  # ---- Reactive dataset filtered by date window ----
  he_windowed <- reactive({
    df <- filtered_hesum()
    req(input$he_scroll)

    df %>%
      filter(
        Archive_Date >= as.Date(input$he_scroll[1]),
        Archive_Date <= as.Date(input$he_scroll[2]),
        Institution != "Total"  # remove pre-aggregated total row to prevent double-counting
      ) %>%
      arrange(Archive_Date)
  })
  
  # ---- Optional: show slider range ----
  output$he_slider_label <- renderText({
    req(input$he_scroll)
    paste0("Showing: ", input$he_scroll[1], " to ", input$he_scroll[2])
  })
  
  # ---- Render plot ----
  output$he_longitudinal_plot <- renderPlotly({
    df <- he_windowed()
    validate(need(nrow(df) > 0, "No data for selected institution/category/date range."))
    
    # Ensure one row per Category × Archive_Date and sort properly
    df <- df %>%
      group_by(Category, Archive_Date) %>%
      summarize(sum = sum(sum), .groups = "drop") %>%
      arrange(Category, Archive_Date) %>%
      mutate(Category = factor(Category))
    
    # Plot
    p <- ggplot(df, aes(
      x = Archive_Date,
      y = sum,
      color = Category,
      group = Category,
      text = paste0(
        "Date: ", Archive_Date, "<br>",
        "Category: ", Category, "<br>",
        "Postings: ", sum
      )
    )) +
      geom_line() +
      geom_point(size = 1) +
      scale_color_manual(values = if (identical(input$he_detail_level_trends, "detail")) HE_CATEGORY_COLORS_DETAIL else HE_CATEGORY_COLORS_AGG) +
      labs(x = "Archive Date", y = "Number of Postings") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)
      )

    ggplotly(p, height = 500, tooltip = "text")
  })




  # Sums across Job_Type (FT + PT) per category -- this table shows "how
  # has this category moved," not a FT/PT breakdown (which lived in the
  # now-removed stacked bar chart; the Appointment control below applies
  # the selected Job_Type before the category comparisons are calculated).
  output$he_current_trends_table <- renderTable({
    req(input$he_detail_level_current, input$he_current_appointment)
    hist_src <- if (identical(input$he_detail_level_current, "detail")) hesum_he else hesum_he_agg
    history <- hist_src %>%
      filter(
        Institution == input$inst_current,
        identical(input$he_current_appointment, "all") | Job_Type == input$he_current_appointment
      ) %>%
      transmute(Category, Archive_Date, n = sum)

    result <- build_current_trends_table(history, accent = "#4a3aa7")
    req(!is.null(result))
    result
  }, sanitize.text.function = function(x) x)

  # ------------------ K-12 Vacancy Spells Server Logic ------------------
  observe({
    if (nrow(job_duration_spells) > 0) {
      dists <- sort(unique(job_duration_spells$District))
      updateSelectInput(session, "k12_spell_district",
                        choices = c("All Districts" = "all", setNames(dists, dists)),
                        selected = "all")
    }
  })

  filtered_spells <- reactive({
    req(nrow(job_duration_spells) > 0)
    df <- job_duration_spells

    if (!is.null(input$k12_spell_status) && !identical(input$k12_spell_status, "all")) {
      df <- df %>% filter(Status == input$k12_spell_status)
    }

    if (!is.null(input$k12_spell_position) && !identical(input$k12_spell_position, "all")) {
      if (identical(input$k12_spell_position, "Support")) {
        df <- df %>% filter(Position != "Teacher" & Position != "Administration")
      } else {
        df <- df %>% filter(Position == input$k12_spell_position)
      }
    }

    if (!is.null(input$k12_spell_discipline) && !identical(input$k12_spell_discipline, "all")) {
      disc <- input$k12_spell_discipline
      if (disc == "stem" && "is_stem" %in% names(df)) df <- df %>% filter(is_stem == 1)
      else if (disc == "sped" && "is_sped" %in% names(df)) df <- df %>% filter(is_sped == 1)
      else if (disc == "elementary" && "is_elementary" %in% names(df)) df <- df %>% filter(is_elementary == 1)
      else if (disc == "secondary" && "is_secondary" %in% names(df)) df <- df %>% filter(is_secondary == 1)
      else if (disc == "admin" && "is_admin" %in% names(df)) df <- df %>% filter(is_admin == 1)
      else if (disc == "support" && "is_support_staff" %in% names(df)) df <- df %>% filter(is_support_staff == 1)
    }

    if (!is.null(input$k12_spell_district) && !identical(input$k12_spell_district, "all")) {
      df <- df %>% filter(District == input$k12_spell_district)
    }

    df
  })

  output$k12_spell_median_box <- renderValueBox({
    df <- filtered_spells()
    med <- if (nrow(df) > 0) round(median(df$Estimated_Days_Open, na.rm = TRUE)) else 0
    valueBox(
      paste0(med, " Days"),
      "Median Vacancy Duration",
      icon = icon("stopwatch"),
      color = "blue"
    )
  })

  output$k12_spell_active_box <- renderValueBox({
    df <- filtered_spells()
    active_n <- if (nrow(df) > 0) sum(df$Status == "Still Active", na.rm = TRUE) else 0
    valueBox(
      scales::comma(active_n),
      "Currently Open Spells",
      icon = icon("door-open"),
      color = "green"
    )
  })

  output$k12_spell_persistent_box <- renderValueBox({
    df <- filtered_spells()
    persist_n <- if (nrow(df) > 0) sum(df$Status == "Still Active" & df$Estimated_Days_Open >= 60, na.rm = TRUE) else 0
    valueBox(
      scales::comma(persist_n),
      "Persistent Spells (≥60 Days)",
      icon = icon("triangle-exclamation"),
      color = "red"
    )
  })

  output$k12_spell_hardest_box <- renderValueBox({
    df <- filtered_spells()
    hardest <- "None"
    if (nrow(df) >= 1 && "Broad_Category" %in% names(df)) {
      cat_dur <- df %>%
        group_by(Broad_Category) %>%
        summarize(med = median(Estimated_Days_Open, na.rm = TRUE), n = n(), .groups = "drop") %>%
        filter(n >= 1) %>%
        arrange(desc(med))
      if (nrow(cat_dur) > 0) hardest <- cat_dur$Broad_Category[1]
    }
    valueBox(
      hardest,
      "Longest Duration Category",
      icon = icon("graduation-cap"),
      color = "purple"
    )
  })

  output$k12_spell_duration_plot <- renderPlotly({
    df <- filtered_spells()
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No vacancy spells match the selected filter criteria", size = 4, color = "#666666") +
        theme_void()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }

    cat_summary <- df %>%
      group_by(Broad_Category) %>%
      summarize(
        Median_Days = round(median(Estimated_Days_Open, na.rm = TRUE)),
        Postings = n(),
        .groups = "drop"
      ) %>%
      filter(!is.na(Broad_Category) & Broad_Category != "" & Postings >= 1) %>%
      arrange(desc(Median_Days)) %>%
      head(12)

    if (nrow(cat_summary) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No category data available for selected filter", size = 4, color = "#666666") +
        theme_void()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }

    p <- ggplot(cat_summary, aes(
      x = reorder(Broad_Category, Median_Days),
      y = Median_Days,
      text = paste0("<b>", Broad_Category, "</b><br>Median Open: ", Median_Days, " days<br>Total Spells: ", Postings)
    )) +
      geom_col(fill = "#2a78d6", width = 0.65) +
      coord_flip() +
      labs(x = NULL, y = "Median Days Open") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 9),
        panel.grid.major.y = element_blank()
      )

    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })

  output$k12_spell_distrib_plot <- renderPlotly({
    df <- filtered_spells()
    bin_levels <- c("1–14 Days", "15–30 Days", "31–60 Days", "61–90 Days", "90+ Days")
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No vacancy spells match the selected filter criteria", size = 4, color = "#666666") +
        theme_void()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }

    df <- df %>%
      mutate(Duration_Bin = case_when(
        Estimated_Days_Open <= 14 ~ "1–14 Days",
        Estimated_Days_Open <= 30 ~ "15–30 Days",
        Estimated_Days_Open <= 60 ~ "31–60 Days",
        Estimated_Days_Open <= 90 ~ "61–90 Days",
        TRUE ~ "90+ Days"
      )) %>%
      mutate(Duration_Bin = factor(Duration_Bin, levels = bin_levels))

    bin_counts <- df %>%
      count(Duration_Bin, name = "Count") %>%
      tidyr::complete(Duration_Bin = factor(bin_levels, levels = bin_levels), fill = list(Count = 0)) %>%
      filter(!is.na(Duration_Bin))

    p <- ggplot(bin_counts, aes(
      x = Duration_Bin, y = Count,
      text = paste0("<b>", Duration_Bin, "</b><br>Spells: ", Count)
    )) +
      geom_col(fill = "#e67e22", width = 0.6) +
      labs(x = NULL, y = "Number of Vacancies") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 25, hjust = 1, size = 8),
        panel.grid.major.x = element_blank()
      )

    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })

  output$k12_spell_table <- renderDT({
    df <- filtered_spells()
    cols_to_show <- c("District", "Title", "Position", "Broad_Category",
                      "First_Seen_Date", "Last_Seen_Date", "Estimated_Days_Open", "Status")
    existing_cols <- intersect(cols_to_show, names(df))

    df_display <- if (nrow(df) > 0) {
      df %>% select(all_of(existing_cols)) %>% arrange(desc(Estimated_Days_Open))
    } else {
      df[0, existing_cols, drop = FALSE]
    }

    datatable(
      df_display,
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50, 100),
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      extensions = "Buttons",
      rownames = FALSE,
      escape = FALSE
    )
  })

  # ------------------ Higher Ed Vacancy Spells ------------------
  observe({
    if (nrow(he_job_duration_spells) > 0) {
      insts <- sort(unique(he_job_duration_spells$Institution))
      updateSelectInput(session, "he_spell_institution",
                        choices = c("All Institutions" = "all", setNames(insts, insts)),
                        selected = "all")
    }
  })

  filtered_he_spells <- reactive({
    req(nrow(he_job_duration_spells) > 0)
    df <- he_job_duration_spells

    if (!is.null(input$he_spell_status) && !identical(input$he_spell_status, "all")) {
      df <- df %>% filter(Status == input$he_spell_status)
    }

    if (!is.null(input$he_spell_job_type) && !identical(input$he_spell_job_type, "all")) {
      df <- df %>% filter(Job_Type == input$he_spell_job_type)
    }

    if (!is.null(input$he_spell_discipline) && !identical(input$he_spell_discipline, "all")) {
      disc <- input$he_spell_discipline
      if (disc == "stem" && "is_stem" %in% names(df)) df <- df %>% filter(is_stem == 1)
      else if (disc == "health" && "is_health" %in% names(df)) df <- df %>% filter(is_health == 1)
      else if (disc == "cte" && "is_cte" %in% names(df)) df <- df %>% filter(is_cte == 1)
    }

    if (!is.null(input$he_spell_institution) && !identical(input$he_spell_institution, "all")) {
      df <- df %>% filter(Institution == input$he_spell_institution)
    }

    df
  })

  output$he_spell_median_box <- renderValueBox({
    df <- filtered_he_spells()
    med <- if (nrow(df) > 0) round(median(df$Estimated_Days_Open, na.rm = TRUE)) else 0
    valueBox(
      paste0(med, " Days"),
      "Median Vacancy Duration",
      icon = icon("stopwatch"),
      color = "blue"
    )
  })

  output$he_spell_active_box <- renderValueBox({
    df <- filtered_he_spells()
    active_n <- if (nrow(df) > 0) sum(df$Status == "Still Active", na.rm = TRUE) else 0
    valueBox(
      scales::comma(active_n),
      "Currently Open Searches",
      icon = icon("door-open"),
      color = "green"
    )
  })

  output$he_spell_persistent_box <- renderValueBox({
    df <- filtered_he_spells()
    persist_n <- if (nrow(df) > 0) sum(df$Status == "Still Active" & df$Estimated_Days_Open >= 90, na.rm = TRUE) else 0
    valueBox(
      scales::comma(persist_n),
      "Persistent Searches (≥90 Days)",
      icon = icon("triangle-exclamation"),
      color = "red"
    )
  })

  output$he_spell_hardest_box <- renderValueBox({
    df <- filtered_he_spells()
    hardest <- "None"
    if (nrow(df) >= 1 && "Category" %in% names(df)) {
      cat_dur <- df %>%
        group_by(Category) %>%
        summarize(med = median(Estimated_Days_Open, na.rm = TRUE), n = n(), .groups = "drop") %>%
        filter(n >= 1) %>%
        arrange(desc(med))
      if (nrow(cat_dur) > 0) hardest <- cat_dur$Category[1]
    }
    valueBox(
      hardest,
      "Longest Duration Discipline",
      icon = icon("building-columns"),
      color = "purple"
    )
  })

  output$he_spell_duration_plot <- renderPlotly({
    df <- filtered_he_spells()
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No vacancy spells match the selected filter criteria", size = 4, color = "#666666") +
        theme_void()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }

    cat_summary <- df %>%
      group_by(Category) %>%
      summarize(
        Median_Days = round(median(Estimated_Days_Open, na.rm = TRUE)),
        Postings = n(),
        .groups = "drop"
      ) %>%
      filter(!is.na(Category) & Category != "" & Postings >= 1) %>%
      arrange(desc(Median_Days)) %>%
      head(12)

    if (nrow(cat_summary) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No discipline data available for selected filter", size = 4, color = "#666666") +
        theme_void()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }

    p <- ggplot(cat_summary, aes(
      x = reorder(Category, Median_Days),
      y = Median_Days,
      text = paste0("<b>", Category, "</b><br>Median Open: ", Median_Days, " days<br>Total Spells: ", Postings)
    )) +
      geom_col(fill = "#2a78d6", width = 0.65) +
      coord_flip() +
      labs(x = NULL, y = "Median Days Open") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 9),
        panel.grid.major.y = element_blank()
      )

    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })

  output$he_spell_distrib_plot <- renderPlotly({
    df <- filtered_he_spells()
    bin_levels <- c("1–30 Days", "31–60 Days", "61–90 Days", "91–180 Days", "180+ Days")
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No vacancy spells match the selected filter criteria", size = 4, color = "#666666") +
        theme_void()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }

    df <- df %>%
      mutate(Duration_Bin = case_when(
        Estimated_Days_Open <= 30 ~ "1–30 Days",
        Estimated_Days_Open <= 60 ~ "31–60 Days",
        Estimated_Days_Open <= 90 ~ "61–90 Days",
        Estimated_Days_Open <= 180 ~ "91–180 Days",
        TRUE ~ "180+ Days"
      )) %>%
      mutate(Duration_Bin = factor(Duration_Bin, levels = bin_levels))

    bin_counts <- df %>%
      count(Duration_Bin, name = "Count") %>%
      tidyr::complete(Duration_Bin = factor(bin_levels, levels = bin_levels), fill = list(Count = 0)) %>%
      filter(!is.na(Duration_Bin))

    p <- ggplot(bin_counts, aes(
      x = Duration_Bin, y = Count,
      text = paste0("<b>", Duration_Bin, "</b><br>Spells: ", Count)
    )) +
      geom_col(fill = "#9b59b6", width = 0.6) +
      labs(x = NULL, y = "Number of Vacancies") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 25, hjust = 1, size = 8),
        panel.grid.major.x = element_blank()
      )

    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })

  output$he_spell_table <- renderDT({
    df <- filtered_he_spells()
    cols_to_show <- c("Institution", "Title", "Job_Type", "Category",
                      "First_Seen_Date", "Last_Seen_Date", "Estimated_Days_Open", "Status")
    existing_cols <- intersect(cols_to_show, names(df))

    df_display <- if (nrow(df) > 0) {
      df %>% select(all_of(existing_cols)) %>% arrange(desc(Estimated_Days_Open))
    } else {
      df[0, existing_cols, drop = FALSE]
    }

    datatable(
      df_display,
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50, 100),
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      extensions = "Buttons",
      rownames = FALSE,
      escape = FALSE
    )
  })
}

shinyApp(ui, server)
