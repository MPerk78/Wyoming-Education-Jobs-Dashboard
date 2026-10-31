# ---------------------------------------------------------------------------
# BLS Occupational Employment and Wage Statistics (OEWS)
# ---------------------------------------------------------------------------
#
# Regional pay by occupation, used only as a clearly labeled fallback for
# K-12 postings that state no pay of their own ("Typical pay (BLS est.)").
# Posted pay stays the gold standard. OEWS figures cover every employer of
# that occupation in the area, not just schools, and are published once a
# year (May reference period, released the following spring).
#
# Fetched through the BLS Public Data API (api.bls.gov). bls.gov's own
# download pages and flat files refuse scripted requests, so the API is the
# only sanctioned route. Without a key the API allows about 25 queries of 25
# series a day per IP; a full refresh is ~15 queries. BLS_API_KEY (free
# registration) raises that to 500 queries of 50 series and is used when set.
# The API serves only the latest year, so the committed CSV is refreshed only
# when a newer year appears -- one query a week otherwise.
#
# Shared verbatim by the Montana and Wyoming dashboards.

# Areas as defined in BLS's "May 2025 OEWS Metropolitan and Nonmetropolitan
# Area Definitions" (bls.gov/oes/2025/may/msa_def.htm). Metro codes are
# "00" + the CBSA code; nonmetropolitan areas use state FIPS + "0000" + n.
# Counties are listed with " County" as in the district registries.
OEWS_AREAS <- list(
  MT = list(
    state = c(code = "3000000", name = "Montana"),
    areas = list(
      list(code = "0013740", name = "Billings, MT", counties = c("Carbon", "Stillwater", "Yellowstone")),
      list(code = "0014580", name = "Bozeman, MT", counties = c("Gallatin")),
      list(code = "0024500", name = "Great Falls, MT", counties = c("Cascade")),
      list(code = "0025740", name = "Helena, MT", counties = c("Broadwater", "Jefferson", "Lewis and Clark")),
      list(code = "0033540", name = "Missoula, MT", counties = c("Mineral", "Missoula")),
      list(code = "3000003", name = "Southwest Montana nonmetro", counties = c(
        "Beaverhead", "Deer Lodge", "Granite", "Madison", "Meagher", "Park", "Powell", "Silver Bow", "Sweet Grass")),
      list(code = "3000004", name = "West Montana nonmetro", counties = c(
        "Flathead", "Lake", "Lincoln", "Ravalli", "Sanders")),
      list(code = "3000006", name = "East-Central Montana nonmetro", counties = c(
        "Big Horn", "Blaine", "Carter", "Chouteau", "Custer", "Daniels", "Dawson", "Fallon", "Fergus",
        "Garfield", "Glacier", "Golden Valley", "Hill", "Judith Basin", "Liberty", "McCone", "Musselshell",
        "Petroleum", "Phillips", "Pondera", "Powder River", "Prairie", "Richland", "Roosevelt", "Rosebud",
        "Sheridan", "Teton", "Toole", "Treasure", "Valley", "Wheatland", "Wibaux"))
    )
  ),
  WY = list(
    state = c(code = "5600000", name = "Wyoming"),
    areas = list(
      list(code = "0016220", name = "Casper, WY", counties = c("Natrona")),
      list(code = "0016940", name = "Cheyenne, WY", counties = c("Laramie")),
      list(code = "5600006", name = "Western Wyoming nonmetro", counties = c(
        "Big Horn", "Fremont", "Hot Springs", "Lincoln", "Park", "Sublette", "Sweetwater", "Teton",
        "Uinta", "Washakie")),
      list(code = "5600007", name = "Eastern Wyoming nonmetro", counties = c(
        "Albany", "Campbell", "Carbon", "Converse", "Crook", "Goshen", "Johnson", "Niobrara", "Platte",
        "Sheridan", "Weston"))
    )
  )
)

# The occupations K-12 postings are matched to (2018 SOC). hourly = TRUE for
# roles usually paid by the hour, whose hourly median is fetched too; BLS
# publishes only an annual figure for school-year roles such as teaching
# assistants, so those keep the annual one.
OEWS_OCCUPATIONS <- data.frame(
  soc = c("252021", "252022", "252031", "253031", "259045", "119032", "211012", "193034",
          "291127", "291141", "254022", "436014", "372011", "499071", "533051", "352012", "399011"),
  label = c("Elementary school teachers", "Middle school teachers", "Secondary school teachers",
            "Substitute teachers", "Teaching assistants", "K-12 education administrators",
            "School counselors", "School psychologists", "Speech-language pathologists",
            "Registered nurses", "Librarians", "Secretaries and administrative assistants",
            "Janitors and cleaners", "Maintenance and repair workers", "School bus drivers",
            "Institution and cafeteria cooks", "Childcare workers"),
  hourly = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
             FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

OEWS_ANNUAL_MEDIAN <- "13"
OEWS_HOURLY_MEDIAN <- "08"

# Every area of one state, statewide first: data.frame(code, name, type).
oews_state_areas <- function(state) {
  cfg <- OEWS_AREAS[[state]]
  data.frame(
    code = c(cfg$state[["code"]], vapply(cfg$areas, `[[`, "", "code")),
    name = c(cfg$state[["name"]], vapply(cfg$areas, `[[`, "", "name")),
    type = c("state", rep("area", length(cfg$areas))),
    stringsAsFactors = FALSE
  )
}

# "OEUS" + area + industry (000000 = all) + SOC + datatype for a state;
# "OEUM" for metropolitan and nonmetropolitan areas.
oews_series_id <- function(area_code, type, soc, datatype) {
  paste0(ifelse(type == "state", "OEUS", "OEUM"), area_code, "000000", soc, datatype)
}

# All series one state needs: annual medians for every occupation, hourly
# medians for the hourly ones.
oews_series_plan <- function(state, occupations = OEWS_OCCUPATIONS) {
  areas <- oews_state_areas(state)
  grid <- merge(areas, occupations, by = NULL)
  annual <- transform(grid, datatype = OEWS_ANNUAL_MEDIAN)
  hourly <- transform(grid[grid$hourly, ], datatype = OEWS_HOURLY_MEDIAN)
  plan <- rbind(annual, hourly)
  plan$series_id <- oews_series_id(plan$code, plan$type, plan$soc, plan$datatype)
  plan
}

# Parses one API response into series_id / year / value. "-" (suppressed or
# not published) and "#" (above the published ceiling) become NA.
parse_oews_response <- function(json_text) {
  parsed <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  if (!identical(parsed$status, "REQUEST_SUCCEEDED")) {
    stop("BLS API: ", parsed$status, " -- ", paste(unlist(parsed$message), collapse = "; "))
  }
  rows <- lapply(parsed$Results$series, function(s) {
    d <- if (length(s$data) > 0) s$data[[1]] else NULL
    data.frame(
      series_id = s$seriesID,
      year = if (is.null(d)) NA_character_ else d$year,
      value = if (is.null(d)) NA_real_ else suppressWarnings(as.numeric(gsub(",", "", d$value))),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

oews_api_request <- function(series_ids, api_key = Sys.getenv("BLS_API_KEY")) {
  body <- list(seriesid = as.list(series_ids), latest = TRUE)
  if (nzchar(api_key)) body$registrationkey <- api_key
  resp <- httr2::request("https://api.bls.gov/publicAPI/v2/timeseries/data/") %>%
    httr2::req_body_json(body) %>%
    perform_with_retry()
  parse_oews_response(httr2::resp_body_string(resp))
}

# Latest OEWS year the API serves, from one statewide series.
fetch_oews_latest_year <- function(state, api_key = Sys.getenv("BLS_API_KEY")) {
  areas <- oews_state_areas(state)
  id <- oews_series_id(areas$code[1], "state", "252021", OEWS_ANNUAL_MEDIAN)
  oews_api_request(id, api_key)$year[1]
}

# One row per area x occupation: Area_Code, Area_Name, SOC, Occupation,
# Annual_Median, Hourly_Median, Year.
fetch_oews_wages <- function(state, api_key = Sys.getenv("BLS_API_KEY"), pause = 1) {
  plan <- oews_series_plan(state)
  batch_size <- if (nzchar(api_key)) 50 else 25
  batches <- split(plan$series_id, ceiling(seq_along(plan$series_id) / batch_size))
  values <- do.call(rbind, lapply(seq_along(batches), function(i) {
    if (i > 1) Sys.sleep(pause)
    oews_api_request(batches[[i]], api_key)
  }))
  plan <- merge(plan, values, by = "series_id", all.x = TRUE)
  key <- paste(plan$code, plan$soc)
  annual <- plan[plan$datatype == OEWS_ANNUAL_MEDIAN, ]
  hourly <- plan[plan$datatype == OEWS_HOURLY_MEDIAN, ]
  out <- data.frame(
    Area_Code = annual$code, Area_Name = annual$name, SOC = annual$soc, Occupation = annual$label,
    Annual_Median = annual$value,
    Hourly_Median = hourly$value[match(paste(annual$code, annual$soc), paste(hourly$code, hourly$soc))],
    Year = annual$year,
    stringsAsFactors = FALSE
  )
  out$Year <- max(out$Year, na.rm = TRUE)
  out[order(out$Area_Code != oews_state_areas(state)$code[1], out$Area_Name, out$SOC), ]
}

# Keeps the committed file unless BLS has published a newer year; a failed
# check or fetch leaves the existing file as it is.
refresh_oews_wages <- function(state, path, api_key = Sys.getenv("BLS_API_KEY")) {
  existing <- if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE,
                                                     colClasses = c(SOC = "character", Area_Code = "character")) else NULL
  latest <- fetch_oews_latest_year(state, api_key)
  if (!is.null(existing) && nrow(existing) > 0 && as.character(existing$Year[1]) == latest) {
    message("OEWS ", state, ": May ", latest, " already on file")
    return(invisible(existing))
  }
  wages <- fetch_oews_wages(state, api_key)
  utils::write.csv(wages, path, row.names = FALSE)
  message("OEWS ", state, ": wrote May ", latest, " (", nrow(wages), " rows)")
  invisible(wages)
}

# ---------------------------------------------------------------------------
# Matching postings to an occupation and an area
# ---------------------------------------------------------------------------

# The OEWS occupation for a K-12 posting, from its position category and
# title. NA where no occupation is a fair match: coaches (season stipends,
# not comparable to an annual coaching wage) and unclassified postings.
oews_occupation_for_posting <- function(position, title) {
  p <- tolower(as.character(position))
  t <- tolower(as.character(title))
  dplyr::case_when(
    p %in% c("teacher", "part-time teacher") & grepl("high school|secondary|\\bhs\\b|grades? 9|9-12|7-12", t) ~ "252031",
    p %in% c("teacher", "part-time teacher") & grepl("middle|junior high|\\bjh\\b|grades? 6|6-8|7-8", t) ~ "252022",
    p %in% c("teacher", "part-time teacher") ~ "252021",
    p == "substitute" ~ "253031",
    p == "paraprofessional" ~ "259045",
    p == "administration" ~ "119032",
    p == "transportation" ~ "533051",
    p == "food services" ~ "352012",
    p == "child care" ~ "399011",
    p == "custodial/maintenance" & grepl("mainten|mechanic|hvac|electric|plumb|carpent|grounds|repair|technician", t) ~ "499071",
    p == "custodial/maintenance" ~ "372011",
    p %in% c("support services", "staff") & grepl("counsel", t) ~ "211012",
    p %in% c("support services", "staff") & grepl("psycholog", t) ~ "193034",
    p %in% c("support services", "staff") & grepl("speech|\\bslp\\b", t) ~ "291127",
    p %in% c("support services", "staff") & grepl("nurse|\\brn\\b", t) ~ "291141",
    p %in% c("support services", "staff") & grepl("librar|media specialist", t) ~ "254022",
    p %in% c("support services", "staff") & grepl("secretar|clerk|receptionist|administrative assistant|office (assistant|manager)|registrar", t) ~ "436014",
    TRUE ~ NA_character_
  )
}

# County -> OEWS area code for one state. Accepts "Yellowstone",
# "Yellowstone County" and "Albany County, Wyoming".
oews_area_for_county <- function(county, state) {
  cfg <- OEWS_AREAS[[state]]
  lookup <- unlist(lapply(cfg$areas, function(a) stats::setNames(rep(a$code, length(a$counties)), a$counties)))
  bare <- sub(" County$", "", sub(",.*$", "", trimws(as.character(county))))
  unname(lookup[bare])
}

# Adds the BLS estimate to K-12 postings: Est_Pay, Est_Pay_Unit (hour for
# hourly roles where BLS publishes an hourly median, else year), Est_Occupation,
# Est_Area and Est_Year. Uses the posting's own area when its county is known
# and BLS publishes the figure there, else the statewide figure. NA for
# roles with no fair OEWS match.
attach_oews_estimates <- function(postings, wages, county, state,
                                  position_col = "position", title_col = "title") {
  soc <- oews_occupation_for_posting(postings[[position_col]], postings[[title_col]])
  area <- oews_area_for_county(county, state)
  state_code <- oews_state_areas(state)$code[1]
  hourly_role <- OEWS_OCCUPATIONS$hourly[match(soc, OEWS_OCCUPATIONS$soc)] %in% TRUE

  pick <- function(area_code, col) wages[[col]][match(paste(area_code, soc), paste(wages$Area_Code, wages$SOC))]
  figure <- function(area_code) ifelse(hourly_role & !is.na(pick(area_code, "Hourly_Median")),
                                       pick(area_code, "Hourly_Median"), pick(area_code, "Annual_Median"))
  local <- figure(area)
  use_local <- !is.na(local)
  chosen_area <- ifelse(use_local, area, state_code)
  postings$Est_Pay <- ifelse(use_local, local, figure(state_code))
  postings$Est_Pay_Unit <- ifelse(is.na(postings$Est_Pay), NA_character_,
                                  ifelse(hourly_role & !is.na(pick(chosen_area, "Hourly_Median")), "hour", "year"))
  postings$Est_Occupation <- ifelse(is.na(postings$Est_Pay), NA_character_,
                                    OEWS_OCCUPATIONS$label[match(soc, OEWS_OCCUPATIONS$soc)])
  postings$Est_Area <- ifelse(is.na(postings$Est_Pay), NA_character_,
                              wages$Area_Name[match(chosen_area, wages$Area_Code)])
  postings$Est_Year <- ifelse(is.na(postings$Est_Pay), NA_character_, as.character(wages$Year[1]))
  postings
}

OEWS_ESTIMATE_COLUMNS <- c("Est_Pay", "Est_Pay_Unit", "Est_Occupation", "Est_Area", "Est_Year")
