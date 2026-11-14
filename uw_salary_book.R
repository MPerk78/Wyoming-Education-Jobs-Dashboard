# ---------------------------------------------------------------------------
# University of Wyoming salary book
# ---------------------------------------------------------------------------
#
# UW publishes every salaried employee's department, title and scheduled
# salary (no names) each fall: "Salary Information - Departmental Listing"
# on its operating-budget page (older years on the budget & salary archive
# page). For UW staff postings that state no pay, the median salary of UW
# staff with the same title is a far closer fallback than the IPEDS category
# average -- UW reports only its top 111 managers as IPEDS "management"
# (average $159K), so an assistant director would otherwise show that.
# Still a labeled fallback, not the job's offer. Hourly and student jobs are
# not in the book (salaried staff only) and get no figure.
#
# The book is one large PDF (~10 MB) published once a year; the weekly
# pipeline re-downloads only when the page links a different file, and keeps
# the committed CSV if anything fails.

UW_OPERATING_BUDGET_URL <- "https://www.uwyo.edu/budget-finance/budget-and-institutional-planning/budget-office/operating-budget/index.html"
UW_SALARY_BOOK_LINK_TEXT <- "Salary Information - Departmental Listing"

# A title's median is shown only when at least this many UW staff hold it,
# the same floor as the IPEDS category averages.
UW_SALARY_BOOK_MIN_COUNT <- 3

# The newest salary book linked from the operating-budget page (the first
# "Salary Information - Departmental Listing" link), as an absolute URL.
uw_salary_book_url <- function(html) {
  links <- rvest::html_elements(xml2::read_html(html), "a")
  hit <- links[trimws(rvest::html_text(links)) == UW_SALARY_BOOK_LINK_TEXT]
  if (length(hit) == 0) stop("No '", UW_SALARY_BOOK_LINK_TEXT, "' link on the UW operating-budget page")
  xml2::url_absolute(rvest::html_attr(hit[[1]], "href"), UW_OPERATING_BUDGET_URL)
}

# One row per salaried employee from the book's page text (pdftools::pdf_text()):
# Department, Title, Salary, Fiscal_Year. Each line is "Department  Title
# Salary", with Department only on a department's first line. Long titles
# wrap onto a line of their own with no salary, and long department names
# onto the next line's department column; every department closes with a
# "Total" line, so a department name can only start right after one.
parse_uw_salary_book <- function(pages) {
  fy <- regmatches(pages[1], regexpr("Fiscal Year [0-9]{4}", pages[1]))
  if (length(fy) == 0) stop("UW salary book: no 'Fiscal Year' heading on page 1")
  lines <- unlist(strsplit(pages, "\n"))
  row_rx <- "^(\\S.*?)?\\s{2,}(\\S.*?)\\s{2,}([0-9][0-9,]*)\\s*$"
  dept <- character(); title <- character(); salary <- character()
  current <- NA_character_; first_row <- 1L; after_total <- TRUE
  for (ln in lines) {
    if (grepl("\\sTotal\\s+[0-9,]+\\s*$", ln)) { after_total <- TRUE; next }
    if (grepl("^\\s*$|Scheduled Salary|Departmental Listing|Fiscal Year|^\\s*September|Note: Some employees", ln)) next
    m <- regmatches(ln, regexec(row_rx, ln))[[1]]
    if (length(m) > 0) {
      if (nzchar(m[2])) {
        if (after_total) {
          current <- trimws(m[2])
          first_row <- length(title) + 1L
        } else {
          current <- paste(current, trimws(m[2]))
          dept[first_row:length(dept)] <- current
        }
      }
      after_total <- FALSE
      dept <- c(dept, current); title <- c(title, trimws(m[3])); salary <- c(salary, m[4])
    } else if (length(title) > 0) {
      title[length(title)] <- paste(title[length(title)], trimws(ln))
    }
  }
  data.frame(Department = dept, Title = title, Salary = as.numeric(gsub(",", "", salary)),
             Fiscal_Year = rep(sub("Fiscal Year ", "FY", fy), length(title)), stringsAsFactors = FALSE)
}

# Keeps the committed file unless the page links a different book; a failed
# check, download or parse, or a book write_reference_file() rejects, leaves
# the existing file as it is.
refresh_uw_salary_book <- function(path) {
  url <- uw_salary_book_url(httr2::resp_body_string(httr2::request(UW_OPERATING_BUDGET_URL) %>% perform_with_retry()))
  existing <- if (file.exists(path)) utils::read.csv(path, stringsAsFactors = FALSE) else NULL
  if (!is.null(existing) && nrow(existing) > 0 && identical(existing$Source_URL[1], url)) {
    message("UW salary book: ", existing$Fiscal_Year[1], " already on file")
    log_reference_result(path, "current")
    return(invisible(existing))
  }
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf))
  httr2::request(url) %>% perform_with_retry(path = pdf)
  book <- parse_uw_salary_book(pdftools::pdf_text(pdf))
  if (nrow(book) < 1000) stop("UW salary book: only ", nrow(book), " rows parsed from ", url)
  book$Source_URL <- url
  if (!write_reference_file(book, path)) return(invisible(existing))
  message("UW salary book: wrote ", book$Fiscal_Year[1], " (", nrow(book), " rows)")
  invisible(book)
}

# Lower-case words with the book's abbreviations spelled out, so "Asst
# Dir, Business Ops" and "Assistant Director, Business Operations" compare
# equal. The comma survives: the part before it is the title's role.
UW_TITLE_ABBREVIATIONS <- c(
  asst = "assistant", assoc = "associate", dir = "director", mgr = "manager", coord = "coordinator",
  spec = "specialist", sr = "senior", supv = "supervisor", svc = "services", svcs = "services",
  prof = "professional", admin = "administrative", hd = "head", exec = "executive", rep = "representative",
  mgmt = "management", ops = "operations", info = "information", prog = "program", ad = "athletic director"
)

normalize_uw_title <- function(title) {
  x <- tolower(as.character(title))
  x <- gsub("&", " and ", x)
  x <- gsub("[^a-z0-9,]", " ", x)
  vapply(strsplit(x, "(?<=,)|\\s+", perl = TRUE), function(w) {
    w <- w[nzchar(w)]
    bare <- sub(",$", "", w)
    hit <- bare %in% names(UW_TITLE_ABBREVIATIONS)
    w[hit] <- paste0(UW_TITLE_ABBREVIATIONS[bare[hit]], ifelse(grepl(",$", w[hit]), ",", ""))
    gsub(" ,", ",", paste(w, collapse = " "))
  }, "")
}

# A UW posting title without its department tail ("Business Manager - UW
# Extension") or parenthetical, the form the book uses.
uw_posting_role <- function(title) {
  x <- sub("\\s+[-\u2013]\\s+.*$", "", as.character(title))
  trimws(gsub("\\s*\\([^)]*\\)", "", x))
}

# For each UW posting title: the median book salary of staff with the same
# title, else of staff whose title has the same role before its first comma
# ("Assistant Director, American Heritage Center" -> every "Assistant
# Director, ..."), each only with UW_SALARY_BOOK_MIN_COUNT holders or more.
# data.frame(Est_Pay, Matched_Title (the posting's own wording of the title
# or role matched), Staff_Count); NA where neither fits.
uw_salary_book_estimate <- function(titles, book) {
  book_title <- sub(",$", "", normalize_uw_title(book$Title))
  book_role <- sub(",.*$", "", book_title)
  posting_role <- uw_posting_role(titles)
  role <- sub(",$", "", normalize_uw_title(posting_role))
  out <- lapply(seq_along(role), function(i) {
    r <- role[i]
    head <- sub(",.*$", "", r)
    for (match in list(list(book_title == r, posting_role[i]), list(book_role == head, sub(",.*$", "", posting_role[i])))) {
      s <- book$Salary[match[[1]]]
      if (length(s) >= UW_SALARY_BOOK_MIN_COUNT) {
        return(data.frame(Est_Pay = round(stats::median(s)), Matched_Title = match[[2]], Staff_Count = length(s)))
      }
    }
    data.frame(Est_Pay = NA_real_, Matched_Title = NA_character_, Staff_Count = NA_integer_)
  })
  do.call(rbind, out)
}

# Replaces Est_* on University of Wyoming staff postings with the salary
# book median (Est_Source "UW salary book"). UW postings the book can't
# match keep their IPEDS figure, except IPEDS "management", whose UW average
# covers only the top managers; those are left blank. Faculty, hourly and
# student postings get nothing.
attach_uw_salary_book_estimates <- function(postings, book) {
  if (!"Est_Source" %in% names(postings)) {
    postings$Est_Source <- ifelse(is.na(postings$Est_Pay), NA_character_, "IPEDS")
  }
  uw <- which(postings$Institution == "University of Wyoming" & is_salaried_staff_posting(postings$Title))
  if (length(uw) == 0) return(postings)
  est <- uw_salary_book_estimate(postings$Title[uw], book)
  hit <- uw[!is.na(est$Est_Pay)]
  e <- est[!is.na(est$Est_Pay), ]
  postings$Est_Pay[hit] <- e$Est_Pay
  postings$Est_Pay_Unit[hit] <- "year"
  postings$Est_Occupation[hit] <- paste0(e$Matched_Title, " (", e$Staff_Count, " UW staff)")
  postings$Est_Area[hit] <- "University of Wyoming"
  postings$Est_Year[hit] <- book$Fiscal_Year[1]
  postings$Est_Source[hit] <- "UW salary book"
  inflated <- setdiff(uw, hit)
  inflated <- inflated[!is.na(postings$Est_Occupation[inflated]) & postings$Est_Occupation[inflated] == "management"]
  postings[inflated, c("Est_Pay", "Est_Pay_Unit", "Est_Occupation", "Est_Area", "Est_Year", "Est_Source")] <- NA
  postings
}
