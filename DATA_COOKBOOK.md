# Education Job Postings Data Cookbook

This data cookbook documents the variables and structure of the datasets collected for K–12 and higher education job postings. All datasets are provided in this repository, with longitudinal and snapshot versions included.

---

## K–12 Job Postings

### 1. `combinedclean`
This table contains job postings scraped from K–12 districts.

| Variable        | Type     | Description                                                                 | Notes / Missingness |
|-----------------|---------|-----------------------------------------------------------------------------|-------------------|
| `title`         | text    | Job title as posted on district website                                     | Some titles may be missing or abbreviated |
| `date_posted`   | date    | Date of posting, converted to relative format (Today, Yesterday, weeks ago) | Format attempts  YYYY-MM-DD if site allows it|
| `position`      | text    | Job position type/category as extracted from posting                        | May need further categorization for analysis |
| `location`      | text    | Community, School, or Sometimes department                                  | Some postings may have multiple locations |
| `url`           | text    | Link to the original job posting site                                       | Always present if scraped successfully |
| `District`      | text    | Name of the school district                                                 | Missing if scraper failed for a district |

---

### 2. `allnow`
This table summarizes **K–12 teacher job postings by category** as of the latest scrape date.

| Variable          | Type     | Description                                               | Notes / Missingness |
|------------------|---------|-----------------------------------------------------------|-------------------|
| `Broad_Category`  | text    | Categorized job type (e.g., Math, Science, Humanities)   | Categories are derived from `position` |
| `Sum`             | integer | Count of postings in this category                       | Computed as sum per category |
| `District`        | text    | Name of the school district                               | Missing if no postings |

---

### 3. `allsum`
This table contains **longitudinal K–12 teacher job postings**, showing counts over time.

| Variable          | Type     | Description                                               | Notes / Missingness |
|------------------|---------|-----------------------------------------------------------|-------------------|
| `Broad_Category`  | text    | Categorized job type (e.g., Math, Science, Humanities)   | Derived from `position` |
| `Archive_Date`    | date    | Date the snapshot was taken                                | Format: `YYYY-MM-DD` |
| `District`        | text    | Name of the school district                               | May be missing if district had no postings |
| `sum`             | integer | Count of postings in this category for the given date    | Computed from `combinedclean` |

---

## Higher Education Job Postings

### 1. `hedata`
This table contains all **faculty and staff postings from higher education institutions**, raw and cleaned.

| Variable         | Type     | Description                                                      | Notes / Missingness |
|-----------------|---------|------------------------------------------------------------------|-------------------|
| `Location`       | text    | Community, College, or Sometimes department                   | May include multiple campuses; missing if not provided |
| `Posted_Date`    | text    | Date of posting, converted to relative format (Today, Yesterday, weeks ago) | Derived from original scrape; missing if not scraped |
| `Institution`    | text    | Name of the higher education institution                         | Always present |
| `Link`           | text    | URL to the posting                                               | Always present |
| `Archive_Date`   | date    | Date this dataset was compiled                                    | Format: `YYYY-MM-DD` |

---

### 2. `allnow_he`
Snapshot table of **faculty job postings by category** as of the latest scrape.

| Variable        | Type     | Description                                             | Notes / Missingness |
|----------------|---------|---------------------------------------------------------|-------------------|
| `Category`      | text    | Broad faculty category (e.g., CTE, STEM)                Derived from job title |
| `Sum`           | integer | Count of postings in each category                     | Computed from `hedata` |
| `Institution`   | text    | Institution name                                        | Always present |

---

### 3. `allsum_he`
Longitudinal table of **faculty job postings by category** over time.

| Variable        | Type     | Description                                             | Notes / Missingness |
|----------------|---------|---------------------------------------------------------|-------------------|
| `Category`      | text    | Broad faculty category                                   | Derived from job title |
| `Archive_Date`  | date    | Date snapshot was taken                                   | Format: `YYYY-MM-DD` |
| `Institution`   | text    | Institution name                                        | Always present |
| `sum`           | integer | Count of postings in each category for the given date   | Computed from `hedata` |

---

## Notes

1. **Variable derivation:**  
   - `Broad_Category` and `Category` are derived from job titles using keyword-based classification.  
   - `Posted_Date` is sometimes converted to relative terms (Today, Yesterday, X days/weeks ago).  

2. **Missingness:**  
   - Missing values are indicated as `NA`.  
   - Some positions may lack specific locations or dates due to scraping limitations.  

3. **Reproducibility:**  
   - All datasets are archived with a timestamp in `Archive_Date`.  
   - Raw and cleaned versions are provided where appropriate.  

4. **Repository organization:**  
   - `K12` folder: contains `combinedclean.csv`, `allnow.csv`, `allsum.csv`  
   - `HigherEd` folder: contains `hedata.xlsx`, `allnow_he.csv`, `allsum_he.csv`  

---
