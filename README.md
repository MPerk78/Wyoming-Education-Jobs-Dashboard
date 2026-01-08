
Wyoming Education Jobs Dashboard

Project Description
The Wyoming Education Jobs Dashboard provides a consolidated view of education job opportunities across the state, including both K–12 and higher education positions. The dashboard allows users to explore job postings by school, district, academic year, and degree type, and provides visualizations for trends in hiring.

This tool is designed to support educators, administrators, and analysts who want a comprehensive and up-to-date picture of education job availability in Wyoming.

Features
- View job postings across K–12 and higher education institutions in one place.
- Filter jobs by:
  - School district
  - School or institution
  - Academic year
  - Degree type
- Visualize trends in postings over time.
- Export filtered data for reporting or analysis.

Installation / Setup Instructions
Requirements:
- R (version >= 4.0)
- RStudio (recommended)
- Packages: DT, data.table, flextable, ggplot2, htmltools, leaflet, lubridate,
magrittr, officer, plotly, RColorBrewer, readxl, scales, shiny, shinydashboard, 
shinythemes, shinyWidgets, shinyjs, stringi, stringr, tidyverse, viridis

Installation:
1. Install R and RStudio if not already installed.
2. Install required R packages:
   install.packages(c(
     'DT', 'data.table', 'flextable', 'ggplot2', 'htmltools', 'leaflet',
     'lubridate', 'magrittr', 'officer', 'plotly', 'RColorBrewer', 'readxl',
     'scales', 'shiny', 'shinydashboard', 'shinythemes', 'shinyWidgets',
     'shinyjs', 'stringi', 'stringr', 'tidyverse', 'viridis'
   ))

3. Load the libraries in R:
   library(DT)
   library(data.table)
   library(flextable)
   library(ggplot2)
   library(htmltools)
   library(leaflet)
   library(lubridate)
   library(magrittr)
   library(officer)
   library(plotly)
   library(RColorBrewer)
   library(readxl)
   library(scales)
   library(shiny)
   library(shinydashboard)
   library(shinythemes)
   library(shinyWidgets)
   library(shinyjs)
   library(stringi)
   library(stringr)
   library(tidyverse)
   library(viridis)

4. Open the 'app.R' file in RStudio.
5. Run the dashboard using:
   shiny::runApp('path/to/your/app/folder')

Usage:
- Use the filters to explore job postings by district, school, academic year, or degree type.
- Export filtered data for reporting or analysis by clicking the Export button.

Licensing:
- Code in this repository is licensed under the MIT License.
- Processed datasets and archived CSV files are licensed under the Creative Commons Attribution 4.0 International (CC BY 4.0) license.
- See the LICENSE and DATA_LICENSE.md files for full terms.

Contact:
For questions or support, contact [Mark Perkins / mperki17@uwyo.edu].



