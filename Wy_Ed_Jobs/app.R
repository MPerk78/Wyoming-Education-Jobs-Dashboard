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

#--------------------------------------------------
# Load K-12 data
#--------------------------------------------------
combineddata <- read.csv("combinedclean.csv", fileEncoding = "UTF-8") %>%
  select(District, title, position, location, date_posted, url) %>%
  mutate(District = str_squish(as.character(District)))

mapdata2_k12 <- read.csv("salarymap2.csv", fileEncoding = "UTF-8") %>%
  mutate(Start_Salary = as.numeric(gsub("[^0-9.]", "", Start_Salary)),
         Top_Salary   = as.numeric(gsub("[^0-9.]", "", Top_Salary)))

k12sum <- read.csv("allsum.csv", fileEncoding = "UTF-8") %>%
  mutate(District = str_squish(as.character(District)),
         Broad_Category = dplyr::recode(Broad_Category,
                                        "English Language Arts Secondary" = "Engl. LA",
                                        "Secondary Social Studies" = "Soc. St.")) %>%
  filter(Broad_Category != "Other")

k12sum$Archive_Date <- as.Date(k12sum$Archive_Date)


k12nowsum <- read.csv("allnow.csv", fileEncoding = "UTF-8") %>%
  mutate(Broad_Category = dplyr::recode(Broad_Category,
                                        "English Language Arts Secondary" = "Engl. LA",
                                        "Secondary Social Studies" = "Soc. St."),
         District = str_squish(iconv(District, from = "", to = "UTF-8"))) %>%
  filter(Broad_Category != "Other")

#--------------------------------------------------
# Load Higher Ed data
#--------------------------------------------------
ccdata <- read_xlsx("hedata.xlsx") %>%
  select(Institution, Title, Location, Posted_Date, Link) %>%
  arrange(Institution, Title)
ccdata$Link <- paste0('<a href="', ccdata$Link, '" target="_blank">', ccdata$Link, '</a>')

mapdata2_he <- read.csv("salarymap.csv")

hesum_he <- read.csv("allsum_he.csv") %>%
  filter(Category != "Uncategorized")

hesum_he$Archive_Date <- as.Date(hesum_he$Archive_Date)
he_dates <- sort(unique(hesum_he$Archive_Date))
WINDOW_WEEKS <- 52
hesum_he$Category<- as.factor(hesum_he$Category)

henowsum_he <- read.csv("allnow_he.csv") %>%
  filter(Category != "Uncategorized")

last_refreshed_date <- "January 2, 2026"

#--------------------------------------------------
# UI
#--------------------------------------------------
ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = "Wyoming Education Careers"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("home")),
      menuItem("K-12 Careers", tabName = "k12_root", icon = icon("school"),
               menuSubItem("Jobs Table", tabName = "k12_table"),
               menuSubItem("District Locations", tabName = "k12_collmap"),
               menuSubItem("Longitudinal Teacher Trends", tabName = "k12_trends"),
               menuSubItem("Current Teacher Trends", tabName = "k12_current")
      ),
      menuItem("Higher Ed Careers", tabName = "he_root", icon = icon("university"),
               menuSubItem("Jobs Table", tabName = "he_table"),
               menuSubItem("Institution Locations", tabName = "he_collmap"),
               menuSubItem("Longitudinal Faculty Trends", tabName = "he_trends"),
               menuSubItem("Current Faculty Trends", tabName = "he_current")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    .leaflet-tooltip {
      max-width: 800px !important;  /* make tooltip wide */
      min-width: 400px !important;  /* optional: ensures minimum width */
      white-space: normal !important;  /* text can wrap if needed */
      background-color: rgba(255,255,255,0.95);
      padding: 6px 10px;
      border-radius: 6px;
      border: 1px solid gray;
      font-size: 14px;
      display: inline-block;
    }
  "))
    )
    ,
    tabItems(
      # ------------------ Global Introduction ------------------
      tabItem(
        tabName = "intro",
        fluidPage(
          tags$style(HTML("
            .intro-background {
              background-image: url('tree.jpg'); 
              background-size: cover; 
              background-position: center; 
              height: 100vh;
              position: relative;
              color: white;
              padding: 0;
              margin: 0;
            }
            .coed-logo {
              position: absolute;
              top: 50px;  
              right: 20px;
              width: 120px;
              height: auto;
              z-index: 10;
            }
            .refresh-info {
              position: absolute; 
              top: 20px; 
              left: 20px;
              background-color: rgba(255, 255, 255, 0.8); 
              color: red; 
              padding: 5px 10px; 
              border: 1px solid red; 
              border-radius: 5px;
              z-index: 10;
              font-weight: bold;
            }
            .intro-text {
              position: absolute;
              bottom: 15%;
              width: 100%;
              text-align: center;
              font-size: 2.5em;
              color: white;
              z-index: 5;
            }
          ")),
          div(class = "intro-background",
              div(class = "refresh-info", paste("Refreshed on:", last_refreshed_date)),
              img(src = "coed.jpg", class = "coed-logo"),
              div(class = "intro-text",
                  h1("Education Jobs in Wyoming")
              )
          )
        )
      ),
      
      # ------------------ K-12 ------------------
      tabItem(tabName = "k12_table", DTOutput("k12_jobs")),
      tabItem(tabName = "k12_collmap", leafletOutput("k12_map", height = 800)),
      tabItem(tabName = "k12_trends",
              selectInput("district_trend", "Choose district:",
                          choices = sort(unique(k12sum$District)), selected = "Total"),
              sliderInput("k12_scroll", "Scroll timeline:",
                min = min(k12sum$Archive_Date),
                max = max(k12sum$Archive_Date),
                value = c(
                  max(k12sum$Archive_Date) - 365,
                  max(k12sum$Archive_Date)),
                timeFormat = "%Y-%m-%d",
                width = "100%"),
              plotlyOutput("k12_longitudinal_plot")),
      tabItem(tabName = "k12_current",
              selectInput("district_current", "Select District:",
                          choices = sort(unique(k12nowsum$District)), selected = "Total"),
              plotlyOutput("k12_current_plot")),
      
      # ------------------ Higher Ed ------------------
      tabItem(tabName = "he_table", DTOutput("he_jobs")),
      tabItem(tabName = "he_collmap", leafletOutput("he_map", height = 800)),
      tabItem(tabName = "he_trends",
              selectInput("inst_trend", "Select Institution:",
                          choices = sort(unique(hesum_he$Institution)), selected = "Total"),
              textOutput("he_slider_label"),
              sliderInput("he_scroll","Scroll timeline:",
                          min = min(hesum_he$Archive_Date),
                          max = max(hesum_he$Archive_Date),
                          value = c(
                            max(hesum_he$Archive_Date) - 365,  
                            max(hesum_he$Archive_Date)),
                          timeFormat = "%Y-%m-%d",
                          width = "100%"),
              plotlyOutput("he_longitudinal_plot")),
      tabItem(tabName = "he_current",
              selectInput("inst_current", "Select Institution:",
                          choices = sort(unique(henowsum_he$Institution)), selected = "Total"),
              plotlyOutput("he_current_plot"))
    )
  )
)


#--------------------------------------------------
# Server
#--------------------------------------------------
server <- function(input, output, session) {
  
  #------------Filter for longitudinal plot--------
  filtered_k12sum <- reactive({
    req(input$district_trend)
    k12sum %>% filter(District %in% input$district_trend)
  })
  
  
  # -------- K-12 --------
  output$k12_jobs <- renderDT({
    datatable(combineddata, filter = "top", options = list(scrollX = TRUE))
  })
  
  output$k12_map <- renderLeaflet({
    leaflet(data = mapdata2_k12) %>%
      addTiles() %>%
      addCircleMarkers(
        group = "name", 
        fillOpacity = 0.8, 
        lng = ~Longitude, 
        lat = ~Latitude,
        popup = ~lapply(paste0(
          "<div><strong>Name:</strong> ", Name, "</div>",
          "<div><strong>Start Salary:</strong> ", scales::dollar(Start_Salary), "</div>",
          "<div><strong>Top Salary:</strong> ", scales::dollar(Top_Salary), "</div>",
          "<div><strong>Year:</strong> ", Salary_Year, "</div>",
          "<div><strong>County:</strong> ", County, "</div>",
          "<div><strong>Job_Link:</strong> <a href='", Job_Link, "' target='_blank'>", Job_Link, "</a></div>"
        ), htmltools::HTML)
      )
  })
  
  
  
 
# K-12 longitudinal plot
  df_windowed <- reactive({
    df <- filtered_k12sum()
    req(nrow(df) > 0, input$k12_scroll)
    
    df %>%
      filter(
        Archive_Date >= input$k12_scroll[1],
        Archive_Date <= input$k12_scroll[2]
      )
  })
  
  
  output$k12_longitudinal_plot <- renderPlotly({
    df <- df_windowed()
    
    validate(
      need(nrow(df) > 0, "No data for selected districts.")
    )
    
    p <- ggplot(
      df,
      aes(
        x = Archive_Date,
        y = sum,
        color = Broad_Category,
        group = Broad_Category,
        text = paste0(
          "Date: ", Archive_Date, "<br>",
          "Category: ", Broad_Category, "<br>",
          "Postings: ", sum
        )
      )
    ) +
      geom_line() +
      geom_point(size = 1) +
      labs(
        x = "Archive Date",
        y = "Number of Postings"
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
  

  
  output$k12_current_plot <- renderPlotly({
    df <- k12nowsum %>% filter(District == input$district_current)
    
  plot <- ggplot(df, aes(x = Broad_Category, y = Sum, fill = Broad_Category)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Sum), vjust = -0.3) +
    labs(x = "Category", y = "Number of Postings") +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 22.5, hjust = 1, size = 7), 
          legend.position = "bottom", 
          legend.key.size = unit(0.5, "cm"), 
          legend.box.spacing = unit(0.2, "cm"), 
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10))
  
  ggplotly(plot)
})
  # -------- Higher Ed --------
  output$he_jobs <- renderDT({
    datatable(ccdata, escape = FALSE, options = list(scrollX = TRUE))
  })
  
  
  output$he_map <- renderLeaflet({
    leaflet(mapdata2_he) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        fillOpacity = 0.8,
        popup = ~lapply(paste0(
          "<strong>Name:</strong> ", Name, "<br/>",
          "<strong>Job Links:</strong> <a href='", Link, "' target='_blank'>", Link, "</a>"
        ), htmltools::HTML)
      )
  })
  
  
  # Longitudinal Higher Education Data 
  # Reactive filtered dataset by institution
  filtered_hesum <- reactive({
    req(input$inst_trend)
    if (input$inst_trend == "Total") {
      hesum_he
    } else {
      hesum_he %>% filter(Institution == input$inst_trend)
    }
  })
  
  # Number of weeks visible at a time
  WINDOW_WEEKS <- 52
  
  # Update slider based on filtered data
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
      value = c(max(he_dates) - WINDOW_WEEKS*7, max(he_dates)),  # last 52 weeks
      timeFormat = "%Y-%m-%d"
    )
    
    # Store for reactive filtering
    session$userData$he_dates <- he_dates
  })
  
  # Reactive dataset filtered by date window
  he_windowed <- reactive({
    df <- filtered_hesum()
    req(input$he_scroll)
    
    df %>%
      filter(
        Archive_Date >= as.Date(input$he_scroll[1]),
        Archive_Date <= as.Date(input$he_scroll[2])
      ) %>%
      arrange(Archive_Date)
  })
  
  # Optional: show slider range
  output$he_slider_label <- renderText({
    req(input$he_scroll)
    paste0("Showing: ", input$he_scroll[1], " to ", input$he_scroll[2])
  })
  
  
  
  # Render plot
  output$he_longitudinal_plot <- renderPlotly({
    df <- he_windowed()
    validate(need(nrow(df) > 0, "No data for selected institution/date range."))
    
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
  
  
  
  output$he_current_plot <- renderPlotly({
    df <- henowsum_he %>% filter(Institution == input$inst_current)
   
    p <- ggplot(df, aes(x = Category, y = Sum, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Sum), nudge_y = 0.05 * max(df$Sum)) +
      labs(x = "Category", y = "Number of Postings") +
      scale_fill_viridis_d() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 22.5, hjust = 1, size = 7),
            legend.position = "bottom",
            legend.key.size = unit(0.5, "cm"),
            legend.box.spacing = unit(0.2, "cm"),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 10))
    
    ggplotly(p)
  })
}

shinyApp(ui, server)
