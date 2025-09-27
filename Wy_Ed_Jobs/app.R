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

henowsum_he <- read.csv("allnow_he.csv") %>%
  filter(Category != "Uncategorized")

last_refreshed_date <- "September 26, 2025"

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
          max-width: 600px !important;
          white-space: normal !important;
          background-color: white;
          padding: 8px;
          border: 1px solid gray;
          font-size: 14px;
        }
      "))
    ),
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
  
  output$k12_map<- renderLeaflet({
    leaflet(data = mapdata2_k12) %>%
      addTiles() %>%
      addCircleMarkers(
        group = "name", 
        fillOpacity = 0.8, 
        lng = ~Longitude, 
        lat = ~Latitude,
        label = ~lapply(paste0(
          "<strong>Name:</strong> ", Name, "<br/>",
          "<strong>Start Salary:</strong> ", scales::dollar(Start_Salary), "<br/>",
          "<strong>Top Salary:</strong> ", scales::dollar(Top_Salary), "<br/>",
          "<strong>Posted:</strong> ", Salary_Year, "<br/>",
          "<strong>County:</strong> ", County
        ), htmltools::HTML),
        labelOptions = labelOptions(
          direction = "auto"  # You can also add offset or other basic options here
        )
      )
  })
  
  
  # K-12 longitudinal plot
  # K-12 longitudinal plot
  output$k12_longitudinal_plot <- renderPlotly({
    df <- filtered_k12sum()
    
    validate(
      need(nrow(df) > 0, "No data for selected districts.")
    )
    
    p <- ggplot(df, aes(x = Archive_Date, y = sum, color = Broad_Category, 
                        group = Broad_Category, text = paste0(
                          "Date: ", Archive_Date, "<br>",
                          "Category: ", Broad_Category, "<br>",
                          "Postings: ", sum
                        ))) +
      geom_line() +
      geom_point(size = 1) +
      labs(x = "Archive Date", y = "Number of Postings") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom", 
            legend.key.size = unit(0.5, "cm"),
            legend.box.spacing = unit(0.2, "cm"),
            legend.text = element_text(size = 8), 
            legend.title = element_text(size = 10))
    
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
        label = ~lapply(paste0(
          "Name: ", Name, "<br/>",
          "Job Links: <a href='", Link, "' target='_blank'>", Link, "</a>"
        ), htmltools::HTML),
        labelOptions = labelOptions(direction = "auto")
      )
  })
  
  # Reactive filtered dataset (optional)
  filtered_hesum <- reactive({
    req(input$inst_trend)
    hesum_he %>% filter(Institution == input$inst_trend)
  })
  
  output$he_longitudinal_plot <- renderPlotly({
    df <- filtered_hesum()
    
    validate(need(nrow(df) > 0, "No data for selected institution."))
    
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
