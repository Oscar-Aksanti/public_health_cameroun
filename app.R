install.packages(c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "tidyverse",
  "readr",
  "lubridate",
  "plotly",
  "DT"
))

# =========================================================
# PUBLIC HEALTH ANALYTICS DASHBOARD
# app.R
# =========================================================

# ---------------------------------------------------------
# 1. PACKAGES
# ---------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(tidyverse)
library(readr)
library(lubridate)

library(plotly)
library(DT)

# ---------------------------------------------------------
# 2. LOAD DATA
# ---------------------------------------------------------
health <- read_csv(
  "outputs/health_clean_final.csv"
)

# ---------------------------------------------------------
# 3. DATA PREPARATION
# ---------------------------------------------------------

# Convert consultation date
health <- health %>%
  mutate(
    consultation_date = as.Date(consultation_date)
  )

# Create month variable
health <- health %>%
  mutate(
    month = floor_date(
      consultation_date,
      unit = "month"
    )
  )

# ---------------------------------------------------------
# 4. UI
# ---------------------------------------------------------
ui <- dashboardPage(
  
  # =======================================================
  # HEADER
  # =======================================================
  dashboardHeader(
    title = "Public Health Analytics"
  ),
  
  # =======================================================
  # SIDEBAR
  # =======================================================
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Data Table",
        tabName = "table",
        icon = icon("table")
      )
    ),
    
    br(),
    
    # -----------------------------------------------------
    # FILTERS
    # -----------------------------------------------------
    
    pickerInput(
      inputId = "region",
      label = "Select Region",
      choices = c(
        "All",
        sort(unique(health$region))
      ),
      selected = "All",
      multiple = FALSE
    ),
    
    pickerInput(
      inputId = "diagnosis",
      label = "Select Diagnosis",
      choices = c(
        "All",
        sort(unique(health$diagnosis))
      ),
      selected = "All",
      multiple = FALSE
    )
  ),
  
  # =======================================================
  # BODY
  # =======================================================
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }

        .small-box {
          border-radius: 10px;
        }

        .box {
          border-radius: 10px;
        }

        h2, h3 {
          font-weight: 600;
        }

      "))
    ),
    
    tabItems(
      
      # ===================================================
      # DASHBOARD TAB
      # ===================================================
      tabItem(
        
        tabName = "dashboard",
        
        fluidRow(
          
          # KPI 1
          valueBoxOutput("total_cases_box", width = 3),
          
          # KPI 2
          valueBoxOutput("regions_box", width = 3),
          
          # KPI 3
          valueBoxOutput("diagnosis_box", width = 3),
          
          # KPI 4
          valueBoxOutput("latest_month_box", width = 3)
        ),
        
        # -------------------------------------------------
        # FIRST ROW
        # -------------------------------------------------
        fluidRow(
          
          box(
            title = "Consultations by Diagnosis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput("diagnosis_plot", height = 400)
          ),
          
          box(
            title = "Monthly Consultation Trend",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput("monthly_plot", height = 400)
          )
        ),
        
        # -------------------------------------------------
        # SECOND ROW
        # -------------------------------------------------
        fluidRow(
          
          box(
            title = "Regional Distribution",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput("region_plot", height = 400)
          ),
          
          box(
            title = "Diagnosis Summary Table",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            DTOutput("summary_table")
          )
        )
      ),
      
      # ===================================================
      # DATA TABLE TAB
      # ===================================================
      tabItem(
        
        tabName = "table",
        
        fluidRow(
          
          box(
            title = "Complete Dataset",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("full_table")
          )
        )
      )
    )
  )
)

# ---------------------------------------------------------
# 5. SERVER
# ---------------------------------------------------------
server <- function(input, output) {
  
  # -------------------------------------------------------
  # FILTERED DATA
  # -------------------------------------------------------
  filtered_data <- reactive({
    
    data <- health
    
    # Region filter
    if (input$region != "All") {
      
      data <- data %>%
        filter(region == input$region)
    }
    
    # Diagnosis filter
    if (input$diagnosis != "All") {
      
      data <- data %>%
        filter(diagnosis == input$diagnosis)
    }
    
    data
  })
  
  # =======================================================
  # KPI BOXES
  # =======================================================
  
  # Total consultations
  output$total_cases_box <- renderValueBox({
    
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Consultations",
      icon = icon("hospital"),
      color = "blue"
    )
  })
  
  # Total regions
  output$regions_box <- renderValueBox({
    
    valueBox(
      value = n_distinct(filtered_data()$region),
      subtitle = "Regions",
      icon = icon("map"),
      color = "green"
    )
  })
  
  # Total diagnosis categories
  output$diagnosis_box <- renderValueBox({
    
    valueBox(
      value = n_distinct(filtered_data()$diagnosis),
      subtitle = "Diagnosis Categories",
      icon = icon("notes-medical"),
      color = "yellow"
    )
  })
  
  # Latest month
  output$latest_month_box <- renderValueBox({
    
    latest_month <- max(
      filtered_data()$consultation_date,
      na.rm = TRUE
    )
    
    valueBox(
      value = format(latest_month, "%b %Y"),
      subtitle = "Latest Record",
      icon = icon("calendar"),
      color = "red"
    )
  })
  
  # =======================================================
  # DIAGNOSIS PLOT
  # =======================================================
  output$diagnosis_plot <- renderPlotly({
    
    plot_data <- filtered_data() %>%
      group_by(diagnosis) %>%
      summarise(
        total = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(total))
    
    p <- ggplot(
      plot_data,
      aes(
        x = reorder(diagnosis, total),
        y = total,
        fill = diagnosis
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Diagnosis",
        y = "Consultations"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none"
      )
    
    ggplotly(p)
  })
  
  # =======================================================
  # MONTHLY TREND
  # =======================================================
  output$monthly_plot <- renderPlotly({
    
    monthly_data <- filtered_data() %>%
      group_by(month) %>%
      summarise(
        total = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      monthly_data,
      aes(
        x = month,
        y = total
      )
    ) +
      geom_line(
        color = "#2c7fb8",
        linewidth = 1.2
      ) +
      geom_point(
        color = "#2c7fb8",
        size = 3
      ) +
      labs(
        x = "Month",
        y = "Consultations"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # =======================================================
  # REGION PLOT
  # =======================================================
  output$region_plot <- renderPlotly({
    
    region_data <- filtered_data() %>%
      group_by(region) %>%
      summarise(
        total = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      region_data,
      aes(
        x = region,
        y = total,
        fill = region
      )
    ) +
      geom_col() +
      labs(
        x = "Region",
        y = "Consultations"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none"
      )
    
    ggplotly(p)
  })
  
  # =======================================================
  # SUMMARY TABLE
  # =======================================================
  output$summary_table <- renderDT({
    
    filtered_data() %>%
      group_by(diagnosis) %>%
      summarise(
        consultations = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(consultations))
    
  },
  options = list(
    pageLength = 8,
    scrollX = TRUE
  )
  )
  
  # =======================================================
  # FULL DATA TABLE
  # =======================================================
  output$full_table <- renderDT({
    
    filtered_data()
    
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE
  )
  )
}

# ---------------------------------------------------------
# 6. RUN APPLICATION
# ---------------------------------------------------------
shinyApp(
  ui = ui,
  server = server
)


