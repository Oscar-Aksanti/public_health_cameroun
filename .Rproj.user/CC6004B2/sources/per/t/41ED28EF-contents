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

# =========================================================
# PUBLIC HEALTH ANALYTICS DASHBOARD
# Professional Healthcare UI/UX Version
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

health <- health %>%
  mutate(
    consultation_date = as.Date(consultation_date)
  )

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
    title = "Health Analytics"
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
      multiple = FALSE,
      options = list(
        `live-search` = TRUE
      )
    ),
    
    pickerInput(
      inputId = "diagnosis",
      label = "Select Diagnosis",
      choices = c(
        "All",
        sort(unique(health$diagnosis))
      ),
      selected = "All",
      multiple = FALSE,
      options = list(
        `live-search` = TRUE
      )
    )
  ),
  
  # =======================================================
  # BODY
  # =======================================================
  dashboardBody(
    
    tags$head(
      
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"
      ),
      
      tags$style(HTML("
      
      /* =====================================================
         GLOBAL
      ===================================================== */
      
      body {
        font-family: 'Inter', sans-serif;
        background-color: #F5F7FA;
      }

      .content-wrapper, .right-side {
        background-color: #F5F7FA;
      }

      /* =====================================================
         HEADER
      ===================================================== */

      .main-header .logo {
        background-color: #0F4C81 !important;
        color: white !important;
        font-weight: 700;
        font-size: 20px;
        border-bottom: 0px;
      }

      .main-header .navbar {
        background-color: #FFFFFF !important;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
      }

      /* =====================================================
         SIDEBAR
      ===================================================== */

      .main-sidebar {
        background-color: #102A43 !important;
      }

      .sidebar-menu > li > a {
        color: #D9E2EC !important;
        font-size: 15px;
        font-weight: 500;
        padding: 14px 20px;
      }

      .sidebar-menu > li.active > a {
        background-color: #0F4C81 !important;
        border-left: 4px solid #2FBF71;
        color: white !important;
      }

      .sidebar-menu > li:hover > a {
        background-color: #1F3C5A !important;
      }

      /* =====================================================
         BOXES
      ===================================================== */

      .box {
        border-radius: 18px;
        border: none;
        background: white;
        box-shadow: 0 4px 20px rgba(0,0,0,0.06);
      }

      .box-header {
        border-bottom: 1px solid #F0F4F8;
        padding: 18px;
      }

      .box-title {
        font-size: 18px;
        font-weight: 600;
        color: #102A43;
      }

      /* =====================================================
         KPI CARDS
      ===================================================== */

      .small-box {
        border-radius: 18px;
        overflow: hidden;
        box-shadow: 0 6px 18px rgba(0,0,0,0.08);
        transition: all 0.3s ease;
      }

      .small-box:hover {
        transform: translateY(-4px);
      }

      .small-box h3 {
        font-size: 30px !important;
        font-weight: 700 !important;
      }

      .small-box p {
        font-size: 15px;
        font-weight: 500;
      }

      /* =====================================================
         CUSTOM KPI COLORS
      ===================================================== */

      .bg-blue {
        background: linear-gradient(135deg, #0F4C81, #1D70B8) !important;
      }

      .bg-green {
        background: linear-gradient(135deg, #2FBF71, #1B9C5A) !important;
      }

      .bg-yellow {
        background: linear-gradient(135deg, #F4B740, #E59E0B) !important;
      }

      .bg-red {
        background: linear-gradient(135deg, #E85D75, #D64562) !important;
      }

      /* =====================================================
         INPUTS
      ===================================================== */

      .bootstrap-select .dropdown-toggle {
        border-radius: 12px !important;
        border: 1px solid #D9E2EC !important;
        height: 45px;
        font-size: 14px;
      }

      label {
        font-weight: 600;
        color: #334E68;
        margin-top: 10px;
      }

      /* =====================================================
         TABLES
      ===================================================== */

      table.dataTable {
        border-collapse: separate !important;
        border-spacing: 0 8px !important;
      }

      table.dataTable tbody tr {
        background-color: white;
        box-shadow: 0 1px 4px rgba(0,0,0,0.04);
      }

      /* =====================================================
         TEXT
      ===================================================== */

      h2, h3 {
        font-weight: 700;
        color: #102A43;
      }

      "))
    ),
    
    tabItems(
      
      # ===================================================
      # DASHBOARD TAB
      # ===================================================
      tabItem(
        
        tabName = "dashboard",
        
        br(),
        
        fluidRow(
          
          valueBoxOutput("total_cases_box", width = 3),
          valueBoxOutput("regions_box", width = 3),
          valueBoxOutput("diagnosis_box", width = 3),
          valueBoxOutput("latest_month_box", width = 3)
        ),
        
        # -------------------------------------------------
        # FIRST ROW
        # -------------------------------------------------
        fluidRow(
          
          box(
            title = "Consultations by Diagnosis",
            width = 6,
            
            plotlyOutput(
              "diagnosis_plot",
              height = 400
            )
          ),
          
          box(
            title = "Monthly Consultation Trend",
            width = 6,
            
            plotlyOutput(
              "monthly_plot",
              height = 400
            )
          )
        ),
        
        # -------------------------------------------------
        # SECOND ROW
        # -------------------------------------------------
        fluidRow(
          
          box(
            title = "Regional Distribution",
            width = 6,
            
            plotlyOutput(
              "region_plot",
              height = 400
            )
          ),
          
          box(
            title = "Diagnosis Summary",
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
        
        br(),
        
        fluidRow(
          
          box(
            title = "Complete Healthcare Dataset",
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
    
    if (input$region != "All") {
      
      data <- data %>%
        filter(region == input$region)
    }
    
    if (input$diagnosis != "All") {
      
      data <- data %>%
        filter(diagnosis == input$diagnosis)
    }
    
    data
  })
  
  # =======================================================
  # KPI BOXES
  # =======================================================
  
  output$total_cases_box <- renderValueBox({
    
    valueBox(
      value = format(nrow(filtered_data()), big.mark = ","),
      subtitle = "Total Consultations",
      icon = icon("stethoscope"),
      color = "blue"
    )
  })
  
  output$regions_box <- renderValueBox({
    
    valueBox(
      value = n_distinct(filtered_data()$region),
      subtitle = "Regions",
      icon = icon("hospital-user"),
      color = "green"
    )
  })
  
  output$diagnosis_box <- renderValueBox({
    
    valueBox(
      value = n_distinct(filtered_data()$diagnosis),
      subtitle = "Diagnosis Categories",
      icon = icon("heartbeat"),
      color = "yellow"
    )
  })
  
  output$latest_month_box <- renderValueBox({
    
    latest_month <- max(
      filtered_data()$consultation_date,
      na.rm = TRUE
    )
    
    valueBox(
      value = format(latest_month, "%b %Y"),
      subtitle = "Latest Record",
      icon = icon("calendar-check"),
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
        y = total
      )
    ) +
      geom_col(
        fill = "#0F4C81",
        width = 0.7
      ) +
      coord_flip() +
      labs(
        x = "Diagnosis",
        y = "Consultations"
      ) +
      theme_minimal(base_family = "Inter") +
      theme(
        plot.background = element_rect(
          fill = "white",
          color = NA
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(
          size = 12,
          face = "bold"
        ),
        axis.text = element_text(
          size = 11,
          color = "#334E68"
        )
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
        color = "#2FBF71",
        linewidth = 1.5
      ) +
      geom_point(
        color = "#2FBF71",
        size = 3
      ) +
      labs(
        x = "Month",
        y = "Consultations"
      ) +
      theme_minimal(base_family = "Inter") +
      theme(
        plot.background = element_rect(
          fill = "white",
          color = NA
        ),
        panel.grid.minor = element_blank(),
        axis.title = element_text(
          size = 12,
          face = "bold"
        ),
        axis.text = element_text(
          size = 11,
          color = "#334E68"
        )
      )
    
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
        x = reorder(region, total),
        y = total
      )
    ) +
      geom_col(
        fill = "#38BDF8",
        width = 0.7
      ) +
      coord_flip() +
      labs(
        x = "Region",
        y = "Consultations"
      ) +
      theme_minimal(base_family = "Inter") +
      theme(
        plot.background = element_rect(
          fill = "white",
          color = NA
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(
          size = 12,
          face = "bold"
        ),
        axis.text = element_text(
          size = 11,
          color = "#334E68"
        )
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
    scrollX = TRUE,
    autoWidth = TRUE
  ),
  class = "stripe hover"
  )
  
  # =======================================================
  # FULL TABLE
  # =======================================================
  output$full_table <- renderDT({
    
    filtered_data()
    
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    autoWidth = TRUE
  ),
  class = "stripe hover"
  )
}

# ---------------------------------------------------------
# 6. RUN APPLICATION
# ---------------------------------------------------------
shinyApp(
  ui = ui,
  server = server
)