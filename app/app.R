# ============================================================
# app.R  (COMPLETE FIXED VERSION)
# Project: Urban Crime Pattern Explorer — Hyderabad/AP Focus
# Phase 6: Shiny Web App — All 6 Tabs
# ============================================================

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(leaflet)
library(randomForest)
library(stringr)
library(scales)
library(tidyr)

# ── LOAD DATA ────────────────────────────────────────────────

ap_crime    <- read.csv("../data/cleaned/ap_crime_clean.csv",    stringsAsFactors = FALSE)
women_clean <- read.csv("../data/cleaned/women_crime_clean.csv", stringsAsFactors = FALSE)
cyber_clean <- read.csv("../data/cleaned/cyber_clean.csv",       stringsAsFactors = FALSE)
rf_model    <- readRDS("../models/random_forest_model.rds")

# ── DISTRICT COORDINATES ─────────────────────────────────────

district_coords <- data.frame(
  DISTRICT = c(
    "HYDERABAD CITY","CYBERABAD","RANGA REDDY",
    "WARANGAL","WARANGAL URBAN","KARIMNAGAR",
    "NALGONDA","NIZAMABAD","ADILABAD","KHAMMAM",
    "MEDAK","MAHABOOBNAGAR","MAHABOOB NAGAR",
    "NELLORE","GUNTUR","KRISHNA",
    "EAST GODAVARI","WEST GODAVARI","VISAKHAPATNAM",
    "SRIKAKULAM","VIZIANAGARAM","CHITTOOR",
    "ANANTAPUR","KURNOOL","CUDDAPAH","PRAKASHAM"
  ),
  LAT = c(
    17.385, 17.440, 17.360, 17.978, 17.978, 18.439,
    17.058, 18.673, 19.667, 17.247, 17.917, 16.733,
    16.733, 14.443, 16.307, 16.519, 17.001, 16.917,
    17.687, 18.295, 18.107, 13.217, 14.682, 15.828,
    14.467, 15.365
  ),
  LON = c(
    78.487, 78.380, 78.400, 79.594, 79.594, 79.129,
    79.268, 78.094, 78.533, 80.151, 78.250, 77.983,
    77.983, 79.987, 80.437, 80.635, 81.804, 81.334,
    83.219, 83.894, 83.421, 79.100, 77.600, 78.037,
    78.824, 79.499
  ),
  stringsAsFactors = FALSE
)

# ── DISTRICT SUMMARY ─────────────────────────────────────────

district_summary <- ap_crime %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ|RLY|RAILWAY")) %>%
  group_by(DISTRICT) %>%
  summarise(
    TOTAL_CRIMES    = sum(TOTAL.IPC.CRIMES,   na.rm = TRUE),
    VIOLENT_CRIMES  = sum(VIOLENT_CRIMES,     na.rm = TRUE),
    PROPERTY_CRIMES = sum(PROPERTY_CRIMES,    na.rm = TRUE),
    WOMEN_CRIMES    = sum(WOMEN_CRIMES,       na.rm = TRUE),
    MURDER          = sum(MURDER,             na.rm = TRUE),
    THEFT           = sum(THEFT,              na.rm = TRUE),
    RAPE            = sum(RAPE,               na.rm = TRUE),
    AVG_PER_YEAR    = round(mean(TOTAL.IPC.CRIMES, na.rm = TRUE)),
    .groups         = "drop"
  ) %>%
  left_join(district_coords, by = "DISTRICT") %>%
  filter(!is.na(LAT))

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  skin = "black",
  
  # ── HEADER ────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$b("🏙️ CrimeScope"),
      tags$span(" Hyderabad", style = "color:#e94560; font-size:14px;")
    ),
    titleWidth = 260
  ),
  
  # ── SIDEBAR ───────────────────────────────────────────────
  dashboardSidebar(
    width = 230,
    tags$style(HTML("
      .main-sidebar { background-color: #1a1a2e !important; }
      .sidebar-menu > li > a { color: #cccccc !important; font-size: 14px; }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li:hover > a {
        background-color: #e94560 !important;
        color: white !important;
        border-left: 4px solid #ff6b6b !important;
      }
      .main-header .logo { background-color: #0f3460 !important; }
      .main-header .navbar { background-color: #0f3460 !important; }
    ")),
    sidebarMenu(
      menuItem("🏠 Overview",       tabName = "overview", icon = icon("home")),
      menuItem("🗺️ Crime Map",      tabName = "map",      icon = icon("map")),
      menuItem("📊 Visualizations", tabName = "viz",      icon = icon("chart-bar")),
      menuItem("🤖 ML Predictor",   tabName = "ml",       icon = icon("robot")),
      menuItem("💻 Cybercrime",     tabName = "cyber",    icon = icon("laptop")),
      menuItem("📋 Data Table",     tabName = "data",     icon = icon("table"))
    ),
    hr(),
    tags$div(style = "padding: 0 15px; color: #aaa; font-size: 12px;",
             tags$b("GLOBAL FILTERS"), br(), br()
    ),
    selectInput("sel_year", "📅 Year:",
                choices = c("All", 2001:2014), selected = "All"),
    selectInput("sel_district", "📍 District:",
                choices = c("All", sort(unique(
                  ap_crime$DISTRICT[
                    !str_detect(ap_crime$DISTRICT, "TOTAL|ZZ|RLY|RAILWAY")
                  ]
                ))),
                selected = "All"),
    br(),
    tags$div(style = "padding:10px 15px; color:#888; font-size:11px;",
             "Data: NCRB Crime in India", br(),
             "Coverage: 2001–2014", br(),
             "Districts: AP + Telangana"
    )
  ),
  
  # ── BODY ──────────────────────────────────────────────────
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.1); }
      .box-header { border-radius:8px 8px 0 0; }
      .small-box { border-radius:8px; }
      .small-box:hover { transform:translateY(-2px); transition:0.2s; }
    "))),
    
    tabItems(
      
      # ════════════════════════════════════════════════════
      # TAB 1: OVERVIEW
      # ════════════════════════════════════════════════════
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("box_total_crimes", width = 3),
                valueBoxOutput("box_districts",    width = 3),
                valueBoxOutput("box_worst",        width = 3),
                valueBoxOutput("box_years",        width = 3)
              ),
              fluidRow(
                box(title = "📈 Crime Trend Over Years", width = 8,
                    status = "danger", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plot_trend", height = 300),
                                color = "#e94560")),
                box(title = "🎯 Risk Level Distribution", width = 4,
                    status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plot_risk_pie", height = 300),
                                color = "#0f3460"))
              ),
              fluidRow(
                box(title = "🏆 Top 10 Most Crime-Prone Districts", width = 6,
                    status = "warning", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plot_top_districts", height = 320),
                                color = "#f5a623")),
                box(title = "📊 Crime Categories by Year", width = 6,
                    status = "success", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plot_categories", height = 320),
                                color = "#27ae60"))
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 2: CRIME MAP
      # ════════════════════════════════════════════════════
      tabItem(tabName = "map",
              fluidRow(
                box(title = "⚙️ Map Controls", width = 12,
                    status = "primary", solidHeader = TRUE,
                    column(4, selectInput("map_type", "Show:",
                                          choices = c(
                                            "Total Crime Intensity" = "TOTAL_CRIMES",
                                            "Violent Crimes"        = "VIOLENT_CRIMES",
                                            "Property Crimes"       = "PROPERTY_CRIMES",
                                            "Women Crimes"          = "WOMEN_CRIMES"
                                          ))),
                    column(8, tags$div(
                      style = "padding-top:25px; color:#666; font-size:13px;",
                      "💡 Click any bubble for district details. Size = crime volume."
                    ))
                )
              ),
              fluidRow(
                box(title = "🗺️ Interactive Crime Map — AP / Telangana",
                    width = 12, status = "danger", solidHeader = TRUE,
                    withSpinner(leafletOutput("crime_map", height = 560),
                                color = "#e94560"))
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 3: VISUALIZATIONS
      # ════════════════════════════════════════════════════
      tabItem(tabName = "viz",
              fluidRow(
                box(title = "📈 Overall Crime Trend", width = 6,
                    status = "danger", solidHeader = TRUE,
                    withSpinner(plotlyOutput("viz_trend", height = 300),
                                color = "#e94560")),
                box(title = "🏙️ Hyderabad vs Rest of AP", width = 6,
                    status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("viz_hyd_vs_ap", height = 300),
                                color = "#0f3460"))
              ),
              fluidRow(
                box(title = "🔥 District × Year Heatmap", width = 12,
                    status = "warning", solidHeader = TRUE,
                    withSpinner(plotlyOutput("viz_heatmap", height = 380),
                                color = "#f5a623"))
              ),
              fluidRow(
                box(title = "👩 Women Safety Trends", width = 6,
                    status = "success", solidHeader = TRUE,
                    withSpinner(plotlyOutput("viz_women", height = 300),
                                color = "#27ae60")),
                box(title = "🔍 Crime Breakdown — Hyderabad Region", width = 6,
                    status = "danger", solidHeader = TRUE,
                    withSpinner(plotlyOutput("viz_breakdown", height = 300),
                                color = "#e94560"))
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 4: ML PREDICTOR (FIXED)
      # ════════════════════════════════════════════════════
      tabItem(tabName = "ml",
              fluidRow(
                box(
                  title = "🤖 Crime Risk Predictor — Random Forest (Tuned, 5-fold CV)",
                  width = 12, status = "danger", solidHeader = TRUE,
                  tags$p(style = "color:#666; margin:0;",
                         "Adjust sliders to a district's annual crime figures.
               The model uses log-scaling internally to handle skewed data.")
                )
              ),
              fluidRow(
                box(
                  title = "⚙️ Input Crime Statistics",
                  width = 5, status = "primary", solidHeader = TRUE,
                  
                  tags$div(
                    style = "background:#fff3cd; padding:8px 12px; border-radius:6px;
                       margin-bottom:12px; font-size:12px; color:#856404;",
                    "💡 LOW < 5,685 crimes/yr  |  MEDIUM 5,685–7,506  |  HIGH > 7,506"
                  ),
                  
                  # Sliders use REAL data ranges from NCRB
                  sliderInput("sl_murder",   "🔪 Murder:",          0, 2808,  91,  step = 10),
                  sliderInput("sl_rape",     "⚠️ Rape:",            0, 1635,  37,  step = 5),
                  sliderInput("sl_theft",    "🏠 Theft:",           0, 31032, 562, step = 200),
                  sliderInput("sl_burglary", "🚪 Burglary:",        0, 9820,  233, step = 50),
                  sliderInput("sl_riots",    "⚡ Riots:",           0, 3001,  60,  step = 10),
                  sliderInput("sl_violent",  "💢 Violent Crimes:",  0, 9359,  295, step = 100),
                  sliderInput("sl_property", "📦 Property Crimes:", 0, 49892, 915, step = 500),
                  sliderInput("sl_women",    "👩 Women Crimes:",    0, 27208, 570, step = 100),
                  sliderInput("sl_year",     "📅 Year:",            2001, 2014, 2010, step = 1),
                  checkboxInput("sl_hyd",    "📍 Hyderabad region?", value = FALSE),
                  
                  br(),
                  fluidRow(
                    column(6, actionButton("btn_predict", "🔮 Predict",
                                           class = "btn btn-danger btn-lg btn-block")),
                    column(6, actionButton("btn_reset",   "🔄 Reset",
                                           class = "btn btn-default btn-lg btn-block"))
                  ),
                  br(),
                  tags$div(
                    tags$b(style = "font-size:13px;", "Quick Presets:"), br(), br(),
                    actionButton("btn_preset_low",    "🟢 Low District",
                                 class = "btn btn-success btn-sm"),
                    actionButton("btn_preset_medium", "🟡 Medium District",
                                 class = "btn btn-warning btn-sm"),
                    actionButton("btn_preset_high",   "🔴 High District",
                                 class = "btn btn-danger btn-sm")
                  )
                ),
                
                box(
                  title = "🎯 Prediction Result",
                  width = 7, status = "warning", solidHeader = TRUE,
                  
                  uiOutput("ml_result"),
                  br(),
                  withSpinner(plotlyOutput("ml_prob_plot", height = 280),
                              color = "#e94560"),
                  br(),
                  tags$div(
                    style = "color:#888; font-size:12px; padding:10px;
                       background:#f8f9fa; border-radius:6px;",
                    "⚡ Algorithm: Random Forest (500 trees, mtry tuned via 5-fold CV)", br(),
                    "📐 Features: log1p-transformed counts + crime ratios + district flags", br(),
                    "🎯 Risk levels based on log-scale tertiles of total IPC crimes"
                  )
                )
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 5: CYBERCRIME
      # ════════════════════════════════════════════════════
      tabItem(tabName = "cyber",
              fluidRow(
                valueBoxOutput("cyber_ap_2018", width = 4),
                valueBoxOutput("cyber_tg_2018", width = 4),
                valueBoxOutput("cyber_ap_rate", width = 4)
              ),
              fluidRow(
                box(title = "💻 Top States by Cybercrime Cases (2018)",
                    width = 7, status = "danger", solidHeader = TRUE,
                    withSpinner(plotlyOutput("cyber_bar", height = 380),
                                color = "#e94560")),
                box(title = "📈 AP + Telangana Trend (2016–2018)",
                    width = 5, status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("cyber_trend", height = 380),
                                color = "#0f3460"))
              ),
              fluidRow(
                box(title = "📊 Cybercrime Rate per Lakh Population",
                    width = 12, status = "warning", solidHeader = TRUE,
                    withSpinner(plotlyOutput("cyber_rate", height = 300),
                                color = "#f5a623"))
              )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 6: DATA TABLE
      # ════════════════════════════════════════════════════
      tabItem(tabName = "data",
              fluidRow(
                box(title = "⚙️ Filters", width = 12,
                    status = "primary", solidHeader = TRUE,
                    column(3, selectInput("tbl_state", "State:",
                                          choices  = c("All", sort(unique(ap_crime$STATE.UT))),
                                          selected = "All")),
                    column(3, selectInput("tbl_year", "Year:",
                                          choices  = c("All", sort(unique(ap_crime$YEAR))),
                                          selected = "All")),
                    column(3, selectInput("tbl_risk", "Risk Level:",
                                          choices  = c("All","HIGH","MEDIUM","LOW"),
                                          selected = "All")),
                    column(3, tags$div(
                      style = "padding-top:25px;",
                      downloadButton("download_data", "⬇️ Download CSV",
                                     class = "btn btn-primary")
                    ))
                )
              ),
              fluidRow(
                box(title = "📋 Full Crime Dataset — AP / Telangana",
                    width = 12, status = "primary", solidHeader = TRUE,
                    withSpinner(DTOutput("crime_table"), color = "#0f3460"))
              )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ── REACTIVE FILTERED DATA ──────────────────────────────
  
  filtered <- reactive({
    df <- ap_crime %>%
      filter(!str_detect(DISTRICT, "TOTAL|ZZ|RLY|RAILWAY"))
    if (input$sel_year     != "All")
      df <- df %>% filter(YEAR     == as.integer(input$sel_year))
    if (input$sel_district != "All")
      df <- df %>% filter(DISTRICT == input$sel_district)
    df
  })
  
  # ── TAB 1: OVERVIEW ─────────────────────────────────────
  
  output$box_total_crimes <- renderValueBox({
    valueBox(
      format(sum(filtered()$TOTAL.IPC.CRIMES, na.rm = TRUE), big.mark = ","),
      "Total IPC Crimes", icon = icon("exclamation-triangle"), color = "red"
    )
  })
  
  output$box_districts <- renderValueBox({
    valueBox(
      length(unique(filtered()$DISTRICT)),
      "Districts", icon = icon("map-marker"), color = "blue"
    )
  })
  
  output$box_worst <- renderValueBox({
    top <- filtered() %>%
      group_by(DISTRICT) %>%
      summarise(T = sum(TOTAL.IPC.CRIMES, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(T)) %>% slice(1)
    valueBox(
      top$DISTRICT[1], "Highest Crime District",
      icon = icon("warning"), color = "yellow"
    )
  })
  
  output$box_years <- renderValueBox({
    valueBox(
      paste(min(filtered()$YEAR), "–", max(filtered()$YEAR)),
      "Years Covered", icon = icon("calendar"), color = "green"
    )
  })
  
  output$plot_trend <- renderPlotly({
    df <- filtered() %>%
      group_by(YEAR) %>%
      summarise(TOTAL = sum(TOTAL.IPC.CRIMES, na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~YEAR, y = ~TOTAL,
            type = "scatter", mode = "lines+markers",
            line   = list(color = "#e94560", width = 3),
            marker = list(color = "#e94560", size  = 8),
            hovertemplate = "Year: %{x}<br>Crimes: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Total Crimes", tickformat = ","),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff") %>%
      config(displayModeBar = FALSE)
  })
  
  output$plot_risk_pie <- renderPlotly({
    df <- filtered() %>% count(RISK_LEVEL)
    plot_ly(df, labels = ~RISK_LEVEL, values = ~n, type = "pie",
            hole   = 0.45,
            marker = list(colors = c("#27ae60","#f5a623","#e94560")),
            textinfo = "label+percent") %>%
      layout(showlegend = TRUE,
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff") %>%
      config(displayModeBar = FALSE)
  })
  
  output$plot_top_districts <- renderPlotly({
    df <- filtered() %>%
      group_by(DISTRICT) %>%
      summarise(AVG = mean(TOTAL.IPC.CRIMES, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(AVG)) %>% slice_head(n = 10)
    plot_ly(df, y = ~reorder(DISTRICT, AVG), x = ~AVG,
            type = "bar", orientation = "h",
            marker = list(color = "#0f3460"),
            text = ~comma(round(AVG)), textposition = "outside",
            hovertemplate = "%{y}<br>Avg: %{x:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Avg Annual Crimes"),
             yaxis = list(title = ""),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff") %>%
      config(displayModeBar = FALSE)
  })
  
  output$plot_categories <- renderPlotly({
    df <- filtered() %>%
      group_by(YEAR) %>%
      summarise(
        Violent  = sum(VIOLENT_CRIMES,  na.rm = TRUE),
        Property = sum(PROPERTY_CRIMES, na.rm = TRUE),
        Women    = sum(WOMEN_CRIMES,    na.rm = TRUE),
        .groups  = "drop"
      )
    plot_ly(df, x = ~YEAR) %>%
      add_bars(y = ~Violent,  name = "Violent",
               marker = list(color = "#e94560")) %>%
      add_bars(y = ~Property, name = "Property",
               marker = list(color = "#0f3460")) %>%
      add_bars(y = ~Women,    name = "Women",
               marker = list(color = "#f5a623")) %>%
      layout(barmode = "stack",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Cases", tickformat = ","),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             legend = list(orientation = "h")) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── TAB 2: MAP ──────────────────────────────────────────
  
  output$crime_map <- renderLeaflet({
    col_var <- input$map_type
    val     <- district_summary[[col_var]]
    pal     <- colorNumeric(
      palette = c("#fff5f0","#fee0d2","#fc9272","#ef3b2c","#67000d"),
      domain  = val
    )
    leaflet(district_summary) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 79.5, lat = 17.0, zoom = 7) %>%
      addCircleMarkers(
        lng         = ~LON, lat = ~LAT,
        radius      = ~sqrt(val / 300) * 8,
        color       = ~pal(val),
        fillColor   = ~pal(val),
        fillOpacity = 0.85,
        weight      = 2,
        popup = ~paste0(
          "<div style='font-family:Arial; min-width:210px'>",
          "<h3 style='color:#e94560; margin:0 0 6px'>", DISTRICT, "</h3>",
          "<hr style='margin:4px 0'>",
          "<b>Total Crimes:</b> ",   format(TOTAL_CRIMES,    big.mark=","), "<br>",
          "<b>Avg/Year:</b> ",       format(AVG_PER_YEAR,    big.mark=","), "<br>",
          "<b>Violent:</b> ",        format(VIOLENT_CRIMES,  big.mark=","), "<br>",
          "<b>Property:</b> ",       format(PROPERTY_CRIMES, big.mark=","), "<br>",
          "<b>Women Crimes:</b> ",   format(WOMEN_CRIMES,    big.mark=","), "<br>",
          "<b>Murder:</b> ",         format(MURDER,          big.mark=","), "<br>",
          "</div>"
        ),
        label = ~paste0(DISTRICT, " — ", format(val, big.mark=",")),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend("bottomright", pal = pal, values = val,
                title   = str_replace_all(col_var, "_", " "),
                opacity = 0.8,
                labFormat = labelFormat(big.mark = ","))
  })
  
  # ── TAB 3: VISUALIZATIONS ───────────────────────────────
  
  output$viz_trend <- renderPlotly({
    df <- ap_crime %>%
      filter(!str_detect(DISTRICT, "TOTAL|ZZ")) %>%
      group_by(YEAR) %>%
      summarise(TOTAL = sum(TOTAL.IPC.CRIMES, na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~YEAR, y = ~TOTAL,
            type = "scatter", mode = "lines+markers",
            line   = list(color = "#e94560", width = 2),
            marker = list(color = "#e94560", size  = 7)) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Total Crimes", tickformat = ","),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff") %>%
      config(displayModeBar = FALSE)
  })
  
  output$viz_hyd_vs_ap <- renderPlotly({
    hyd_d <- c("HYDERABAD CITY","CYBERABAD","RANGA REDDY")
    df <- ap_crime %>%
      filter(!str_detect(DISTRICT, "TOTAL|ZZ")) %>%
      mutate(REGION = ifelse(DISTRICT %in% hyd_d,
                             "Hyderabad", "Rest of AP")) %>%
      group_by(YEAR, REGION) %>%
      summarise(TOTAL = sum(TOTAL.IPC.CRIMES, na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~YEAR, y = ~TOTAL, color = ~REGION,
            type = "scatter", mode = "lines+markers",
            colors = c("Hyderabad" = "#e94560", "Rest of AP" = "#0f3460")) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Crimes", tickformat = ","),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             legend = list(orientation = "h")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$viz_heatmap <- renderPlotly({
    key_d <- c(
      "HYDERABAD CITY","CYBERABAD","RANGA REDDY",
      "WARANGAL","KARIMNAGAR","NALGONDA",
      "NIZAMABAD","ADILABAD","KHAMMAM","MEDAK","MAHABOOBNAGAR"
    )
    df <- ap_crime %>%
      filter(DISTRICT %in% key_d) %>%
      group_by(DISTRICT, YEAR) %>%
      summarise(TOTAL = sum(TOTAL.IPC.CRIMES, na.rm = TRUE), .groups = "drop")
    mat <- pivot_wider(df, names_from = YEAR,
                       values_from = TOTAL, values_fill = 0)
    z   <- as.matrix(mat[, -1])
    plot_ly(x = colnames(z), y = mat$DISTRICT, z = z,
            type = "heatmap",
            colorscale = list(
              c(0,   "#f8f9fa"),
              c(0.5, "#fc9272"),
              c(1,   "#67000d")
            )) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = ""),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff") %>%
      config(displayModeBar = FALSE)
  })
  
  output$viz_women <- renderPlotly({
    df <- women_clean %>%
      filter(!str_detect(DISTRICT, "TOTAL|ZZ")) %>%
      group_by(Year) %>%
      summarise(
        Rape       = sum(Rape,                     na.rm = TRUE),
        Kidnapping = sum(Kidnapping.and.Abduction, na.rm = TRUE),
        Dowry      = sum(Dowry.Deaths,             na.rm = TRUE),
        Cruelty    = sum(Cruelty.by.Husband.or.his.Relatives, na.rm = TRUE),
        .groups    = "drop"
      )
    plot_ly(df, x = ~Year) %>%
      add_lines(y = ~Rape,       name = "Rape",
                line = list(color = "#e94560", width = 2)) %>%
      add_lines(y = ~Kidnapping, name = "Kidnapping",
                line = list(color = "#0f3460", width = 2)) %>%
      add_lines(y = ~Dowry,      name = "Dowry Deaths",
                line = list(color = "#f5a623", width = 2)) %>%
      add_lines(y = ~Cruelty,    name = "Cruelty",
                line = list(color = "#27ae60", width = 2)) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Cases"),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             legend = list(orientation = "h")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$viz_breakdown <- renderPlotly({
    hyd_d <- c("HYDERABAD CITY","CYBERABAD","RANGA REDDY")
    df <- ap_crime %>%
      filter(DISTRICT %in% hyd_d) %>%
      summarise(
        Murder   = sum(MURDER,   na.rm = TRUE),
        Rape     = sum(RAPE,     na.rm = TRUE),
        Theft    = sum(THEFT,    na.rm = TRUE),
        Burglary = sum(BURGLARY, na.rm = TRUE),
        Riots    = sum(RIOTS,    na.rm = TRUE),
        Cheating = sum(CHEATING, na.rm = TRUE),
        Arson    = sum(ARSON,    na.rm = TRUE),
        Robbery  = sum(ROBBERY,  na.rm = TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "Type", values_to = "Count") %>%
      arrange(desc(Count))
    plot_ly(df, y = ~reorder(Type, Count), x = ~Count,
            type = "bar", orientation = "h",
            marker = list(color = "#e94560"),
            text = ~comma(Count), textposition = "outside") %>%
      layout(xaxis = list(title = "Total Cases", tickformat = ","),
             yaxis = list(title = ""),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff") %>%
      config(displayModeBar = FALSE)
  })
  
  # ── TAB 4: ML PREDICTOR (FIXED) ─────────────────────────
  
  # Preset buttons
  observeEvent(input$btn_preset_low, {
    updateSliderInput(session, "sl_murder",   value = 15)
    updateSliderInput(session, "sl_rape",     value = 8)
    updateSliderInput(session, "sl_theft",    value = 250)
    updateSliderInput(session, "sl_burglary", value = 80)
    updateSliderInput(session, "sl_riots",    value = 12)
    updateSliderInput(session, "sl_violent",  value = 80)
    updateSliderInput(session, "sl_property", value = 330)
    updateSliderInput(session, "sl_women",    value = 100)
    updateCheckboxInput(session, "sl_hyd",    value = FALSE)
  })
  
  observeEvent(input$btn_preset_medium, {
    updateSliderInput(session, "sl_murder",   value = 90)
    updateSliderInput(session, "sl_rape",     value = 40)
    updateSliderInput(session, "sl_theft",    value = 900)
    updateSliderInput(session, "sl_burglary", value = 280)
    updateSliderInput(session, "sl_riots",    value = 65)
    updateSliderInput(session, "sl_violent",  value = 380)
    updateSliderInput(session, "sl_property", value = 1200)
    updateSliderInput(session, "sl_women",    value = 480)
    updateCheckboxInput(session, "sl_hyd",    value = FALSE)
  })
  
  observeEvent(input$btn_preset_high, {
    updateSliderInput(session, "sl_murder",   value = 600)
    updateSliderInput(session, "sl_rape",     value = 400)
    updateSliderInput(session, "sl_theft",    value = 15000)
    updateSliderInput(session, "sl_burglary", value = 3000)
    updateSliderInput(session, "sl_riots",    value = 900)
    updateSliderInput(session, "sl_violent",  value = 4000)
    updateSliderInput(session, "sl_property", value = 20000)
    updateSliderInput(session, "sl_women",    value = 5000)
    updateCheckboxInput(session, "sl_hyd",    value = TRUE)
  })
  
  observeEvent(input$btn_reset, {
    updateSliderInput(session, "sl_murder",   value = 91)
    updateSliderInput(session, "sl_rape",     value = 37)
    updateSliderInput(session, "sl_theft",    value = 562)
    updateSliderInput(session, "sl_burglary", value = 233)
    updateSliderInput(session, "sl_riots",    value = 60)
    updateSliderInput(session, "sl_violent",  value = 295)
    updateSliderInput(session, "sl_property", value = 915)
    updateSliderInput(session, "sl_women",    value = 570)
    updateCheckboxInput(session, "sl_hyd",    value = FALSE)
  })
  
  # Prediction reactive
  prediction <- eventReactive(input$btn_predict, {
    total <- input$sl_violent + input$sl_property
    new_data <- data.frame(
      LOG_MURDER     = log1p(input$sl_murder),
      LOG_RAPE       = log1p(input$sl_rape),
      LOG_THEFT      = log1p(input$sl_theft),
      LOG_BURGLARY   = log1p(input$sl_burglary),
      LOG_RIOTS      = log1p(input$sl_riots),
      LOG_VIOLENT    = log1p(input$sl_violent),
      LOG_PROPERTY   = log1p(input$sl_property),
      LOG_WOMEN      = log1p(input$sl_women),
      VIOLENT_RATIO  = ifelse(total > 0, input$sl_violent  / total, 0),
      PROPERTY_RATIO = ifelse(total > 0, input$sl_property / total, 0),
      WOMEN_RATIO    = 0.1,
      IS_HYDERABAD   = as.integer(input$sl_hyd),
      ERA_NUM        = ifelse(input$sl_year <= 2005, 1,
                              ifelse(input$sl_year <= 2010, 2, 3)),
      YEAR           = input$sl_year
    )
    list(
      pred  = predict(rf_model, new_data),
      probs = predict(rf_model, new_data, type = "prob")
    )
  })
  
  output$ml_result <- renderUI({
    req(prediction())
    pred  <- as.character(prediction()$pred)
    color <- switch(pred,
                    "HIGH"   = "#e94560",
                    "MEDIUM" = "#f5a623",
                    "LOW"    = "#27ae60")
    emoji <- switch(pred, "HIGH" = "🔴", "MEDIUM" = "🟡", "LOW" = "🟢")
    tags$div(
      style = paste0(
        "text-align:center; padding:25px; border-radius:12px;",
        "background:", color, "18; border:3px solid ", color, ";"
      ),
      tags$h1(style = paste0("color:", color, "; margin:0; font-size:48px;"),
              paste(emoji, pred)),
      tags$h3(style = paste0("color:", color, "; margin:5px 0 0 0;"),
              "RISK LEVEL"),
      tags$p(style = "color:#666; margin:10px 0 0 0; font-size:13px;",
             "Tuned Random Forest | 5-fold CV | Log-scaled features")
    )
  })
  
  output$ml_prob_plot <- renderPlotly({
    req(prediction())
    probs <- as.numeric(prediction()$probs) * 100
    df <- data.frame(
      Level = c("LOW", "MEDIUM", "HIGH"),
      Prob  = probs,
      Color = c("#27ae60", "#f5a623", "#e94560")
    )
    plot_ly(df, x = ~Level, y = ~Prob, type = "bar",
            marker       = list(color = ~Color),
            text         = ~paste0(round(Prob, 1), "%"),
            textposition = "outside") %>%
      layout(
        title  = "Probability by Risk Class",
        xaxis  = list(title = "Risk Level"),
        yaxis  = list(title = "Probability (%)", range = c(0, 115)),
        plot_bgcolor  = "#ffffff",
        paper_bgcolor = "#ffffff"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── TAB 5: CYBERCRIME ───────────────────────────────────
  
  output$cyber_ap_2018 <- renderValueBox({
    val <- cyber_clean %>%
      filter(STATE == "ANDHRA PRADESH") %>% pull(crimes_2018)
    valueBox(format(val, big.mark = ","), "AP Cybercrimes (2018)",
             icon = icon("laptop"), color = "red")
  })
  
  output$cyber_tg_2018 <- renderValueBox({
    val <- cyber_clean %>%
      filter(STATE == "TELANGANA") %>% pull(crimes_2018)
    valueBox(format(val, big.mark = ","), "Telangana Cybercrimes (2018)",
             icon = icon("shield"), color = "blue")
  })
  
  output$cyber_ap_rate <- renderValueBox({
    val <- cyber_clean %>%
      filter(STATE == "ANDHRA PRADESH") %>% pull(rate_per_lakh)
    valueBox(paste0(val, " /lakh"), "AP Cybercrime Rate",
             icon = icon("percent"), color = "yellow")
  })
  
  output$cyber_bar <- renderPlotly({
    df <- cyber_clean %>%
      filter(!str_detect(STATE, "TOTAL")) %>%
      arrange(desc(crimes_2018)) %>%
      slice_head(n = 12) %>%
      mutate(COLOR = ifelse(STATE %in% c("ANDHRA PRADESH","TELANGANA"),
                            "#e94560", "#aaaaaa"))
    plot_ly(df, y = ~reorder(STATE, crimes_2018), x = ~crimes_2018,
            type = "bar", orientation = "h",
            marker = list(color = ~COLOR),
            text   = ~comma(crimes_2018), textposition = "outside",
            hovertemplate = "%{y}<br>Cases: %{x:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Cybercrime Cases", tickformat = ","),
             yaxis = list(title = ""),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff") %>%
      config(displayModeBar = FALSE)
  })
  
  output$cyber_trend <- renderPlotly({
    df <- cyber_clean %>%
      filter(STATE %in% c("ANDHRA PRADESH","TELANGANA")) %>%
      select(STATE, crimes_2016, crimes_2017, crimes_2018) %>%
      pivot_longer(-STATE, names_to = "Year", values_to = "Cases") %>%
      mutate(Year = as.integer(str_extract(Year, "\\d+")))
    plot_ly(df, x = ~Year, y = ~Cases, color = ~STATE,
            type = "scatter", mode = "lines+markers",
            colors = c("ANDHRA PRADESH" = "#0f3460",
                       "TELANGANA"      = "#e94560"),
            line   = list(width = 3),
            marker = list(size  = 10)) %>%
      layout(xaxis = list(title = "Year", tickvals = c(2016,2017,2018)),
             yaxis = list(title = "Cases"),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
             legend = list(orientation = "h")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$cyber_rate <- renderPlotly({
    df <- cyber_clean %>%
      filter(!str_detect(STATE, "TOTAL")) %>%
      arrange(desc(rate_per_lakh)) %>%
      slice_head(n = 15) %>%
      mutate(COLOR = ifelse(STATE %in% c("ANDHRA PRADESH","TELANGANA"),
                            "#e94560", "#0f3460"))
    plot_ly(df, x = ~reorder(STATE, rate_per_lakh), y = ~rate_per_lakh,
            type = "bar",
            marker = list(color = ~COLOR),
            text   = ~rate_per_lakh, textposition = "outside") %>%
      layout(xaxis = list(title = "", tickangle = -35),
             yaxis = list(title = "Rate per Lakh Population"),
             plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff") %>%
      config(displayModeBar = FALSE)
  })
  
  # ── TAB 6: DATA TABLE ───────────────────────────────────
  
  table_data <- reactive({
    df <- ap_crime %>%
      filter(!str_detect(DISTRICT, "TOTAL|ZZ|RLY|RAILWAY")) %>%
      select(STATE.UT, DISTRICT, YEAR, MURDER, RAPE,
             THEFT, BURGLARY, RIOTS, TOTAL.IPC.CRIMES,
             VIOLENT_CRIMES, PROPERTY_CRIMES, WOMEN_CRIMES, RISK_LEVEL)
    if (input$tbl_state != "All")
      df <- df %>% filter(STATE.UT   == input$tbl_state)
    if (input$tbl_year  != "All")
      df <- df %>% filter(YEAR       == as.integer(input$tbl_year))
    if (input$tbl_risk  != "All")
      df <- df %>% filter(RISK_LEVEL == input$tbl_risk)
    df
  })
  
  output$crime_table <- renderDT({
    datatable(
      table_data(),
      options  = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip"),
      rownames = FALSE,
      filter   = "top"
    ) %>%
      formatStyle("RISK_LEVEL",
                  color           = "white",
                  fontWeight      = "bold",
                  backgroundColor = styleEqual(
                    c("HIGH","MEDIUM","LOW"),
                    c("#e94560","#f5a623","#27ae60")
                  )
      ) %>%
      formatStyle("TOTAL.IPC.CRIMES",
                  background         = styleColorBar(
                    range(ap_crime$TOTAL.IPC.CRIMES), "#0f3460"
                  ),
                  backgroundSize     = "100% 80%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center"
      )
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("ap_crime_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(table_data(), file, row.names = FALSE)
  )
}

# ============================================================
# RUN APP
# ============================================================

shinyApp(ui = ui, server = server)