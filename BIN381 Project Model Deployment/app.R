# ==============================================================================
# South African Water Access Analysis Dashboard
# BIN381 Project Milestone 6 - Group H
# 
# Purpose:
#   Interactive Shiny dashboard for exploring provincial water access patterns
#   (1998-2023) using Linear Regression and C5.0 classification models.
#
# Data Source:
#   General Household Survey (Stats SA), aggregated by province and water source
#
# Models:
#   - Linear Regression: Predicts continuous AccessRatio (piped / total sources)
#   - C5.0 Decision Tree: Classifies access level (High/Low) using boosting
#
# Author: BIN381 Group H
# Date: October 2025
# ==============================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(readr)
library(DT)

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Load RDS file safely; returns NULL if missing or error
safe_readRDS <- function(path) {
  if (file.exists(path)) {
    tryCatch(
      readRDS(path),
      error = function(e) {
        warning("Error reading ", path, ": ", e$message)
        NULL
      }
    )
  } else {
    NULL
  }
}

# Load GeoJSON safely; returns NULL if missing or error
safe_st_read <- function(path) {
  if (file.exists(path)) {
    tryCatch(
      sf::st_read(path, quiet = TRUE),
      error = function(e) {
        warning("Error reading GeoJSON: ", e$message)
        NULL
      }
    )
  } else {
    NULL
  }
}

# Standardise province names across data sources
recode_prov <- function(x) {
  x <- trimws(x)
  x <- ifelse(x == "Kwazulu-Natal", "KwaZulu-Natal", x)
  x
}

# Rebuild processing dataset from CSV if RDS unavailable
rebuild_from_csv <- function(raw_data) {
  prov <- raw_data %>%
    filter(
      Province != "National (Average)",
      grepl("^Households", Indicator)
    )
  
  years <- sort(unique(prov$SurveyYear))
  provs <- sort(unique(prov$Province))
  
  # Aggregate by year and province
  working_data <- dplyr::bind_rows(lapply(years, function(y) {
    dplyr::bind_rows(lapply(provs, function(pv) {
      subset_data <- prov %>%
        filter(SurveyYear == y, Province == pv)
      
      piped <- mean(
        subset_data$Value[grepl("piped into dwelling", subset_data$Indicator)],
        na.rm = TRUE
      )
      improved <- mean(
        subset_data$Value[grepl("improved water source", subset_data$Indicator)],
        na.rm = TRUE
      )
      surface <- mean(
        subset_data$Value[grepl("surface water", subset_data$Indicator)],
        na.rm = TRUE
      )
      other <- mean(
        subset_data$Value[grepl("Other/Unspecified", subset_data$Indicator)],
        na.rm = TRUE
      )
      
      if (all(is.na(c(piped, improved, surface, other)))) {
        return(NULL)
      }
      
      data.frame(
        SurveyYear = y,
        Province = pv,
        PipedWater = ifelse(is.na(piped), 0, piped),
        ImprovedSource = ifelse(is.na(improved), 0, improved),
        SurfaceWater = ifelse(is.na(surface), 0, surface),
        OtherUnspecified = ifelse(is.na(other), 0, other)
      )
    }))
  }))
  
  # Compute AccessRatio and classification
  working_data <- working_data %>%
    filter(PipedWater + ImprovedSource + SurfaceWater + OtherUnspecified > 0) %>%
    mutate(
      AccessScore = PipedWater,
      AccessRatio = PipedWater / 
        (PipedWater + ImprovedSource + SurfaceWater + OtherUnspecified),
      AccessLevel = factor(
        ifelse(AccessRatio > median(AccessRatio), "High", "Low"),
        levels = c("Low", "High")
      )
    )
  
  return(working_data)
}

# ==============================================================================
# DATA LOADING
# ==============================================================================

lm_model      <- safe_readRDS("water_access_lrm.rds")
c50_model     <- safe_readRDS("water_access_c50.rds")
proc_data     <- safe_readRDS("water_access_processed_data.rds")
median_ratio  <- safe_readRDS("water_access_median_ratio.rds")

# Fallback to CSV if RDS missing
if (is.null(proc_data) && file.exists("southafrica_water_access_v2.csv")) {
  raw_data <- read_csv("southafrica_water_access_v2.csv", show_col_types = FALSE) %>%
    mutate(
      SurveyYear = as.numeric(SurveyYear),
      Value = as.numeric(Value),
      Province = trimws(Province)
    ) %>%
    tidyr::drop_na()
  
  proc_data <- rebuild_from_csv(raw_data)
  
  if (is.null(median_ratio)) {
    median_ratio <- median(proc_data$AccessRatio)
  }
}

# Load geographic data (optional)
za <- safe_st_read("za.json")

# Standardise province names
if (!is.null(proc_data)) {
  proc_data$Province <- recode_prov(proc_data$Province)
}

# UI choices
province_choices <- if (!is.null(proc_data)) sort(unique(proc_data$Province)) else character(0)
year_choices     <- if (!is.null(proc_data)) sort(unique(proc_data$SurveyYear)) else numeric(0)

# ==============================================================================
# VISUAL THEME
# ==============================================================================

sa_theme <- list(
  primary = "#003A70",
  accent1 = "#FFB81C",
  accent2 = "#007A5E",
  dark = "#1a1a1a",
  light = "#f5f5f5",
  neutral = "#6b7280"
)

# ==============================================================================
# UI DEFINITION
# ==============================================================================

header <- dashboardHeader(
  title = div(
    style = "display: flex; align-items: center; gap: 12px;",
    span("SA Water Access", style = "font-weight: 700; font-size: 18px;"),
    span("Group H | BIN381", style = "font-size: 12px; opacity: 0.8;")
  ),
  titleWidth = "100%"
)

sidebar <- dashboardSidebar(
  width = 240,
  sidebarMenu(
    id = "tabs",
    menuItem("Dashboard", tabName = "about", icon = icon("home")),
    menuItem("Explore", tabName = "explore", icon = icon("chart-bar")),
    menuItem("Map", tabName = "map", icon = icon("map")),
    menuItem("Forecast", tabName = "forecast", icon = icon("chart-line")),
    menuItem("Predict", tabName = "predict", icon = icon("magic")),
    menuItem("Evaluation", tabName = "eval", icon = icon("microscope"))
  ),
  div(
    style = "padding: 15px; font-size: 11px; color: #666; line-height: 1.6;",
    "Data: GHS 1998–2023",
    br(),
    "Models: LRM + C5.0",
    br(),
    "Threshold: Median Ratio"
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600;700&display=swap"),
    tags$style(HTML(sprintf("
      * { font-family: 'Poppins', system-ui, sans-serif; }
      body { background: %s; }
      .content-wrapper { background: %s; }
      .main-header .navbar { background: %s; }
      .main-sidebar { background: %s; }
      .sidebar-menu > li.active > a { background: %s; border-left: 4px solid %s; }
      .box { border-top: 3px solid %s; }
      .btn-primary { background: %s; border: none; }
      .btn-primary:hover { background: %s; }
      .stat-box { background: #fff; border-radius: 10px; padding: 15px; border-left: 4px solid %s; }
      .stat-value { font-size: 24px; font-weight: 700; color: %s; }
      .stat-label { font-size: 12px; color: %s; margin-top: 5px; }
      .note { color: %s; font-size: 12px; }
    ", sa_theme$light, sa_theme$light, sa_theme$primary, sa_theme$primary,
                            sa_theme$accent1, sa_theme$accent1, sa_theme$primary, sa_theme$primary,
                            sa_theme$accent2, sa_theme$accent2, sa_theme$primary, sa_theme$neutral, sa_theme$neutral)))
  ),
  
  tabItems(
    
    # TAB 1: DASHBOARD
    tabItem(tabName = "about",
            fluidRow(
              box(width = 12, status = "primary", solidHeader = TRUE,
                  title = "Water Access Analysis",
                  p("Analysis of household water access patterns across South African provinces (1998-2023), ",
                    "aligned with SDG 6 (Clean Water and Sanitation)."),
                  p(strong("Models:"), "Linear Regression (continuous) + C5.0 Decision Tree (classification)"),
                  p(strong("Data:"), "General Household Survey (Stats SA), provincial aggregates")
              )
            ),
            fluidRow(
              box(width = 3, status = "success", solidHeader = TRUE, title = "Observations",
                  div(class = "stat-box",
                      div(class = "stat-value", if (!is.null(proc_data)) nrow(proc_data) else 0),
                      div(class = "stat-label", "Province-Year Records")
                  )
              ),
              box(width = 3, status = "info", solidHeader = TRUE, title = "Provinces",
                  div(class = "stat-box",
                      div(class = "stat-value", length(province_choices)),
                      div(class = "stat-label", "SA Provinces")
                  )
              ),
              box(width = 3, status = "warning", solidHeader = TRUE, title = "Years",
                  div(class = "stat-box",
                      div(class = "stat-value", length(year_choices)),
                      div(class = "stat-label", "Survey Years")
                  )
              ),
              box(width = 3, status = "danger", solidHeader = TRUE, title = "Threshold",
                  div(class = "stat-box",
                      div(class = "stat-value", if (!is.null(median_ratio)) round(as.numeric(median_ratio), 2) else "N/A"),
                      div(class = "stat-label", "Median Ratio")
                  )
              )
            ),
            fluidRow(
              box(width = 12, title = "Team",
                  p("Tokelo Ramogase • Kaidy Edwards • Tumiso Lethabo Koee • Paballo Keneilwe Mokadi"),
                  p(class = "note", "BIN381 Project Milestone 6")
              )
            )
    ),
    
    # TAB 2: EXPLORE
    tabItem(tabName = "explore",
            fluidRow(
              box(width = 12, title = "Data Explorer", status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(3, selectInput("exp_year", "Year", choices = year_choices, selected = max(year_choices))),
                    column(3, selectInput("exp_metric", "Metric", 
                                          choices = c("AccessRatio", "PipedWater", "ImprovedSource", "SurfaceWater", "OtherUnspecified"), 
                                          selected = "AccessRatio")),
                    column(3, selectInput("exp_prov", "Province (for trend)", choices = province_choices, selected = province_choices[1])),
                    column(3, p(class = "note", "Visualise provincial patterns across years or by year."))
                  ),
                  plotOutput("exp_plot", height = 420)
              )
            ),
            fluidRow(
              box(width = 6, title = "Temporal Trend (Selected Province)",
                  plotOutput("exp_trend", height = 380)
              ),
              box(width = 6, title = "Summary Statistics",
                  uiOutput("exp_summary_stats")
              )
            ),
            fluidRow(
              box(width = 12, title = "Full Dataset",
                  DT::dataTableOutput("exp_table"))
            )
    ),
    
    # TAB 3: MAP
    tabItem(tabName = "map",
            fluidRow(
              box(width = 3, title = "Map Controls", status = "primary", solidHeader = TRUE,
                  selectInput("map_year", "Year", choices = year_choices, selected = max(year_choices)),
                  selectInput("map_metric", "Metric", 
                              choices = c("AccessRatio", "PipedWater", "ImprovedSource", "SurfaceWater"), 
                              selected = "AccessRatio"),
                  helpText("Hover provinces for values.")
              ),
              box(width = 9, title = "Provincial Choropleth", status = "primary", solidHeader = TRUE,
                  leafletOutput("map_view", height = 550))
            )
    ),
    
    # TAB 4: FORECAST
    tabItem(tabName = "forecast",
            fluidRow(
              box(width = 12, title = "Water Access Forecast", status = "primary", solidHeader = TRUE,
                  p("Linear extrapolation of AccessRatio trends to forecast future provincial water access patterns."),
                  fluidRow(
                    column(4, selectInput("fc_prov", "Province", choices = province_choices)),
                    column(4, sliderInput("fc_years", "Forecast Years Ahead", min = 1, max = 10, value = 5)),
                    column(4, actionButton("fc_btn", "Generate Forecast", class = "btn-primary", 
                                           style = "margin-top: 25px; width: 100%;"))
                  ),
                  plotOutput("fc_plot", height = 450)
              )
            ),
            fluidRow(
              box(width = 6, title = "Forecast Table",
                  DT::dataTableOutput("fc_table")
              ),
              box(width = 6, title = "Interpretation",
                  uiOutput("fc_notes")
              )
            )
    ),
    tabItem(tabName = "predict",
            fluidRow(
              box(width = 5, title = "Scenario Inputs", status = "primary", solidHeader = TRUE,
                  selectInput("pr_prov", "Province", choices = province_choices),
                  sliderInput("pr_year", "Year", min = min(year_choices), max = max(year_choices), 
                              value = max(year_choices), step = 1, sep = ""),
                  numericInput("pr_piped", "Piped Water (k HH)", value = 500, min = 0),
                  numericInput("pr_impr", "Improved Source (k HH)", value = 600, min = 0),
                  numericInput("pr_surface", "Surface Water (k HH)", value = 200, min = 0),
                  numericInput("pr_other", "Other/Unspecified (k HH)", value = 50, min = 0),
                  actionButton("predict_btn", "Run Prediction", class = "btn-primary", 
                               style = "width: 100%; margin-top: 15px;")
              ),
              box(width = 7, title = "Model Output", status = "primary", solidHeader = TRUE,
                  uiOutput("pred_summary"),
                  br(),
                  p(class = "note", "LRM predicts continuous ratio. Classification uses median threshold.")
              )
            )
    ),
    
    # TAB 5: EVALUATION
    tabItem(tabName = "eval",
            fluidRow(
              box(width = 6, title = "Linear Regression", status = "primary", solidHeader = TRUE,
                  uiOutput("eval_lm"),
                  br(),
                  plotOutput("eval_diag", height = 300)
              ),
              box(width = 6, title = "C5.0 Classification", status = "primary", solidHeader = TRUE,
                  uiOutput("eval_c50"),
                  br(),
                  p(class = "note", "For 5-fold CV metrics, see model training script output.")
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # Guardrails
  observe({
    if (is.null(proc_data)) 
      showNotification("Processed data not found.", type = "error", duration = 6)
    if (is.null(lm_model)) 
      showNotification("LRM model missing.", type = "warning", duration = 6)
    if (is.null(c50_model)) 
      showNotification("C5.0 model missing.", type = "warning", duration = 6)
    if (is.null(za)) 
      showNotification("GeoJSON missing. Map will be empty.", type = "warning", duration = 6)
  })
  
  # EXPLORE TAB
  output$exp_plot <- renderPlot({
    req(proc_data)
    df <- proc_data %>% filter(SurveyYear == input$exp_year)
    
    ggplot(df, aes(x = reorder(Province, .data[[input$exp_metric]]), 
                   y = .data[[input$exp_metric]])) +
      geom_col(fill = sa_theme$primary, alpha = 0.85) +
      geom_col(aes(fill = .data[[input$exp_metric]]), show.legend = FALSE) +
      scale_fill_gradient(low = sa_theme$accent1, high = sa_theme$primary) +
      coord_flip() +
      labs(x = NULL, y = input$exp_metric, 
           title = paste("Provincial", input$exp_metric, "—", input$exp_year)) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", color = sa_theme$primary, size = 14),
        panel.grid.major.x = element_line(color = "#e0e0e0", size = 0.3)
      )
  })
  
  output$exp_table <- DT::renderDataTable({
    req(proc_data)
    proc_data %>% arrange(desc(SurveyYear), Province)
  }, options = list(pageLength = 12, scrollX = TRUE))
  
  # EXPLORE: Trend plot for selected province
  output$exp_trend <- renderPlot({
    req(proc_data)
    
    trend_df <- proc_data %>%
      filter(Province == input$exp_prov) %>%
      arrange(SurveyYear)
    
    ggplot(trend_df, aes(x = SurveyYear, y = AccessRatio, group = 1)) +
      geom_line(color = sa_theme$primary, size = 1.2) +
      geom_point(color = sa_theme$accent1, size = 3) +
      labs(x = "Year", y = "Access Ratio", 
           title = paste("Trend:", input$exp_prov)) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", color = sa_theme$primary),
        panel.grid.major = element_line(color = "#e0e0e0", size = 0.3)
      )
  })
  
  # EXPLORE: Summary statistics
  output$exp_summary_stats <- renderUI({
    req(proc_data)
    
    latest_data <- proc_data %>%
      filter(SurveyYear == max(SurveyYear))
    
    prov_data <- latest_data %>%
      filter(Province == input$exp_prov)
    
    if (nrow(prov_data) == 0) {
      return(p(class = "note", "No data for selected province."))
    }
    
    tagList(
      h5("Latest Year Statistics", style = sprintf("color: %s; font-weight: 700;", sa_theme$primary)),
      p(HTML(paste0(
        "<b>Province:</b> ", prov_data$Province[1], "<br>",
        "<b>Survey Year:</b> ", prov_data$SurveyYear[1], "<br>",
        "<b>Access Ratio:</b> ", round(prov_data$AccessRatio[1], 4), "<br>",
        "<b>Piped Water (k HH):</b> ", round(prov_data$PipedWater[1], 1), "<br>",
        "<b>Improved Source (k HH):</b> ", round(prov_data$ImprovedSource[1], 1), "<br>",
        "<b>Surface Water (k HH):</b> ", round(prov_data$SurfaceWater[1], 1), "<br>",
        "<b>Access Level:</b> <span style='color: ", 
        ifelse(prov_data$AccessLevel[1] == "High", sa_theme$accent2, sa_theme$accent1),
        "; font-weight: 700;'>", prov_data$AccessLevel[1], "</span>"
      ))),
      br(),
      h5("Trend Analysis", style = sprintf("color: %s; font-weight: 700;", sa_theme$primary)),
      uiOutput("trend_direction")
    )
  })
  
  # EXPLORE: Trend direction
  output$trend_direction <- renderUI({
    req(proc_data)
    
    trend_df <- proc_data %>%
      filter(Province == input$exp_prov) %>%
      arrange(SurveyYear)
    
    if (nrow(trend_df) < 2) {
      return(p(class = "note", "Insufficient data for trend analysis."))
    }
    
    first_ratio <- trend_df$AccessRatio[1]
    last_ratio <- trend_df$AccessRatio[nrow(trend_df)]
    change <- last_ratio - first_ratio
    pct_change <- (change / first_ratio) * 100
    
    direction <- ifelse(change > 0, "⬆ Improving", "⬇ Declining")
    
    p(HTML(paste0(
      "From ", round(first_ratio, 3), " (", trend_df$SurveyYear[1], ") to ",
      round(last_ratio, 3), " (", trend_df$SurveyYear[nrow(trend_df)], ")<br>",
      "<b>Change:</b> ", ifelse(change > 0, "+", ""), round(change, 3), 
      " (", round(pct_change, 1), "%) ", direction
    )))
  })
  
  # MAP TAB
  output$map_view <- renderLeaflet({
    req(za, proc_data)
    
    d <- proc_data %>%
      filter(SurveyYear == input$map_year) %>%
      group_by(Province) %>%
      summarise(
        AccessRatio = mean(AccessRatio, na.rm = TRUE),
        PipedWater = mean(PipedWater, na.rm = TRUE),
        ImprovedSource = mean(ImprovedSource, na.rm = TRUE),
        SurfaceWater = mean(SurfaceWater, na.rm = TRUE),
        .groups = "drop"
      )
    
    za$name <- recode_prov(as.character(za$name))
    map_df <- za %>% left_join(d, by = c("name" = "Province"))
    
    # Get metric values for colour palette
    metric_col <- map_df[[input$map_metric]]
    
    pal <- colorNumeric(
      palette = c(sa_theme$accent1, sa_theme$accent2), 
      domain = metric_col, 
      na.color = "#cccccc"
    )
    
    # Create label text
    label_text <- paste0(
      "<b>", map_df$name, "</b><br>", 
      input$map_metric, ": ",
      ifelse(is.na(metric_col), "N/A", round(metric_col, 3))
    )
    
    leaflet(map_df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(metric_col),
        color = sa_theme$primary, weight = 2, opacity = 0.9, fillOpacity = 0.75,
        label = lapply(label_text, htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "6px 8px", "background-color" = "rgba(255,255,255,0.95)"),
          textsize = "13px", direction = "auto"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = metric_col,
                title = input$map_metric, opacity = 0.85, labFormat = labelFormat(digits = 3))
  })
  
  # PREDICT TAB
  predictions <- reactiveValues(
    input_data = NULL,
    lm_pred = NA,
    lm_level = NA,
    c50_pred = NA,
    ratio_in = NA
  )
  
  observeEvent(input$predict_btn, {
    req(!is.null(proc_data))
    
    total <- input$pr_piped + input$pr_impr + input$pr_surface + input$pr_other
    ratio_in <- ifelse(total > 0, input$pr_piped / total, NA_real_)
    
    new_row <- data.frame(
      SurveyYear = as.numeric(input$pr_year),
      Province = recode_prov(input$pr_prov),
      PipedWater = as.numeric(input$pr_piped),
      ImprovedSource = as.numeric(input$pr_impr),
      SurfaceWater = as.numeric(input$pr_surface),
      OtherUnspecified = as.numeric(input$pr_other),
      stringsAsFactors = FALSE
    )
    
    predictions$input_data <- new_row
    predictions$ratio_in <- ratio_in
    
    # LRM prediction
    lm_pred <- NA_real_
    lm_level <- NA_character_
    if (!is.null(lm_model)) {
      tryCatch({
        if (!is.null(lm_model$xlevels) && "Province" %in% names(lm_model$xlevels)) {
          new_row$Province <- factor(new_row$Province, levels = lm_model$xlevels$Province)
        }
        lm_pred <- as.numeric(predict(lm_model, newdata = new_row))
        if (!is.na(lm_pred) && !is.null(median_ratio)) {
          lm_level <- ifelse(lm_pred > as.numeric(median_ratio), "High", "Low")
        }
      }, error = function(e) message("LRM error: ", e$message))
    }
    predictions$lm_pred <- lm_pred
    predictions$lm_level <- lm_level
    
    # C5.0 prediction
    c50_pred <- NA_character_
    if (!is.null(c50_model)) {
      tryCatch({
        new_row$Province <- factor(new_row$Province, levels = sort(unique(proc_data$Province)))
        c50_pred <- as.character(predict(c50_model, newdata = new_row, type = "class"))
      }, error = function(e) message("C5.0 error: ", e$message))
    }
    predictions$c50_pred <- c50_pred
  })
  
  output$pred_summary <- renderUI({
    req(predictions$input_data)
    
    tagList(
      h5("Scenario", style = sprintf("color: %s; font-weight: 700;", sa_theme$primary)),
      tags$ul(
        tags$li(paste("Province:", predictions$input_data$Province)),
        tags$li(paste("Year:", predictions$input_data$SurveyYear)),
        tags$li(paste("Piped:", predictions$input_data$PipedWater, "k HH")),
        tags$li(paste("Improved:", predictions$input_data$ImprovedSource, "k HH")),
        tags$li(paste("Surface:", predictions$input_data$SurfaceWater, "k HH")),
        tags$li(paste("Other:", predictions$input_data$OtherUnspecified, "k HH")),
        tags$li(paste("Ratio:", ifelse(is.na(predictions$ratio_in), "N/A", 
                                       round(predictions$ratio_in, 3))))
      ),
      h5("Predictions", style = sprintf("color: %s; font-weight: 700; margin-top: 15px;", sa_theme$primary)),
      tags$ul(
        tags$li(HTML(paste0("<b>LRM AccessRatio:</b> ", 
                            ifelse(is.na(predictions$lm_pred), "N/A", 
                                   paste0(round(predictions$lm_pred, 3), 
                                          " <span style='color: ", sa_theme$accent2, "; font-weight: 700;'>(", 
                                          predictions$lm_level, ")</span>"))))),
        tags$li(HTML(paste0("<b>C5.0 Level:</b> ", 
                            ifelse(is.na(predictions$c50_pred), "N/A", 
                                   paste0("<span style='color: ", sa_theme$accent1, "; font-weight: 700;'>", 
                                          predictions$c50_pred, "</span>")))))
      )
    )
  })
  
  # FORECAST TAB
  forecast_data <- reactiveValues(results = NULL)
  
  observeEvent(input$fc_btn, {
    req(!is.null(lm_model), !is.null(proc_data))
    
    # Get historical data for selected province
    hist_data <- proc_data %>%
      filter(Province == input$fc_prov) %>%
      arrange(SurveyYear)
    
    if (nrow(hist_data) == 0) {
      showNotification("No data for selected province.", type = "error")
      return()
    }
    
    # Generate future years
    last_year <- max(hist_data$SurveyYear)
    future_years <- seq(last_year + 1, last_year + input$fc_years, by = 1)
    
    # Build forecast rows using LRM coefficients
    # Simple approach: use latest water source values, vary year
    latest_values <- hist_data %>% slice(n())
    
    forecast_rows <- lapply(future_years, function(y) {
      fc_row <- data.frame(
        SurveyYear = y,
        Province = input$fc_prov,
        PipedWater = latest_values$PipedWater,
        ImprovedSource = latest_values$ImprovedSource,
        SurfaceWater = latest_values$SurfaceWater,
        OtherUnspecified = latest_values$OtherUnspecified
      )
      
      # Predict AccessRatio using LRM
      if (!is.null(lm_model)) {
        tryCatch({
          if (!is.null(lm_model$xlevels) && "Province" %in% names(lm_model$xlevels)) {
            fc_row$Province <- factor(fc_row$Province, levels = lm_model$xlevels$Province)
          }
          fc_row$AccessRatio <- as.numeric(predict(lm_model, newdata = fc_row))
        }, error = function(e) {
          fc_row$AccessRatio <- NA
        })
      } else {
        fc_row$AccessRatio <- NA
      }
      
      return(fc_row)
    })
    
    forecast_df <- bind_rows(forecast_rows)
    forecast_df$Type <- "Forecast"
    
    # Combine with historical data
    hist_data$Type <- "Historical"
    combined <- bind_rows(
      hist_data %>% select(SurveyYear, Province, AccessRatio, Type),
      forecast_df %>% select(SurveyYear, Province, AccessRatio, Type)
    )
    
    forecast_data$results <- list(
      historical = hist_data,
      forecast = forecast_df,
      combined = combined
    )
  })
  
  # FORECAST: Plot
  output$fc_plot <- renderPlot({
    req(forecast_data$results)
    
    combined <- forecast_data$results$combined
    
    ggplot(combined, aes(x = SurveyYear, y = AccessRatio, color = Type, group = 1)) +
      geom_line(size = 1.1) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Historical" = sa_theme$primary, "Forecast" = sa_theme$accent1)) +
      labs(x = "Year", y = "Access Ratio", 
           title = paste("AccessRatio Forecast:", input$fc_prov),
           color = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", color = sa_theme$primary),
        panel.grid.major = element_line(color = "#e0e0e0", size = 0.3),
        legend.position = "top"
      )
  })
  
  # FORECAST: Table
  output$fc_table <- DT::renderDataTable({
    req(forecast_data$results)
    
    forecast_data$results$forecast %>%
      select(SurveyYear, AccessRatio, PipedWater, ImprovedSource, SurfaceWater) %>%
      mutate(
        SurveyYear = as.integer(SurveyYear),
        AccessRatio = round(AccessRatio, 4),
        PipedWater = round(PipedWater, 1),
        ImprovedSource = round(ImprovedSource, 1),
        SurfaceWater = round(SurfaceWater, 1)
      )
  }, options = list(pageLength = 5, scrollX = TRUE))
  
  # FORECAST: Notes
  output$fc_notes <- renderUI({
    req(forecast_data$results)
    
    hist <- forecast_data$results$historical
    fc <- forecast_data$results$forecast
    
    last_hist_ratio <- hist$AccessRatio[nrow(hist)]
    last_fc_ratio <- fc$AccessRatio[nrow(fc)]
    
    change <- last_fc_ratio - last_hist_ratio
    
    tagList(
      h5("Forecast Summary", style = sprintf("color: %s; font-weight: 700;", sa_theme$primary)),
      p(HTML(paste0(
        "<b>Province:</b> ", input$fc_prov, "<br>",
        "<b>Forecast Period:</b> ", min(fc$SurveyYear), " to ", max(fc$SurveyYear), "<br>",
        "<b>Latest Historical Ratio:</b> ", round(last_hist_ratio, 4), "<br>",
        "<b>Projected Final Ratio:</b> ", round(last_fc_ratio, 4), "<br>",
        "<b>Expected Change:</b> ", ifelse(change > 0, "+", ""), round(change, 4)
      ))),
      br(),
      h5("Methodology", style = sprintf("color: %s; font-weight: 700;", sa_theme$primary)),
      p(class = "note",
        "Forecast uses Linear Regression coefficients with historical water source values. ",
        "Year parameter varies while source indicators remain constant at latest observed values. ",
        "This assumes infrastructure patterns stabilise.")
    )
  })
  output$eval_lm <- renderUI({
    if (is.null(lm_model)) return(div(class = "note", "LRM model not loaded."))
    
    summ <- summary(lm_model)
    tagList(
      p(HTML(paste0(
        "<b style='color: ", sa_theme$primary, ";'>Adjusted R²:</b> ", round(summ$adj.r.squared, 4), "<br>",
        "<b style='color: ", sa_theme$primary, ";'>Residual Std. Error:</b> ", round(summ$sigma, 4), "<br>",
        "<b style='color: ", sa_theme$primary, ";'>F-statistic p-value:</b> ", 
        format.pval(pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3], lower.tail = FALSE), digits = 3)
      ))),
      p(class = "note", "Training fit. For 5-fold CV results, see model script.")
    )
  })
  
  output$eval_diag <- renderPlot({
    req(lm_model)
    plot(lm_model, which = 1)
  })
  
  output$eval_c50 <- renderUI({
    if (is.null(c50_model)) return(div(class = "note", "C5.0 model not loaded."))
    
    tagList(
      p(HTML("<b style='color: ", sa_theme$primary, ";'>C5.0 Boosted Classification</b>")),
      p("Trained with 10 boosting trials for High/Low access classification."),
      p(class = "note", "Use Predict tab for scenarios. See script for 5-fold CV metrics.")
    )
  })
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================

shinyApp(ui, server)

# ==============================================================================
# DEPLOYMENT - RSCONNECT
# ==============================================================================
#1. Install RSConnect
# install.packages("rsconnect")

#2. Account Authorisation
#rsconnect::setAccountInfo(name='pmokadi', 
  #                        token='2C95EA6D5686DED04D1825A3F488F866', 
    #                      secret='sg8H0hY25Fm29fA7NHdj9q4vueIUsv/55bqUY9xA')

#3. Setting Directory
#setwd("/Users/pmokadi/BIN381 Project Model Deployment")

#4.Deploy
#library(rsconnect)
#rsconnect::deployApp(
#  appName = "SA-Water-Access-GroupH",
 # appTitle = "SA Water Access - Group H"
#)
