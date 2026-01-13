# app.R
# UAE Car Listings Dashboard (clean + sleek + USD conversion)
# Put this file in the same folder as: dubizzle_cars_dataset.csv

library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(shinydashboard)
library(scales)

# -----------------------------
# Load + Clean Data
# -----------------------------

# Friendly error if the dataset isn't present
if (!file.exists("dubizzle_cars_dataset.csv")) {
  stop("Missing file: dubizzle_cars_dataset.csv. Put it in the same folder as app.R.")
}

dubizzle_cars_dataset <- read.csv("dubizzle_cars_dataset.csv", stringsAsFactors = FALSE)

# Helper: safely coerce to numeric (handles commas, currency symbols, etc.)
to_num <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(x))))
}

# Conversion constants (document these in README)
aed_to_usd  <- 0.27
km_to_miles <- 0.621371

cars <- dubizzle_cars_dataset %>%
  mutate(
    price = to_num(price),
    kilometers = to_num(kilometers),
    miles = kilometers * km_to_miles,
    year = to_num(year),
    engine_capacity_cc = if ("engine_capacity_cc" %in% names(.)) to_num(engine_capacity_cc) else NA_real_,
    horsepower = if ("horsepower" %in% names(.)) to_num(horsepower) else NA_real_,
    price_usd = price * aed_to_usd
  ) %>%
  filter(!is.na(price_usd), price_usd > 0) %>%
  mutate(
    vehicle_age_years = as.numeric(format(Sys.Date(), "%Y")) - year,
    price_per_km   = ifelse(!is.na(kilometers) & kilometers > 0, price_usd / kilometers, NA_real_),
    price_per_mile = ifelse(!is.na(miles) & miles > 0, price_usd / miles, NA_real_)
  )

# Precompute choices
brand_choices <- sort(unique(na.omit(cars$brand)))
model_choices <- sort(unique(na.omit(cars$model)))
fuel_choices  <- sort(unique(na.omit(cars$fuel_type)))
year_choices  <- sort(unique(na.omit(cars$year)), decreasing = TRUE)

# -----------------------------
# UI
# -----------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$span("UAE Used Cars — Interactive Pricing Dashboard", style = "font-weight: 600;")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
      menuItem("Price Distribution", tabName = "price_distribution", icon = icon("chart-column")),
      menuItem("Brand Comparison", tabName = "brand_comparison", icon = icon("chart-bar")),
      menuItem("Explore Listings", tabName = "dataset_table", icon = icon("table")),
      menuItem("About", tabName = "about_data", icon = icon("circle-info"))
    ),
    hr(),
    tags$div(style = "padding: 0 10px;",
             selectInput("brand", "Brand",
                         choices = c("All" = "All", brand_choices),
                         selected = "All",
                         multiple = TRUE
             ),
             selectInput("model", "Model",
                         choices = c("All" = "All", model_choices),
                         selected = "All",
                         multiple = TRUE
             ),
             selectInput("year", "Year",
                         choices = c("All" = "All", year_choices),
                         selected = "All",
                         multiple = TRUE
             ),
             selectInput("fuel_type", "Fuel Type",
                         choices = c("All" = "All", fuel_choices),
                         selected = "All",
                         multiple = TRUE
             ),
             sliderInput("price_range", "Price Range (USD)",
                         min = floor(min(cars$price_usd, na.rm = TRUE)),
                         max = ceiling(max(cars$price_usd, na.rm = TRUE)),
                         value = c(
                           floor(quantile(cars$price_usd, 0.01, na.rm = TRUE)),
                           ceiling(quantile(cars$price_usd, 0.99, na.rm = TRUE))
                         ),
                         step = 100,
                         pre = "$"
             ),
             checkboxInput("remove_outliers", "Trim extreme outliers (recommended)", value = TRUE),
             checkboxInput("us_view", "US-friendly table view (USD + miles, key columns only)", value = TRUE),
             checkboxInput("log_scale", "Use log scale for price charts", value = FALSE),
             hr(),
             radioButtons(
               inputId = "distance_unit",
               label = "Distance Unit",
               choices = c("Miles (US)" = "mi", "Kilometers" = "km"),
               selected = "mi",
               inline = TRUE
             ),
             downloadButton("download_filtered", "Download Filtered CSV")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #F5F7FB; }
        .box { border-radius: 14px; border-top: 0; box-shadow: 0 6px 18px rgba(17, 24, 39, 0.08); }
        .box-header { padding: 14px 16px; border-bottom: 1px solid rgba(17, 24, 39, 0.06); }
        .box-title { font-weight: 700; }
        .skin-blue .main-header .logo { background-color: #111827; font-weight: 700; }
        .skin-blue .main-header .navbar { background-color: #111827; }
        .skin-blue .main-sidebar { background-color: #0B1220; }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a { color: #E5E7EB; }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover { background-color: #111827; }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { background-color: #1F2937; }
        .value-box { border-radius: 14px; box-shadow: 0 6px 18px rgba(17, 24, 39, 0.08); }
        .small-box { border-radius: 14px; }
        .small-box p { font-size: 13px; opacity: 0.9; }
      "))
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("kpi_count"),
                valueBoxOutput("kpi_median_price"),
                valueBoxOutput("kpi_avg_distance"),
                valueBoxOutput("kpi_avg_age")
              ),
              fluidRow(
                box(
                  title = "Price vs. Vehicle Age (USD)",
                  width = 12,
                  plotlyOutput("scatter_age_price", height = "420px")
                )
              )
      ),
      
      tabItem(tabName = "price_distribution",
              fluidRow(
                box(
                  title = "Price Distribution (USD)",
                  width = 12,
                  plotlyOutput("price_distribution_plot", height = "420px")
                )
              ),
              fluidRow(
                box(
                  title = "Price per Distance (USD per mile/km)",
                  width = 12,
                  plotlyOutput("ppk_plot", height = "420px")
                )
              )
      ),
      
      tabItem(tabName = "brand_comparison",
              fluidRow(
                box(
                  title = "Average Price by Brand (USD)",
                  width = 12,
                  plotlyOutput("comparative_plot", height = "420px")
                )
              ),
              fluidRow(
                box(
                  title = "Top Brands by Listing Count",
                  width = 12,
                  plotlyOutput("top_brands_plot", height = "420px")
                )
              )
      ),
      
      tabItem(tabName = "dataset_table",
              fluidRow(
                box(
                  title = "Filtered Listings",
                  width = 12,
                  DTOutput("data_table")
                )
              )
      ),
      
      tabItem(tabName = "about_data",
              fluidRow(
                box(
                  title = "About the Data",
                  width = 12,
                  h3("Dataset Overview"),
                  p("This dashboard explores used car listings in the United Arab Emirates."),
                  tags$ul(
                    tags$li("Prices are normalized from AED to USD."),
                    tags$li("Distance can be viewed in miles or kilometers."),
                    tags$li("Optional outlier trimming improves chart readability."),
                    tags$li("All visualizations update dynamically based on filters.")
                  ),
                  h4("Key Columns (High-Level)"),
                  tableOutput("data_summary")
                )
              )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  # Distance unit helpers
  distance_col <- reactive({
    if (!is.null(input$distance_unit) && input$distance_unit == "mi") "miles" else "kilometers"
  })
  
  distance_label <- reactive({
    if (!is.null(input$distance_unit) && input$distance_unit == "mi") "Miles (mi)" else "Kilometers (km)"
  })
  
  # Keep model choices in sync with chosen brand(s)
  observeEvent(input$brand, {
    if (is.null(input$brand) || "All" %in% input$brand) {
      updateSelectInput(session, "model",
                        choices = c("All" = "All", model_choices),
                        selected = isolate(input$model)
      )
    } else {
      possible_models <- cars %>%
        filter(brand %in% input$brand) %>%
        pull(model) %>%
        unique() %>%
        na.omit() %>%
        sort()
      updateSelectInput(session, "model", choices = c("All" = "All", possible_models), selected = "All")
    }
  }, ignoreInit = TRUE)
  
  filtered_data <- reactive({
    filtered <- cars
    
    if (!is.null(input$brand) && !("All" %in% input$brand)) {
      filtered <- filtered %>% filter(brand %in% input$brand)
    }
    if (!is.null(input$model) && !("All" %in% input$model)) {
      filtered <- filtered %>% filter(model %in% input$model)
    }
    if (!is.null(input$year) && !("All" %in% input$year)) {
      filtered <- filtered %>% filter(year %in% input$year)
    }
    if (!is.null(input$fuel_type) && !("All" %in% input$fuel_type)) {
      filtered <- filtered %>% filter(fuel_type %in% input$fuel_type)
    }
    
    # Price range filter (USD)
    filtered <- filtered %>%
      filter(price_usd >= input$price_range[1], price_usd <= input$price_range[2])
    
    # Outlier trimming
    if (isTRUE(input$remove_outliers) && nrow(filtered) > 20) {
      lo <- quantile(filtered$price_usd, 0.01, na.rm = TRUE)
      hi <- quantile(filtered$price_usd, 0.99, na.rm = TRUE)
      filtered <- filtered %>% filter(price_usd >= lo, price_usd <= hi)
    }
    
    filtered
  })
  
  # KPI Boxes
  output$kpi_count <- renderValueBox({
    valueBox(
      value = comma(nrow(filtered_data())),
      subtitle = "Listings in Selection",
      icon = icon("car"),
      color = "blue"
    )
  })
  
  output$kpi_median_price <- renderValueBox({
    valueBox(
      value = dollar(median(filtered_data()$price_usd, na.rm = TRUE)),
      subtitle = "Median Price (USD)",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  output$kpi_avg_distance <- renderValueBox({
    df <- filtered_data()
    dcol <- distance_col()
    avg_dist <- mean(df[[dcol]], na.rm = TRUE)
    
    valueBox(
      value = comma(round(avg_dist)),
      subtitle = paste0("Avg Distance (", ifelse(input$distance_unit == "mi", "mi", "km"), ")"),
      icon = icon("road"),
      color = "blue"
    )
  })
  
  output$kpi_avg_age <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$vehicle_age_years, na.rm = TRUE), 1),
      subtitle = "Avg Vehicle Age (Years)",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  # Plots
  output$price_distribution_plot <- renderPlotly({
    df <- filtered_data()
    
    p <- plot_ly(
      data = df,
      x = ~price_usd,
      type = "histogram",
      marker = list(color = "#2563EB"),
      hovertemplate = paste(
        "Price (USD): %{x}<br>",
        "Count: %{y}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = "Price (USD)"),
        yaxis = list(title = "Count"),
        bargap = 0.05
      )
    
    if (isTRUE(input$log_scale)) {
      p <- p %>% layout(xaxis = list(type = "log", title = "Price (USD, log scale)"))
    }
    p
  })
  
  output$comparative_plot <- renderPlotly({
    avg_price_brand <- filtered_data() %>%
      filter(!is.na(brand)) %>%
      group_by(brand) %>%
      summarize(price_avg = mean(price_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(price_avg)) %>%
      slice_head(n = 25)
    
    p <- plot_ly(
      data = avg_price_brand,
      x = ~reorder(brand, price_avg),
      y = ~price_avg,
      type = "bar",
      marker = list(color = "#2563EB"),
      hovertemplate = "Brand: %{x}<br>Avg Price: %{y:$,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Brand"),
        yaxis = list(title = "Average Price (USD)")
      )
    
    if (isTRUE(input$log_scale)) {
      p <- p %>% layout(yaxis = list(type = "log", title = "Average Price (USD, log scale)"))
    }
    p
  })
  
  output$top_brands_plot <- renderPlotly({
    top_brands <- filtered_data() %>%
      filter(!is.na(brand)) %>%
      count(brand, sort = TRUE) %>%
      slice_head(n = 25)
    
    plot_ly(
      data = top_brands,
      x = ~reorder(brand, n),
      y = ~n,
      type = "bar",
      marker = list(color = "#2563EB"),
      hovertemplate = "Brand: %{x}<br>Listings: %{y}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Brand"),
        yaxis = list(title = "Listing Count")
      )
  })
  
  output$scatter_age_price <- renderPlotly({
    df <- filtered_data() %>%
      filter(!is.na(vehicle_age_years), !is.na(price_usd), vehicle_age_years >= 0)
    
    dcol <- distance_col()
    dlabel <- distance_label()
    
    plot_ly(
      data = df,
      x = ~vehicle_age_years,
      y = ~price_usd,
      type = "scatter",
      mode = "markers",
      marker = list(size = 6, opacity = 0.6),
      customdata = cbind(df$brand, df$model, df[[dcol]]),
      hovertemplate = paste0(
        "Age (years): %{x}<br>",
        "Price (USD): %{y:$,.0f}<br>",
        "Brand: %{customdata[0]}<br>",
        "Model: %{customdata[1]}<br>",
        dlabel, ": %{customdata[2]:,.0f}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = "Vehicle Age (Years)"),
        yaxis = list(title = "Price (USD)")
      )
  })
  
  output$ppk_plot <- renderPlotly({
    df <- filtered_data()
    
    if (!is.null(input$distance_unit) && input$distance_unit == "mi") {
      colname <- "price_per_mile"
      label <- "USD/MI"
    } else {
      colname <- "price_per_km"
      label <- "USD/KM"
    }
    
    df <- df %>%
      filter(!is.na(.data[[colname]]), is.finite(.data[[colname]]), .data[[colname]] > 0)
    
    # Trim extreme values for readability
    if (nrow(df) > 50) {
      lo <- quantile(df[[colname]], 0.01, na.rm = TRUE)
      hi <- quantile(df[[colname]], 0.99, na.rm = TRUE)
      df <- df %>% filter(.data[[colname]] >= lo, .data[[colname]] <= hi)
    }
    
    plot_ly(
      data = df,
      x = df[[colname]],
      type = "histogram",
      marker = list(color = "#2563EB"),
      hovertemplate = paste0(label, ": %{x:$,.4f}<br>Count: %{y}<extra></extra>")
    ) %>%
      layout(
        xaxis = list(title = paste0("Price per Distance (", label, ")")),
        yaxis = list(title = "Count"),
        bargap = 0.05
      )
  })
  
  # Data table (filtered)
  output$data_table <- renderDT({
    df <- filtered_data()
    
    if (isTRUE(input$us_view)) {
      keep <- intersect(
        c("brand", "model", "trim", "year", "price_usd", "miles", "kilometers",
          "fuel_type", "transmission_type", "body_type", "horsepower", "engine_capacity_cc",
          "exterior_color", "interior_color", "seller_type"),
        names(df)
      )
      
      df <- df %>%
        select(all_of(keep)) %>%
        rename(
          `Price (USD)` = price_usd,
          `Miles` = miles,
          `Kilometers` = kilometers,
          `Fuel Type` = fuel_type,
          `Transmission` = transmission_type,
          `Body Type` = body_type,
          `Engine (cc)` = engine_capacity_cc,
          `Seller Type` = seller_type,
          `Exterior` = exterior_color,
          `Interior` = interior_color
        )
      
      if ("Price (USD)" %in% names(df)) df$`Price (USD)` <- round(df$`Price (USD)`, 0)
      if ("Miles" %in% names(df)) df$Miles <- round(df$Miles, 0)
      if ("Kilometers" %in% names(df)) df$Kilometers <- round(df$Kilometers, 0)
    }
    
    datatable(
      df,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # Download filtered CSV
  output$download_filtered <- downloadHandler(
    filename = function() paste0("dubizzle_cars_filtered_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
  
  # About table
  output$data_summary <- renderTable({
    data.frame(
      Column = c(
        "price_usd", "brand", "model", "kilometers", "miles", "year",
        "vehicle_age_years", "fuel_type", "price_per_km", "price_per_mile"
      ),
      Description = c(
        "Asking price converted from AED to USD.",
        "Vehicle manufacturer/brand.",
        "Vehicle model.",
        "Mileage (kilometers).",
        "Mileage (miles).",
        "Year of manufacture.",
        "Derived: current year minus vehicle year.",
        "Fuel type (e.g., petrol, diesel).",
        "Derived: price_usd divided by kilometers.",
        "Derived: price_usd divided by miles."
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui, server)
