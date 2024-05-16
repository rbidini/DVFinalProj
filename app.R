library(shiny)
library(readr)
library(plotly)
library(dplyr)
library(DT)
library(shinydashboard)

dubizzle_cars_dataset <- read.csv("dubizzle_cars_dataset.csv", stringsAsFactors = FALSE)

#Changed the currency from AED to USD
aed_to_usd <- 0.27

ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$span("United Arab Emirates Cars Data Analysis", style = "color: #FFFFFF;")
    )
  ),
 #Adding icons for the side bar to make it more appealing
   dashboardSidebar(
    sidebarMenu(
      menuItem("Price Distribution", tabName = "price_distribution", icon = icon("chart-bar")),
      menuItem("Comparative Analysis", tabName = "comparative_analysis", icon = icon("chart-line")),
      menuItem("Dataset Table", tabName = "dataset_table", icon = icon("table")),
      menuItem("About the Data", tabName = "about_data", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
    #making it a little more fancy and adding different color :)  
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #007B3A;
          color: #FFFFFF;
          border-bottom: 0 solid transparent;
        }
        .skin-blue .main-header .navbar {
          background-color: #007B3A;
        }
        .skin-blue .main-sidebar {
          background-color: #000000;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #007B3A;
          color: #FFFFFF;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
          color: #FFFFFF;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #007B3A;
          color: #FFFFFF;
        }
        .skin-blue .main-header .navbar .sidebar-toggle {
          color: #FFFFFF;
          border-right: 1px solid #FFFFFF;
        }
        .box.box-solid.box-primary>.box-header {
          background-color: #007B3A;
          color: #FFFFFF;
        }
        .box.box-solid.box-primary {
          border-color: #007B3A;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "price_distribution",
              fluidRow(
                box(
                  title = "Filter Options",
                  selectInput("brand", "Select Car Brand:",
                              choices = c("All Brands" = "All", unique(dubizzle_cars_dataset$brand)),
                              selected = "All",
                              multiple = TRUE),
                  selectInput("model", "Select Car Model:",
                              choices = c("All Models" = "All", unique(dubizzle_cars_dataset$model)),
                              selected = "All",
                              multiple = TRUE),
                  selectInput("year", "Select Year:",
                              choices = c("All Years" = "All", sort(unique(dubizzle_cars_dataset$year), decreasing = TRUE)),
                              selected = "All",
                              multiple = TRUE),
                  selectInput("fuel_type", "Select Fuel Type:",
                              choices = c("All Fuels" = "All", unique(dubizzle_cars_dataset$fuel_type)),
                              selected = "All",
                              multiple = TRUE)
                )
              ),
              fluidRow(
                box(
                  title = "Price Distribution",
                  plotlyOutput("price_distribution_plot"),
                  width = 12
                )
              )
      ),
      # allow user to choose different options
      tabItem(tabName = "comparative_analysis",
              fluidRow(
                box(
                  title = "Filter Options",
                  selectInput("brand", "Select Car Brand:",
                              choices = c("All Brands" = "All", unique(dubizzle_cars_dataset$brand)),
                              selected = "All",
                              multiple = TRUE),
                  selectInput("model", "Select Car Model:",
                              choices = c("All Models" = "All", unique(dubizzle_cars_dataset$model)),
                              selected = "All",
                              multiple = TRUE),
                  selectInput("year", "Select Year:",
                              choices = c("All Years" = "All", sort(unique(dubizzle_cars_dataset$year), decreasing = TRUE)),
                              selected = "All",
                              multiple = TRUE),
                  selectInput("fuel_type", "Select Fuel Type:",
                              choices = c("All Fuels" = "All", unique(dubizzle_cars_dataset$fuel_type)),
                              selected = "All",
                              multiple = TRUE)
                )
              ),
              fluidRow(
                box(
                  title = "Comparative Analysis",
                  plotlyOutput("comparative_plot"),
                  width = 12  
                )
              )
      ),
      tabItem(tabName = "dataset_table",
              fluidRow(
                box(
                  title = "Dataset Table",
                  DTOutput("data_table"),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "about_data",
              fluidRow(
                box(
                  title = "About the Data",
                  h3("Dataset Overview"),
                  p("This dataset contains features about different kinds of cars in the United Arab Emirates. It includes data on about 10,000 different cars from the UAE."),
                  
                  h4("Important Columns"),
                  tableOutput("data_summary")
                )
              )
      )
    )
  )
)

  # Filtered data based on user inputs from the server function below
server <- function(input, output, session) {
  filtered_data <- reactive({
    filtered <- dubizzle_cars_dataset
    
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
    
    filtered$price <- filtered$price * aed_to_usd
    
    return(filtered)
  })
  
  output$price_distribution_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~price, type = "histogram", marker = list(color = '#FF0000')) %>%
      layout(title = "Price Distribution (in USD)")
  })
  
  output$comparative_plot <- renderPlotly({
    avg_price_brand <- filtered_data() %>% 
      group_by(brand) %>% 
      summarize(price_avg = mean(price, na.rm = TRUE)) %>% 
      arrange(desc(price_avg))
    
    plot_ly(data = avg_price_brand, x = ~brand, y = ~price_avg, type = "bar", marker = list(color = '#FF0000')) %>%
      layout(title = "Average Price by Car Brand (in USD)",
             xaxis = list(title = "Brand"),
             yaxis = list(title = "Average Price (USD)"))
  })
  
  output$data_table <- renderDT({
    datatable(dubizzle_cars_dataset)
  })
  
  output$data_summary <- renderTable({
    data.frame(
      Column = c("Price", "Brand", "Model", "Trim", "Kilometers", "Year", "Vehicle Age Years", 
                 "Regional Specs", "Doors", "Body Type", "Fuel Type", "Seating Capacity", 
                 "Transmission Type", "Engine Capacity CC", "Horsepower", "No of Cylinders", 
                 "Exterior Color", "Interior Color", "Warranty", "Address/Country/City/Area Name/Location Name", 
                 "Latitude/Longitude", "Seller Type"),
      Description = c("Asking price for the vehicle.",
                      "Vehicle manufacturer.",
                      "Specific model of the vehicle.",
                      "Trim level of the vehicle, indicating different features or packages.",
                      "Mileage of the vehicle, indicating how much it has been used.",
                      "Year of manufacture.",
                      "The age of the vehicle calculated from the current year.",
                      "Specifications tailored to the GCC or other regions.",
                      "Number of doors in the vehicle.",
                      "Type of vehicle body (e.g., SUV, hatchback).",
                      "Type of fuel the vehicle uses (e.g., petrol, diesel).",
                      "Number of seats in the vehicle.",
                      "Manual or automatic transmission.",
                      "Engine size in cubic centimeters.",
                      "Power output of the vehicle's engine.",
                      "Number of engine cylinders.",
                      "Color of the vehicle's exterior.",
                      "Color of the vehicle's interior.",
                      "Indicates if the vehicle comes with a warranty.",
                      "Detailed location information where the vehicle is sold.",
                      "Geographical coordinates of the listed vehicle.",
                      "Indicates if the seller is a dealership or a private individual.")
    )
  })
}

shinyApp(ui, server)



