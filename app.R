library(shiny)
library(shinyalert)
library(tidyverse)
library(bslib) 

source("helpers.R")
source("static.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Title
  "Exploration of Banking Data",
  
  # Side panel with Widgets
  sidebarLayout(
    sidebarPanel(
      
      # Select X/Y Axis
      h2("Select Variables to Investigate:"),
      selectizeInput(inputId = 'x', 
                     label = 'x Variable', 
                     choices = numeric_vars, 
                     selected = 'age'),
      uiOutput("dynamic_slider_x"),
      
      selectizeInput(inputId = 'y',
                     label = 'y Variable',
                     choices = numeric_vars,
                     selected = 'balance'),
      uiOutput("dynamic_slider_y"),
      
      # Radio Buttons will go here
      br(),
      h2("Choose a subset of the data:"),
      radioButtons("rel_corr",
                   "Marital Status",
                   choiceNames = c("All","Divorced","Married","Single"),
                   choiceValues = c("all","divorced","married","single"),
                   selected = "all"),
      radioButtons("schl_corr",
                   "Educational Attainment",
                   choiceNames = c("All","High School not completed","High School or GED","College Degree"),
                   choiceValues = c("all","high school not completed","high school or ged","college degree"),
                   selected = "all"),
      br(),
      h2("Select Age-Range"),
      sliderInput(
        inputId = "age_range",
        label = "Select Age Range:",
        min = 0,
        max = 100,
        value = c(18, 65), # Sets the initial min and max handles
        step = 1
      ),
      actionButton("corr_sample","Subset and Submit")
    ),
    
    # main Panel
    mainPanel(
      
      tabsetPanel(
        type = "tabs",
        
        # About Section
        tabPanel(
          title = "About This App",
          br(),
          h3("Application Purpose & Metadata"),
          p("This application provides an interactive platform to explore banking datasets. 
            Users can dynamically subset data fields, filter across multi-variate continuous boundaries, 
            and isolate key categorical factors such as marital status and education attainment.
            data was taken from the following URL link. both the test and train csv files were called, and 
            merged together in order to create the output. Link: https://www.kaggle.com/datasets/prakharrathi25/banking-dataset-marketing-targets"),
          
          br(),
          h4("App Information Overview"),
          # HTML styled table for scannable layout
          tags$table(
            class = "table table-striped table-bordered",
            style = "max-width: 600px;",
            tags$thead(
              tags$tr(
                tags$th("Side Bar function"),
                tags$th("Description")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(strong("x Variable")),
                tags$td("Numeric variable to place on X axis. Slider Provides filtering for dataset")
              ),
              tags$tr(
                tags$td(strong("y Variable")),
                tags$td("Numeric Variable to set on Y axis. Slider Provides filtering for dataset")
              ),
              tags$tr(
                tags$td(strong("Marital Status")),
                tags$td("Subset data by population's marital status.")
              ),
              tags$tr(
                tags$td(strong("Educational Attainment")),
                tags$td("Subset data by population highest reported level of education.")
              ),
              tags$tr(
                tags$td(strong("Select Age Range")),
                tags$td("Optional subsetting to select specific age range of subjects.")
              )
            )
          ),
          
          br(),
          h4("Visual Overview"),
          img(
            src = "banking_overview.png", 
            alt = "Application workflow or placeholder graphic", 
            style = "max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px; padding: 5px;"
          ),
        ),
        # Data Download Tab
        tabPanel(
          title = "Data Download",
          br(),
          h3("Export Filtered Dataset"),
          p("Click the button below to download the data matching your current filter selections as a CSV file."),
          br(),
          # Add the download button widget
          downloadButton(outputId = "download_csv", label = "Download Filtered Data (.csv)"),
          br(), br(),
          h4("Preview of Data (First N.obs = 50)"),
          tableOutput("preview_download_table")
        ),
        #  Data Exploration Table
        tabPanel(
          title = "Create Data Output",
          br(),
          h3("Variable Configurations"),
          
          # Layout split: Left side for toggle choices, right side for switching frames
          sidebarLayout(
            sidebarPanel(
              width = 3,
              radioButtons(
                inputId = "summary_type",
                label = "Choose Summary Dimension:",
                choices = c("Categorical Summaries" = "cat", "Numeric Summaries" = "num"),
                selected = "cat"
              )
            ),
            
            mainPanel(
              width = 9,
              # This placeholder will load different layout wires dynamically
              uiOutput("exploration_framework_ui")
            )
          )
        )
      )
    )
  )
)

my_sample <- readRDS("combined.bankdata.rds")

# Define server logic
server <- function(input, output, session) {
  
  # Reactive X Numeric Slider UI
  output$dynamic_slider_x <- renderUI({
    req(input$x)
    
    var_data <- my_sample[[input$x]]
    min_val <- min(var_data, na.rm = TRUE)
    max_val <- max(var_data, na.rm = TRUE)
    
    sliderInput(
      inputId = "range_filter_x",
      label = paste("Filter by", input$x, "range:"),
      min = min_val,
      max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  # Reactive Y Numeric Slider UI
  output$dynamic_slider_y <- renderUI({
    req(input$y)
    
    var_data <- my_sample[[input$y]]
    min_val <- min(var_data, na.rm = TRUE)
    max_val <- max(var_data, na.rm = TRUE)
    
    sliderInput(
      inputId = "range_filter_y",
      label = paste("Filter by", input$y, "range:"),
      min = min_val,
      max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  # Unified Reactive data chain tied to the Action Button submission
  filtered_data <- eventReactive(input$corr_sample, {
    req(input$x, input$y)
    
    df <- my_sample
    
    # Apply X bounds if available
    if (!is.null(input$range_filter_x)) {
      df <- df[df[[input$x]] >= input$range_filter_x[1] & df[[input$x]] <= input$range_filter_x[2], ]
    }
    
    # Apply Y bounds if available
    if (!is.null(input$range_filter_y)) {
      df <- df[df[[input$y]] >= input$range_filter_y[1] & df[[input$y]] <= input$range_filter_y[2], ]
    }
    
    # Apply age range bounds
    if (!is.null(input$age_range)) {
      df <- df[df$age >= input$age_range[1] & df$age <= input$age_range[2], ]
    }
    
    # Apply Marital Status filter
    if (input$rel_corr != "all") {
      df <- df[df$marital == input$rel_corr, ]
    }
    
    # Apply Education filter
    if (input$schl_corr != "all") {
      df <- df[df$education == input$schl_corr, ]
    }
    
    return(df)
  }, ignoreNULL = FALSE) # ignoreNULL = FALSE ensures data loads immediately on startup
  
  # Text outputs placeholder logic
  output$selected_range <- renderText({
    req(input$age_range)
    paste("Selected age range:", input$age_range[1], "to", input$age_range[2])
  })
  
  output$min_age <- renderText({ paste("Min Age:", input$age_range[1]) })
  output$max_age <- renderText({ paste("Max Age:", input$age_range[2]) })
  
  #Download tab handler
  output$download_csv <- downloadHandler(
    filename = function() {
      # Creates a filename like: filtered_bank_data_2026-10-27.csv
      paste("filtered_bank_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Writes the reactive data to the download file path
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Preview table
  output$preview_download_table <- renderTable({
    head(filtered_data(), 50)
  })
  
  #Data Summary tab
  output$exploration_framework_ui <- renderUI({
    req(input$summary_type)
    
    if (input$summary_type == "cat") {
      # Categorical Summaries Layout with Select Input
      tagList(
        h4("Categorical Summaries"),
        p("Configuration:"),
        hr(),
        fluidRow(
          column(width = 4,
                 radioButtons(
                   inputId = "cat_mode",
                   label = "Table Type:",
                   choices = c("One-Way Frequency" = "one_way", "Two-Way Cross Tab" = "two_way"),
                   selected = "one_way"
                 ),
                 br(),
                 selectInput(
                   inputId = "selected_cat_var1",
                   label = "Primary Variable (Rows):",
                   choices = cat_vars,
                   selected = "marital"
                 ),
                 # Render the secondary selection input field only if Two-Way is selected
                 conditionalPanel(
                   condition = "input.cat_mode == 'two_way'",
                   selectInput(
                     inputId = "selected_cat_var2",
                     label = "Secondary Variable (Columns):",
                     choices = cat_vars,
                     selected = "education"
                   )
                 )
          ),
          column(width = 8, 
                 h5("Distribution Result Frame"),
                 tableOutput("categorical_summary_table")
          )
        )
      )
    } else if (input$summary_type == "num") {
      tagList(
        h4("Numeric Summaries"),
        p("Generate grouped continuous statistics for selected data."),
        hr(),
        fluidRow(
          column(width = 4,
                 selectInput(
                   inputId = "num_group_var",
                   label = "Grouping Variable (Categorical):",
                   choices = cat_vars,
                   selected = "marital"
                 ),
                 selectInput(
                   inputId = "num_target_var",
                   label = "Target Metric (Numeric):",
                   choices = numeric_vars,
                   selected = "balance"
                 )
          ),
          column(width = 8, 
                 h5("Grouped Descriptive Statistics"),
                 tableOutput("numeric_summary_table")
          )
        )
      )
    }
  })
  
  # Data Output
  # Categorical outputs
  output$categorical_summary_table <- renderTable({
    req(filtered_data(), input$cat_mode, input$selected_cat_var1)
    df <- filtered_data()
    
    # Validation safety checks
    req(input$selected_cat_var1 %in% names(df))
    
    if (input$cat_mode == "one_way") {
      # Execute single-variable layout
      one_way_table(df[[input$selected_cat_var1]])
      
    } else if (input$cat_mode == "two_way") {
      req(input$selected_cat_var2, input$selected_cat_var2 %in% names(df))
      
      # Execute your two_way_table function
      two_way_table(df[[input$selected_cat_var1]], df[[input$selected_cat_var2]])
    }
  })
  
  # Numeric summary statistics
  output$numeric_summary_table <- renderTable({
    req(filtered_data(), input$num_group_var, input$num_target_var)
    df <- filtered_data()
    
    # Structural check to ensure choices map correctly into the data frame columns
    req(input$num_group_var %in% names(df), input$num_target_var %in% names(df))
    
    # Bridge tidy evaluation by transforming string choices using .data pronouns
    summaries(
      data = df, 
      group_var = .data[[input$num_group_var]], 
      num_var = .data[[input$num_target_var]]
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
