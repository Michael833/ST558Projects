# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)												 
options("shiny.sanitize.errors" = FALSE) # Turn off error sanitization

# Load data --------------------------------------------------------------------


my_data <- read.csv("data/CensusAtSchool2026.csv", na.strings = c("", "NA"))
clean_age <- na.omit(survey_data$ageyears)
clean_travel <- na.omit(survey_data$travel_time_to_school)
available_months  <- sort(unique(my_data$birth_month))
available_genders <- sort(unique(my_data$gender))

numvar_choices <- c(
  "School Grade" = "classgrade", 
  "Age (years)" = "ageyears", 
  "Langauges Spoken" = "languages_spoken",
  "Time to School (mins)" = "travel_time_to_school", 
  "Home Occupants" = "home_occupants", 
  "Texts Sent Yesterday" = "text_messages_sent_yesterday",
  "Texts Received Yesterday" = "text_messages_received_yesterday",
  "Homework Last Week (Hours)" = "doing_homework_hours",
  "Activities w/ Family Last Week (Hours)" = "doing_things_with_family_hours"
)									 

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarPanel(
    selectInput(inputId = "selected_months",
                label = "Select Birth Month(s):",
                choices = available_months,
                selected = "January",
                multiple = TRUE),
    checkboxGroupInput(inputId = "selected_genders", 
                       label = "Gender of respondents:", 
                       choices = available_genders,
                       selected = available_genders),
    selectInput(inputId = "y", 
                label = "Y-axis:",
                choices = numvar_choices,
                selected = NULL),
    selectInput(inputId = "x", 
                label = "X-axis:",
                choices = numvar_choices,
                selected = NULL),
    
    uiOutput(outputId = "x_slider"),
    numericInput("usedRange",
                 label = "Set range for variable", 
                 value = 1, min = 0, max = 1)
    ),
  
  card(
    plotOutput(outputId = "scatterplot")
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Double slideer for my x and y variables, usin renderUI to make dynamic as 
  #  different x-vars are selected.
  output$x_slider <- renderUI({
    req(input$x)
    x_vector <- my_data[[input$x]]
    x_min_val <- min(x_vector)
    x_max_val <- max(x_vector)
    x_label <- names(numvar_choices)[numvar_choices == input$x]
    
    sliderInput(
      inputId = "x_range",
      label = paste("Filter Range for", x_label),
      min = x_min_val,
      max = x_max_val,
      value = c(x_min_val, x_max_val) # Default to showig full range
    )
  })
  
  # Reactive expression to filter the data based on selected checkboxes
  filtered_data <- reactive({
    req(input$selected_months) 
    req(input$selected_genders)
    req(input$x)
    
    # Separate the base data from the dynamic slider condition checks
    base_data <- my_data |> 
      filter(birth_month %in% input$selected_months) |>
      filter(gender %in% input$selected_genders)
    
    # FIXED: Check if the dynamic slider has finished initializing on screen before filtering
    if (!is.null(input$x_range)) {
      base_data <- base_data |>
        filter(.data[[input$x]] >= input$x_range[1] & .data[[input$x]] <= input$x_range[2])
    }
    
    base_data
  })
  
  output$scatterplot <- renderPlot({
    #Prepare background stuff
    
    x_label <- names(numvar_choices)[numvar_choices == input$x]
    y_label <- names(numvar_choices)[numvar_choices == input$y]
    
    req(nrow(filtered_data()) > 0)
    ggplot(filtered_data(), aes(x = .data[[input$x]], y = .data[[input$y]])) +
      geom_point() +
      labs(
        title = paste(y_label, "vs.", x_label),
        x = x_label,
        y = y_label
      ) +
      theme_minimal()
  })
  #output$distPlot <- renderPlot({
  #  students_from_selected_months <- censusAtSchool2026 %>%
  #    filter(birth_month == input$birth_month) %>%
  #    select(title:birth_month)
  #})
  
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
