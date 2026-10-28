# Load packages ----------------------------------------------------------------

library(shiny)
#library(bslib)
library(tidyverse)
library(DT)
library(shinycssloaders)
options("shiny.sanitize.errors" = FALSE) # Turn off error sanitization

# Load data, small susbetting and building objects -----------------------------

my_data <- read.csv("data/CensusAtSchool2026.csv", na.strings = c("", "NA"))
clean_age <- na.omit(my_data$ageyears)
clean_travel <- na.omit(my_data$travel_time_to_school)
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
catvar_choices <- c(
  "Gender" = "gender", 
  "Handedness" = "handed", 
  "Commute Method" = "travel_to_school", 
  "Birth Month" = "birth_month", 
  "Favorite Season" = "favorite_season",
  "Has Allergies" = "allergies",
  "Vegetarian Status" = "vegetarian",
  "Favorite Subject" = "favorite_school_subject",
  "Internet Access" = "home_internet_access",
  "Preferred Communication Method" = "communication_with_friends"
)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Categorical Mods
      selectInput(inputId = "selected_months",
                  label = "Select Birth Month(s):",
                  choices = available_months,
                  selected = "January",
                  multiple = TRUE),
      checkboxGroupInput(inputId = "selected_genders", 
                         label = "Gender of respondents:", 
                         choices = available_genders,
                         selected = available_genders),
      
      # Sliders for numberic subsetting
      selectInput(inputId = "num_var1", 
                  label = "First Numeric Variable Filter:",
                  choices = numvar_choices,
                  selected = "ageyears"),
      uiOutput("slider1"),
      selectInput(inputId = "num_var2", 
                  label = "Second Numeric Variable Filter:",
                  choices = numvar_choices,
                  selected = "classgrade"),
      uiOutput("slider2"),
      
      # Action Button Execution Lock
      actionButton("filter_btn", "Apply Sidebar Filters", 
                   class = "btn-primary btn-block w-100", icon = icon("filter"))
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        # 1. ABOUT Tab
        tabPanel("About",
                 fluidRow(
                   column(12,
                          br(),
                          tags$h3("Project Overview"),
                          p("This R Shiny application was created to explore data from the ASA's Census@School project.",
                            "While the full dataset has more variables, I've selected about a half dozen that can be used to filter the data and then conduct some light EDA.", 
                            "More inforamtion can be found on the ASA's website (link below) and you can take your students to the site to add their own responses to the survey."),
                          tags$h4("Dataset Context"),
                          p("The data originates from an expansive student census profile documenting baseline metrics such as school transit times,",
                            "preferred extra-curricular structures, digital usage baselines, and environmental profiles. Learn more at the ",
                            tags$a(href = "https://ww2.amstat.org/censusatschool/", "Census at School - United States", target = "_blank")),
                          tags$h4("Layout Inforamtion"),
                          tags$ul(
                            tags$li(strong("Sidebar Panel:"), "Use the checkboxes and sliders to select the subset of the data you want, then use the action button to update the request from the full dataset."),
                            tags$li(strong("Data Download Tab:"), "Download a filtered spreadsheet of the data you selected from the sidebar."),
                            tags$li(strong("Data Exploration Tab:"), "View some graphs, tables, and metrics on various combinations of variables you chose.")
                          ),
                          br(),
                          # Image from C@S webiste
                          tags$img(
                            src = "https://lh3.googleusercontent.com/d/1fjY8J4Bylq91wRQMvtJZGxIy_90A-esd=w1920" , 
                            alt = "Census at School Logo",
                            style = "max-width: 100%; height: auto; display: block; margin-bottom: 20px;"
                          )
                   )
                 )
        ),
        
        # 2. DOWNLOAD Tab
        tabPanel("Data Download",
                 br(),
                 downloadButton("download_csv", "Download Filtered Dataset (.csv)"),
                 br(), br(),
                 withSpinner(DT::dataTableOutput("reactive_table"))
        ),
        
        # 3. EDA Tab
        tabPanel("Data Exploration",
                 br(),
                 fluidRow(
                   # Give them a place to select their vars
                   column(4,
                          #tags$h4("Options for Graphs and Summary Data"),
                          #radioButtons("expl_mode", "EDA Focus:",
                          #choices = c("Categorical Summaries" = "cat", "Numeric Variable Summaries" = "num")),
                          
                          # Dynamic variable selections for mapping graphs
                          # This is to create 4 selectors: X & Y vars, plus a third that will add fill color. The last will turn on facets.
                          selectInput("plot_x", "Primary X-Axis Variable:",
                                      choices = c(catvar_choices, numvar_choices)),
                          selectInput("plot_y", "Quantitative Y-Axis (For Box/Scatter plots Only):",
                                      choices = c("None" = "none", numvar_choices)),
                          selectInput("plot_fill", "Grouping Modifier / Color Split:",
                                      choices = c("None" = "none", "Gender" = "gender", "Handedness" = "handed", "Vegetarian Status" = "vegetarian")),
                          selectInput("plot_facet", "Faceting Variable Modifier:",
                                      choices = c("None" = "none", "Gender" = "gender", "Handedness" = "handed", "Internet Access" = "home_internet_access", "Favorite Season" = "favorite_season", "Type of Role Model" = "role_model_type"))
                   ),
                   # Now display the output graps
                   column(8,
                          tags$h4("Outputs"),
                          tabsetPanel(
                            tabPanel("Graphical View", br(), withSpinner(plotOutput("exploration_plot"))),
                            tabPanel("Numerical View", br(), withSpinner(uiOutput("exploration_tables")))
                          )
                   )
                 )
        )
      )
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Double slideer for my x and y variables, usin renderUI to make dynamic as 
  #  different x-vars are selected.
  output$slider1 <- renderUI({
    req(input$num_var1)
    num_vector_1 <- my_data[[input$num_var1]]
    num_min_1 <- min(num_vector_1)
    num_max_1 <- max(num_vector_1)
    num_lab_1 <- names(numvar_choices)[numvar_choices == input$num_var1]
    
    sliderInput(
      inputId = "slider1",
      label = paste("Filter Range for", num_lab_1),
      min = num_min_1,
      max = num_max_1,
      value = c(num_min_1, num_max_1) # Default to showig full range
    )
  })
  output$slider2 <- renderUI({
    req(input$num_var2)
    num_vector_2 <- my_data[[input$num_var2]]
    num_min_2 <- min(num_vector_2)
    num_max_2 <- max(num_vector_2)
    num_lab_2 <- names(numvar_choices)[numvar_choices == input$num_var2]
    
    sliderInput(
      inputId = "slider2",
      label = paste("Filter Range for", num_lab_2),
      min = num_min_2,
      max = num_max_2,
      value = c(num_min_2, num_max_2)
    )
  })
  
  # Reactive expression to filter the data based on selected checkboxes
  final_data <- reactive({
    req(input$slider1)
    req(input$slider2)
    
    # Separate the base data from the dynamic slider condition checks
    base_data <- my_data |> 
      filter(birth_month %in% input$selected_months) |>
      filter(gender %in% input$selected_genders)
    
    # Separate the base data from the dynamic slider condition checks
    base_data <- base_data |>
      filter(.data[[input$num_var1]] >= input$slider1[1] & .data[[input$num_var1]] <= input$slider1[2]) |>
      filter(.data[[input$num_var2]] >= input$slider2[1] & .data[[input$num_var2]] <= input$slider2[2])
    
    base_data
  })
  
  #Let's see the data table. If this works, then we should be good for graphing.
  output$reactive_table <- DT::renderDataTable({
    DT::datatable(final_data(), options = list(pageLength = 10))
  })
  #Download button
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("filtered_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(final_data(), file)
    }
  ) # and that's a win folks. Downloading is working.
  
  # Now we have to make the plots. First let's envision what each combination of input vars looks like.
  output$exploration_plot <- renderPlot({
    req(input$plot_x)
    req(input$plot_y)
    
    # Beginning to make plots: One-way plots
    if(input$plot_y == "none") {
      my_plot <- ggplot(final_data(), aes(x = .data[[input$plot_x]]))  #setup plot
      
      # 1. Single categorical variable section
      if(input$plot_fill != "none") {
        #Single cat var + color "third" var as fill (input$plot_fill)
        my_plot <- my_plot + aes(fill = .data[[input$plot_fill]]) + geom_bar(position = "dodge")
      } else {
        #Straight single cat var ()
        my_plot <- my_plot + geom_bar(fill = "steelblue")
      }
      my_plot <- my_plot + labs(title = paste("Distribution of", input$plot_x), y = "Total Count Responses")
    } else {  # 2. For 2-way plots
      # setup plot again
      my_plot <- ggplot(final_data(), aes(x = .data[[input$plot_x]], y = .data[[input$plot_y]]))
      
      #If X is numeric, and y is categorical
      if(input$plot_x %in% numvar_choices && input$plot_y %in% catvar_choices) { 
        my_plot <- my_plot + geom_histogram(aes(x = .data[[input$plot_x]], fill = .data[[input$plot_y]]), position = "stack", bins = 20, color = "white", alpha = 0.8) +
          labs(title = paste("Stacked Histogram of", input$plot_x, "by", input$plot_y), x = input$plot_x, y = "Count")
      } else if(input$plot_x %in% numvar_choices && input$plot_y %in% numvar_choices) { 
        my_plot <- my_plot + geom_point(color = "steelblue", size = 2.5, alpha = 0.7) +
          labs(title = paste("Scatterplot of", input$plot_x, "vs.", input$plot_y), x = input$plot_x, y = input$plot_y)
      } else if(input$plot_x %in% catvar_choices && input$plot_y %in% numvar_choices) {
        # This probably shouldn't happen. It's not great to put the numeric on the y, so if they do, I swap them here to make it look like if they had swapped the variables, otherwise R is going to lose its mind.
        my_plot <- my_plot + geom_histogram(aes(x = .data[[input$plot_y]], y = .data[[input$plot_x]]), position = "stack", binwidth = 1, color = "white", alpha = 0.8) +
          labs(title = paste("Stacked Histogram of", input$plot_y, "by", input$plot_x), x = input$plot_y, y = "Count")
      } else {
        # double categorical, re-explain the inputs to the function with a new aes()
        my_plot <- my_plot + geom_bar(aes(x = .data[[input$plot_x]], fill = .data[[input$plot_y]]), position = "dodge") + labs(y = "Count")
      }
    }
    # Global plot theme rules an faceting integration
    my_plot <- my_plot + theme_minimal(base_size = 14) +
      # theme(axis.text.x = element_text(angle = 45, hjust = 1),
      theme(plot.title = element_text(face = "bold", size = 16, margin = margin(b=10))) +
      labs(x = input$plot_x)
    if(input$plot_facet != "none") {
      my_plot <- my_plot + facet_wrap(vars(.data[[input$plot_facet]]))
    }  # If facetwrap isn't turned on, just ignore, no else statement.
    print(my_plot)
  })
  
  
  # Numerical Data Summary Table Computations Engine
  output$exploration_tables <- renderUI({
    plot_df <- final_data()
    if(input$plot_x %in% catvar_choices) {
      
      # 1 & 2-way Contingency Table Calculations Block
      tbl1 <- table(plot_df[[input$plot_x]])
      df1 <- as.data.frame(tbl1) %>% rename(!!input$plot_x := Var1, Count = Freq)
      out_list <- list(
        tags$h5(paste("One-Way Frequency Count Table:", input$plot_x)),
        renderTable(df1, striped = TRUE, bordered = TRUE)
      )
      if(input$plot_fill != "none") {
        tbl2 <- table(plot_df[[input$plot_x]], plot_df[[input$plot_fill]])
        df2 <- as.data.frame(tbl2) %>% rename(!!input$plot_x := Var1, !!input$plot_fill := Var2, Count = Freq)
        out_list <- c(out_list, list(
          hr(),
          tags$h5(paste("Two-Way Cross Contingency Table:", input$plot_x, "by", input$plot_fill)),
          renderTable(df2, striped = TRUE, bordered = TRUE)
        ))
      }
      return(out_list)
    } else {
      # Numerical Variable Distribution Summaries across group levels
      sum_df <- plot_df |>
        group_by(.data[[input$plot_x]]) |>
        summarize(
          Observations = n(),
          Mean         = mean(.data[[input$plot_y]], na.rm = TRUE),
          Median       = median(.data[[input$plot_y]], na.rm = TRUE),
          Min          = min(.data[[input$plot_y]], na.rm = TRUE),
          Max          = max(.data[[input$plot_y]], na.rm = TRUE),
          MAD          = mad(.data[[input$plot_y]], na.rm = TRUE),
          Std_Dev      = sd(.data[[input$plot_y]], na.rm = TRUE)
        )
      return(list(
        tags$h5(paste("Descriptive Summary Metrics:", input$plot_y, "by", input$plot_x)),
        renderTable(sum_df, striped = TRUE, bordered = TRUE)
      ))
    }
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
