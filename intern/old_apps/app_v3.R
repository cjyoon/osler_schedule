library(shiny)
library(tidyverse)
library(DT)

# Read tables
new_table <- read_delim('schedule/osler_schedule_table.tsv', delim='\t')
intern_block_table <- read_delim('schedule/intern_block_view.tsv', delim='\t')
jarsar_block_table <- read_delim('schedule/jar_sar_block_view.tsv', delim='\t')

# Extract unique names for dropdown
unique_individual_names <- unique(new_table$name)

# Define UI
ui <- fluidPage(
  fluidRow(
    column(2, checkboxInput("show_table", "Blocks", FALSE)),
    
    # Default Table Filters (shown when checkbox is unchecked)
    conditionalPanel(
      condition = "input.show_table == false",
      column(2, dateInput("date", "Select Date:", value = Sys.Date())),
      column(3, selectInput("individual", "Name(s):", choices = unique_individual_names, selected = NULL, multiple = TRUE)),
      column(2, selectInput("rotation", "Rotation:", choices = c("All", "Janeway", "Barker", "Longcope", "Thayer", "Brancati", "MICU", "CCU", "MPC", "Subspecialty", "Ambulatory", "MTL", "Solids", "Leuks", "PCCU"), selected = "All")),
      column(2, selectInput("callSchedule", "Call Schedule:", choices = c("All", "CALL", "POST", "OFF", "DAY", "GOOD", "NIGHT"), selected = "All"))
    ),
    
    # Intern & JAR/SAR Table Filters (shown when checkbox is checked)
    conditionalPanel(
      condition = "input.show_table == true",
      column(2, selectInput("role_choice", "PGY", choices = c("Intern", "JAR/SAR"))),
      column(2, selectInput("Block", "Block:", choices = c("All", "1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B", "5A", "5B", "6A", "6B", "7A", "Holiday 1", "Holiday 2", "7B", "8A", "8B", "9A", "9B", "10A", "10B", "11A", "11B", "12A", "12B", "13A", "13B"))),
      column(3, uiOutput("conditional_name_selector")),
      column(2, selectInput("Rotation", "Rotation:", choices = c("All", "Janeway", "Barker", "Longcope", "Thayer", "Brancati", "MICU", "CCU", "MPC", "Subspecialty", "Ambulatory", "MTL", "Solids", "Leuks", "PCCU"))),
      column(2, selectInput("shift_choice", "Shift:", choices = c("All", "Call", "Post")))
    )
  ),
  
  hr(),  # Horizontal line to separate UI filters from table output
  
  # Table outputs
  conditionalPanel(condition = "input.show_table == false", DTOutput("default_table")),
  conditionalPanel(condition = "input.show_table == true", DTOutput("conditional_table"))
)

# Define server logic
server <- function(input, output, session) {
  ## Default Table Logic ##
  filtered_default_data <- reactive({
    req(new_table)  # Ensure the data exists
    data <- new_table
    
    # Apply Name filter if any names are selected
    if (!is.null(input$individual) && length(input$individual) > 0) {
      data <- data %>% filter(name %in% input$individual)
    }
    
    # Apply Rotation filter if not "All"
    if (input$rotation != "All") {
      data <- data %>% filter(schedule == input$rotation)
    }
    
    # Apply Call Schedule filter if not "All"
    if (input$callSchedule != "All") {
      data <- data %>% filter(schedule == input$callSchedule)
    }
    
    return(data)
  })
  
  # Render default table
  output$default_table <- renderDT({
    filtered_default_data()
  }, options = list(pageLength = 10))
  
  ## Conditional Table Logic (when checkbox is checked) ##
  filtered_conditional_data <- reactive({
    req(input$show_table)  # Ensure checkbox is checked
    
    # Default to Intern table
    data <- intern_block_table
    
    # If JAR/SAR is selected, switch to that table
    if (input$role_choice == "JAR/SAR") {
      data <- jarsar_block_table
    }
    
    # Apply Name filter if multiple names are selected
    if (!is.null(input$name_choice) && length(input$name_choice) > 0 && input$name_choice != "All") {
      data <- data %>% filter(Name %in% input$name_choice)
    }
    
    # Apply Block filter if not "All"
    if (input$Block != "All") {
      data <- data %>% filter(Block == input$Block)
    }
    
    # Apply Rotation filter if not "All"
    if (input$Rotation != "All") {
      data <- data %>% filter(Rotation == input$Rotation)
    }
    
    # Apply Shift filter if not "All"
    if (input$shift_choice != "All") {
      data <- data %>% filter(Shift == input$shift_choice)
    }
    
    return(data)
  })
  
  # Update Name selection dropdown for conditional table
  output$conditional_name_selector <- renderUI({
    req(filtered_conditional_data())  # Ensure data exists
    selectInput("name_choice", "Name(s):", choices = c("All", unique(filtered_conditional_data()$Name)), selected = NULL, multiple = TRUE)
  })
  
  # Render conditional table
  output$conditional_table <- renderDT({
    filtered_conditional_data()
  }, options = list(pageLength = 10))
}

# Run the application
shinyApp(ui = ui, server = server)
