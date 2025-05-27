library(tidyverse)
library(rsconnect)
library(DT)
library(shiny)

# time zone set to nyc
`attr<-`(as.POSIXct(Sys.time()), "tzone", "America/New_York")

setwd("~/Dropbox/Osler/Osler_shiny")

#creating a single table, commmented out for faster loading

# # # List all .tsv files in the directory
# intern_file_list <- list.files('intern_schedule/', pattern = "\\.tsv$", full.names = TRUE)
# jarsar_file_list <- list.files('jar_sar_schedule/', pattern = "\\.tsv$", full.names = TRUE)
# 
# # Read each .tsv file into a list of data frames
# data_list <- lapply(c(intern_file_list, jarsar_file_list), read_delim)
# 
# # Convert columns to consistent types
# data_list <- lapply(data_list, function(df) {
#   # Example: Ensure 'name' column is character
#   if ("name" %in% colnames(df)) {
#     df$name <- as.character(df$name)
#   }
#   # Add more conversions if needed
#   return(df)
# })
# 
# # Combine all data frames into one
# combined_data <- bind_rows(data_list)
# 
# # Print the combined data frame
# print(combined_data)
# 
# new_table = combined_data  %>% pivot_wider(
#   names_from = "date",
#   names_prefix = "",
#   values_from = "schedule",
#   values_fn = list(schedule = ~ paste(., collapse = "/"))  # Combine duplicates into a comma-separated string
# )
# 
# 
# add_asterisk <- function(vec) {
#   # Apply the condition to each element in the vector
#   vec <- ifelse(grepl("OPH|ED", vec), paste0(".", vec), vec)
#   return(vec)
# }
# 
# new_table$name = add_asterisk(new_table$name)
# # Create a logical vector to separate alphabetic from non-alphabetic starting names
# is_alpha <- grepl("^[A-Za-z]", new_table$name)
# new_table <- new_table[order(!is_alpha, new_table$name), ]
# # 
# # 
# write_delim(new_table, file='~/Dropbox/Osler/Osler_shiny/schedule/osler_schedule_table.tsv', delim='\t')
new_table = read_delim('schedule/osler_schedule_table.tsv', delim='\t')
intern_block_table = read_delim('schedule/intern_block_view.tsv', delim='\t')
jarsar_block_table = read_delim('schedule/jar_sar_block_view.tsv', delim='\t')


date_columns <- as.Date(names(new_table)[-1], format = "%m-%d-%Y")
today_date <- as.Date(Sys.Date(), "%m/%d/%y")
today_col_name <- as.character(today_date)

unique_individual_names = unique(new_table$name)




# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    tags$link(rel = "manifest", href = "www/site.webmanifest")
  ),
  tags$div(
    style = "display: flex; align-items: center;",  # Flexbox container to align items horizontally
    img(src = "apple-touch-icon.png", height = "50px", width = "50px"),  # Image next to the title
    tags$h1(" Osler Resident Schedule", style = "margin-right: 15px;"),  # Title with some margin
    
  ),
  tags$title("Osler Resident Schedule"), 
  # Add a toggle switch to choose between original and new table
  
  # Apply CSS for text font and size adjustments
  tags$style(HTML("
    .container-fluid {
      padding-left: 4px;  /* Remove the default padding on the left */
      padding-right: 4px; /* Optional: Remove the right padding */
    }
    table.dataTable, thead th {
      font-family: 'Courier New', Courier, monospace;  /* Set table font to Courier */
      font-size: 12px;  /* Adjust font size */
    }
    thead th {
      font-weight: bold;  /* Bold header */
    }
    .shiny-input-container, .shiny-date-input, .shiny-select-input,
    .selectize-input, .form-control, .shiny-input-container label {
      font-size: 12px;  /* Decrease font size across inputs and labels */
    }
    .shiny-date-input, .shiny-select-input {
      width: 150px;  /* Set width for date and select inputs */
    }
    .selectize-input, .form-control {
      width: 120px;  /* Set width for selected values */
    }
    /* Add some padding adjustments for smaller screens */
    @media (max-width: 768px) {
      .container-fluid {
        padding-left: 0px; 
        padding-right: 0px;
      }
      .col-xs-2 {
        width: 100px; /* Adjust column width for mobile */
        padding: 2px 5px; /* Remove extra padding on mobile */
      }
      /* Ensure uniform height for labels */
      .shiny-input-container label {
        min-height: 20px; /* Force labels to have the same height */
      }
    }
      /* Custom style for title font size */
      h1 {
        font-size: 25px;  /* Adjust the font size of the title */
        color: #2E4053;   /* Optional: change the color of the title */
      }
  ")),
  
  # Place the date range input above the table
  column(2, checkboxInput("show_table", "Blocks", FALSE)),

  # Default Table Filters (shown when checkbox is unchecked)
  conditionalPanel(
    condition = "input.show_table == false",
    column(2, dateInput("date", "Select Date:", value = Sys.Date())),
    column(2, class = "col-xs-2", 
           # Filter by individual name
           selectInput("name_choice", "Name(s):",
                       choices = c("", unique_individual_names),  # Replace with your list of names
                       selected = NULL, multiple=TRUE)),
    column(2, selectInput("rotation", "Rotation:", choices = c("All", "Janeway", "Barker", "Longcope", "Thayer", "Brancati", "MICU", "CCU", "MPC", "Subspecialty", "Ambulatory", "MTL", "Solids", "Leuks", "PCCU"), selected = "All")),
    column(2, selectInput("callSchedule", "Call Schedule:", choices = c("All", "CALL", "POST", "OFF", "DAY", "GOOD", "NIGHT"), selected = "All"))
  ),
  
  # Intern & JAR/SAR Table Filters (shown when checkbox is checked)
  conditionalPanel(
    condition = "input.show_table == true",
    column(2, selectInput("role_choice", "PGY", choices = c("Intern", "JAR/SAR"))),
    column(2, selectInput("Block", "Block:", choices = c("All", "1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B", "5A", "5B", "6A", "6B", "7A", "Holiday 1", "Holiday 2", "7B", "8A", "8B", "9A", "9B", "10A", "10B", "11A", "11B", "12A", "12B", "13A", "13B"))),
    column(2, uiOutput("conditional_name_selector")),
    column(2, selectInput("Rotation", "Rotation:", choices = c("All", "Janeway", "Barker", "Longcope", "Thayer", "Brancati", "MICU", "CCU", "MPC", "Subspecialty", "Ambulatory", "MTL", "Solids", "Leuks", "PCCU"))),
  ),

hr(),  # Horizontal line to separate UI filters from table output

# Table outputs
conditionalPanel(condition = "input.show_table == false", DTOutput("default_table")),
conditionalPanel(condition = "input.show_table == true", DTOutput("conditional_table"))
)


# Define server logic
# Define server logic

date_columns <- as.Date(names(new_table)[-1], format = "%m-%d-%Y")
today_date <- as.Date(Sys.Date(), "%m/%d/%y")
today_col_name <- as.character(today_date)



server <- function(input, output, session) {
  output$conditional_name_selector <- renderUI({
    req(filtered_conditional_data())  # Ensure data exists
    selectInput("name_choice", "Name(s):", choices = c("All", unique(filtered_conditional_data()$Name)), selected = NULL, multiple = TRUE)
  })
  
  
  
  
  ## Default Table Logic ##
  filtered_default_data <- reactive({
    req(new_table)  # Ensure the data exists
    data <- new_table
    selected_start <- input$date
    
    
    # Filter columns based on the selected date range
    selected_columns <- names(data)[which(date_columns >= selected_start) + 1]  # +1 to account for 'ID' column
    

    
    # Subset the dataset to include only the 'ID' column and the filtered date columns
    data <- data[c('name', selected_columns)]
    
    # Apply Name filter if any names are selected
    if (!is.null(input$name_choice) && length(input$name_choice) > 0) {
      data <- data %>% filter(name %in% input$name_choice)
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
  
  output$conditional_name_selector <- renderUI({
    req(filtered_conditional_data())  # Ensure data exists
    selectInput("name_choice", "Name(s):", choices = c("All", unique_individual_names), selected = NULL, multiple = TRUE)
  })
  
  
  # Render default table
  output$default_table <- renderDT({
    filtered_default_data()
  }, options = list(pageLength = 10))
  
  
  # BLOCK VIEW 
  ## Conditional Table Logic (when checkbox is checked) ##
  filtered_conditional_data <- reactive({
    req(input$show_table)
    
    data <- if (input$role_choice == "JAR/SAR") jarsar_block_table else intern_block_table
    
    print("Before filtering:")
    print(head(data))
    
    if (!is.null(input$name_choice) && length(input$name_choice) > 0 && input$name_choice != "All") {
      data <- data %>% filter(Name %in% input$name_choice)
    }
    
    if (input$Block != "All") {
      data <- data %>% filter(Block == input$Block)
    }
    
    if (input$Rotation != "All") {
      data <- data %>% filter(Rotation == input$Rotation)
    }
    
    if (!is.null(input$shift_choice) && input$shift_choice != "All") {
      data <- data %>% filter(Shift == input$shift_choice)
    }
    
    print("After filtering:")
    print(head(data))
    
    return(data)
  })

  
  
  # Render conditional table
  output$conditional_table <- renderDT({
    filtered_conditional_data()
  }, options = list(pageLength = 10))
}
# Run the application 
shinyApp(ui = ui, server = server)

# # 
# # 
# rsconnect::setAccountInfo(name='cyoon14',
#                           token='773EBAD1D531698A8B9BB0CC651EC109',
#                           secret='cwUWIrEHyC6EULoGBaDwMz69pM6fw/50Bq5QrNww')
# 
# 
# rsconnect::deployApp(forceUpdate = TRUE)

