library(tidyverse)
library(rsconnect)
library(DT)
library(shiny)

# time zone set to nyc
`attr<-`(as.POSIXct(Sys.time()), "tzone", "America/New_York")

# creating a single table, commmented out for faster loading

# # List all .tsv files in the directory
# file_list <- list.files('data/', pattern = "\\.tsv$", full.names = TRUE)
# 
# # Read each .tsv file into a list of data frames
# data_list <- lapply(file_list, read_delim)
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

# write_delim(new_table, file='~/Dropbox/Osler/osler/data/osler_schedule_table.tsv', delim='\t')
new_table = read_delim('schedule/osler_schedule_table.tsv', delim='\t')
block_table = read_delim('schedule/block_view.tsv', delim='\t')
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
    tags$h1(" Osler Intern Schedule", style = "margin-right: 15px;"),  # Title with some margin
    
  ),
  tags$title("Osler Intern Schedule"), 
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
  fluidRow(
    column(3, checkboxInput("toggle_table", "Block schedule", value = FALSE)),  # Toggle switch
    
    column(3, class = "col-xs-2", 
           # Date range input for selecting start and end date
           dateInput("date", "Select Date:", value = today_date) 
    ), 
    column(3, class = "col-xs-2", 
           # Filter by individual name
           selectInput("individual", "Individual:",
                       choices = c("", unique_individual_names),  # Replace with your list of names
                       selected = NULL, multiple=TRUE)
    ),
    column(3, class = "col-xs-2", style = "padding-left:0px;", 
           # Filter by call schedule
           selectInput("callSchedule", "Call Schedule:",
                       choices = c("", "CALL", "POST", "OFF", "DAY", "GOOD"), 
                       selected = NULL)
    )
  ),
  
  fluidRow(
    column(12, 
           # Display the filtered datatable
           DTOutput("table")
    )
  )
)

# Define server logic

server <- function(input, output, session) {
  Sys.setenv(TZ = "America/New_York")
  
  # Get today's date in the proper format for column names
  today_date <- as.Date(Sys.Date(), "%m/%d/%y")
  today_col_name <- as.character(today_date)

  # Render the interactive datatable
  output$table <- renderDT({
    if(input$toggle_table){ # if want to view in block view
      individual = input$individual
      # if specific filter is applied
      if(!is.null(individual)){
        block_table = block_table %>% filter(name %in% individual)
        
      }
      
      
      
      datatable(
        block_table, 
        options = list(
          pageLength = 100, 
          autoWidth = TRUE,
          autoWidth = TRUE,
          scrollX = TRUE,                # Enable horizontal scrolling
          scrollY = "1000px",             # Vertical scroll for table height
          fixedColumns = list(leftColumns = 1),  # Fix the first column (row names)
          fixedHeader = TRUE,            # Fix the header when scrolling vertically
          lengthChange = FALSE,   # Disable the "Show entries" dropdown
          searching = FALSE,       # Disable the search box
          
          columnDefs = list(list(className = 'dt-center', targets = "_all")), # Center-align all columns
          # Use JS to scroll to today's column when initialized
          initComplete = JS(
            paste0(
              "function(settings, json) {",
              "var colIndex = settings.aoColumns.map(function(col){ return col.sTitle; }).indexOf('", today_col_name, "');",
              "if(colIndex > -1) {",
              "  var tableWrapper = $(this.api().table().container());",
              "  var scrollBody = tableWrapper.find('.dataTables_scrollBody');",
              "  var header = tableWrapper.find('thead th:eq(' + colIndex + ')');",
              "  var headerPosition = header.position();",
              "  scrollBody.scrollLeft(headerPosition.left);",  # Scroll to today's date
              "}",
              "}"
            )
          )
        ),
        rownames = FALSE,   # Hide row numbers if not needed, or set to TRUE if needed
        extensions = 'FixedColumns'
      )
    }else{
      selected_start <- input$date
      
      char_filter <- input$charFilter  # Get the input character filter
      
      
      individual = input$individual
      callSchedule = input$callSchedule
      
      # Filter columns based on the selected date range
      selected_columns <- names(new_table)[which(date_columns >= selected_start) + 1]  # +1 to account for 'ID' column
      
      
      
      
      # Subset the dataset to include only the 'ID' column and the filtered date columns
      new_table <- new_table[c('name', selected_columns)]
      
      # if specific filter is applied
      if(!is.null(individual)){
        new_table = new_table %>% filter(name %in% individual)
        
      }
      
      selected_start_col_name <- as.character(format(selected_start, "%m-%d-%Y"))  # Ensure it's a string
      
      # Filter by call schedule if selected
      if (!is.null(callSchedule) && callSchedule != "") {
        # Use str_detect to filter for shifts that contain the selected schedule pattern
        new_table <- new_table %>%
          filter_at(vars(ends_with(selected_start_col_name)), any_vars(str_detect(., callSchedule)))
      }
      
      
      # Get date columns (assuming first column is 'name')
      date_columns <- names(new_table)[-1]
      
      # Extract days of the week for each date
      days_of_week <- format(as.Date(date_columns, format = "%m-%d-%Y"), "%a")  # "%a" gives abbreviated day names like Mon, Tue, etc.
      
      # Create new column names that include both the date and the day of the week
      new_column_names <- paste(date_columns, "\n", days_of_week, sep = "")
      
      # Update column names in new_table (excluding the 'name' column)
      colnames(new_table)[-1] <- new_column_names
      
      
      
      datatable(
        new_table, 
        options = list(
          pageLength = 100, 
          autoWidth = TRUE,
          autoWidth = TRUE,
          scrollX = TRUE,                # Enable horizontal scrolling
          scrollY = "800px",             # Vertical scroll for table height
          fixedColumns = list(leftColumns = 1),  # Fix the first column (row names)
          fixedHeader = TRUE,            # Fix the header when scrolling vertically
          paging = FALSE,        # Disable pagination
          lengthChange = FALSE,   # Disable the "Show entries" dropdown
          searching = FALSE,       # Disable the search box
          
          columnDefs = list(list(className = 'dt-center', targets = "_all")), # Center-align all columns
          # Use JS to scroll to today's column when initialized
          initComplete = JS(
            paste0(
              "function(settings, json) {",
              "var colIndex = settings.aoColumns.map(function(col){ return col.sTitle; }).indexOf('", today_col_name, "');",
              "if(colIndex > -1) {",
              "  var tableWrapper = $(this.api().table().container());",
              "  var scrollBody = tableWrapper.find('.dataTables_scrollBody');",
              "  var header = tableWrapper.find('thead th:eq(' + colIndex + ')');",
              "  var headerPosition = header.position();",
              "  scrollBody.scrollLeft(headerPosition.lef0t);",  # Scroll to today's date
              "}",
              "}"
            )
          )
        ),
        rownames = FALSE,   # Hide row numbers if not needed, or set to TRUE if needed
        extensions = 'FixedColumns'
      )
    }

    }
  )
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

