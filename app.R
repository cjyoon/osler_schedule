library(tidyverse)
library(rsconnect)
library(DT)
library(shiny)
#
# time zone set to nyc
Sys.setenv(TZ = "America/New_York")
# 
# 
# setwd("~/Dropbox/Osler/Osler_shiny")
# 
# 
# #creating a single table, commmented out for faster loading
# 
# # # # List all .tsv files in the directory
# intern_file_list <- list.files('intern_schedule/', pattern = "\\.tsv$", full.names = TRUE)
# jarsar_file_list <- list.files('jar_sar_schedule/', pattern = "\\.tsv$", full.names = TRUE)
# 
# jarsar_file_list2 <- list.files('jarsar2025/schedule//', pattern = "\\.tsv$", full.names = TRUE)
# 
# # Read each .tsv file into a list of data frames
# data_list <- lapply(c(intern_file_list, jarsar_file_list, jarsar_file_list2), read_delim)
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
# write_delim(new_table, file='~/Dropbox/Osler/Osler_shiny/osler_schedule_table.tsv', delim='\t')


find_block <- function(input_date, block_table) {
  # Extract column names
  col_names <- colnames(block_table)
  
  # Create a data frame to store block names and date ranges
  blocks <- data.frame(Block = character(), Start = as.Date(character()), End = as.Date(character()), stringsAsFactors = FALSE)
  
  # Regular expression to extract block names and date ranges
  pattern <- "(.+?)\\s(\\d{1,2}/\\d{1,2}/\\d{2})-(\\d{1,2}/\\d{1,2}/\\d{2})"
  
  for (col in col_names) {
    matches <- regmatches(col, regexec(pattern, col))
    if (length(matches[[1]]) == 4) {
      block_name <- matches[[1]][2]
      start_date <- as.Date(matches[[1]][3], format="%m/%d/%y")
      end_date <- as.Date(matches[[1]][4], format="%m/%d/%y")
      
      # Append to blocks data frame
      blocks <- rbind(blocks, data.frame(Block = block_name, Start = start_date, End = end_date, stringsAsFactors = FALSE))
    }
  }
  
  # Convert input_date to Date format
  input_date <- as.Date(input_date, format="%Y-%m-%d")
  
  # Find the corresponding block
  matching_block <- blocks$Block[input_date >= blocks$Start & input_date <= blocks$End]
  
  # Return the block name or NA if not found
  if (length(matching_block) == 0) {
    return('1A')
  } else {
    return(matching_block)
  }
}

# Function to filter blocks and return the subset
filter_blocks <- function(block_id, block_table) {
  col_names <- colnames(block_table)

  # Find the index of the given block_id
  match_index <- which(grepl(paste0("^", block_id, " "), col_names))

  # If block_id is found, subset the table
  if (length(match_index) > 0) {
    return(block_table[, c(1, match_index:ncol(block_table))])  # Keep 'name' column
  } else {
    return(block_table)  # Return full table if block_id is not found
  }
}
# filter_blocks <- function(block_id, block_table) {
#   col_names <- colnames(block_table)
#   
#   # Find the indices of all matching block_id columns
#   match_indices <- which(grepl(paste0("^", block_id, " "), col_names))
#   
#   # If two or more matches, select only the second one
#   if (length(match_indices) >= 2) {
#     return(block_table[, c(1, match_indices[2])])  # Keep 'name' column + second match
#   } else if (length(match_indices) == 1) {
#     return(block_table[, c(1, match_indices[1])])  # Only one match found
#   } else {
#     return(block_table)  # Return full table if no match found
#   }
# }
remove_unnamed_columns <- function(df) {
  df <- df[, !grepl("^Unnamed", colnames(df))]
  df = df %>% filter(!is.na(name))
  return(df)
}

new_table = read_delim('osler_schedule_table.tsv', delim='\t')
intern_block_table = remove_unnamed_columns(read_delim('block_schedule/intern_block_view.tsv', delim='\t'))
jarsar_block_table = remove_unnamed_columns(read_delim('block_schedule/jar_sar_block_view.tsv', delim='\t'))
jarsar_block_table2 = remove_unnamed_columns(read_delim('block_schedule/block_view_jarsar2025.tsv', delim='\t'))

jarsar_block_table = jarsar_block_table %>% full_join(jarsar_block_table2, by='name')

date_columns <- as.Date(names(new_table)[-1], format = "%m-%d-%Y")
today_date <- as.Date(Sys.Date(), "%m/%d/%y")
today_col_name <- as.character(today_date)

unique_individual_names = unique(new_table$name)
today_date <- as.Date(Sys.Date(), "%m/%d/%y")

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
  fluidRow(
    column(2, checkboxInput("show_table", "Blocks", FALSE)),
    conditionalPanel(
      condition = "input.show_table == false",
      column(2, class = "col-xs-2", 
             # Date range input for selecting start and end date
             dateInput("date", "Select Date:", value = Sys.Date())  # Fixed: Use Sys.Date() for today's date
      ), 
      column(2, class = "col-xs-2", 
             # Filter by individual name
             selectInput("individual", "Individual:",
                         choices = c("", unique_individual_names),  # Replace with your list of names
                         selected = NULL, multiple=TRUE)
      ),
      column(2, class = "col-xs-2", style = "padding-left:0px;", 
             # Filter by call schedule
             selectInput("rotation", "Rotation:",
                         choices = c("", "Janeway", "Barker", "Longcope", "Thayer", "Wolf", "Brancati", "MICU", "CCU", "MPC", "NightWatch", "Ambulatory", "MTL", "Solids", "Leuks", "PCCU", "Relief","NATO/DATO", "Jeopardy", "MICU", "CCU", "Triage", "MPC","BMICU", 'BCCU', "CJ", "Liver", "Polk", "Ambulatory", "MTL", "Solids", "Leuks", "GenCards", 'Cardiomyopathy', "FirmJAR", "Research", "Elective", 'Peds', "Women'sHealth", "AddictionMedicine", "HIV/HCV", "Geri"), 
                         selected = NULL)
      ),
      column(2, class = "col-xs-2", style = "padding-left:0px;", 
             # Filter by call schedule
             selectInput("callSchedule", "Call Schedule:",
                         choices = c("", "CALL", "POST", "OFF", "DAY", "GOOD", "NIGHT", "SHORT", "NEWS/OLDS", "BOOK", "NATO/DATO"), 
                         selected = NULL)
      )
    ), 
    conditionalPanel(
      condition = "input.show_table == true",
      column(2, class = "col-xs-2", 
             # Date range input for selecting start and end date
             selectInput("block", selected = find_block(today_date, intern_block_table), "Select Block:", choices = c("All", "1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B", "5A", "5B", "6A", "6B", "7A", "Holiday 1", "Holiday 2", "7B", "8A", "8B", "9A", "9B", "10A", "10B", "11A", "11B", "12A", "12B", "13A", "13B"))  # Fixed: Use Sys.Date() for today's date
      ), 
      column(2, class = "col-xs-2", 
             # Filter by individual name
             selectInput("individual", "Individual:",
                         choices = c("", unique_individual_names),  # Replace with your list of names
                         selected = NULL, multiple=TRUE)
      ),
      column(2, class = "col-xs-2", style = "padding-left:0px;", 
             # Filter by call schedule
             selectInput("rotation", "Rotation:",
                         choices = c("", "Janeway", "Barker", "Longcope", "Thayer", "Wolf", "Brancati", "MICU", "CCU", "MPC", "Subspecialty", "Ambulatory", "MTL", "Solids", "Leuks", "PCCU", "NightWatch", "Relief","NATO", "Jeopardy", "MICU", "CCU", "Triage", "MPC","BMICU", 'BCCU', "CJ", "Liver", "Polk", "Ambulatory", "MTL", "Solids", "Leuks", "GenCards", 'Cardiomyopathy', "FirmJAR", "Research", "Elective", 'Peds', "Women'sHealth", "AddictionMedicine", "HIV/HCV", "Geri"), 
                         selected = NULL)
      ), 
      column(2, class = "col-xs-2", style = "padding-left:0px;", 
             # Filter by call schedule
             selectInput("pgy", "Intern/JARSAR:",
                         choices = c("Intern", "JAR/SAR"), 
                         selected = NULL)
      ), 
      
      
    ), 
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
    if(input$show_table){ # if want to view in block view
      individual = input$individual
      rotation = input$rotation
      
      if (input$pgy=='Intern'){
        block_table = intern_block_table
      }else{
        block_table = jarsar_block_table
      }
    # by default, will print today's block as the most left block
      block_table = filter_blocks(block_id = input$block, block_table = block_table)

      # if specific filter is applied
      if(!is.null(individual)){
        block_table = block_table %>% filter(name %in% individual)
        
      }
      
      # Identify the first non-"name" column (leftmost block column)
      block_columns <- colnames(block_table)[-1]  # Exclude "name" column
      if (length(block_columns) > 0) {
        leftmost_block_col <- block_columns[1]  # Take the first block column

        
        if (!is.null(rotation) && rotation != "") {
          block_table <- block_table %>% filter_at(vars(leftmost_block_col), any_vars(str_detect(., paste0("^", rotation))))
        }
      }
      

      datatable(
        block_table, 
        options = list(
          pageLength = 100, 
          autoWidth = FALSE,
          scrollX = TRUE,                # Enable horizontal scrolling
          scrollY = "1000px",             # Vertical scroll for table height
          fixedColumns = list(leftColumns = 1),  # Fix the first column (row names)
          fixedHeader = TRUE,            # Fix the header when scrolling vertically
          lengthChange = FALSE,   # Disable the "Show entries" dropdown
          searching = FALSE,       # Disable the search box
          fillContainer = FALSE,             # Prevents table from stretching
          
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
      rotation = input$rotation
      
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
      
      
      # Filter by rotation if selected
      if (!is.null(rotation) && rotation != "") {
        # Use str_detect to filter for shifts that contain the selected schedule pattern
        new_table <- new_table %>%
          filter_at(vars(ends_with(selected_start_col_name)), any_vars(str_detect(., paste0(" ", rotation))))
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
          autoWidth = FALSE,
          scrollX = TRUE,                # Enable horizontal scrolling
          scrollY = "800px",             # Vertical scroll for table height
          fixedColumns = list(leftColumns = 1),  # Fix the first column (row names)
          fixedHeader = TRUE,            # Fix the header when scrolling vertically
          paging = FALSE,        # Disable pagination
          lengthChange = FALSE,   # Disable the "Show entries" dropdown
          searching = FALSE,       # Disable the search box
          fillContainer = FALSE,             # Prevents table from stretching
          
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

