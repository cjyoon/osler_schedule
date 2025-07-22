library(tidyverse)
library(rsconnect)
library(DT)
library(shiny)

# Time zone set to NYC
Sys.setenv(TZ = "America/New_York")

# Get the modification time of the table
last_modified <- format(file.info("osler_schedule_table.tsv")$mtime, "%Y-%m-%d")

find_block <- function(input_date, block_table) {
  col_names <- colnames(block_table)
  blocks <- data.frame(Block = character(), Start = as.Date(character()), End = as.Date(character()), stringsAsFactors = FALSE)
  pattern <- "(.+?)\\s(\\d{1,2}/\\d{1,2}/\\d{2})-(\\d{1,2}/\\d{1,2}/\\d{2})"
  for (col in col_names) {
    matches <- regmatches(col, regexec(pattern, col))
    if (length(matches[[1]]) == 4) {
      block_name <- matches[[1]][2]
      start_date <- as.Date(matches[[1]][3], format="%m/%d/%y")
      end_date <- as.Date(matches[[1]][4], format="%m/%d/%y")
      blocks <- rbind(blocks, data.frame(Block = block_name, Start = start_date, End = end_date, stringsAsFactors = FALSE))
    }
  }
  input_date <- as.Date(input_date, format="%Y-%m-%d")
  matching_block <- blocks$Block[input_date >= blocks$Start & input_date <= blocks$End]
  if (length(matching_block) == 0) {
    return('1A')
  } else {
    return(matching_block)
  }
}

filter_blocks <- function(block_id, block_table) {
  col_names <- colnames(block_table)
  match_index <- which(grepl(paste0("^", block_id, " "), col_names, ignore.case = T))
  if (length(match_index) > 0) {
    return(block_table[, c(1, match_index:ncol(block_table))])
  } else {
    return(block_table)
  }
}

remove_unnamed_columns <- function(df) {
  df <- df[, !grepl("^Unnamed", colnames(df))]
  df <- df %>% filter(!is.na(name))
  return(df)
}

new_table <- read_delim('osler_schedule_table.tsv', delim='\t')
intern_block_table <- remove_unnamed_columns(read_delim('block_schedule/intern_block_view2025.tsv', delim='\t'))
jarsar_block_table <- remove_unnamed_columns(read_delim('block_schedule/block_view_jarsar2025.tsv', delim='\t'))

date_columns <- as.Date(names(new_table)[-1], format = "%m-%d-%Y")
today_date <- as.Date(Sys.Date(), "%m/%d/%y")
unique_individual_names <- unique(new_table$name)

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    tags$link(rel = "manifest", href = "www/site.webmanifest")
  ),
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center;",
    tags$div(
      style = "display: flex; align-items: center;",
      img(src = "apple-touch-icon.png", height = "50px", width = "50px"),
      tags$h1(" Osler Resident Schedule", style = "margin-left: 10px;")
    ),
    tags$div(
      style = "font-size: 14px; color: gray;",
      HTML(paste0(
        '<a href="ics_links.html" target="_blank" style="color: gray;">Click to download .ics files</a>',
        " | Last updated: ", last_modified
      ))
    )
  ),
  tags$title("Osler Resident Schedule"),
  tags$style(HTML("
    .container-fluid {
      padding-left: 4px;
      padding-right: 4px;
    }
    table.dataTable, thead th {
      font-family: 'Courier New', Courier, monospace;
      font-size: 12px;
    }
    thead th {
      font-weight: bold;
    }
    .shiny-input-container, .shiny-date-input, .shiny-select-input,
    .selectize-input, .form-control, .shiny-input-container label {
      font-size: 12px;
    }
    .shiny-date-input, .shiny-select-input {
      width: 150px;
    }
    .selectize-input, .form-control {
      width: 120px;
    }
    @media (max-width: 768px) {
      .container-fluid {
        padding-left: 0px;
        padding-right: 0px;
      }
      .col-xs-2 {
        width: 100px;
        padding: 2px 5px;
      }
      .shiny-input-container label {
        min-height: 20px;
      }
    }
    h1 {
      font-size: 25px;
      color: #2E4053;
    }
  ")),
  
  fluidRow(
    column(1,  checkboxInput("show_table", "Blocks", FALSE)),
    conditionalPanel(
      condition = "input.download == true",
      column(12,
             uiOutput("ics_download_links")
      )
    ),
    conditionalPanel(
      condition = "input.show_table == false",
      column(2, class = "col-xs-2",
             dateInput("date", "Select Date:", value = Sys.Date())
      ),
      column(2, class = "col-xs-2",
             selectInput("individual", "Individual:",
                         choices = c("", unique_individual_names),
                         selected = NULL, multiple=TRUE)
      ),
      column(2, class = "col-xs-2", style = "padding-left:0px;",
             selectInput("rotation", "Rotation:",
                         choices = c("", "Janeway", "Barker", "Longcope", "Thayer", "Wolf", "Brancati",
                                     "MICU", "CCU", "MPC", "NightWatch", "Ambulatory", "MTL", "Solids", "Leuks", "PCCU",
                                     "Relief", "NATO/DATO", "Jeopardy", "MICU", "CCU", "Triage", "MPC", "BMICU", 'BCCU',
                                     "CJ", "Liver", "Polk", "Ambulatory", "MTL", "Solids", "Leuks", "GenCards",
                                     'Cardiomyopathy', "FirmJAR", "Research", "Elective", 'Peds', "Women'sHealth",
                                     "AddictionMedicine", "HIV/HCV", "Geri", "MP_Clinic"),
                         selected = NULL)
      ),
      column(2, class = "col-xs-2", style = "padding-left:0px;",
             selectInput("callSchedule", "Call Schedule:",
                         choices = c("", "CALL", "POST", "OFF", "DAY", "GOOD", "NIGHT", "SHORT", "NEWS/OLDS", "BOOK", "NATO/DATO"),
                         selected = NULL)
      )
    ),
    conditionalPanel(
      condition = "input.show_table == true",
      column(2, class = "col-xs-2",
             selectInput("block", selected = find_block(today_date, intern_block_table), "Select Block:",
                         choices = c("All", "1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B",
                                     "5A", "5B", "6A", "6B", "7A", "Holiday 1", "Holiday 2", "7B", "8A", "8B",
                                     "9A", "9B", "10A", "10B", "11A", "11B", "12A", "12B", "13A", "13B"))
      ),
      column(2, class = "col-xs-2",
             selectInput("individual", "Individual:",
                         choices = c("", unique_individual_names),
                         selected = NULL, multiple=TRUE)
      ),
      column(2, class = "col-xs-2", style = "padding-left:0px;",
             selectInput("rotation", "Rotation:",
                         choices = c("", "Janeway", "Barker", "Longcope", "Thayer", "Wolf", "Brancati",
                                     "MICU", "CCU", "MPC", "Subspecialty", "Ambulatory", "MTL", "Solids", "Leuks", "PCCU",
                                     "NightWatch", "Relief", "NATO", "Jeopardy", "MICU", "CCU", "Triage", "MPC", "BMICU",
                                     'BCCU', "CJ", "Liver", "Polk", "Ambulatory", "MTL", "Solids", "Leuks", "GenCards",
                                     'Cardiomyopathy', "FirmJAR", "Research", "Elective", 'Peds', "Women'sHealth",
                                     "AddictionMedicine", "HIV/HCV", "Geri", "MP_Clinic"),
                         selected = NULL)
      ),
      column(2, class = "col-xs-2", style = "padding-left:0px;",
             selectInput("pgy", "Intern/JARSAR:",
                         choices = c("Intern", "JAR/SAR"),
                         selected = NULL)
      )
    )
  ),
  
  fluidRow(
    column(12,
           uiOutput("update_box")
    )
  ),
  
  fluidRow(
    column(12,
           DTOutput("table")
    )
  )
)

# Server
server <- function(input, output, session) {
  Sys.setenv(TZ = "America/New_York")
  
  # Read first line of the past updates text file
  first_update_line <- tryCatch({
    readLines("www/past_updates.txt", n = 1, warn = FALSE)
  }, error = function(e) {
    "No recent updates."
  })
  
  output$update_box <- renderUI({
    HTML(paste0(
      "<div style='background-color: #ffffcc; border: 1px solid #cccc00; padding: 10px; margin-bottom: 10px; border-radius: 5px;'>",
      " <strong>When in doubt refer to <a href='https://app.qgenda.com/landingpage/jhhres' target='_blank' style='color: #004085; font-weight: bold;'>QGenda</a>.</strong> ",
      "<a href='past_updates.txt' target='_blank' style='font-size: 12px; color: #004085;'>View update details</a>",
      "</div>"
    ))
  })
  
  # The rest of your table rendering logic goes here (unchanged)
  
  output$table <- renderDT({
    if(input$show_table) {
      individual <- input$individual
      rotation <- input$rotation
      
      block_table <- if (input$pgy == 'Intern') intern_block_table else jarsar_block_table
      block_table <- filter_blocks(block_id = input$block, block_table = block_table)
      
      if(!is.null(individual)) {
        block_table <- block_table %>% filter(name %in% individual)
      }
      
      block_columns <- colnames(block_table)[-1]
      if(length(block_columns) > 0) {
        leftmost_block_col <- block_columns[1]
        if(!is.null(rotation) && rotation != "") {
          block_table <- block_table %>% filter_at(vars(leftmost_block_col), any_vars(str_detect(., paste0("^", rotation))))
        }
      }
      
      datatable(
        block_table,
        options = list(
          pageLength = 100,
          autoWidth = FALSE,
          scrollX = TRUE,
          scrollY = "1000px",
          fixedColumns = list(leftColumns = 1),
          fixedHeader = TRUE,
          lengthChange = FALSE,
          searching = FALSE,
          fillContainer = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          initComplete = JS(
            paste0(
              "function(settings, json) {",
              "var colIndex = settings.aoColumns.map(function(col){ return col.sTitle; }).indexOf('", as.character(today_date), "');",
              "if(colIndex > -1) {",
              "  var tableWrapper = $(this.api().table().container());",
              "  var scrollBody = tableWrapper.find('.dataTables_scrollBody');",
              "  var header = tableWrapper.find('thead th:eq(' + colIndex + ')');",
              "  var headerPosition = header.position();",
              "  scrollBody.scrollLeft(headerPosition.left);",
              "}",
              "}"
            )
          )
        ),
        rownames = FALSE,
        extensions = 'FixedColumns'
      )
      
    } else {
      selected_start <- input$date
      individual <- input$individual
      callSchedule <- input$callSchedule
      rotation <- input$rotation
      
      selected_columns <- names(new_table)[which(date_columns >= selected_start) + 1]
      filtered_table <- new_table[c('name', selected_columns)]
      
      if(!is.null(individual)) {
        filtered_table <- filtered_table %>% filter(name %in% individual)
      }
      
      selected_start_col_name <- as.character(format(selected_start, "%m-%d-%Y"))
      
      if(!is.null(callSchedule) && callSchedule != "") {
        filtered_table <- filtered_table %>% filter_at(vars(ends_with(selected_start_col_name)), any_vars(str_detect(., callSchedule)))
      }
      
      if(!is.null(rotation) && rotation != "") {
        filtered_table <- filtered_table %>% filter_at(vars(ends_with(selected_start_col_name)), any_vars(str_detect(., paste0(" ", rotation))))
      }
      
      date_columns_filtered <- names(filtered_table)[-1]
      days_of_week <- format(as.Date(date_columns_filtered, format = "%m-%d-%Y"), "%a")
      new_column_names <- paste(date_columns_filtered, "\n", days_of_week, sep = "")
      colnames(filtered_table)[-1] <- new_column_names
      
      datatable(
        filtered_table,
        options = list(
          pageLength = 100,
          autoWidth = FALSE,
          scrollX = TRUE,
          scrollY = "800px",
          fixedColumns = list(leftColumns = 1),
          fixedHeader = TRUE,
          paging = FALSE,
          lengthChange = FALSE,
          searching = FALSE,
          fillContainer = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          initComplete = JS(
            paste0(
              "function(settings, json) {",
              "var colIndex = settings.aoColumns.map(function(col){ return col.sTitle; }).indexOf('", as.character(today_date), "');",
              "if(colIndex > -1) {",
              "  var tableWrapper = $(this.api().table().container());",
              "  var scrollBody = tableWrapper.find('.dataTables_scrollBody');",
              "  var header = tableWrapper.find('thead th:eq(' + colIndex + ')');",
              "  var headerPosition = header.position();",
              "  scrollBody.scrollLeft(headerPosition.left);",
              "}",
              "}"
            )
          )
        ),
        rownames = FALSE,
        extensions = 'FixedColumns'
      )
    }
  })
}

shinyApp(ui = ui, server = server)