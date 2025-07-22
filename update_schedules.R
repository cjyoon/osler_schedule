library(tidyverse)

setwd("~/Dropbox/Osler/Osler_shiny")
# creating a single table, commmented out for faster loading
# List all .tsv files in the directory
# intern_file_list <- list.files('intern_schedule/', pattern = "\\.tsv$", full.names = TRUE)
# jarsar_file_list <- list.files('jar_sar_schedule/', pattern = "\\.tsv$", full.names = TRUE)

intern_file_list2 <- list.files('intern2025/schedule/', pattern = "\\.tsv$", full.names = TRUE)
jarsar_file_list2 <- list.files('jarsar2025/schedule/', pattern = "\\.tsv$", full.names = TRUE)

# Read each .tsv file into a list of data frames
data_list <- lapply(c(intern_file_list2, jarsar_file_list2), read_delim)

# Convert columns to consistent types
data_list <- lapply(data_list, function(df) {
  # Example: Ensure 'name' column is character
  if ("name" %in% colnames(df)) {
    df$name <- as.character(df$name)
  }
  # Add more conversions if needed
  return(df)
})

# Combine all data frames into one
combined_data <- bind_rows(data_list)

new_table = combined_data  %>% pivot_wider(
  names_from = "date",
  names_prefix = "",
  values_from = "schedule",
  values_fn = list(schedule = ~ paste(., collapse = "/"))  # Combine duplicates into a comma-separated string
)

add_asterisk <- function(vec) {
  # Apply the condition to each element in the vector
  vec <- ifelse(grepl("$OPH|$ED|$Anes", vec), paste0(".", vec), vec)
  return(vec)
}
#
new_table$name = add_asterisk(new_table$name)
# Create a logical vector to separate alphabetic from non-alphabetic starting names
is_alpha <- grepl("^[A-Za-z]", new_table$name)
new_table <- new_table[order(!is_alpha, new_table$name), ]



# # To reduce loading time, keep only dates dating 3 months back
# # Get today's date
today_date <- Sys.Date()
#
# # Define cutoff date (3 months prior)
cutoff_date <- today_date %m-% months(3)
#
# Extract column names
col_names <- colnames(new_table)

# Identify date columns (assumes format "MM-DD-YYYY")
date_cols <- col_names[str_detect(col_names, "^\\d{2}-\\d{2}-\\d{4}$")]

# Convert to Date type
date_col_dates <- mdy(date_cols)

# Keep columns up to the cutoff date
keep_date_cols <- date_cols[order(date_col_dates >= cutoff_date)]

# Include non-date columns like "name"
non_date_cols <- setdiff(col_names, date_cols)
#
# Create filtered table
new_table <- new_table[, c(non_date_cols, keep_date_cols)]

#make sure dates are sorted properly
date_cols <- setdiff(names(new_table), "name")
sorted_dates <- date_cols[order(as.Date(date_cols, format = "%m-%d-%Y"))]
new_table <- new_table[, c("name", sorted_dates)]

write_delim(new_table, file='~/Dropbox/Osler/Osler_shiny/osler_schedule_table.tsv', delim='\t')
