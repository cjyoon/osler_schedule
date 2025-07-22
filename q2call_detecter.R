library(tidyverse)
library(lubridate)

# Load the file
setwd('~/Dropbox/Osler/Osler_shiny/www/schedule2025/')
tsv_list <- list.files(pattern = "\\.tsv$", full.names = TRUE)

close_call_list = data.frame()
for(i in 1:length(tsv_list)){
  file_path <- tsv_list[i]
  df <- read_tsv(file_path)
  
  # Convert date column to Date type
  df <- df %>%
    mutate(date = mdy(date))  # if your format is "MM-DD-YYYY"
  
  # Filter for rows containing 'Call' (case-insensitive)
  call_df <- df %>%
    filter(str_detect(schedule, regex("Call", ignore_case = TRUE))) %>%
    arrange(date)
  
  # Calculate differences between consecutive calls
  call_df <- call_df %>%
    mutate(date_diff = as.numeric(date - lag(date)))
  
  # Filter for calls less than 3 days apart
  close_calls <- call_df %>%
    filter(!is.na(date_diff) & date_diff < 3)
  
  # View the result
  print(close_calls)
  close_call_list = rbind(close_call_list, close_calls)
  
}


setwd('~/Dropbox/Osler/Osler_shiny/intern2025/schedule/')
tsv_list <- list.files(pattern = "\\.tsv$", full.names = TRUE)

close_call_list = data.frame()
for(i in 1:length(tsv_list)){
  file_path <- tsv_list[i]
  df <- read_tsv(file_path)
  
  # Convert date column to Date type
  df <- df %>%
    mutate(date = mdy(date))  # if your format is "MM-DD-YYYY"
  
  # Filter for rows containing 'Call' (case-insensitive)
  call_df <- df %>%
    filter(str_detect(schedule, regex("Call", ignore_case = TRUE))) %>%
    arrange(date)
  
  # Calculate differences between consecutive calls
  call_df <- call_df %>%
    mutate(date_diff = as.numeric(date - lag(date)))
  
  # Filter for calls less than 3 days apart
  close_calls <- call_df %>%
    filter(!is.na(date_diff) & date_diff < 3)
  
  # View the result
  print(close_calls)
  close_call_list = rbind(close_call_list, close_calls)
  
}

