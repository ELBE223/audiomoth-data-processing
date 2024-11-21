# ============================
# Script: Process_Kartierung_Data.R
# Purpose: Calculate biodiversity metrics for Kartierung (manual mapping) data.
# Author: Lucas Beseler
# Date: 2024-11-20
# ============================

# ============================
# 1. Load Required Libraries
# ============================

# Install and load necessary packages if not already installed
required_packages <- c("dplyr", "readxl", "vegan", "data.table", "stringr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the packages
library(dplyr)
library(readxl)
library(vegan)
library(data.table)
library(stringr)

# ============================
# 2. Define Paths and Initialize Logging
# ============================

# Define the path to the Kartierung data
kartierung_path <- "/Users/lucasbeseler/Desktop/Audiomoth_Kartierung_Shannonhabitat/Audiomoth_unzipped/Kartierung.xlsx"  # Update if necessary

# Define the output path for the results
output_path <- "/Users/lucasbeseler/Desktop/Audiomoth_Kartierung_Shannonhabitat/Kartierung_Biodiversity_Metrics.csv"  # Update if necessary

# Define the path for the log file
log_file_path <- "/Users/lucasbeseler/Desktop/Audiomoth_Kartierung_Shannonhabitat/Kartierung_Processing_Log.txt"  # Update if necessary

# Function to log messages with timestamps
log_message <- function(message, log_file) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_message <- paste(timestamp, "-", message, "\n")
  cat(full_message)
  cat(full_message, file = log_file, append = TRUE)
}

# Initialize the log file
if(file.exists(log_file_path)) {
  file.remove(log_file_path)  # Remove existing log file to start fresh
}
log_message("===== Kartierung Data Processing Started =====", log_file_path)

# ============================
# 3. Load and Clean the Kartierung Data
# ============================

log_message("Loading Kartierung.xlsx.", log_file_path)

kartierung_data <- tryCatch({
  read_excel(kartierung_path, skip = 2, 
             col_names = c("Gebiet", "Ort", "Fläche", "Datum", 
                           "Durchgang", "Art_deutsch", "Zahl", 
                           "Wissenschaftlicher_Name"))
}, error = function(e) {
  log_message(paste("Error loading Kartierung.xlsx:", e$message), log_file_path)
  return(NULL)
})

# Check if data was loaded successfully
if (is.null(kartierung_data)) {
  stop("Kartierung.xlsx could not be loaded. Please check the file path and format.")
}

log_message("Kartierung data loaded successfully.", log_file_path)

# Clean the data: Ensure correct data types
kartierung_data <- kartierung_data %>%
  mutate(
    Gebiet = as.character(Gebiet),
    Ort = as.character(Ort),
    Wissenschaftlicher_Name = as.character(Wissenschaftlicher_Name),
    Art_deutsch = as.character(Art_deutsch),
    total_zahl = as.integer(Zahl)
  )

log_message("Kartierung data cleaned.", log_file_path)

# ============================
# 4. Calculate Biodiversity Metrics per Region
# ============================

log_message("Calculating biodiversity metrics per region.", log_file_path)

# Calculate biodiversity metrics
kartierung_metrics <- kartierung_data %>%
  group_by(Gebiet, Wissenschaftlicher_Name) %>%
  summarise(
    count = sum(total_zahl, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Gebiet) %>%
  summarise(
    Total_Count = sum(count),
    Shannon_Index = vegan::diversity(count, index = "shannon"),
    Simpson_Index = vegan::diversity(count, index = "simpson"),
    Richness = n_distinct(Wissenschaftlicher_Name),
    .groups = 'drop'
  ) %>%
  rename(region = Gebiet)

# Display the results
print(kartierung_metrics)

# ============================
# 5. Save the Results to a CSV File
# ============================

log_message("Saving biodiversity metrics to CSV.", log_file_path)

tryCatch({
  write.csv(kartierung_metrics, output_path, row.names = FALSE)
  log_message(paste("Kartierung biodiversity metrics saved at:", output_path), log_file_path)
}, error = function(e) {
  log_message(paste("Error saving CSV:", e$message), log_file_path)
})

log_message("===== Kartierung Data Processing Completed =====", log_file_path)

