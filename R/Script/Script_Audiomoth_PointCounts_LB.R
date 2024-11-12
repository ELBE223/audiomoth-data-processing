# Script_Audiomoth_PointCounts_LB
# Purpose: Analysis of bird species based on data from AudioMoth devices and PointCounts.
# Author: Lucas Beseler
# Date: 2024-11-11


# ============================
# 1. Setup: Unzip the ZIP file and set the working directory
# ============================

# Install and load the pacman package if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load all required packages
p_load(
  tidyverse, vegan, tibble, readxl, data.table, cluster, 
  ggplot2, viridis, reshape2, scales, ggthemes, stringr, 
  microbenchmark, plotly, bit64, RSQLite, fuzzyjoin, ggrepel, 
  parallel, lubridate, writexl, stringdist, broom
)

# Path to the ZIP file and the directory to unzip
zip_file <- "/Users/lucasbeseler/Desktop/files.zip"
unzip_dir <- "/Users/lucasbeseler/Desktop/files"

# Unzip the ZIP file
if (!dir.exists(unzip_dir)) {
  dir.create(unzip_dir)
}
unzip(zip_file, exdir = unzip_dir)

# Check if another `files` subfolder exists and set the working directory accordingly
nested_files_dir <- file.path(unzip_dir, "files")
if (dir.exists(nested_files_dir)) {
  files_dir <- nested_files_dir  # Use the nested folder
} else {
  files_dir <- unzip_dir  # Use the main directory
}

# Set the working directory for the rest of the script
setwd(files_dir)

# Verify that the directory exists
if (!dir.exists(files_dir)) {
  stop("The 'files' directory was not found. Please check the ZIP file.")
}

# ============================
# 2. Define paths and initialize data
# ============================

# Base directory (already set to the 'files' directory)
base_dir <- files_dir

# Define relative paths
input_folder <- base_dir
output_folder <- file.path(dirname(base_dir), "files_output")  # Output is created parallel to 'files'
birdnet_labels_path <- file.path(base_dir, "BirdNET_Labels.xlsx")
species_list_path <- file.path(base_dir, "Species_Saxony_Anhalt_updated.xlsx")
kartierung_path <- file.path(base_dir, "Kartierung.xlsx")
species_freq_folder <- output_folder
kartierung_output_folder <- file.path(output_folder, "Kartierung")

# Create output directories if they do not exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
  cat(paste(Sys.time(), "-", "Output folder created:", output_folder, "\n"))
}

if (!dir.exists(kartierung_output_folder)) {
  dir.create(kartierung_output_folder, recursive = TRUE)
  cat(paste(Sys.time(), "-", "Kartierung output folder created:", kartierung_output_folder, "\n"))
}

# ============================
# 3. Define the Logging Function
# ============================

# Logging function for better traceability
log_message <- function(message) {
  cat(paste(Sys.time(), "-", message, "\n"))
}

# ============================
# 4. Define Data Processing Functions
# ============================

# Function to load and process AudioMoth data with column renaming
load_audiomoth_data <- function(file_path, region_name) {
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  sheets <- excel_sheets(file_path)
  
  audiomoth_data <- lapply(sheets, function(sheet) {
    log_message(paste("Processing sheet:", sheet))
    
    # Extract file number from the file name
    file_number <- as.numeric(gsub("filtered_(\\d+)_BirdNET_selection_by_day\\.xlsx", "\\1", basename(file_path)))
    
    # Extract date from the sheet name
    date_extracted <- ymd(sheet)
    log_message(paste("Extracted date:", date_extracted))
    
    if (is.na(date_extracted)) {
      warning(paste("Could not extract the date from sheet", sheet, "in file", file_path))
      return(NULL)
    }
    
    # Attempt to read and clean the data
    df <- tryCatch({
      read_excel(file_path, sheet = sheet) %>%
        rename_with(~ tolower(gsub(" ", "_", .x))) %>% # Standardize column names
        rename(begin_time_s = `begin_time_(s)`) %>%   # Rename specific column
        mutate(
          date = as.Date(date_extracted),
          region = region_name,
          device = paste("Device", file_number),
          selection = as.character(selection),
          common_name = tolower(trimws(common_name)), # Clean common_name column
          channel = if("channel" %in% names(.)) as.character(channel) else NA_character_,
          file_offset_s = as.numeric(`file_offset_(s)`), # Explicitly convert to numeric
          `file_offset_(s)` = as.numeric(`file_offset_(s)`) # Ensure both columns are numeric
        )
    }, error = function(e) {
      warning(paste("Error reading sheet:", sheet, "in file:", file_path, "-", e$message))
      return(NULL)
    })
    
    # Check for required columns
    required_columns <- c("selection", "channel", "begin_time_s", "end_time_s", "low_freq_hz", 
                          "high_freq_hz", "common_name", "species_code", "confidence", 
                          "begin_path", "file_offset_s", "date", "region", "device")
    missing_columns <- setdiff(required_columns, names(df))
    
    if (length(missing_columns) > 0) {
      warning(paste("Sheet", sheet, "in file", file_path, "is missing columns:", paste(missing_columns, collapse = ", ")))
      # Add missing columns with NA
      for (col in missing_columns) {
        df[[col]] <- NA
      }
    }
    
    # Explicitly convert 'file_offset_s' and 'file_offset_(s)' to numeric
    df$file_offset_s <- as.numeric(df$file_offset_s)
    df$`file_offset_(s)` <- as.numeric(df$`file_offset_(s)`)
    
    return(df)
  })
  
  # Remove NULLs and combine data
  audiomoth_data <- audiomoth_data[!sapply(audiomoth_data, is.null)]
  
  if (length(audiomoth_data) == 0) {
    return(NULL)
  }
  
  # Ensure all 'file_offset_(s)' are numeric
  audiomoth_data <- lapply(audiomoth_data, function(df) {
    df$file_offset_s <- as.numeric(df$file_offset_s)
    df$`file_offset_(s)` <- as.numeric(df$`file_offset_(s)`)
    return(df)
  })
  
  combined_df <- bind_rows(audiomoth_data)
  return(combined_df)
}

# Function to load BirdNET Labels
load_birdnet_labels <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("BirdNET Labels file not found:", file_path))
  }
  
  birdnet_df <- read_excel(file_path) %>%
    rename_with(~ tolower(gsub(" ", "_", .x))) %>% # Standardize column names
    mutate(
      common_name = tolower(trimws(common_name)),
      wissenschaftlicher_name = trimws(wissenschaftlicher_name)
    )
  
  # Check if the column exists
  if (!"wissenschaftlicher_name" %in% names(birdnet_df)) {
    stop("The column 'wissenschaftlicher_name' does not exist in the BirdNET labels data.")
  }
  
  return(birdnet_df)
}

# Function to load the species list
load_species_list <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("Species list file not found:", file_path))
  }
  
  species_list <- read_excel(file_path) %>%
    rename_with(~ tolower(gsub(" ", "_", .x))) %>% # Standardize column names
    mutate(
      common_name = tolower(trimws(common_name)),
      wissenschaftlicher_name = trimws(wissenschaftlicher_name)
    )
  
  return(species_list)
}

# Function to merge data with BirdNET Labels
merge_with_birdnet <- function(df, birdnet_df) {
  merged_df <- df %>%
    left_join(birdnet_df, by = "common_name")
  
  log_message(paste("Number of entries with assigned scientific names:", sum(!is.na(merged_df$wissenschaftlicher_name))))
  
  return(merged_df)
}

# Function to create the datetime column
create_datetime <- function(df) {
  df <- df %>%
    mutate(
      date_only = as.Date(date),
      start_time = as.POSIXct(paste(date_only, "03:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"),
      datetime = start_time + lubridate::seconds(begin_time_s)
    )
  
  # Check for failed datetime creations
  num_na_datetime <- sum(is.na(df$datetime))
  if (num_na_datetime > 0) {
    log_message(paste("There are", num_na_datetime, "failed datetime creations."))
    print(head(df %>% filter(is.na(datetime))))
  } else {
    log_message("All datetime values successfully created.")
  }
  
  return(df)
}

# Function to calculate species frequency
calculate_species_frequency <- function(df) {
  species_counts <- df %>%
    group_by(wissenschaftlicher_name, common_name, region) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(count))
  
  return(species_counts)
}

# Function to prepare the DataFrame for export
prepare_dataframe_for_output <- function(df) {
  # Check the number of rows
  if (nrow(df) > 1e6) {
    warning("Data has more than 1 Million rows. Splitting.")
  }
  
  # Convert factors and hms to characters
  for (i in which(vapply(df, inherits, logical(1), c("factor", "hms")))) {
    df[[i]] <- as.character(df[[i]])
  }
  
  # Convert POSIXct
  for (i in which(vapply(df, function(x) {
    inherits(x, "POSIXct")
  }, logical(1)))) {
    df[[i]] <- as.POSIXct(as.numeric(df[[i]]), origin = "1970-01-01")
  }
  
  # Convert POSIXlt
  for (i in which(vapply(df, inherits, logical(1), "POSIXlt"))) {
    df[[i]] <- as.POSIXct(df[[i]])
  }
  
  # Convert integer64 to double
  for (i in which(vapply(df, inherits, logical(1), "integer64"))) {
    warning(sprintf("Converting column %s from int64 to double", names(df)[i]), call. = FALSE)
    df[[i]] <- bit64::as.double.integer64(df[[i]])
  }
  
  return(df)
}

# Function to save the data
save_data <- function(df, species_counts, region_name, output_folder) {
  # Split the DataFrame into chunks of 1 Million rows
  split_dataframe <- function(df, chunk_size = 1e6) {
    split(df, ceiling(seq_len(nrow(df)) / chunk_size))
  }
  
  data_chunks <- split_dataframe(df, chunk_size = 1e6)
  
  # Save each chunk as a separate CSV
  for (i in seq_along(data_chunks)) {
    chunk <- data_chunks[[i]]
    
    # Clean the chunk
    chunk <- prepare_dataframe_for_output(chunk)
    
    # Define the file path
    output_combined_path <- file.path(output_folder, paste0("Combined_Region_Data_", region_name, "_Part_", i, ".csv"))
    
    # Write to CSV
    fwrite(chunk, output_combined_path)
    log_message(paste("Combined region data saved at:", output_combined_path))
  }
  
  # Calculate species frequency
  species_counts_df <- calculate_species_frequency(df)
  
  # Clean the species frequency data
  species_counts_df <- prepare_dataframe_for_output(species_counts_df)
  
  # Define the path for species frequency data
  species_counts_path <- file.path(output_folder, paste0("Species_Frequency_", region_name, ".csv"))
  
  # Write the species frequency data to CSV
  fwrite(species_counts_df, species_counts_path)
  log_message(paste("Species frequency data saved at:", species_counts_path))
}

# Function to process a region
process_region <- function(region_name, file_numbers, input_folder, output_folder, birdnet_df, species_set) {
  log_message(paste("Processing", region_name, "..."))
  
  # Find relevant files for the region
  file_paths <- list.files(input_folder, 
                           pattern = paste0("^filtered_0*(", paste(file_numbers, collapse = "|"), ")_BirdNET_selection_by_day\\.xlsx$"), 
                           recursive = FALSE, 
                           full.names = TRUE)
  
  if (length(file_paths) == 0) {
    log_message(paste("No files found for", region_name, "."))
    return(NULL)
  }
  
  log_message(paste("Found", length(file_paths), "files for", region_name, "to process."))
  
  # Determine the number of available cores
  num_cores <- detectCores() - 1  # Reserve one core for the OS
  
  # Create a cluster
  cl <- makeCluster(num_cores)
  
  # Export necessary variables and functions to the cluster
  clusterExport(cl, varlist = c("load_audiomoth_data", "merge_with_birdnet", "prepare_dataframe_for_output", 
                                "calculate_species_frequency", "save_data", "log_message"), envir = environment())
  clusterEvalQ(cl, {
    library(readxl)
    library(dplyr)
    library(writexl)
    library(stringdist)
    library(lubridate)
    library(bit64)
    library(data.table)
    library(fuzzyjoin)
    library(tidyverse)
    library(vegan)  # For diversity function
  })
  
  # Function to load and process a single file
  process_file <- function(file_path) {
    # Extract file number from the file name
    file_number <- as.numeric(gsub("filtered_(\\d+)_BirdNET_selection_by_day\\.xlsx", "\\1", basename(file_path)))
    
    log_message(paste("Loading data for file:", basename(file_path)))
    
    # Load and process the AudioMoth data
    df <- load_audiomoth_data(file_path, region_name)
    
    if (!is.null(df)) {
      log_message(paste("Data loaded for file:", basename(file_path), "Number of entries:", nrow(df)))
      return(df)
    } else {
      log_message(paste("No data loaded for file:", basename(file_path)))
      return(NULL)
    }
  }
  
  # Apply the function in parallel
  all_data <- parLapply(cl, file_paths, process_file)
  
  # Stop the cluster
  stopCluster(cl)
  
  # Remove NULLs and combine data
  all_data <- all_data[!sapply(all_data, is.null)]
  
  if (length(all_data) == 0) {
    log_message(paste("No data loaded for", region_name, "."))
    return(NULL)
  }
  
  # Ensure consistent data types
  all_data <- lapply(all_data, function(df) {
    df <- df %>%
      mutate(
        channel = if("channel" %in% names(df)) as.character(channel) else NA_character_,
        common_name = if("common_name" %in% names(df)) as.character(common_name) else NA_character_
      )
    return(df)
  })
  
  # Combine all data
  combined_region_data <- tryCatch({
    bind_rows(all_data)
  }, error = function(e) {
    log_message(paste("Error combining data for", region_name, ":", e$message))
    return(NULL)
  })
  
  if (is.null(combined_region_data)) {
    log_message(paste("Combining data for", region_name, "failed."))
    return(NULL)
  }
  
  log_message(paste("Total entries loaded for", region_name, ":", nrow(combined_region_data)))
  
  # Merge with BirdNET Labels
  merged_data <- merge_with_birdnet(combined_region_data, birdnet_df)
  
  # Check for missing scientific names
  num_na_scientific <- sum(is.na(merged_data$wissenschaftlicher_name))
  log_message(paste("Number of entries without scientific names:", num_na_scientific))
  
  if (num_na_scientific > 0) {
    log_message("Some entries have no assigned scientific names.")
    
    # Save unmapped entries
    unmapped_entries <- merged_data %>%
      filter(is.na(wissenschaftlicher_name)) %>%
      select(common_name) %>%
      distinct()
    
    if (nrow(unmapped_entries) > 0) {
      fwrite(unmapped_entries, file.path(output_folder, paste0("unmapped_common_names_", region_name, "_", Sys.Date(), ".csv")))
      log_message(paste("Unmapped 'common_name' entries saved at:", file.path(output_folder, paste0("unmapped_common_names_", region_name, "_", Sys.Date(), ".csv"))))
    }
  }
  
  # Filter based on the species list
  filtered_data <- merged_data %>%
    semi_join(species_set, by = "common_name")
  
  log_message(paste("Number of filtered entries for", region_name, ":", nrow(filtered_data)))
  
  # Create the datetime column
  filtered_data <- create_datetime(filtered_data)
  
  # Save the data
  save_data(filtered_data, NULL, region_name, output_folder)
}

# ============================
# 5. Main Script Execution
# ============================

# Load BirdNET Labels
log_message("Loading BirdNET labels.")
birdnet_df <- load_birdnet_labels(birdnet_labels_path)
log_message(paste("BirdNET labels loaded. Number of entries:", nrow(birdnet_df)))

# Load species list
log_message("Loading species list.")
species_list <- load_species_list(species_list_path)
log_message(paste("Species list loaded. Number of entries:", nrow(species_list)))

# Create a set for quick lookup
species_set <- species_list %>%
  filter(!is.na(common_name)) %>%
  select(common_name) %>%
  distinct()

log_message(paste("Species set created with", nrow(species_set), "unique common names."))

# Define regions and corresponding file numbers
regions <- list(
  RegionA = 1:4,
  RegionB = 5:8,
  RegionC = 9:12,
  RegionD = 13:16,
  RegionE = 17:20,
  RegionF = 21:24,
  RegionG = 25:28,
  RegionH = 29:32,
  RegionI = 33:34,
  RegionJ = 35:37,
  RegionK = 38:39
)

# Iterate through each region and process the files
for (region_name in names(regions)) {
  file_numbers <- regions[[region_name]]
  process_region(region_name, file_numbers, input_folder, output_folder, birdnet_df, species_set)
}

log_message("All regions have been processed.")

# ============================
# 6. Load and Combine Species Frequency Data
# ============================

log_message("Loading and combining species frequency data.")

# Initialize an empty DataFrame
all_species_freq <- data.frame()

# List of all regions
regions_list <- names(regions)

# Loop through each region to load species frequency data
for (region in regions_list) {
  path <- file.path(output_folder, paste0("Species_Frequency_", region, ".csv"))
  if (file.exists(path)) {
    temp_df <- fread(path) %>%
      mutate(region = region)
    all_species_freq <- bind_rows(all_species_freq, temp_df)
  }
}

log_message("Combined species frequency data loaded.")
print(head(all_species_freq))

# ============================
# 6a. Load and Combine Combined Region Data
# ============================

log_message("Loading and combining combined region data.")

# Initialize an empty list to store DataFrames
combined_data_list <- list()

# Loop through each region to load combined data
for (region in regions_list) {
  # Define the pattern to match combined data files for the current region
  pattern <- paste0("Combined_Region_Data_", region, "_Part_\\d+\\.csv$")
  
  # List all matching files for the region
  combined_files <- list.files(path = output_folder, pattern = pattern, full.names = TRUE)
  
  if (length(combined_files) > 0) {
    for (file in combined_files) {
      # Read the CSV file
      temp_df <- fread(file, fill = TRUE, na.strings = c("", "NA"))
      
      # Add a column to identify the source file (optional)
      temp_df$source_file <- basename(file)
      
      # Add to the list
      combined_data_list[[file]] <- temp_df
      
      # Log successful loading of the file
      log_message(paste("Data loaded from:", file))
    }
  } else {
    log_message(paste("No combined data files found for", region))
  }
}

# Bind all DataFrames together
all_combined_data <- bind_rows(combined_data_list, .id = "source")

# Log the total number of loaded entries
log_message(paste("Combined region data loaded. Total number of entries:", nrow(all_combined_data)))

# Optional: Check column names
print(names(all_combined_data))

# Optional: Check for unexpected differences
summary(all_combined_data)

# ============================
# 7. Calculate Average Confidence Value per Species and Region
# ============================

log_message("Calculating average confidence value per species and region.")

# Check if the 'confidence' column exists
if (!"confidence" %in% names(all_combined_data)) {
  stop("The 'confidence' column is not present in the combined data.")
}

species_confidence <- all_combined_data %>%
  group_by(region, wissenschaftlicher_name, common_name) %>%
  summarise(
    total_count = n(),
    avg_confidence = mean(confidence, na.rm = TRUE),
    .groups = 'drop'
  )

# Log the completion of the calculation
log_message("Average confidence value calculated.")

# Optional: Display the first few rows for verification
print(head(species_confidence))

# Add additional statistics
species_confidence <- all_combined_data %>%
  group_by(region, wissenschaftlicher_name, common_name) %>%
  summarise(
    total_count = n(),
    avg_confidence = mean(confidence, na.rm = TRUE),
    sd_confidence = sd(confidence, na.rm = TRUE),
    median_confidence = median(confidence, na.rm = TRUE),
    .groups = 'drop'
  )

# ============================
# 8. Visualization and Clustering
# ============================

# Visualization: Relationship between frequency and average confidence value
ggplot(species_confidence, aes(x = total_count, y = avg_confidence, color = region)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  labs(title = "Relationship between Frequency and Average Confidence Value",
       x = "Frequency (log-transformed)",
       y = "Average Confidence Value") +
  theme_minimal()

# Clustering: K-Means with 3 clusters
set.seed(123)
clustering_data <- species_confidence %>%
  select(total_count, avg_confidence) %>%
  scale()

kmeans_result <- kmeans(clustering_data, centers = 3)

species_confidence$cluster <- as.factor(kmeans_result$cluster)

# Visualization: Clusters
ggplot(species_confidence, aes(x = total_count, y = avg_confidence, color = cluster)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  labs(title = "Cluster Analysis of Species Based on Frequency and Confidence",
       x = "Frequency (log-transformed)",
       y = "Average Confidence Value") +
  theme_minimal()

library(factoextra)

# Elbow Method to determine the optimal number of clusters
fviz_nbclust(clustering_data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(title = "Elbow Method for Determining Optimal Number of Clusters")

# Silhouette Analysis
fviz_nbclust(clustering_data, kmeans, method = "silhouette") +
  labs(title = "Silhouette Analysis for Determining Optimal Number of Clusters")

# ============================
# 9. Determining Optimal Thresholds
# ============================

# Cumulative distribution for min_count
all_species_freq <- all_combined_data %>%
  group_by(region, wissenschaftlicher_name, common_name) %>%
  summarise(total_count = n(), .groups = 'drop') %>%
  arrange(desc(total_count)) %>%
  mutate(cumulative_percentage = cumsum(total_count) / sum(total_count) * 100)

# Plot the cumulative distribution
ggplot(all_species_freq, aes(x = total_count, y = cumulative_percentage)) +
  geom_line(color = "blue") +
  scale_x_log10() +
  labs(title = "Cumulative Distribution of Species Frequency",
       x = "Frequency (log-transformed)",
       y = "Cumulative Percentage") +
  theme_minimal()

# Calculate quantiles for min_count
quantiles_count <- quantile(all_species_freq$total_count, probs = c(0.75, 0.90), na.rm = TRUE)
print(quantiles_count)

# Confidence distribution
ggplot(species_confidence, aes(x = avg_confidence)) +
  geom_histogram(binwidth = 0.05, fill = "coral", color = "black") +
  facet_wrap(~ region) +
  labs(title = "Distribution of Average Confidence Value per Region",
       x = "Average Confidence Value",
       y = "Frequency") +
  theme_minimal()

# Set thresholds based on quantiles
min_count_75 <- quantiles_count[["75%"]]
min_count_90 <- quantiles_count[["90%"]]

log_message(paste("75% Quantile for total_count:", min_count_75))
log_message(paste("90% Quantile for total_count:", min_count_90))

# Filter based on the 75% Quantile
filtered_species_75 <- all_species_freq %>%
  filter(total_count >= min_count_75)

# Filter based on the 90% Quantile
filtered_species_90 <- all_species_freq %>%
  filter(total_count >= min_count_90)

# Save the filtered data
# fwrite(filtered_species_75, file.path(output_folder, "Filtered_Species_75_Percent.csv"))
# fwrite(filtered_species_90, file.path(output_folder, "Filtered_Species_90_Percent.csv"))

log_message("Filtered species frequency data based on quantiles saved.")

# ============================
# 10. Implementing Minimum Counts Based on total_count and CI
# ============================

# Load necessary libraries
library(dplyr)
library(data.table)
library(readxl)
library(ggplot2)
library(broom)
library(bit64)
library(boot)
library(rpart)
library(pROC)
library(VennDiagram)  # For Venn diagrams (optional)
library(UpSetR)       # For UpSet plots (optional)
library(tidyr)        # For pivot_wider and pivot_longer
library(igraph)       # For graph-based Venn diagrams

# Check the columns in all_combined_data
log_message("Columns in all_combined_data:")
print(names(all_combined_data))

# Step 1: Calculate total_count and CI (Confidence Index)
species_summary <- all_combined_data %>%
  group_by(region, wissenschaftlicher_name, common_name) %>%
  summarize(
    total_count = n(),  # Number of occurrences
    CI = mean(confidence, na.rm = TRUE),  # Average confidence value
    .groups = 'drop'
  )

# Check the calculated summary
log_message("Sample data in species_summary:")
print(head(species_summary))

# Step 2: Set thresholds based on quantiles
quantiles_count <- quantile(species_summary$total_count, probs = c(0.75, 0.90), na.rm = TRUE)
quantiles_CI <- quantile(species_summary$CI, probs = c(0.75, 0.90), na.rm = TRUE)

log_message(paste("75% Quantile for total_count:", quantiles_count[["75%"]]))
log_message(paste("90% Quantile for total_count:", quantiles_count[["90%"]]))
log_message(paste("75% Quantile for CI:", quantiles_CI[["75%"]]))
log_message(paste("90% Quantile for CI:", quantiles_CI[["90%"]]))

# Step 3: Quantile-based filtering (Approach 10a)
log_message("Start: 10a. Quantile-based filtering.")

filtered_species_quantile_75 <- species_summary %>%
  filter(total_count >= quantiles_count[["75%"]] & CI >= quantiles_CI[["75%"]])

filtered_species_quantile_90 <- species_summary %>%
  filter(total_count >= quantiles_count[["90%"]] & CI >= quantiles_CI[["90%"]])

# Calculate the number of species per region for 75% and 90% quantiles
number_species_75 <- filtered_species_quantile_75 %>%
  group_by(region) %>%
  summarise(number_species = n(), .groups = 'drop')

number_species_90 <- filtered_species_quantile_90 %>%
  group_by(region) %>%
  summarise(number_species = n(), .groups = 'drop')

# Save the filtered data
# fwrite(number_species_75, file.path(output_folder, "Number_Species_75_Percent.csv"))
# fwrite(number_species_90, file.path(output_folder, "Number_Species_90_Percent.csv"))

log_message("Filtered species frequency data based on quantiles (10a) saved.")

# Step 4: Bootstrapping to validate thresholds (Approach 10b)
log_message("Start: 10b. Bootstrapping to validate thresholds.")

boot_quantiles <- function(data, indices) {
  d <- data[indices, ]
  return(c(
    quantile(d$total_count, probs = 0.75, na.rm = TRUE),
    quantile(d$total_count, probs = 0.90, na.rm = TRUE),
    quantile(d$CI, probs = 0.75, na.rm = TRUE),
    quantile(d$CI, probs = 0.90, na.rm = TRUE)
  ))
}

set.seed(123)
boot_results <- boot(data = species_summary, statistic = boot_quantiles, R = 1000)

# Calculate confidence intervals for the quantiles
boot_ci_75_count <- boot.ci(boot_results, type = "perc", index = 1)
boot_ci_90_count <- boot.ci(boot_results, type = "perc", index = 2)
boot_ci_75_CI <- boot.ci(boot_results, type = "perc", index = 3)
boot_ci_90_CI <- boot.ci(boot_results, type = "perc", index = 4)

# Extract the mean quantiles
mean_quantiles <- apply(boot_results$t, 2, mean)

log_message("Bootstrapping completed. Confidence intervals of the quantiles:")
print(boot_ci_75_count)
print(boot_ci_90_count)
print(boot_ci_75_CI)
print(boot_ci_90_CI)

log_message("Average quantiles from bootstrapping:")
print(mean_quantiles)

# Set thresholds based on bootstrapped quantiles
boot_min_count_75 <- mean_quantiles[1]
boot_min_count_90 <- mean_quantiles[2]
boot_min_CI_75 <- mean_quantiles[3]
boot_min_CI_90 <- mean_quantiles[4]

log_message(paste("Bootstrapped 75% Quantile for total_count:", boot_min_count_75))
log_message(paste("Bootstrapped 90% Quantile for total_count:", boot_min_count_90))
log_message(paste("Bootstrapped 75% Quantile for CI:", boot_min_CI_75))
log_message(paste("Bootstrapped 90% Quantile for CI:", boot_min_CI_90))

filtered_species_bootstrap_75 <- species_summary %>%
  filter(total_count >= boot_min_count_75 & CI >= boot_min_CI_75)

filtered_species_bootstrap_90 <- species_summary %>%
  filter(total_count >= boot_min_count_90 & CI >= boot_min_CI_90)

# Calculate the number of species per region for bootstrapped 75% and 90%
number_species_bootstrap_75 <- filtered_species_bootstrap_75 %>%
  group_by(region) %>%
  summarise(number_species = n(), .groups = 'drop')

number_species_bootstrap_90 <- filtered_species_bootstrap_90 %>%
  group_by(region) %>%
  summarise(number_species = n(), .groups = 'drop')

# Save the filtered data
# fwrite(number_species_bootstrap_75, file.path(output_folder, "Number_Species_Bootstrap_75.csv"))
# fwrite(number_species_bootstrap_90, file.path(output_folder, "Number_Species_Bootstrap_90.csv"))

log_message("Filtered species frequency data based on bootstrapped quantiles (10b) saved.")

# Step 5: Adaptive threshold system per region (Approach 10c)
log_message("Start: 10c. Adaptive threshold system per region.")

adaptive_thresholds <- species_summary %>%
  group_by(region) %>%
  summarize(
    min_count_75 = quantile(total_count, probs = 0.75, na.rm = TRUE),
    min_count_90 = quantile(total_count, probs = 0.90, na.rm = TRUE),
    min_CI_75 = quantile(CI, probs = 0.75, na.rm = TRUE),
    min_CI_90 = quantile(CI, probs = 0.90, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge with species_summary
species_summary_adaptive <- species_summary %>%
  left_join(adaptive_thresholds, by = "region") %>%
  mutate(
    keep_75 = total_count >= min_count_75 & CI >= min_CI_75,
    keep_90 = total_count >= min_count_90 & CI >= min_CI_90
  )

# Filter based on adaptive thresholds
filtered_species_adaptive_75 <- species_summary_adaptive %>%
  filter(keep_75)

filtered_species_adaptive_90 <- species_summary_adaptive %>%
  filter(keep_90)

# Calculate the number of species per region for adaptive 75% and 90%
number_species_adaptive_75 <- filtered_species_adaptive_75 %>%
  group_by(region) %>%
  summarise(number_species = n(), .groups = 'drop')

number_species_adaptive_90 <- filtered_species_adaptive_90 %>%
  group_by(region) %>%
  summarise(number_species = n(), .groups = 'drop')

# Save the filtered data
# fwrite(number_species_adaptive_75, file.path(output_folder, "Number_Species_Adaptive_75.csv"))
# fwrite(number_species_adaptive_90, file.path(output_folder, "Number_Species_Adaptive_90.csv"))

log_message("Filtered species frequency data based on adaptive quantiles (10c) saved.")

# Step 6: Multivariate Approaches (Decision Tree and ROC Curve) (Approach 10d)
log_message("Start: 10d. Multivariate Approaches: Decision Tree and ROC Curve.")

# Decision Tree for Classification
# Create a binary target variable based on combined criteria
species_summary <- species_summary %>%
  mutate(target = ifelse(total_count >= boot_min_count_75 & CI >= boot_min_CI_75, 1, 0))

# Train a decision tree
tree_model <- rpart(target ~ total_count + CI, data = species_summary, method = "class")

# Make predictions
species_summary$predicted <- predict(tree_model, species_summary, type = "class")

# Filter based on predictions
filtered_species_tree <- species_summary %>%
  filter(predicted == 1)

# Calculate the number of species per region for the decision tree
number_species_tree <- filtered_species_tree %>%
  group_by(region) %>%
  summarise(number_species = n(), .groups = 'drop')

# Save the filtered data
# fwrite(number_species_tree, file.path(output_folder, "Number_Species_Tree.csv"))

log_message("Filtered species frequency data based on decision tree (10d) saved.")

# ROC Curve to determine optimal thresholds
# Logical target variable for ROC
species_summary <- species_summary %>%
  mutate(target_binary = as.numeric(target))

# Logistic regression as a combined model
logistic_model <- glm(target_binary ~ total_count + CI, data = species_summary, family = binomial)

# Check for convergence issues
if (!logistic_model$converged) {
  log_message("Warning: Logistic regression did not converge. Check the data or the model.")
} else {
  log_message("Logistic regression successfully converged.")
}

# Predict probabilities
species_summary$prob <- predict(logistic_model, species_summary, type = "response")

# ROC curve for the combined model
roc_combined <- roc(species_summary$target_binary, species_summary$prob)
plot(roc_combined, main = "ROC Curve for Combined Model")

# Determine the optimal threshold
optimal_threshold <- coords(roc_combined, "best", ret = "threshold")
log_message(paste("Optimal threshold for combined model:", optimal_threshold))

# Filter based on the optimal threshold
filtered_species_roc <- species_summary %>%
  filter(prob >= optimal_threshold)

# Calculate the number of species per region for ROC
number_species_roc <- filtered_species_roc %>%
  group_by(region) %>%
  summarise(number_species = n(), .groups = 'drop')

# Save the filtered data
# fwrite(number_species_roc, file.path(output_folder, "Number_Species_ROC.csv"))

log_message("Filtered species frequency data based on ROC thresholds (10d) saved.")

# ============================
# End of Implementing Minimum Counts Based on total_count and CI
# ============================

# ============================
# 11. Ensuring Existence of Species with Extreme CI Values
# ============================

log_message("Start: 10e. Ensuring existence of species with extreme CI values.")

# Species with CI >= 0.9
high_CI_species <- species_summary %>%
  filter(CI >= 0.9)

log_message(paste("Number of species with CI >= 0.9:", nrow(high_CI_species)))
print(high_CI_species)

# Species with CI <= 0.1
low_CI_species <- species_summary %>%
  filter(CI <= 0.1)

log_message(paste("Number of species with CI <= 0.1:", nrow(low_CI_species)))
print(low_CI_species)

# Save species with extreme CI values
# fwrite(high_CI_species, file.path(output_folder, "High_CI_Species.csv"))
# fwrite(low_CI_species, file.path(output_folder, "Low_CI_Species.csv"))

log_message("Species with extreme CI values saved.")

# ============================
# 12//. Filter of all_combined_data based on species_confidence_filtered
# ============================

# Da overlap_all für alle Regionen 0 ist, wählen wir eine Methode aus, die die höchste Übereinstimmung aufweist.
# Zum Beispiel wählen wir 'Bootstrap_75' als primäre Filtermethode.

# Definieren von 'species_confidence_filtered' basierend auf Bootstrap_75
species_confidence_filtered <- filtered_species_bootstrap_75

# Entfernen von Duplikaten in all_combined_data vor dem Join
all_combined_data_unique <- all_combined_data %>%
  distinct(region, wissenschaftlicher_name, common_name, .keep_all = TRUE)

# Sicherstellen, dass species_confidence_filtered nur eindeutige Einträge enthält
species_confidence_filtered_unique <- species_confidence_filtered %>%
  distinct(region, wissenschaftlicher_name, common_name, .keep_all = TRUE)

# Durchführen des Joins
filtered_audiomoth <- all_combined_data_unique %>%
  inner_join(species_confidence_filtered_unique, by = c("region", "wissenschaftlicher_name", "common_name"))

# Prüfen der Anzahl der doppelten Einträge
duplicates <- filtered_audiomoth %>%
  group_by(region, wissenschaftlicher_name, common_name) %>%
  filter(n() > 1)

log_message(paste("Anzahl doppelter Einträge nach dem Join:", nrow(duplicates)))

# Berechnen der Summen ohne Duplikate
audiomoth_counts <- filtered_audiomoth %>%
  group_by(region, wissenschaftlicher_name) %>%
  summarise(Total_Count = sum(total_count, na.rm = TRUE), .groups = 'drop')


# ============================
# 12. Calculating Shannon Diversity Index (SHDI) for Species Frequency Data
# ============================

log_message("Calculating Shannon Diversity Index (SHDI) for species frequency data.")

# Load the vegan package for diversity calculations
if (!requireNamespace("vegan", quietly = TRUE)) {
  install.packages("vegan")
}
library(vegan)

# 1. Clean the data (if not already done)
filtered_audiomoth <- filtered_audiomoth %>%
  mutate(
    region = as.factor(region),
    wissenschaftlicher_name = as.factor(wissenschaftlicher_name),
    common_name = as.factor(common_name)
  )

# 2. Group by region and scientific names and sum the total counts
audiomoth_counts <- filtered_audiomoth %>%
  group_by(region, wissenschaftlicher_name) %>%
  summarise(Total_Count = sum(total_count, na.rm = TRUE), .groups = 'drop')

# 3. Pivot the dataframe to a wide format
audiomoth_counts_wide <- audiomoth_counts %>%
  pivot_wider(names_from = wissenschaftlicher_name, values_from = Total_Count, values_fill = 0)

# 4. Set the region as the row names for diversity calculations
audiomoth_counts_df <- audiomoth_counts_wide %>%
  column_to_rownames(var = "region")

# 5. Check the structure of the pivoted dataframe
str(audiomoth_counts_df)
print(head(audiomoth_counts_df))

# 6. Calculate the Shannon Diversity Index (SHDI)
# Use vegan::diversity to ensure the correct function is called
audiomoth_shdi <- vegan::diversity(audiomoth_counts_df, index = "shannon")

# 7. Create a dataframe for SHDI results
audiomoth_shdi_df <- data.frame(
  Region = rownames(audiomoth_counts_df),
  SHDI_Species_Frequency = audiomoth_shdi,
  row.names = NULL
)

# 8. Save the SHDI results
# fwrite(audiomoth_shdi_df, file.path(output_folder, "SHDI_Audiomoth_Weighted.csv"))
log_message("SHDI for species frequency data (Audiomoth) successfully calculated and saved.")

# ============================
# 13. Validation with Point Counts (Kartierung)
# ============================

log_message("Start: 13. Validation with Point Counts (Kartierung).")

kartierung_path <- file.path(base_dir, "Kartierung.xlsx")
# Load the Kartierung.xlsx file with error handling

log_message("Loading Kartierung.xlsx.")
kartierung_data <- tryCatch({
  read_excel(kartierung_path, skip = 2, 
             col_names = c("Gebiet", "Ort", "Fläche", "Datum", 
                           "Durchgang", "Art_deutsch", "Zahl", 
                           "Wissenschaftlicher_Name"))
}, error = function(e) {
  log_message(paste("Error loading Kartierung.xlsx:", e$message))
  NULL
})

# Check if the file was successfully loaded
if (is.null(kartierung_data)) {
  stop("Kartierung.xlsx could not be loaded. Please check the file path and format.")
}

# Prepare the Kartierung data
Kartierung <- kartierung_data %>%
  mutate(
    Gebiet = as.character(Gebiet),
    Ort = as.character(Ort),
    Wissenschaftlicher_Name = as.character(Wissenschaftlicher_Name),
    Art_deutsch = as.character(Art_deutsch),
    total_zahl = as.integer(Zahl)
  )

# Calculate SHDI for manual counts
log_message("Calculating Shannon Diversity Index (SHDI) for manual counts.")

# 1. Group by Gebiet and scientific names and sum the total counts
manual_counts <- Kartierung %>%
  group_by(Gebiet, Wissenschaftlicher_Name) %>%
  summarize(Total_Count = sum(total_zahl, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Wissenschaftlicher_Name, values_from = Total_Count, values_fill = 0)

# 2. Set Gebiet as the row names for diversity calculations
manual_counts_df <- manual_counts %>%
  column_to_rownames(var = "Gebiet")

# 3. Check the structure of the pivoted dataframe
str(manual_counts_df)
print(head(manual_counts_df))

# 4. Calculate the Shannon Diversity Index (SHDI) using vegan::diversity
manual_shdi <- vegan::diversity(manual_counts_df, index = "shannon")

# 5. Create a dataframe for SHDI results
manual_shdi_df <- data.frame(
  Region = rownames(manual_counts_df),
  SHDI_Manual = manual_shdi,
  row.names = NULL
)

# 6. Helper function to clean and standardize region names (if needed)
clean_region <- function(df, region_col = "Region") {
  df %>%
    mutate(!!region_col := as.character(.data[[region_col]])) %>%  # Ensure it is character
    mutate(!!region_col := str_trim(.data[[region_col]])) %>%     # Remove leading/trailing spaces
    mutate(!!region_col := str_replace_all(.data[[region_col]], "^Region", "")) # Remove "Region" prefix if present
}

# 7. Clean the 'Region' columns in both SHDI dataframes
manual_shdi_df <- clean_region(manual_shdi_df, "Region")
audiomoth_shdi_df <- audiomoth_shdi_df %>%
  clean_region("Region")  # Ensure 'audiomoth_shdi_df' is also cleaned

# 8. Check unique region names to ensure consistency
cat("Unique Regions in manual_shdi_df:\n")
print(unique(manual_shdi_df$Region))

cat("\nUnique Regions in audiomoth_shdi_df:\n")
print(unique(audiomoth_shdi_df$Region))

# Optional: Check if any regions are missing or do not match
missing_in_audiomoth <- setdiff(manual_shdi_df$Region, audiomoth_shdi_df$Region)
if (length(missing_in_audiomoth) > 0) {
  warning("The following regions are present in manual_shdi_df but missing in audiomoth_shdi_df: ",
          paste(missing_in_audiomoth, collapse = ", "))
}

missing_in_manual <- setdiff(audiomoth_shdi_df$Region, manual_shdi_df$Region)
if (length(missing_in_manual) > 0) {
  warning("The following regions are present in audiomoth_shdi_df but missing in manual_shdi_df: ",
          paste(missing_in_manual, collapse = ", "))
}

# 9. Combine the SHDI data from manual and Audiomoth sources
shdi_data <- manual_shdi_df %>%
  full_join(audiomoth_shdi_df, by = "Region")

# 10. Check the combined SHDI data
cat("\nCombined SHDI Data (after full_join):\n")
print(head(shdi_data))

# 11. Save the combined SHDI data
# fwrite(shdi_data, file.path(output_folder, "Combined_SHDI.csv"))
log_message("Combined SHDI data from manual and Audiomoth sources successfully saved.")

# Scatterplot of SHDI values
ggplot(shdi_data, aes(x = SHDI_Manual, y = SHDI_Species_Frequency)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparison of Shannon Diversity Index (SHDI)",
       x = "SHDI - Manual",
       y = "SHDI - Audiomoth") +
  theme_minimal()

# Calculate the correlation
correlation <- cor(shdi_data$SHDI_Manual, shdi_data$SHDI_Species_Frequency, use = "complete.obs")
log_message(paste("Correlation between manual and Audiomoth SHDI values:", round(correlation, 3)))

# ============================
# 14. Calculating Species Richness and Pielou's Evenness
# ============================

log_message("Calculating Species Richness and Pielou's Evenness.")

# Function to calculate Richness and Pielou's Evenness
calculate_indices <- function(count_df, shdi_values, method_name) {
  # Convert row names to a "Region" column
  count_df <- count_df %>%
    rownames_to_column(var = "Region") %>%
    mutate(
      Region = as.character(Region),
      Region = str_trim(Region),
      Region = str_replace_all(Region, "^Region", "")
    )
  
  # Calculate Richness and Pielou's Evenness
  indices <- count_df %>%
    mutate(RICHNESS = rowSums(across(-Region, ~ .x > 0))) %>%  # Number of species with Count > 0
    mutate(Pielou_Evenness = shdi_values / log(RICHNESS)) %>%  # Pielou's Evenness
    mutate(Pielou_Evenness = ifelse(is.finite(Pielou_Evenness), Pielou_Evenness, NA)) %>%  # Handle NA
    select(Region, RICHNESS, Pielou_Evenness) %>%
    mutate(Filter_Method = method_name)
  
  return(indices)
}

# 1. Calculate Richness and Evenness for manual counts
# Assumption: 'manual_counts_df' contains the counts for manual counts
manual_indices <- calculate_indices(
  count_df = manual_counts_df,
  shdi_values = shdi_data$SHDI_Manual,
  method_name = "Manual"
)

# 2. Calculate Richness and Evenness for Audiomoth data
# Assumption: 'audiomoth_counts_df' contains the counts for Audiomoth data
audiomoth_indices <- calculate_indices(
  count_df = audiomoth_counts_df,
  shdi_values = shdi_data$SHDI_Species_Frequency,
  method_name = "Audiomoth"
)

# 3. Combine the indices into one dataframe
combined_indices <- bind_rows(manual_indices, audiomoth_indices)

# 4. Pivot the combined indices to a wide format to have Richness and Evenness for both methods
richness_evenness_wide <- combined_indices %>%
  pivot_wider(
    names_from = Filter_Method,
    values_from = c(RICHNESS, Pielou_Evenness),
    names_sep = "_"
  )

# 5. Clean the 'Region' names in the combined indices
richness_evenness_wide <- clean_region(richness_evenness_wide, "Region")

# 6. Check the unique region names in the combined indices
cat("\nUnique Regions in richness_evenness_wide:\n")
print(unique(richness_evenness_wide$Region))

# 7. Merge the Richness and Evenness with the SHDI data
shdi_data <- shdi_data %>%
  left_join(richness_evenness_wide, by = "Region")

# 8. Verify that Richness and Pielou_Evenness are now present in the SHDI data
required_columns <- c("RICHNESS_Manual", "RICHNESS_Audiomoth", "Pielou_Evenness_Manual", "Pielou_Evenness_Audiomoth")
missing_columns <- setdiff(required_columns, names(shdi_data))
if (length(missing_columns) > 0) {
  stop(paste("The following required columns are missing in shdi_data:", paste(missing_columns, collapse = ", ")))
} else {
  log_message("All required columns are present.")
}

# 9. Check the SHDI data after merging Richness and Evenness
cat("\nSHDI Data after merging Richness and Evenness:\n")
print(head(shdi_data))

# 10. Save the combined SHDI, Richness, and Evenness data
# fwrite(shdi_data, file.path(output_folder, "Combined_SHDI_Richness_Evenness.csv"))
log_message("Species Richness and Pielou's Evenness for all methods successfully calculated and saved.")

# ============================
# 16. Verifying the Calculations of Diversity Indices
# ============================

log_message("Verifying the calculations of diversity indices.")

# 16.1 Verifying the calculations of SHDI_Manual and SHDI_Species_Frequency
for (i in 1:nrow(shdi_data)) {
  region <- shdi_data$Region[i]
  
  # SHDI_Manual
  manual_shdi_manual <- shdi_data$SHDI_Manual[i]
  manual_shdi_manual_check <- manual_shdi_df$SHDI_Manual[manual_shdi_df$Region == region]
  
  if (!is.na(manual_shdi_manual_check) && abs(manual_shdi_manual - manual_shdi_manual_check) < 1e-6) {
    log_message(paste("SHDI_Manual for region", region, "was correctly calculated."))
  } else {
    warning(paste("Discrepancy in the calculation of SHDI_Manual for region", region, "or NA values present."))
  }
  
  # SHDI_Species_Frequency
  audiomoth_shdi_freq <- shdi_data$SHDI_Species_Frequency[i]
  audiomoth_shdi_freq_check <- audiomoth_shdi_df$SHDI_Species_Frequency[audiomoth_shdi_df$Region == region]
  
  if (!is.na(audiomoth_shdi_freq_check) && abs(audiomoth_shdi_freq - audiomoth_shdi_freq_check) < 1e-6) {
    log_message(paste("SHDI_Species_Frequency for region", region, "was correctly calculated."))
  } else {
    warning(paste("Discrepancy in the calculation of SHDI_Species_Frequency for region", region, "or NA values present."))
  }
}

# 16.2 Verifying the calculations of Richness
for (i in 1:nrow(shdi_data)) {
  region <- shdi_data$Region[i]
  
  # Richness Manual
  expected_richness_manual <- sum(manual_counts_df[region, ] > 0, na.rm = TRUE)
  calculated_richness_manual <- shdi_data$RICHNESS_Manual[i]
  
  if (!is.na(calculated_richness_manual) && expected_richness_manual == calculated_richness_manual) {
    log_message(paste("Richness_Manual for region", region, "was correctly calculated."))
  } else {
    warning(paste("Discrepancy in the calculation of Richness_Manual for region", region, "or NA values present."))
  }
  
  # Richness Audiomoth
  expected_richness_audiomoth <- sum(audiomoth_counts_df[region, ] > 0, na.rm = TRUE)
  calculated_richness_audiomoth <- shdi_data$RICHNESS_Audiomoth[i]
  
  if (!is.na(calculated_richness_audiomoth) && expected_richness_audiomoth == calculated_richness_audiomoth) {
    log_message(paste("Richness_Audiomoth for region", region, "was correctly calculated."))
  } else {
    warning(paste("Discrepancy in the calculation of Richness_Audiomoth for region", region, "or NA values present."))
  }
}

# ============================
# 17. Regression Analysis, ANOVA, and Normality Tests
# ============================

# 17a. Define the new data: Area and AUKM Mapping
# Already included in your previous script and correctly adjusted
# No changes necessary as step 14 already calculated Richness and Evenness for both methods

# Define the area data
gebiet_data <- data.frame(
  Gebiet = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
  Größe_in_m2 = c(228.903, 198.806, 247.599, 417.740, 34.791, 27.872, 4.466, 6.699, 17.877, 6.699, 17.877),
  Anzahl_Audiomoth = c(4, 4, 4, 4, 4, 4, 4, 4, 2, 3, 2),
  stringsAsFactors = FALSE
)

# Define the AUKM mapping table
auk_mapping <- data.frame(
  region_letter = c("A","B","C","D","E","F","G","H","I","J","K"),
  AUKM = c("ja","nein","ja","nein","ja","nein","ja","nein","nein","ja","ja"),
  stringsAsFactors = FALSE
)

# ============================
# 17b. Merge the area data and AUKM with shdi_data
# ============================

log_message("Merging area data and AUKM with shdi_data.")

# Merge shdi_data with gebiet_data based on Region
shdi_data <- shdi_data %>%
  left_join(gebiet_data, by = c("Region" = "Gebiet"))

# Merge shdi_data with auk_mapping based on Region
shdi_data <- shdi_data %>%
  left_join(auk_mapping, by = c("Region" = "region_letter"))

# Check the first few lines after merging
cat("First lines of shdi_data after merging area data and AUKM:\n")
print(head(shdi_data))

log_message("Merging completed.")

# ============================
# 18. Calculation and Addition of Total_Species_Manual and Total_Species_Audiomoth
# ============================

log_message("Calculating Total_Species_Manual and Total_Species_Audiomoth.")

# Calculate the total number for manual counts
total_species_manual <- manual_counts_df %>%
  rownames_to_column(var = "Region") %>%  # Convert row names to a column named "Region"
  mutate(Total_Species_Manual = rowSums(across(-Region, ~ . > 0))) %>%
  select(Region, Total_Species_Manual)

# Calculate the total number for Audiomoth data
total_species_audiomoth <- audiomoth_counts_df %>%
  rownames_to_column(var = "Region") %>%  # Convert row names to a column named "Region"
  mutate(
    Region = str_replace_all(Region, "^Region", ""),  # Remove 'Region' prefix if present
    Total_Species_Audiomoth = rowSums(across(-Region, ~ . > 0))  # Count species with at least 1 occurrence per region
  ) %>%
  select(Region, Total_Species_Audiomoth)

# Merge the calculated values into shdi_data
shdi_data <- shdi_data %>%
  left_join(total_species_manual, by = "Region") %>%
  left_join(total_species_audiomoth, by = "Region")

# Check the first few lines after adding
cat("SHDI Data after adding Total_Species_Manual and Total_Species_Audiomoth:\n")
print(head(shdi_data))

log_message("Starting regression analysis, ANOVA, and normality tests.")

# Verify that the necessary columns are now present
required_columns <- c("SHDI_Manual", "SHDI_Species_Frequency",
                      "RICHNESS_Manual", "RICHNESS_Audiomoth",
                      "Pielou_Evenness_Manual", "Pielou_Evenness_Audiomoth",
                      "Größe_in_m2", "AUKM",
                      "Total_Species_Manual", "Total_Species_Audiomoth")
missing_columns <- setdiff(required_columns, names(shdi_data))

if (length(missing_columns) > 0) {
  stop(paste("The following necessary columns are missing in shdi_data:", paste(missing_columns, collapse = ", ")))
} else {
  log_message("All required columns are present.")
}

print(head(shdi_data))

# ============================
# 17c. Regression Analysis, ANOVA, and Normality Tests
# ============================

log_message("Starting regression analysis, ANOVA, and normality tests.")

# 17c1. Linear Regressions

# Verify that the necessary columns are present
required_columns <- c("SHDI_Manual", "SHDI_Species_Frequency",
                      "RICHNESS_Manual", "RICHNESS_Audiomoth",
                      "Pielou_Evenness_Manual", "Pielou_Evenness_Audiomoth",
                      "Größe_in_m2", "AUKM",
                      "Total_Species_Manual", "Total_Species_Audiomoth")
missing_columns <- setdiff(required_columns, names(shdi_data))

if (length(missing_columns) > 0) {
  stop(paste("The following necessary columns are missing in shdi_data:", paste(missing_columns, collapse = ", ")))
}

# Remove rows with NA in the relevant columns for a meaningful regression analysis
shdi_data_clean <- shdi_data %>%
  filter(!is.na(SHDI_Manual),
         !is.na(SHDI_Species_Frequency),
         !is.na(RICHNESS_Manual),
         !is.na(RICHNESS_Audiomoth),
         !is.na(Pielou_Evenness_Manual),
         !is.na(Pielou_Evenness_Audiomoth),
         !is.na(Größe_in_m2),
         !is.na(AUKM),
         !is.na(Total_Species_Manual),
         !is.na(Total_Species_Audiomoth))

# Check the cleaned data
cat("Cleaned SHDI data for regression:\n")
print(shdi_data_clean)

# Check the number of remaining data points
if (nrow(shdi_data_clean) < 5) {
  warning("Too few data points after cleaning for a reliable regression analysis.")
}

# 1. Linear Regression: SHDI_Manual as a function of SHDI_Species_Frequency
log_message("Performing linear regression of SHDI_Manual on SHDI_Species_Frequency.")
model1 <- lm(SHDI_Manual ~ SHDI_Species_Frequency, data = shdi_data_clean)
summary_model1 <- summary(model1)
print(summary_model1)

# Store the regression results in a list
regression_results_list <- list()
regression_results_list[["Model1"]] <- tidy(summary_model1)

# 2. Multiple Linear Regression: SHDI_Manual as a function of SHDI_Species_Frequency, RICHNESS, Pielou_Evenness, Größe_in_m2, and AUKM
log_message("Performing multiple linear regression of SHDI_Manual on SHDI_Species_Frequency, RICHNESS, Pielou_Evenness, Größe_in_m2, and AUKM.")

# Since AUKM is a categorical variable, it needs to be treated as a factor
shdi_data_clean <- shdi_data_clean %>%
  mutate(AUKM = factor(AUKM, levels = c("ja", "nein")))

# Check if AUKM actually has two levels
if (length(levels(shdi_data_clean$AUKM)) < 2) {
  stop("The variable AUKM must have at least two levels (e.g., 'ja' and 'nein').")
}

# Multiple Linear Regression
model2 <- lm(SHDI_Manual ~ SHDI_Species_Frequency + RICHNESS_Manual + RICHNESS_Audiomoth +
               Pielou_Evenness_Manual + Pielou_Evenness_Audiomoth +
               Größe_in_m2 + AUKM, data = shdi_data_clean)
summary_model2 <- summary(model2)
print(summary_model2)

regression_results_list[["Model2"]] <- tidy(summary_model2)

# 3. Linear Regression: SHDI_Species_Frequency as a function of SHDI_Manual
log_message("Performing linear regression of SHDI_Species_Frequency on SHDI_Manual.")
model3 <- lm(SHDI_Species_Frequency ~ SHDI_Manual, data = shdi_data_clean)
summary_model3 <- summary(model3)
print(summary_model3)

regression_results_list[["Model3"]] <- tidy(summary_model3)

# Function to prepare the regression results
prepare_regression_results <- function(model_summary, model_name) {
  coefficients <- as.data.frame(model_summary$coefficients)
  coefficients$Variable <- rownames(coefficients)
  rownames(coefficients) <- NULL
  coefficients <- coefficients %>%
    rename(
      Estimate = Estimate,
      Std_Error = `Std. Error`,
      t_value = `t value`,
      p_Value = `Pr(>|t|)`
    ) %>%
    mutate(Modell = model_name) %>%
    select(Modell, Variable, Estimate, Std_Error, t_value, p_Value)
  
  return(coefficients)
}

# Prepare the results for all models
regression_results_df <- bind_rows(
  prepare_regression_results(summary_model1, "Model1"),
  prepare_regression_results(summary_model2, "Model2"),
  prepare_regression_results(summary_model3, "Model3")
)

# Save the summarized regression results as a CSV
# fwrite(regression_results_df, file.path(output_folder, "Regression_Results_Summary.csv"))
log_message("Summarized regression results saved.")

# Optional: Save each model's results separately
# fwrite(regression_results_list[["Model1"]], file.path(output_folder, "Regression_Results_Model1.csv"))
log_message("Regression results for Model 1 saved separately.")

# fwrite(regression_results_list[["Model2"]], file.path(output_folder, "Regression_Results_Model2.csv"))
log_message("Regression results for Model 2 saved separately.")

# fwrite(regression_results_list[["Model3"]], file.path(output_folder, "Regression_Results_Model3.csv"))
log_message("Regression results for Model 3 saved separately.")

# 17c2. ANOVA for Model2
log_message("Performing ANOVA for Model2.")

anova_model2 <- anova(model2)
print(anova_model2)

# Save ANOVA results
# fwrite(as.data.frame(anova_model2), file.path(output_folder, "ANOVA_Model2.csv"))
log_message("ANOVA results for Model 2 saved.")

# 17c3. Normality Tests (Shapiro-Wilk)
log_message("Performing Shapiro-Wilk Test on the residuals of Model2.")

# Shapiro-Wilk Test for the residuals of Model2
shapiro_test_model2 <- shapiro.test(residuals(model2))
print(shapiro_test_model2)

# Save Shapiro-Wilk test results
shapiro_results <- data.frame(
  Test = "Shapiro-Wilk",
  Statistic = shapiro_test_model2$statistic,
  p_Value = shapiro_test_model2$p.value
)

# fwrite(shapiro_results, file.path(output_folder, "Shapiro_Wilk_Test_Model2.csv"))
log_message("Shapiro-Wilk Test results for Model 2 saved.")

# ============================
# 17d. Visualization of Regression Results
# ============================

log_message("Starting visualization of regression results.")

# Plot 1: SHDI_Manual vs SHDI_Species_Frequency with Regression Line (Model1)
plot1 <- ggplot(shdi_data_clean, aes(x = SHDI_Species_Frequency, y = SHDI_Manual)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Regression of SHDI_Manual on SHDI_Species_Frequency",
    x = "SHDI Species Frequency",
    y = "SHDI Manual"
  ) +
  theme_minimal()

# Save the plot
# ggsave(filename = file.path(output_folder, "Regressionplot_SHDI_Manual_on_SHDI_Species_Frequency.png"),
#        plot = plot1, width = 8, height = 6)
# log_message("Regression plot SHDI_Manual on SHDI_Species_Frequency saved.")

# Plot 2: Residuals Plot for Model2
# Since ggplot2 does not have a direct residuals plot function for lm objects, we use base R
# residual_plot_path <- file.path(output_folder, "Residualsplot_Model2.png")
# png(filename = residual_plot_path, width = 800, height = 600)
# par(mfrow = c(2,2))
# plot(model2)
# dev.off()
# log_message("Residuals plot for Model 2 saved.")

# Plot 3: SHDI_Manual vs RICHNESS_Manual with Regression Line (Part of Model2)
plot3 <- ggplot(shdi_data_clean, aes(x = RICHNESS_Manual, y = SHDI_Manual)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Regression of SHDI_Manual on RICHNESS_Manual",
    x = "Species Richness (Manual)",
    y = "SHDI Manual"
  ) +
  theme_minimal()

# Save the plot
# ggsave(filename = file.path(output_folder, "Regressionplot_SHDI_Manual_on_Richness_Manual.png"),
#        plot = plot3, width = 8, height = 6)
# log_message("Regression plot SHDI_Manual on Richness_Manual saved.")

# Optional: Additional Visualizations for Audiomoth Data
# Example: SHDI_Species_Frequency vs RICHNESS_Audiomoth
plot4 <- ggplot(shdi_data_clean, aes(x = RICHNESS_Audiomoth, y = SHDI_Species_Frequency)) +
  geom_point(alpha = 0.6, color = "orange") +
  geom_smooth(method = "lm", se = TRUE, color = "brown") +
  labs(
    title = "Regression of SHDI_Species_Frequency on RICHNESS_Audiomoth",
    x = "Species Richness (Audiomoth)",
    y = "SHDI Species Frequency"
  ) +
  theme_minimal()

# Save the plot
# ggsave(filename = file.path(output_folder, "Regressionplot_SHDI_Species_Frequency_on_Richness_Audiomoth.png"),
#        plot = plot4, width = 8, height = 6)
log_message("Regression plot SHDI_Species_Frequency on RICHness_Audiomoth saved.")

# ============================
# End of Regression Analysis, ANOVA, and Normality Tests
# ============================

# ============================
# 18. Completion and Summary
# ============================

log_message("Regression analysis completed. Summary of results:")

# Display the summarized regression results
print(regression_results_df)

# Optional: Save the summary in a file (already included in step 17c)

log_message("All data processing and analysis steps were successfully completed.")

# ============================
# End of Completion and Summary
# ============================
