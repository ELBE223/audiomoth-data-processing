getwd()


# ============================
# Script_Audiomoth_PointCounts_LB
# Purpose: Analysis of bird species based on data from AudioMoth devices and PointCounts.
# Author: Lucas Beseler
# Date: 2024-11-16
# ============================

# ============================
# 1. Setup: Install and Load Required Packages
# ============================

# Install and load the pacman package if not already installed
if (!require("pacman")) install.packages("pacman", dependencies = TRUE)
library(pacman)

# Load all required packages
p_load(
  tidyverse, vegan, tibble, readxl, data.table, cluster, 
  ggplot2, viridis, reshape2, scales, ggthemes, stringr, 
  microbenchmark, plotly, bit64, RSQLite, fuzzyjoin, ggrepel, 
  parallel, lubridate, writexl, stringdist, broom, boot, rpart, pROC
)

# ============================
# 2. Define Paths and Initialize Data
# ============================

# Define a configuration list for paths
config <- list(
  unzip_dir = "/Users/lucasbeseler/Desktop/Audiomoth_Kartierung_Shannonhabitat/Audiomoth_unzipped",
  output_folder = "/Users/lucasbeseler/Desktop/files/files_output",
  birdnet_labels = "BirdNET_Labels.xlsx",
  species_list = "Species_Saxony_Anhalt_updated.xlsx",
  kartierung = "Kartierung.xlsx",
  biodiversity_per_device = "Biodiversity_Per_Device_AudioMoth.csv",
  log_file = "processing_log.txt"
)

# Create the output directory if it doesn't exist
dir.create(config$output_folder, recursive = TRUE, showWarnings = FALSE)

# Define subdirectories in the output folder
config$audiomoth_output <- file.path(config$output_folder, "AudioMoth_Data")
config$kartierung_output <- file.path(config$output_folder, "Kartierung_Data")
config$plots_output <- file.path(config$output_folder, "Plots")

# Create subdirectories if they don't exist
dir.create(config$audiomoth_output, recursive = TRUE, showWarnings = FALSE)
dir.create(config$kartierung_output, recursive = TRUE, showWarnings = FALSE)
dir.create(config$plots_output, recursive = TRUE, showWarnings = FALSE)

# Define the path to the log file
config$log_file <- file.path(config$output_folder, "processing_log.txt")

# Function to log messages
log_message <- function(message, log_file) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_message <- paste(timestamp, "-", message, "\n")
  cat(full_message)
  cat(full_message, file = log_file, append = TRUE)
}

# Log the start of the analysis
log_message("===== Analysis Started =====", config$log_file)

# ============================
# 3. Set Working Directory and Locate Required Files
# ============================

# Set the working directory to the unzipped directory
setwd(config$unzip_dir)
log_message(paste("Working directory set to:", config$unzip_dir), config$log_file)

# Search for the required files
required_files <- c(config$birdnet_labels, config$species_list, config$kartierung)

# Function to find the file in the unzipped directory
find_file_in_unzip_dir <- function(file_name, unzip_dir) {
  file_path <- list.files(unzip_dir, pattern = paste0("^", file_name, "$"), recursive = TRUE, full.names = TRUE)
  if (length(file_path) == 0) {
    return(NULL)
  } else {
    return(file_path[1]) # Return the first match
  }
}

# Initialize a list to store the paths of required files
required_files_paths <- list()

for (file_name in required_files) {
  file_path <- find_file_in_unzip_dir(file_name, config$unzip_dir)
  if (is.null(file_path)) {
    log_message(paste("Required file not found in:", config$unzip_dir, "-", file_name), config$log_file)
    stop(paste("Required file missing:", file_name))
  } else {
    log_message(paste("Found file:", file_path), config$log_file)
    required_files_paths[[file_name]] <- file_path
  }
}

# Update the config paths to point to the actual file locations
config$birdnet_labels <- required_files_paths[[config$birdnet_labels]]
config$species_list <- required_files_paths[[config$species_list]]
config$kartierung <- required_files_paths[[config$kartierung]]

# ============================
# 4. Define Data Processing Functions
# ============================

# 4a. Function to Standardize Region Names
standardize_region_names <- function(region_vector, prefix_remove = FALSE) {
  region_vector <- toupper(trimws(region_vector))
  if (prefix_remove) {
    region_vector <- str_remove(region_vector, "^REGION")
  }
  return(region_vector)
}

# 4b. Function to Calculate SHDI for Manual Mapping Data
calculate_manual_shdi <- function(kartierung_path, output_folder, log_file_path) {
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
  
  if (is.null(kartierung_data)) {
    stop("Kartierung.xlsx could not be loaded. Please check the file path and format.")
  }
  
  # Clean Kartierung data
  Kartierung <- kartierung_data %>%
    mutate(
      Gebiet = as.character(Gebiet),
      Ort = as.character(Ort),
      Wissenschaftlicher_Name = as.character(Wissenschaftlicher_Name),
      Art_deutsch = as.character(Art_deutsch),
      total_zahl = as.integer(Zahl)
    )
  
  log_message("Kartierung data cleaned.", log_file_path)
  
  # Group by Gebiet and Wissenschaftlicher_Name and sum counts
  manual_counts <- Kartierung %>%
    group_by(Gebiet, Wissenschaftlicher_Name) %>%
    summarise(Total_Count = sum(total_zahl, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = Wissenschaftlicher_Name, values_from = Total_Count, values_fill = 0)
  
  # Set 'Gebiet' as row names for diversity calculation
  manual_counts_df <- manual_counts %>%
    column_to_rownames(var = "Gebiet")
  
  # Calculate Shannon Diversity Index (SHDI) for Kartierung
  log_message("Calculating Shannon Diversity Index (SHDI) for Kartierung.", log_file_path)
  manual_shdi <- vegan::diversity(manual_counts_df, index = "shannon")
  
  # Create a DataFrame for SHDI results
  manual_shdi_df <- data.frame(
    Region = rownames(manual_counts_df),
    SHDI_Manual = manual_shdi,
    row.names = NULL
  )
  
  # Save SHDI values
  manual_shdi_path <- file.path(config$kartierung_output, "SHDI_Manual_Kartierung.csv")
  fwrite(manual_shdi_df, manual_shdi_path)
  log_message(paste("SHDI values for Kartierung saved at:", manual_shdi_path), log_file_path)
  
  return(manual_shdi_df)
}

# 4c. Function to Aggregate Biodiversity Metrics at Regional Level with Weighted Approach
aggregate_biodiversity_audio <- function(biodiversity_per_device_path, output_folder, log_file_path) {
  log_message("Aggregating biodiversity metrics at regional level with weighted approach.", log_file_path)
  
  # Check if the file exists
  if (!file.exists(biodiversity_per_device_path)) {
    log_message(paste("Error: Biodiversity data file does not exist at:", biodiversity_per_device_path), log_file_path)
    stop("Biodiversity metrics per device could not be loaded because the file does not exist.")
  }
  
  biodiversity_per_device <- tryCatch({
    fread(biodiversity_per_device_path)
  }, error = function(e) {
    log_message(paste("Error loading Biodiversity_Per_Device_AudioMoth.csv:", e$message), log_file_path)
    return(NULL)
  })
  
  if (is.null(biodiversity_per_device)) {
    stop("Biodiversity metrics per device could not be loaded.")
  }
  
  if (nrow(biodiversity_per_device) == 0) {
    log_message("Biodiversity_Per_Device_AudioMoth.csv is empty.", log_file_path)
    stop("No biodiversity data available for aggregation.")
  }
  
  # Aggregate metrics per region using weighted approach
  aggregated_biodiversity <- biodiversity_per_device %>%
    group_by(Region) %>%
    summarise(
      Weighted_SHDI_AudioMoth = sum(SHDI_AudioMoth * Total_Count) / sum(Total_Count),
      Weighted_Species_Richness = sum(Species_Richness * Total_Count) / sum(Total_Count),
      Weighted_Total_Count = sum(Total_Count),
      .groups = 'drop'
    )
  
  # Save aggregated biodiversity metrics
  aggregated_biodiversity_path <- file.path(output_folder, "Aggregated_Biodiversity_Per_Region.csv")
  fwrite(aggregated_biodiversity, aggregated_biodiversity_path)
  log_message(paste("Aggregated biodiversity metrics per region saved at:", aggregated_biodiversity_path), log_file_path)
  
  return(aggregated_biodiversity)
}

# 4d. Function to Merge SHDI Values and Calculate Correlation with Scatterplots
merge_and_compare_shdi <- function(aggregated_biodiversity, manual_shdi_df, output_folder, log_file_path) {
  log_message("Comparing aggregated biodiversity metrics with manual mapping data.", log_file_path)
  
  # Standardize region names
  aggregated_biodiversity$Region <- standardize_region_names(aggregated_biodiversity$Region, prefix_remove = TRUE)
  manual_shdi_df$Region <- standardize_region_names(manual_shdi_df$Region, prefix_remove = FALSE)
  
  # Merge SHDI values
  shdi_comparison <- aggregated_biodiversity %>%
    left_join(manual_shdi_df, by = "Region") %>%
    mutate(SHDI_Difference = Weighted_SHDI_AudioMoth - SHDI_Manual)
  
  # Save SHDI comparison
  shdi_comparison_path <- file.path(output_folder, "SHDI_Comparison_AudioMoth_vs_Kartierung.csv")
  fwrite(shdi_comparison, shdi_comparison_path)
  log_message(paste("SHDI comparison saved at:", shdi_comparison_path), log_file_path)
  
  # Filter complete pairs
  complete_shdi_comparison <- shdi_comparison %>%
    filter(!is.na(SHDI_Manual) & !is.na(Weighted_SHDI_AudioMoth))
  
  log_message(paste("Number of complete pairs:", nrow(complete_shdi_comparison)), log_file_path)
  
  # Calculate correlation if there are complete pairs
  if (nrow(complete_shdi_comparison) > 0) {
    correlation <- cor(
      complete_shdi_comparison$SHDI_Manual, 
      complete_shdi_comparison$Weighted_SHDI_AudioMoth, 
      use = "complete.obs"
    )
    log_message(paste("Correlation between manual and AudioMoth SHDI values:", round(correlation, 3)), log_file_path)
    
    # Create a scatterplot of SHDI values
    scatterplot <- ggplot(complete_shdi_comparison, aes(x = SHDI_Manual, y = Weighted_SHDI_AudioMoth, color = Region)) +
      geom_point(alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Comparison of Shannon Diversity Index (SHDI)",
        x = "SHDI - Manual (Kartierung)",
        y = "SHDI - AudioMoth (Weighted)",
        color = "Region"
      ) +
      theme_minimal()
    
    # Save the scatterplot
    plot_path <- file.path(config$plots_output, "SHDI_Scatterplot_Aggregated.png")
    ggsave(plot_path, scatterplot, width = 8, height = 6)
    log_message(paste("Aggregated SHDI scatterplot saved at:", plot_path), log_file_path)
  } else {
    log_message("No complete pairs found for correlation. Please check the data.", log_file_path)
  }
  
  return(shdi_comparison)
}

# 4e. Function to Compare Species Richness
compare_species_richness <- function(aggregated_biodiversity, kartierung_path, output_folder, log_file_path) {
  log_message("Comparing Species Richness between AudioMoth and manual mapping.", log_file_path)
  
  # Load Kartierung data again
  kartierung_data <- tryCatch({
    read_excel(kartierung_path, skip = 2, 
               col_names = c("Gebiet", "Ort", "Fläche", "Datum", 
                             "Durchgang", "Art_deutsch", "Zahl", 
                             "Wissenschaftlicher_Name"))
  }, error = function(e) {
    log_message(paste("Error loading Kartierung.xlsx:", e$message), log_file_path)
    return(NULL)
  })
  
  if (is.null(kartierung_data)) {
    stop("Kartierung.xlsx could not be loaded. Please check the file path and format.")
  }
  
  # Clean Kartierung data
  Kartierung <- kartierung_data %>%
    mutate(
      Gebiet = as.character(Gebiet),
      Ort = as.character(Ort),
      Wissenschaftlicher_Name = as.character(Wissenschaftlicher_Name),
      Art_deutsch = as.character(Art_deutsch),
      total_zahl = as.integer(Zahl)
    )
  
  log_message("Kartierung data cleaned for Species Richness comparison.", log_file_path)
  
  # Standardize region names
  aggregated_biodiversity$Region <- standardize_region_names(aggregated_biodiversity$Region, prefix_remove = TRUE)
  Kartierung$Gebiet <- standardize_region_names(Kartierung$Gebiet, prefix_remove = FALSE)
  
  # Calculate manual Species Richness
  manual_species_richness <- Kartierung %>%
    group_by(Gebiet) %>%
    summarise(Manual_Species_Richness = n_distinct(Wissenschaftlicher_Name), .groups = 'drop')
  
  # Merge Species Richness values
  richness_comparison <- aggregated_biodiversity %>%
    select(Region, Weighted_Species_Richness) %>%
    left_join(manual_species_richness, by = c("Region" = "Gebiet")) %>%
    mutate(Richness_Difference = Weighted_Species_Richness - Manual_Species_Richness)
  
  # Save Richness comparison
  richness_comparison_path <- file.path(output_folder, "Richness_Comparison_AudioMoth_vs_Kartierung.csv")
  fwrite(richness_comparison, richness_comparison_path)
  log_message(paste("Richness comparison saved at:", richness_comparison_path), log_file_path)
  
  # Filter complete pairs
  complete_richness_comparison <- richness_comparison %>%
    filter(!is.na(Manual_Species_Richness) & !is.na(Weighted_Species_Richness))
  
  # Calculate correlation for Species Richness
  if (nrow(complete_richness_comparison) > 0) {
    correlation_richness <- cor(
      complete_richness_comparison$Manual_Species_Richness, 
      complete_richness_comparison$Weighted_Species_Richness, 
      use = "complete.obs"
    )
    log_message(paste("Correlation between manual and AudioMoth Species Richness:", round(correlation_richness, 3)), log_file_path)
    
    # Create a scatterplot for Species Richness
    scatterplot_richness <- ggplot(complete_richness_comparison, aes(x = Manual_Species_Richness, y = Weighted_Species_Richness, color = Region)) +
      geom_point(alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Comparison of Species Richness",
        x = "Species Richness - Manual (Kartierung)",
        y = "Species Richness - AudioMoth (Weighted)",
        color = "Region"
      ) +
      theme_minimal()
    
    # Save the Richness scatterplot
    richness_plot_path <- file.path(config$plots_output, "Richness_Scatterplot_Aggregated.png")
    ggsave(richness_plot_path, scatterplot_richness, width = 8, height = 6)
    log_message(paste("Richness scatterplot saved at:", richness_plot_path), log_file_path)
  } else {
    log_message("No complete pairs found for Species Richness correlation. Please check the data.", log_file_path)
  }
  
  return(richness_comparison)
}

# 4f. Function to Load and Process AudioMoth Data
load_audiomoth_data <- function(file_path, log_file_path) {
  if (!file.exists(file_path)) {
    log_message(paste("File not found:", file_path), log_file_path)
    return(NULL)
  }
  
  sheets <- tryCatch({
    excel_sheets(file_path)
  }, error = function(e) {
    log_message(paste("Error reading sheets from", file_path, ":", e$message), log_file_path)
    return(NULL)
  })
  
  if (is.null(sheets)) {
    return(NULL)
  }
  
  audiomoth_data <- lapply(sheets, function(sheet) {
    log_message(paste("Processing sheet:", sheet), log_file_path)
    
    # Extract date from sheet name
    date_extracted <- tryCatch({
      ymd(sheet)
    }, error = function(e) {
      log_message(paste("Could not extract date from sheet", sheet, ":", e$message), log_file_path)
      return(NA)
    })
    
    if (is.na(date_extracted)) {
      warning(paste("Could not extract date from sheet", sheet))
      log_message(paste("Could not extract date from sheet", sheet), log_file_path)
      return(NULL)
    }
    log_message(paste("Extracted date:", date_extracted), log_file_path)
    
    # Attempt to read and clean data
    df <- tryCatch({
      read_excel(file_path, sheet = sheet) %>%
        rename_with(~ tolower(gsub(" ", "_", .x))) %>% # Standardize column names
        rename(begin_time_s = `begin_time_(s)`) %>%   # Rename specific column
        mutate(
          date = as.Date(date_extracted),
          common_name = tolower(trimws(common_name)),
          confidence = as.numeric(confidence),
          begin_time_s = as.numeric(begin_time_s)
        )
    }, error = function(e) {
      warning(paste("Error reading sheet:", sheet, "-", e$message))
      log_message(paste("Error reading sheet:", sheet, "-", e$message), log_file_path)
      return(NULL)
    })
    
    # Validate essential columns
    required_columns <- c("common_name", "confidence", "begin_time_s", "date")
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      log_message(paste("Missing columns in sheet", sheet, ":", paste(missing_columns, collapse = ", ")), log_file_path)
      return(NULL)
    }
    
    return(df)
  })
  
  # Remove NULL values and combine data
  audiomoth_data <- bind_rows(audiomoth_data[!sapply(audiomoth_data, is.null)])
  
  if (nrow(audiomoth_data) == 0) {
    log_message("No data was loaded from AudioMoth devices.", log_file_path)
    return(NULL)
  }
  
  log_message(paste("Total records loaded from AudioMoth data:", nrow(audiomoth_data)), log_file_path)
  
  return(audiomoth_data)
}

# 4g. Function to Load BirdNET Labels
load_birdnet_labels <- function(file_path, log_file_path) {
  if (!file.exists(file_path)) {
    log_message(paste("BirdNET Labels file not found:", file_path), log_file_path)
    return(NULL)
  }
  
  birdnet_df <- tryCatch({
    read_excel(file_path) %>%
      rename_with(~ tolower(gsub(" ", "_", .x))) %>% # Standardize column names
      mutate(
        common_name = tolower(trimws(common_name)),
        wissenschaftlicher_name = trimws(wissenschaftlicher_name)
      )
  }, error = function(e) {
    log_message(paste("Error loading BirdNET Labels:", e$message), log_file_path)
    return(NULL)
  })
  
  # Check if required column exists
  if (!"wissenschaftlicher_name" %in% names(birdnet_df)) {
    log_message("The column 'wissenschaftlicher_name' does not exist in BirdNET Labels data.", log_file_path)
    return(NULL)
  }
  
  log_message(paste("BirdNET Labels loaded with", nrow(birdnet_df), "entries."), log_file_path)
  
  return(birdnet_df)
}

# 4h. Function to Load Species List
load_species_list <- function(file_path, log_file_path) {
  if (!file.exists(file_path)) {
    log_message(paste("Species list file not found:", file_path), log_file_path)
    return(NULL)
  }
  
  species_list <- tryCatch({
    read_excel(file_path) %>%
      rename_with(~ tolower(gsub(" ", "_", .x))) %>% # Standardize column names
      mutate(
        common_name = tolower(trimws(common_name)),
        wissenschaftlicher_name = trimws(wissenschaftlicher_name)
      )
  }, error = function(e) {
    log_message(paste("Error loading species list:", e$message), log_file_path)
    return(NULL)
  })
  
  # Check if 'common_name' column exists
  if (!"common_name" %in% names(species_list)) {
    log_message("The column 'common_name' does not exist in the species list.", log_file_path)
    return(NULL)
  }
  
  log_message(paste("Species list loaded with", nrow(species_list), "entries."), log_file_path)
  
  return(species_list)
}

# 4i. Function to Merge Data with BirdNET Labels
merge_with_birdnet <- function(df, birdnet_df, log_file_path) {
  # Check column types
  log_message(paste("Type of 'common_name' in df:", class(df$common_name)), log_file_path)
  log_message(paste("Type of 'begin_time_s' in df:", class(df$begin_time_s)), log_file_path)
  log_message(paste("Type of 'confidence' in df:", class(df$confidence)), log_file_path)
  log_message(paste("Type of 'date' in df:", class(df$date)), log_file_path)
  
  merged_df <- df %>%
    left_join(birdnet_df, by = "common_name")
  
  log_message(paste("Number of entries with assigned scientific names:", sum(!is.na(merged_df$wissenschaftlicher_name))), log_file_path)
  
  return(merged_df)
}

# 4j. Function to Create the datetime Column
create_datetime <- function(df, log_file_path) {
  df <- df %>%
    mutate(
      datetime = as.POSIXct(date) + lubridate::seconds(begin_time_s)
    )
  
  # Check for failed datetime creations
  num_na_datetime <- sum(is.na(df$datetime))
  if (num_na_datetime > 0) {
    log_message(paste("There are", num_na_datetime, "failed datetime creations."), log_file_path)
    print(head(df %>% filter(is.na(datetime))))
  } else {
    log_message("All datetime values successfully created.", log_file_path)
  }
  
  return(df)
}

# 4k. Function to Save Data and Append Metrics
save_data <- function(df, output_folder, device_number, region_name, log_file_path) {
  # Define the filename for the current device
  output_combined_path <- file.path(config$audiomoth_output, paste0("Combined_AudioMoth_", device_number, "_Region_", region_name, ".csv"))
  
  # Save the combined data
  fwrite(df, output_combined_path)
  log_message(paste("Combined data saved at:", output_combined_path), log_file_path)
  
  # Calculate species frequency
  species_counts_df <- df %>%
    group_by(wissenschaftlicher_name, common_name) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(count))
  
  # Define the filepath for species frequency CSV
  species_counts_path <- file.path(config$audiomoth_output, paste0("Species_Frequency_AudioMoth_", device_number, "_Region_", region_name, ".csv"))
  
  # Save species frequency data
  fwrite(species_counts_df, species_counts_path)
  log_message(paste("Species frequency data saved at:", species_counts_path), log_file_path)
  
  # Calculate SHDI for the device
  shdi_value <- vegan::diversity(df %>% select(wissenschaftlicher_name) %>% table(), index = "shannon")
  
  # Calculate Species Richness
  species_richness <- df$wissenschaftlicher_name %>% unique() %>% length()
  
  # Calculate Total Count
  total_count <- nrow(df)
  
  # Log calculated metrics
  log_message(paste("Device", device_number, "Metrics - SHDI:", shdi_value, 
                    "Species Richness:", species_richness, 
                    "Total Count:", total_count), log_file_path)
  
  # Append to Biodiversity_Per_Device_AudioMoth.csv
  biodiversity_path <- file.path(output_folder, "Biodiversity_Per_Device_AudioMoth.csv")
  
  device_metrics <- data.frame(
    Device_Number = device_number,
    Region = region_name,
    SHDI_AudioMoth = shdi_value,
    Species_Richness = species_richness,
    Total_Count = total_count,
    stringsAsFactors = FALSE
  )
  
  # If the file doesn't exist, create it with headers
  if (!file.exists(biodiversity_path)) {
    fwrite(device_metrics, biodiversity_path)
    log_message(paste("Biodiversity metrics file created:", biodiversity_path), log_file_path)
  } else {
    # Append without headers
    fwrite(device_metrics, biodiversity_path, append = TRUE)
    log_message(paste("Device metrics appended to:", biodiversity_path), log_file_path)
  }
  
  # Verify if metrics were appended
  appended_data <- tryCatch({
    fread(biodiversity_path)
  }, error = function(e) {
    log_message(paste("Error reading appended Biodiversity data:", e$message), log_file_path)
    return(NULL)
  })
  
  if (!is.null(appended_data)) {
    log_message(paste("Current Biodiversity Data Count:", nrow(appended_data)), log_file_path)
  } else {
    log_message("Failed to verify appended Biodiversity data.", log_file_path)
  }
  
  # Return the device metrics and species counts
  return(list(
    filtered_data = df,
    species_counts_df = species_counts_df,
    device_metrics = device_metrics
  ))
}

# ============================
# 5. Main Function to Execute All Steps
# ============================

run_analysis <- function(input_folder, kartierung_path, biodiversity_per_device_path, output_folder, log_file_path, birdnet_labels_path, species_list_path) {
  # Load BirdNET Labels with error handling
  log_message("Loading BirdNET Labels.", log_file_path)
  birdnet_df <- load_birdnet_labels(birdnet_labels_path, log_file_path)
  
  if (is.null(birdnet_df)) {
    stop("BirdNET Labels could not be loaded. Script will terminate.")
  }
  
  log_message(paste("BirdNET Labels loaded. Number of entries:", nrow(birdnet_df)), log_file_path)
  
  # Load Species List with error handling
  log_message("Loading Species List.", log_file_path)
  species_list <- load_species_list(species_list_path, log_file_path)
  
  if (is.null(species_list)) {
    stop("Species list could not be loaded. Script will terminate.")
  }
  
  log_message(paste("Species list loaded. Number of entries:", nrow(species_list)), log_file_path)
  
  # Create a set for quick lookup
  species_set <- species_list %>%
    filter(!is.na(common_name)) %>%
    distinct(common_name)
  
  log_message(paste("Species set created with", nrow(species_set), "unique common_names."), log_file_path)
  
  # Define region mapping for each AudioMoth device
  # Adjust this mapping according to your actual data
  region_mapping <- c(
    rep("RegionA", 4),  # Devices 1-4
    rep("RegionB", 4),  # Devices 5-8
    rep("RegionC", 4),  # Devices 9-12
    rep("RegionD", 4),  # Devices 13-16
    rep("RegionE", 4),  # Devices 17-20
    rep("RegionF", 4),  # Devices 21-24
    rep("RegionG", 4),  # Devices 25-28
    rep("RegionH", 4),  # Devices 29-32
    rep("RegionI", 2),  # Devices 33-34
    rep("RegionJ", 3),  # Devices 35-37
    rep("RegionK", 2)   # Devices 38-39
  )
  
  # Verify the length of the mapping
  if(length(region_mapping) != 39){
    stop("The length of 'region_mapping' does not match the number of AudioMoth devices (39). Please adjust accordingly.")
  }
  
  # Initialize Biodiversity_Per_Device_AudioMoth.csv
  biodiversity_path <- file.path(output_folder, "Biodiversity_Per_Device_AudioMoth.csv")
  if (file.exists(biodiversity_path)) {
    file.remove(biodiversity_path)  # Remove existing file to prevent duplication
    log_message(paste("Existing biodiversity data file removed:", biodiversity_path), log_file_path)
  }
  
  # Log initialization
  log_message("Initialized Biodiversity_Per_Device_AudioMoth.csv for appending.", log_file_path)
  
  # Initialize a list to store per-device data
  audiomoth_device_data <- list()
  
  # Loop through all AudioMoth devices
  for (device_number in 1:39) {
    # Determine the region for the current device
    region_name <- region_mapping[device_number]
    
    log_message(paste("Starting processing for AudioMoth device", device_number, "assigned to", region_name), log_file_path)
    
    # Define the filename pattern for the current device
    file_pattern <- paste0("^filtered_", device_number, "_BirdNET_selection_by_day\\.xlsx$")
    file_path <- list.files(input_folder, pattern = file_pattern, recursive = FALSE, full.names = TRUE)
    
    if (length(file_path) == 0) {
      log_message(paste("No file found for AudioMoth device", device_number), log_file_path)
      next  # Move to the next iteration
    }
    
    log_message(paste("Found file:", basename(file_path)), log_file_path)
    
    # Load AudioMoth data
    audiomoth_df <- load_audiomoth_data(file_path, log_file_path)
    
    if (is.null(audiomoth_df)) {
      log_message("No data to process for this device.", log_file_path)
      next  # Move to the next iteration
    }
    
    # Merge with BirdNET Labels
    merged_df <- merge_with_birdnet(audiomoth_df, birdnet_df, log_file_path)
    
    # Check for missing scientific names
    num_na_scientific <- sum(is.na(merged_df$wissenschaftlicher_name))
    log_message(paste("Number of entries without scientific names:", num_na_scientific), log_file_path)
    
    if (num_na_scientific > 0) {
      log_message("Some entries do not have assigned scientific names.", log_file_path)
      
      # Save unmapped entries
      unmapped_entries <- merged_df %>%
        filter(is.na(wissenschaftlicher_name)) %>%
        distinct(common_name)
      
      if (nrow(unmapped_entries) > 0) {
        unmapped_path <- file.path(config$audiomoth_output, paste0("unmapped_common_names_", device_number, "_", region_name, "_", Sys.Date(), ".csv"))
        fwrite(unmapped_entries, unmapped_path)
        log_message(paste("Unmapped 'common_name' entries saved at:", unmapped_path), log_file_path)
      }
    }
    
    # Filter based on the species list
    filtered_data <- merged_df %>%
      semi_join(species_set, by = "common_name")
    
    log_message(paste("Number of filtered entries for AudioMoth device", device_number, ":", nrow(filtered_data)), log_file_path)
    
    if (nrow(filtered_data) == 0) {
      log_message(paste("No filtered data found for AudioMoth device", device_number), log_file_path)
      next  # Move to the next iteration
    }
    
    # Create the datetime column
    filtered_data <- create_datetime(filtered_data, log_file_path)
    
    # Save the data and append metrics
    device_results <- save_data(filtered_data, output_folder, device_number, region_name, log_file_path)
    
    # Store the data in the list
    audiomoth_device_data[[paste0("Device_", device_number)]] <- list(
      device_number = device_number,
      region_name = region_name,
      filtered_data = device_results$filtered_data,
      species_counts_df = device_results$species_counts_df,
      device_metrics = device_results$device_metrics
    )
  }
  
  log_message("All AudioMoth devices have been processed.", log_file_path)
  
  # Proceed only if Biodiversity_Per_Device_AudioMoth.csv exists and has data
  if (!file.exists(biodiversity_per_device_path)) {
    log_message("Biodiversity_Per_Device_AudioMoth.csv is missing. Attempting to generate.", log_file_path)
    # Since the original script had a placeholder, we remove it to prevent further errors
    stop("Biodiversity_Per_Device_AudioMoth.csv is missing and no generation logic is provided.")
  }
  
  # Load biodiversity data
  biodiversity_data <- tryCatch({
    fread(biodiversity_per_device_path)
  }, error = function(e) {
    log_message(paste("Error loading Biodiversity_Per_Device_AudioMoth.csv:", e$message), log_file_path)
    return(NULL)
  })
  
  if (is.null(biodiversity_data) || nrow(biodiversity_data) == 0) {
    log_message("Biodiversity_Per_Device_AudioMoth.csv is empty. Aggregation cannot proceed.", log_file_path)
    stop("No biodiversity data available for aggregation.")
  } else {
    log_message(paste("Biodiversity_Per_Device_AudioMoth.csv contains", nrow(biodiversity_data), "entries."), log_file_path)
  }
  
  # Calculate SHDI for manual mapping
  manual_shdi_df <- calculate_manual_shdi(kartierung_path, output_folder, log_file_path)
  
  # Aggregate biodiversity metrics at regional level
  aggregated_biodiversity <- aggregate_biodiversity_audio(biodiversity_per_device_path, output_folder, log_file_path)
  
  # Merge and compare SHDI values
  shdi_comparison <- merge_and_compare_shdi(aggregated_biodiversity, manual_shdi_df, output_folder, log_file_path)
  
  # Compare Species Richness
  richness_comparison <- compare_species_richness(aggregated_biodiversity, kartierung_path, output_folder, log_file_path)
  
  log_message("===== Analysis Completed =====", log_file_path)
  
  return(list(
    shdi_comparison = shdi_comparison,
    richness_comparison = richness_comparison,
    biodiversity_data = biodiversity_data,
    manual_shdi_df = manual_shdi_df,
    aggregated_biodiversity = aggregated_biodiversity,
    audiomoth_device_data = audiomoth_device_data
  ))
}

# ============================
# 6. Execute the Analysis Process
# ============================

# Define 'input_folder' as 'config$unzip_dir' since it contains the AudioMoth files
input_folder <- config$unzip_dir

# Execute the analysis process
results <- run_analysis(
  input_folder = input_folder,
  kartierung_path = config$kartierung,
  biodiversity_per_device_path = file.path(config$output_folder, config$biodiversity_per_device),
  output_folder = config$output_folder,
  log_file_path = config$log_file,
  birdnet_labels_path = config$birdnet_labels,
  species_list_path = config$species_list
)

# Log the completion of the analysis
log_message("Analysis process completed successfully.", config$log_file)

# ============================
# 7. Create all_combined_data per Device
# ============================

# Function to extract the device number from the filename
extract_device_number <- function(filename) {
  # Assuming the pattern is "Combined_AudioMoth_X_Region_Y.csv"
  # We extract the number after "AudioMoth_" and before "_Region"
  device_number <- str_extract(basename(filename), "(?<=AudioMoth_)[0-9]+")
  return(as.integer(device_number))
}

# List of combined files
combined_data_files <- list.files(
  config$audiomoth_output, 
  pattern = "^Combined_AudioMoth_\\d+_Region_.*\\.csv$", 
  full.names = TRUE
)

# Check if any combined data files are found
if (length(combined_data_files) == 0) {
  log_message("No combined data files found in the AudioMoth output directory.", config$log_file)
  stop("No combined data files to process.")
}

# Load and combine the data with the 'device_number' column
all_combined_data <- bind_rows(lapply(combined_data_files, function(file) {
  df <- fread(file)
  
  # Extract the device number from the filename
  device_number <- extract_device_number(file)
  
  # Add the 'device_number' column
  df$device_number <- device_number
  
  return(df)
}))

# Logging
log_message(paste("All combined data loaded with", nrow(all_combined_data), "entries."), config$log_file)

# Check the column names after adding the 'device_number' column
print(names(all_combined_data))
print(head(all_combined_data))

# ============================
# 8. Calculate species_confidence per Device
# ============================

# Ensure that the 'device_number' column exists
if (!"device_number" %in% names(all_combined_data)) {
  log_message("Column 'device_number' not found. Please check the combined data.", config$log_file)
  stop("Column 'device_number' not found in all_combined_data.")
} else {
  # Calculate average confidence per species and device
  species_confidence <- all_combined_data %>%
    group_by(device_number, wissenschaftlicher_name, common_name) %>%
    summarise(
      total_count = n(),  # Number of occurrences
      CI = mean(confidence, na.rm = TRUE),  # Average Confidence Index
      .groups = 'drop'
    )
  
  log_message("Species confidence calculated per device.", config$log_file)
}

# ============================
# 9. Determine Optimal Thresholds per Device
# ============================

# Load necessary libraries (if not already loaded)
library(dplyr)
library(ggplot2)
library(boot)
library(rpart)
library(pROC)
library(data.table)
library(stringr)

# Calculate cumulative distribution for min_count per device
calculate_cumulative_distribution <- function(data) {
  # Check if the required columns exist
  required_cols <- c("device_number", "wissenschaftlicher_name", "common_name", "total_count")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(paste("The following required columns are missing in the dataset:", paste(missing_cols, collapse = ", ")))
  }
  
  # Calculate cumulative distribution per device
  all_species_freq <- data %>%
    arrange(device_number, desc(total_count)) %>%
    group_by(device_number) %>%
    mutate(cumulative_percentage = cumsum(total_count) / sum(total_count) * 100) %>%
    ungroup()
  
  return(all_species_freq)
}

# Calculate cumulative distribution
all_species_freq <- calculate_cumulative_distribution(species_confidence)

# Logging
log_message("Cumulative distribution of species frequency calculated per device.", config$log_file)

# Save the cumulative distribution
fwrite(all_species_freq, file.path(config$audiomoth_output, "All_Species_Frequency_Cumulative_per_Device.csv"))
log_message("All species frequency cumulative data per device saved.", config$log_file)

# Create a plot for the cumulative distribution per device
cumulative_plot <- ggplot(all_species_freq, aes(x = total_count, y = cumulative_percentage, color = as.factor(device_number))) +
  geom_line() +
  scale_x_log10() +
  labs(title = "Cumulative Distribution of Species Frequency per Device",
       x = "Frequency (log-transformed)",
       y = "Cumulative Percentage",
       color = "Device Number") +
  theme_minimal()

# Save the plot
ggsave(file.path(config$plots_output, "Cumulative_Distribution_Species_Frequency_per_Device.png"), cumulative_plot, width = 10, height = 6)
log_message("Cumulative distribution plot per device created and saved.", config$log_file)

# Calculate quantiles for total_count per device
quantiles_count_per_device <- species_confidence %>%
  group_by(device_number) %>%
  summarise(
    quantile_75 = quantile(total_count, probs = 0.75, na.rm = TRUE),
    quantile_90 = quantile(total_count, probs = 0.90, na.rm = TRUE)
  )

print(quantiles_count_per_device)

# Create a Confidence distribution plot per device
confidence_plot <- ggplot(species_confidence, aes(x = CI)) +
  geom_histogram(binwidth = 0.05, fill = "coral", color = "black") +
  facet_wrap(~ device_number) +
  labs(title = "Distribution of Average Confidence Values per Device",
       x = "Average Confidence Value",
       y = "Frequency") +
  theme_minimal()

# Save the Confidence distribution plot
ggsave(file.path(config$plots_output, "Confidence_Distribution_per_Device.png"), confidence_plot, width = 12, height = 8)
log_message("Confidence distribution plot per device created and saved.", config$log_file)

# Set thresholds based on quantiles per device
# For example, create a threshold table per device
thresholds_per_device <- quantiles_count_per_device %>%
  left_join(
    species_confidence %>%
      group_by(device_number) %>%
      summarise(
        CI_quantile_75 = quantile(CI, probs = 0.75, na.rm = TRUE),
        CI_quantile_90 = quantile(CI, probs = 0.90, na.rm = TRUE)
      ),
    by = "device_number"
  )

# Logging thresholds
log_message("Thresholds per device calculated.", config$log_file)
print(thresholds_per_device)

# Apply thresholds per device to filter species
filtered_species_per_device <- species_confidence %>%
  left_join(thresholds_per_device, by = "device_number") %>%
  mutate(
    keep_75 = total_count >= quantile_75 & CI >= CI_quantile_75,
    keep_90 = total_count >= quantile_90 & CI >= CI_quantile_90
  )

# Save the filtered data per device
filtered_species_75 <- filtered_species_per_device %>% filter(keep_75)
filtered_species_90 <- filtered_species_per_device %>% filter(keep_90)

fwrite(filtered_species_75, file.path(config$audiomoth_output, "Filtered_Species_75_Percent_per_Device.csv"))
fwrite(filtered_species_90, file.path(config$audiomoth_output, "Filtered_Species_90_Percent_per_Device.csv"))

log_message("Filtered species frequency data per device based on quantiles saved.", config$log_file)

# ============================
# 10. Bootstrapping to Validate Thresholds per Device
# ============================

log_message("Start: Bootstrapping to validate thresholds per device.", config$log_file)

# Function to perform bootstrapping per device
boot_quantiles_per_device <- function(data, indices) {
  d <- data[indices, ]
  return(c(
    quantile(d$total_count, probs = 0.75, na.rm = TRUE),
    quantile(d$total_count, probs = 0.90, na.rm = TRUE),
    quantile(d$CI, probs = 0.75, na.rm = TRUE),
    quantile(d$CI, probs = 0.90, na.rm = TRUE)
  ))
}

# Initialize a list to store bootstrapping results per device
boot_results_per_device <- list()

# Loop over each device
for (dev in unique(species_confidence$device_number)) {
  log_message(paste("Performing bootstrapping for device", dev), config$log_file)
  
  data_dev <- species_confidence %>% filter(device_number == dev)
  
  # Only proceed if there is sufficient data
  if (nrow(data_dev) >= 10) {
    set.seed(123)
    boot_results <- boot(data = data_dev, statistic = boot_quantiles_per_device, R = 1000)
    
    # Calculate mean quantiles
    mean_quantiles <- apply(boot_results$t, 2, mean)
    
    # Store the results
    boot_results_per_device[[as.character(dev)]] <- list(
      boot_results = boot_results,
      mean_quantiles = mean_quantiles
    )
    
    # Logging
    log_message(paste("Bootstrapping completed for device", dev), config$log_file)
    log_message(paste("Mean quantiles for device", dev, ":", paste(mean_quantiles, collapse = ", ")), config$log_file)
    
  } else {
    log_message(paste("Not enough data to perform bootstrapping for device", dev), config$log_file)
  }
}

# Create a data frame of bootstrapped thresholds per device
bootstrapped_thresholds <- data.frame(
  device_number = as.integer(),
  boot_min_count_75 = numeric(),
  boot_min_count_90 = numeric(),
  boot_min_CI_75 = numeric(),
  boot_min_CI_90 = numeric()
)

for (dev in names(boot_results_per_device)) {
  mean_quantiles <- boot_results_per_device[[dev]]$mean_quantiles
  bootstrapped_thresholds <- rbind(bootstrapped_thresholds, data.frame(
    device_number = as.integer(dev),
    boot_min_count_75 = mean_quantiles[1],
    boot_min_count_90 = mean_quantiles[2],
    boot_min_CI_75 = mean_quantiles[3],
    boot_min_CI_90 = mean_quantiles[4]
  ))
}

# Logging
log_message("Bootstrapped thresholds per device calculated.", config$log_file)
print(bootstrapped_thresholds)

# Apply bootstrapped thresholds to filter species per device
species_summary_bootstrap <- species_confidence %>%
  left_join(bootstrapped_thresholds, by = "device_number") %>%
  mutate(
    keep_boot_75 = total_count >= boot_min_count_75 & CI >= boot_min_CI_75,
    keep_boot_90 = total_count >= boot_min_count_90 & CI >= boot_min_CI_90
  )

filtered_species_bootstrap_75 <- species_summary_bootstrap %>% filter(keep_boot_75)
filtered_species_bootstrap_90 <- species_summary_bootstrap %>% filter(keep_boot_90)

# Save the filtered data
fwrite(filtered_species_bootstrap_75, file.path(config$audiomoth_output, "Filtered_Species_Bootstrap_75_Per_Device.csv"))
fwrite(filtered_species_bootstrap_90, file.path(config$audiomoth_output, "Filtered_Species_Bootstrap_90_Per_Device.csv"))

log_message("Filtered species frequency data per device based on bootstrapped thresholds saved.", config$log_file)

# ============================
# 11. Adaptive Thresholds per Device
# ============================

log_message("Start: Adaptive thresholds per device.", config$log_file)

# (We already implemented adaptive thresholds per device above)

# ============================
# 12. Multivariate Approaches per Device (Decision Tree and ROC Curve)
# ============================

log_message("Start: Multivariate approaches per device.", config$log_file)

# Initialize lists to store models and results per device
tree_models <- list()
roc_results <- list()

for (dev in unique(species_confidence$device_number)) {
  log_message(paste("Processing multivariate analysis for device", dev), config$log_file)
  
  data_dev <- species_confidence %>% filter(device_number == dev)
  
  # Only proceed if there is sufficient data
  if (nrow(data_dev) >= 10) {
    # Create binary target variable based on combined criteria (e.g., bootstrapped thresholds)
    thresholds <- bootstrapped_thresholds %>% filter(device_number == dev)
    
    if (nrow(thresholds) > 0) {
      data_dev <- data_dev %>%
        mutate(target = ifelse(total_count >= thresholds$boot_min_count_75 & CI >= thresholds$boot_min_CI_75, 1, 0))
      
      # Train decision tree
      tree_model <- rpart(target ~ total_count + CI, data = data_dev, method = "class")
      tree_models[[as.character(dev)]] <- tree_model
      
      # Predictions
      data_dev$predicted <- predict(tree_model, data_dev, type = "class")
      
      # ROC Curve
      data_dev$prob <- predict(tree_model, data_dev, type = "prob")[,2]
      roc_dev <- roc(data_dev$target, data_dev$prob)
      roc_results[[as.character(dev)]] <- roc_dev
      
      # Save ROC plot per device
      roc_plot <- ggroc(roc_dev) +
        ggtitle(paste("ROC Curve for Device", dev)) +
        theme_minimal()
      
      ggsave(file.path(config$plots_output, paste0("ROC_Curve_Device_", dev, ".png")), roc_plot, width = 8, height = 6)
      
      log_message(paste("Multivariate analysis completed for device", dev), config$log_file)
      
    } else {
      log_message(paste("No thresholds found for device", dev), config$log_file)
    }
    
  } else {
    log_message(paste("Not enough data for multivariate analysis for device", dev), config$log_file)
  }
}

# ============================
# 13. Ensuring Existence of Species with Extreme CI Values per Device
# ============================

log_message("Start: Checking for species with extreme CI values per device.", config$log_file)

# Species with CI >= 0.9 per device
high_CI_species <- species_confidence %>%
  filter(CI >= 0.9)

log_message(paste("Number of species with CI >= 0.9:", nrow(high_CI_species)), config$log_file)
fwrite(high_CI_species, file.path(config$audiomoth_output, "High_CI_Species_per_Device.csv"))

# Species with CI <= 0.1 per device
low_CI_species <- species_confidence %>%
  filter(CI <= 0.1)

log_message(paste("Number of species with CI <= 0.1:", nrow(low_CI_species)), config$log_file)
fwrite(low_CI_species, file.path(config$audiomoth_output, "Low_CI_Species_per_Device.csv"))

log_message("Species with extreme CI values per device saved.", config$log_file)

# ============================
# 14. Filtering all_combined_data based on species_confidence_filtered per Device
# ============================

log_message("Start: Filtering all_combined_data based on species_confidence_filtered per device.", config$log_file)

# Decide which filtering method to use (e.g., Bootstrapped 75%)
species_confidence_filtered <- filtered_species_bootstrap_75

# Remove duplicates in all_combined_data before the join
all_combined_data_unique <- all_combined_data %>%
  distinct(device_number, wissenschaftlicher_name, common_name, .keep_all = TRUE)

# Ensure that species_confidence_filtered contains only unique entries
species_confidence_filtered_unique <- species_confidence_filtered %>%
  distinct(device_number, wissenschaftlicher_name, common_name, .keep_all = TRUE)

# Perform the join
filtered_audiomoth <- all_combined_data_unique %>%
  inner_join(species_confidence_filtered_unique, by = c("device_number", "wissenschaftlicher_name", "common_name"))

# Check the number of duplicates
duplicates <- filtered_audiomoth %>%
  group_by(device_number, wissenschaftlicher_name, common_name) %>%
  filter(n() > 1)

log_message(paste("Number of duplicate entries after the join:", nrow(duplicates)), config$log_file)

# Calculate sums without duplicates
audiomoth_counts <- filtered_audiomoth %>%
  group_by(device_number, wissenschaftlicher_name) %>%
  summarise(Total_Count = sum(total_count, na.rm = TRUE), .groups = 'drop')

# Save audiomoth_counts
fwrite(audiomoth_counts, file.path(config$audiomoth_output, "Audiomoth_Counts_Filtered_per_Device.csv"))

log_message("Audiomoth Counts after filtering per device saved.", config$log_file)

# ============================
# Completion of the Script
# ============================

log_message("===== Analysis process successfully completed =====", config$log_file)


# ============================
# Visualize Quantile Thresholds Across Devices
# ============================

# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# Melt the data for plotting
library(tidyr)
thresholds_long <- thresholds_per_device %>%
  pivot_longer(cols = -device_number, names_to = "quantile_type", values_to = "value")

# Separate total_count and CI quantiles
total_count_quantiles <- thresholds_long %>%
  filter(quantile_type %in% c("quantile_75", "quantile_90"))

CI_quantiles <- thresholds_long %>%
  filter(quantile_type %in% c("CI_quantile_75", "CI_quantile_90"))

# Plot total_count quantiles
ggplot(total_count_quantiles, aes(x = device_number, y = value, color = quantile_type)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Count Quantiles per Device",
       x = "Device Number",
       y = "Total Count",
       color = "Quantile") +
  theme_minimal()

# Save the plot
ggsave(file.path(config$plots_output, "Total_Count_Quantiles_per_Device.png"), width = 10, height = 6)

# Plot CI quantiles
ggplot(CI_quantiles, aes(x = device_number, y = value, color = quantile_type)) +
  geom_line() +
  geom_point() +
  labs(title = "Confidence Index (CI) Quantiles per Device",
       x = "Device Number",
       y = "Confidence Index",
       color = "Quantile") +
  theme_minimal()

# Save the plot
ggsave(file.path(config$plots_output, "CI_Quantiles_per_Device.png"), width = 10, height = 6)


# Scatter plot of quantile_75 vs. quantile_90 for total_count
ggplot(thresholds_per_device, aes(x = quantile_75, y = quantile_90)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Relationship Between 75th and 90th Percentile of Total Count",
       x = "75th Percentile of Total Count",
       y = "90th Percentile of Total Count") +
  theme_minimal()

# Save the plot
ggsave(file.path(config$plots_output, "Total_Count_Quantiles_Scatter.png"), width = 8, height = 6)

# Scatter plot for CI quantiles
ggplot(thresholds_per_device, aes(x = CI_quantile_75, y = CI_quantile_90)) +
  geom_point(color = "forestgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  labs(title = "Relationship Between 75th and 90th Percentile of CI",
       x = "75th Percentile of CI",
       y = "90th Percentile of CI") +
  theme_minimal()

# Save the plot
ggsave(file.path(config$plots_output, "CI_Quantiles_Scatter.png"), width = 8, height = 6)

# Assuming you have 'species_confidence' data frame with total_count and CI per device
# Plotting boxplot for total_count per device
ggplot(species_confidence, aes(x = factor(device_number), y = total_count)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(title = "Boxplot of Total Count per Device",
       x = "Device Number",
       y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot
ggsave(file.path(config$plots_output, "Total_Count_Boxplot_per_Device.png"), width = 12, height = 6)

# Plotting boxplot for CI per device
ggplot(species_confidence, aes(x = factor(device_number), y = CI)) +
  geom_boxplot(outlier.colour = "blue", outlier.shape = 1) +
  labs(title = "Boxplot of Confidence Index per Device",
       x = "Device Number",
       y = "Confidence Index (CI)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot
ggsave(file.path(config$plots_output, "CI_Boxplot_per_Device.png"), width = 12, height = 6)

# ============================
# Correlation Analysis
# ============================
# Compute correlation matrix
quantiles_data <- thresholds_per_device %>%
  select(-device_number)

correlation_matrix <- cor(quantiles_data, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Visualize the correlation matrix
library(corrplot)
corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.col = "black", number.cex = 0.7)

# Save the correlation plot
ggsave(file.path(config$plots_output, "Quantiles_Correlation_Matrix.png"), width = 6, height = 6)

# Density plot for total_count across all devices
ggplot(species_confidence, aes(x = total_count)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot of Total Count Across All Devices",
       x = "Total Count",
       y = "Density") +
  theme_minimal()

# Save the plot
ggsave(file.path(config$plots_output, "Total_Count_Density_All_Devices.png"), width = 8, height = 6)

# Density plot for CI across all devices
ggplot(species_confidence, aes(x = CI)) +
  geom_density(fill = "pink", alpha = 0.5) +
  labs(title = "Density Plot of Confidence Index Across All Devices",
       x = "Confidence Index",
       y = "Density") +
  theme_minimal()

# Save the plot
ggsave(file.path(config$plots_output, "CI_Density_All_Devices.png"), width = 8, height = 6)

# ============================
# Calculate Total Count per Device
# ============================
# Ensure 'bootstrapped_thresholds' and 'species_confidence' are already defined

# Apply bootstrapped thresholds to filter species per device
species_confidence_filtered <- species_confidence %>%
  left_join(
    bootstrapped_thresholds %>% select(device_number, boot_min_count_75, boot_min_CI_75),
    by = "device_number"
  ) %>%
  mutate(
    keep_boot_75 = total_count >= boot_min_count_75 & CI >= boot_min_CI_75
  ) %>%
  filter(keep_boot_75) %>%
  select(device_number, wissenschaftlicher_name, common_name)

# Merge 'all_combined_data' with 'species_confidence_filtered' to filter data
filtered_all_combined_data <- all_combined_data %>%
  inner_join(species_confidence_filtered, by = c("device_number", "wissenschaftlicher_name", "common_name"))

# Recalculate total count per device based on filtered data
total_count_per_device <- filtered_all_combined_data %>%
  group_by(device_number) %>%
  summarise(
    Total_Count = n(),  # Total number of detections
    .groups = 'drop'
  )

# Recalculate diversity indices per device based on filtered data
diversity_indices <- filtered_all_combined_data %>%
  group_by(device_number, wissenschaftlicher_name) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  group_by(device_number) %>%
  summarise(
    Shannon_Index = vegan::diversity(count, index = "shannon"),
    Simpson_Index = vegan::diversity(count, index = "simpson"),
    .groups = 'drop'
  )

# Function to calculate diversity indices for bootstrapping
diversity_bootstrap <- function(data, indices) {
  d <- data[indices, ]
  counts <- d %>%
    count(wissenschaftlicher_name) %>%
    pull(n)
  return(c(
    Shannon_Index = vegan::diversity(counts, index = "shannon"),
    Simpson_Index = vegan::diversity(counts, index = "simpson")
  ))
}

# Initialize a data frame to store confidence intervals per device
confidence_intervals <- data.frame(
  device_number = integer(),
  Shannon_Lower_CI = numeric(),
  Shannon_Upper_CI = numeric(),
  Simpson_Lower_CI = numeric(),
  Simpson_Upper_CI = numeric()
)

# Perform bootstrapping per device
library(boot)

for (dev in unique(filtered_all_combined_data$device_number)) {
  data_dev <- filtered_all_combined_data %>% filter(device_number == dev)
  
  if (nrow(data_dev) >= 10) {
    set.seed(123)
    boot_results <- boot(
      data = data_dev,
      statistic = diversity_bootstrap,
      R = 1000
    )
    
    # Calculate 95% confidence intervals
    ci_shannon <- boot.ci(boot_results, type = "perc", index = 1)
    ci_simpson <- boot.ci(boot_results, type = "perc", index = 2)
    
    # Store the confidence intervals
    confidence_intervals <- rbind(confidence_intervals, data.frame(
      device_number = dev,
      Shannon_Lower_CI = ci_shannon$percent[4],
      Shannon_Upper_CI = ci_shannon$percent[5],
      Simpson_Lower_CI = ci_simpson$percent[4],
      Simpson_Upper_CI = ci_simpson$percent[5]
    ))
  } else {
    # If not enough data, set confidence intervals to NA
    confidence_intervals <- rbind(confidence_intervals, data.frame(
      device_number = dev,
      Shannon_Lower_CI = NA,
      Shannon_Upper_CI = NA,
      Simpson_Lower_CI = NA,
      Simpson_Upper_CI = NA
    ))
  }
}

# Merge total count and diversity indices
device_metrics <- total_count_per_device %>%
  left_join(diversity_indices, by = "device_number")

# Merge with confidence intervals
device_metrics <- device_metrics %>%
  left_join(confidence_intervals, by = "device_number")

# Ensure that 'device_region_mapping' is defined
device_region_mapping <- all_combined_data %>%
  select(device_number, region) %>%
  distinct()

# Merge with device_region_mapping
final_data <- device_metrics %>%
  left_join(device_region_mapping, by = "device_number")

# Define the output path for the CSV file
output_csv_path <- file.path(config$output_folder, "AudioMoth_Diversity_Indices_Filtered.csv")

# Save the final data frame to CSV
write.csv(final_data, output_csv_path, row.names = FALSE)

# Log the completion
log_message(paste("Final CSV file saved at:", output_csv_path), config$log_file)

# Print the final data
print(final_data)
