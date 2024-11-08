# BirdSpecies_Regional_Analysis_AudioMoth_BirdNET.R
# Purpose: Analysis of bird species based on data from AudioMoth devices and BirdNET labels across various regions.
# Author: Lucas Beseler
# Date: 2024-11-05


# Step 1: Load necessary libraries with pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load and install required packages
p_load(
  readxl, dplyr, writexl, lubridate, 
  ggplot2, viridis, reshape2, scales, 
  ggthemes, stringr, tibble, vegan, tidyr, 
  here, data.table, microbenchmark, plotly
)

# Logging function
log_message <- function(message) {
  cat(paste(Sys.time(), "-", message, "\n"))
}

# Function to load an Excel sheet with a date extracted from the sheet name
load_sheet_with_date <- function(sheet_name, file_path) {
  date_extracted <- ymd(sheet_name)
  if (is.na(date_extracted)) {
    stop(paste("Could not extract the date from the sheet name", sheet_name, "Ensure the sheet name contains the date in 'YYYYMMDD' format."))
  }
  df <- read_excel(file_path, sheet = sheet_name) %>%
    mutate(Date = as.Date(date_extracted))
  return(df)
}

# Function to load and process AudioMoth data
load_audiomoth_data <- function(file_path, region_name, device_name) {
  if(!file.exists(file_path)){
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  sheets <- excel_sheets(file_path)
  
  audiomoth_data <- lapply(sheets, function(sheet) {
    df <- read_excel(file_path, sheet = sheet) %>%
      mutate(
        Date = ymd(sheet),
        Region = region_name,
        Device = device_name
      )
    return(df)
  })
  
  combined_df <- bind_rows(audiomoth_data)
  return(combined_df)
}

# Step 2: Define file paths
base_path <- "/Users/lucasbeseler/Desktop"

# General file paths
file_paths <- list(
  birdnet_labels = file.path(base_path, "BirdNET_Labels.xlsx"),
  species_saxony = file.path(base_path, "Species_Saxony_Anhalt.xlsx"),
  filtered_output = file.path(base_path, "BirdNET_Saxony_Anhalt_Filtered.xlsx"),
  non_matches = file.path(base_path, "Arten_nicht_in_Species_Saxony_Anhalt.xlsx")
)

# Output directory
output_directory <- file.path(base_path, "results", "plots", "Regional_Analysis")

# Sicherstellen, dass das Ausgabe-Verzeichnis existiert
if(!dir.exists(output_directory)){
  dir.create(output_directory, recursive = TRUE)
  log_message("Ausgabe-Verzeichnis erstellt.")
} else {
  log_message("Ausgabe-Verzeichnis existiert bereits.")
}

# Step 3: Load data
# 3a. Load BirdNET Labels
log_message("Starting to load BirdNET Labels.")
birdnet_df <- tryCatch(
  read_excel(file_paths$birdnet_labels),
  error = function(e) {
    stop("Error loading BirdNET Labels: ", e$message)
  }
)
log_message(paste("BirdNET Labels loaded. Number of entries:", nrow(birdnet_df)))

# 3b. Load species list (Species_Saxony_Anhalt.xlsx)
log_message("Starting to load species list.")
species_saxony_df <- tryCatch(
  read_excel(file_paths$species_saxony),
  error = function(e) {
    stop("Error loading species list: ", e$message)
  }
)
log_message(paste("Species list loaded. Number of entries:", nrow(species_saxony_df)))

# 3c. Load AudioMoth data from all sheets
# Assuming that the general 'audiomoth' file should also be loaded
# If not, remove this section
audiomoth_file <- file.path(base_path, "resultsexcel", "1", "1_BirdNET_selection_by_day.xlsx")
log_message("Starting to load general AudioMoth data.")
sheets <- excel_sheets(audiomoth_file)
log_message(paste("Sheets in the general AudioMoth file:", paste(sheets, collapse = ", ")))

audiomoth_df_list <- lapply(sheets, load_sheet_with_date, file_path = audiomoth_file)
audiomoth_df <- bind_rows(audiomoth_df_list)
log_message(paste("General AudioMoth data loaded from all sheets. Total number of entries:", nrow(audiomoth_df)))

# Step 4: Check and adjust column names
log_message("Adjusting column names.")
audiomoth_df <- audiomoth_df %>% rename_with(~ gsub(" ", "_", .), matches("Common Name"))
birdnet_df <- birdnet_df %>% rename_with(~ gsub(" ", "_", .), matches("Common Name"))

# Step 5: Data cleaning before join
log_message("Cleaning data before join.")
audiomoth_df <- audiomoth_df %>%
  mutate(Common_Name = tolower(trimws(Common_Name)))

birdnet_df <- birdnet_df %>%
  mutate(Common_Name = tolower(trimws(Common_Name)),
         Wissenschaftlicher_Name = trimws(Wissenschaftlicher_Name))

species_saxony_df <- species_saxony_df %>%
  mutate(Wissenschaftlicher_Name = trimws(Wissenschaftlicher_Name))

# Step 6: Add scientific names (merge)
log_message("Adding scientific names (merge).")
merged_df <- audiomoth_df %>%
  left_join(birdnet_df %>% select(Common_Name, Wissenschaftlicher_Name), by = "Common_Name")

# Step 7: Check if all entries have been assigned a scientific nam
num_missing <- merged_df %>% filter(is.na(Wissenschaftlicher_Name)) %>% nrow()
if(num_missing > 0){
  log_message(paste("There are", num_missing, "entries without a scientific name."))
  missing_df <- merged_df %>% filter(is.na(Wissenschaftlicher_Name))
  write_xlsx(missing_df, file.path(base_path, "Missing_Wissenschaftliche_Namen.xlsx"))
  log_message("Missing scientific names have been saved.")
} else {
  log_message("All entries have been assigned a scientific name.")
}

# Step 8: Match and filter data based on the species list
log_message("Filtering data based on the species list.")
filtered_df <- merged_df %>%
  semi_join(species_saxony_df, by = "Wissenschaftlicher_Name")

non_matches_df <- merged_df %>%
  anti_join(species_saxony_df, by = "Wissenschaftlicher_Name")

log_message(paste("Filtered data contains", nrow(filtered_df), "entries."))

# Step 9: Save filtered data (optional)
# write_xlsx(filtered_df, file_paths$filtered_output)
# write_xlsx(non_matches_df, file_paths$non_matches)
# log_message("Filtered data has been saved.")

# Step 10: Create DateTime column based on start time at 3:00 AM
log_message("Creating DateTime column.")
filtered_df <- filtered_df %>%
  mutate(
    Date_only = as.Date(Date),
    Start_Time = as.POSIXct(paste(Date_only, "03:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"),
    DateTime = Start_Time + lubridate::seconds(`Begin Time (s)`)
  )

# Check for failed DateTime creations
num_na_datetime <- sum(is.na(filtered_df$DateTime))
if(num_na_datetime > 0){
  log_message(paste("Es gibt", num_na_datetime, "failed DateTime creations."))
  print(head(filtered_df %>% filter(is.na(DateTime))))
} else {
  log_message("All DateTime values were successfully created.")
}

# Step 11: Analyze temporal activity patterns
log_message("Analyzing temporal activity patterns.")
filtered_df <- filtered_df %>%
  mutate(Begin_Min = floor(`Begin Time (s)` / 60))

# Step 12: Calculate species frequency
log_message("Calculating species frequency.")
species_counts <- filtered_df %>%
  group_by(Wissenschaftlicher_Name, Common_Name) %>%
  summarise(Anzahl = n(), .groups = 'drop') %>%
  arrange(desc(Anzahl))


#_____________________________________________________________________________________________________

# --- Further code for Step 2: Analyzing a Region Covered by Multiple AudioMoth Devices ---

# Step 2: Analyzing a Region Covered by Multiple AudioMoth Devices

# Step 2.1: Define file paths for regions and devices
log_message("Defining file paths for regions and devices.")
region_paths <- list(
  "RegionA" = list(
    Audiomoth1 = file.path(base_path, "resultsexcel", "1", "1_BirdNET_selection_by_day.xlsx"),
    Audiomoth2 = file.path(base_path, "resultsexcel", "2", "2_BirdNET_selection_by_day.xlsx"),
    Audiomoth3 = file.path(base_path, "resultsexcel", "3", "3_BirdNET_selection_by_day.xlsx"),
    Audiomoth4 = file.path(base_path, "resultsexcel", "4", "4_BirdNET_selection_by_day.xlsx")
  ),
  "RegionB" = list(
    Audiomoth5 = file.path(base_path, "resultsexcel", "5", "5_BirdNET_selection_by_day.xlsx"),
    Audiomoth6 = file.path(base_path, "resultsexcel", "6", "6_BirdNET_selection_by_day.xlsx"),
    Audiomoth7 = file.path(base_path, "resultsexcel", "7", "7_BirdNET_selection_by_day.xlsx"),
    Audiomoth8 = file.path(base_path, "resultsexcel", "8", "8_BirdNET_selection_by_day.xlsx")
  )
)

# Step 2.2: Load and combine data for all regions
log_message("Loading and combining data for all regions.")
all_regions_data <- list()

for(region in names(region_paths)){
  devices <- region_paths[[region]]
  for(device in names(devices)){
    file_path <- devices[[device]]
    log_message(paste("Loading data for", region, "-", device))
    device_data <- load_audiomoth_data(file_path, region, device)
    if(!is.null(device_data)){
      all_regions_data[[paste(region, device, sep = "_")]] <- device_data
      log_message(paste("data for", region, "-", device, "successfully loaded."))
    }
  }
}

# Combine all data into a DataFrame
combined_region_data <- bind_rows(all_regions_data)
log_message(paste("Total number of loaded entries:", nrow(combined_region_data)))

# Step 2.4: Data cleaning and preparation
log_message("Cleaning and preparing regional data.")
combined_region_data <- combined_region_data %>%
  rename_with(~ gsub(" ", "_", .), matches("Common Name")) %>%
  mutate(Common_Name = tolower(trimws(Common_Name)))

# Load species list for Saxony-Anhalt (reloaded for regional context)
log_message("Reloading species list for Saxony-Anhalt for regional data.")
species_saxony_df <- tryCatch(
  read_excel(file_paths$species_saxony),
  error = function(e) {
    stop("Error loading species list: ", e$message)
  }
)
log_message(paste("Species list loaded. Number of entries:", nrow(species_saxony_df)))

# Load BirdNET Labels (reloaded for regional context)
log_message("Loading BirdNET Labels for regional data.")
birdnet_df <- tryCatch(
  read_excel(file_paths$birdnet_labels),
  error = function(e) {
    stop("Error loading BirdNET Labels: ", e$message)
  }
)
log_message(paste("BirdNET Labels loaded. Number of entries:", nrow(birdnet_df)))

# Clean BirdNET data
log_message("Cleaning BirdNET data for regional data.")
birdnet_df <- birdnet_df %>%
  rename_with(~ gsub(" ", "_", .), matches("Common Name")) %>%
  mutate(
    Common_Name = tolower(trimws(Common_Name)),
    Wissenschaftlicher_Name = trimws(Wissenschaftlicher_Name)
  )

# Check for missing scientific names
log_message("Adding scientific names to regional data.")
merged_region_df <- combined_region_data %>%
  left_join(birdnet_df %>% select(Common_Name, Wissenschaftlicher_Name), by = "Common_Name")

# Überprüfen auf fehlende wissenschaftliche Namen
num_missing_region <- merged_region_df %>% filter(is.na(Wissenschaftlicher_Name)) %>% nrow()
if(num_missing_region > 0){
  log_message(paste("Es gibt", num_missing_region, "entries without scientific names in regional data."))
  missing_region_df <- merged_region_df %>% filter(is.na(Wissenschaftlicher_Name))
  write_xlsx(missing_region_df, file.path(base_path, "Missing_Scientific_Names_Regional.xlsx"))
  log_message("Missing scientific names in regional data have been saved.")
} else {
  log_message("All entries in regional data have been assigned a scientific name.")
}

# Filter data based on species list
log_message("Filtering regional data based on the species list.")
filtered_region_df <- merged_region_df %>%
  semi_join(species_saxony_df, by = "Wissenschaftlicher_Name")

non_matches_region_df <- merged_region_df %>%
  anti_join(species_saxony_df, by = "Wissenschaftlicher_Name")

log_message(paste("Filtered regional data contains", nrow(filtered_region_df), "entries."))

# Step 2.5: Create DateTime column and analyze temporal patterns
log_message("Creating DateTime column for regional data.")
filtered_region_df <- filtered_region_df %>%
  mutate(
    Date_only = as.Date(Date),
    Start_Time = as.POSIXct(paste(Date_only, "03:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"),
    DateTime = Start_Time + lubridate::seconds(`Begin Time (s)`)
  )

# Check for failed DateTime creations
num_na_datetime_region <- sum(is.na(filtered_region_df$DateTime))
if(num_na_datetime_region > 0){
  log_message(paste("Es gibt", num_na_datetime_region, "failed DateTime creations in regional data."))
  print(head(filtered_region_df %>% filter(is.na(DateTime))))
} else {
  log_message("All DateTime values in regional data were successfully created.")
}

# Analyze temporal activity patterns
log_message("Analyzing temporal activity patterns in regional data.")
filtered_region_df <- filtered_region_df %>%
  mutate(Begin_Min = floor(`Begin Time (s)` / 60))

# Step 2.6: Calculate species frequency
log_message("Calculating species frequency for regional data.")
species_counts_region <- filtered_region_df %>%
  group_by(Region, Device, Common_Name) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

# Determine Top 10 Species
log_message("Determining the Top 10 bird species based on detection counts.")
top_species_region <- species_counts_region %>%
  group_by(Common_Name) %>%
  summarise(Total = sum(Count)) %>%
  arrange(desc(Total)) %>%
  slice_max(order_by = Total, n = 10) %>%
  pull(Common_Name)

log_message("Top 10 bird species:")
print(top_species_region)

# Filter for Top 10 species
filtered_top_region_df <- filtered_region_df %>%
  filter(Common_Name %in% top_species_region)

# Step 2.7: Visualize Species Activity Across Devices and Regions

# Plot species activity by Device within each Region
log_message("Creating species frequency plot by device and region.")
activity_plot <- ggplot(
  species_counts_region %>% 
    filter(Common_Name %in% top_species_region), 
  aes(x = Device, y = Count, fill = Common_Name)
) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Region) +
  labs(
    title = "Species Frequency by Device and Region",
    x = "Device",
    y = "Number of Detections",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the plot with higher resolution
ggsave(
  filename = file.path(output_directory, "Species_Activity_By_Device_Region.png"),
  plot = activity_plot,
  width = 14,
  height = 8,
  dpi = 300
)
log_message("Species frequency plot by device and region saved.")

# Compare species frequency between regions
log_message("Creating comparison plot of species frequency between regions.")
activity_by_region <- filtered_region_df %>%
  group_by(Region, Common_Name) %>%
  summarise(Count = n(), .groups = 'drop')

# Plot comparison between regions
comparison_plot <- ggplot(
  activity_by_region %>% 
    filter(Common_Name %in% top_species_region), 
  aes(x = Common_Name, y = Count, fill = Region)
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparison of Species Frequency Between Regions",
    x = "Species",
    y = "Number of Detections",
    fill = "Region"
  ) +
  theme_minimal()

# Save the comparison plot with higher resolution
ggsave(
  filename = file.path(output_directory, "Species_Activity_Comparison_Between_Regions.png"),
  plot = comparison_plot,
  width = 12,
  height = 8,
  dpi = 300
)
log_message("Comparison plot of species frequency between regions saved.")

###### ANOVA
log_message("Performing ANOVA.")
# Prepare data for ANOVA
anova_data <- species_counts_region %>%
  filter(Common_Name %in% top_species_region) %>%
  mutate(
    Region = as.factor(Region),
    Species = as.factor(Common_Name)
  )

# Conduct ANOVA
anova_results <- aov(Count ~ Region * Species, data = anova_data)
anova_summary <- summary(anova_results)

# Display ANOVA results
print(anova_summary)

# Save ANOVA results
capture.output(anova_summary, 
               file = file.path(output_directory, "ANOVA_Results_Regional.txt"))
log_message("ANOVA results saved.")

###### NMDS
log_message("Performing NMDS.")
# Create a species frequency matrix with 'unite'
species_matrix_region <- species_counts_region %>%
  filter(Common_Name %in% top_species_region) %>%
  select(Region, Device, Common_Name, Count) %>%
  pivot_wider(names_from = Common_Name, values_from = Count, values_fill = list(Count = 0)) %>%
  unite("Region_Device", Region, Device, sep = "_") %>%
  column_to_rownames(var = "Region_Device")

# Perform NMDS
set.seed(123)  # For reproducibility
nmds_region <- metaMDS(species_matrix_region, distance = "bray", k = 2, trymax = 100)

# Check the stress value
log_message(paste("NMDS Stress:", nmds_region$stress))

# Prepare data for visualization
nmds_points_region <- as.data.frame(nmds_region$points) %>%
  rownames_to_column(var = "Region_Device") %>%
  separate(Region_Device, into = c("Region", "Device"), sep = "_")

# Plot NMDS
nmds_plot_region <- ggplot(nmds_points_region, aes(x = MDS1, y = MDS2, color = Region)) +
  geom_point(size = 3) +
  geom_text(aes(label = Device), vjust = -1, hjust = 0.5, size = 3) +
  labs(
    title = "NMDS of Bird Species Composition",
    x = "NMDS1",
    y = "NMDS2",
    color = "Region"
  ) +
  theme_minimal()

# Save the NMDS plot with higher resolution
ggsave(
  filename = file.path(output_directory, "NMDS_Species_Composition_Regional.png"),
  plot = nmds_plot_region,
  width = 10,
  height = 8,
  dpi = 300
)
log_message("NMDS plot saved.")

# Calculate Shannon diversity index
diversity_indices <- species_matrix_region %>%
  as.matrix() %>%
  diversity(index = "shannon")

diversity_df <- data.frame(
  Region_Device = names(diversity_indices),
  Shannon_Diversity = diversity_indices
)

# Plot Shannon diversity index by Region and Device
ggplot(diversity_df, aes(x = Region_Device, y = Shannon_Diversity, fill = Region_Device)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Shannon Diversity Index by Region and Device",
    x = "Region and Device",
    y = "Shannon Diversity"
  ) +
  theme_minimal()
