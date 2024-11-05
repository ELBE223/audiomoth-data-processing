# Step 1: Load the necessary libraries
# Automatically install missing packages
required_packages <- c("readxl", "dplyr", "writexl", "lubridate", 
                       "ggplot2", "viridis", "reshape2", "scales", "ggthemes", "stringr")

installed_packages <- rownames(installed.packages())
for(pkg in required_packages){
  if(!pkg %in% installed_packages){
    install.packages(pkg, dependencies = TRUE)
  }
}
# Load the packages
library(readxl)     
library(dplyr)      
library(writexl)    
library(lubridate)  
library(ggplot2)    
library(viridis)    
library(reshape2)   
library(scales)
library(ggthemes)
library(stringr)

# Step 2: Define file paths
# Use file.path for cross-platform compatibility
base_path <- "/Users/lucasbeseler/Desktop"
audiomoth_file <- file.path(base_path, "resultsexcel", "1", "1_BirdNET_selection_by_day.xlsx")
birdnet_labels_file <- file.path(base_path, "BirdNET_Labels.xlsx")
species_saxony_file <- file.path(base_path, "Species_Saxony_Anhalt.xlsx")
filtered_output_file <- file.path(base_path, "BirdNET_Saxony_Anhalt_Filtered.xlsx")
non_matches_file <- file.path(base_path, "Species_Not_In_Species_Saxony_Anhalt.xlsx")
output_directory <- file.path(base_path, "results", "plots")

# Ensure the output directory exists
if(!dir.exists(output_directory)){
  dir.create(output_directory, recursive = TRUE)
}

# Step 3: Load data
# 3a. Load BirdNET labels
birdnet_df <- tryCatch(
  read_excel(birdnet_labels_file),
  error = function(e) {
    stop("Error loading BirdNET labels: ", e$message)
  }
)
cat("BirdNET labels loaded. Number of entries:", nrow(birdnet_df), "\n")

# 3b. Load species list (Species_Saxony_Anhalt.xlsx)
species_saxony_df <- tryCatch(
  read_excel(species_saxony_file),
  error = function(e) {
    stop("Error loading species list: ", e$message)
  }
)
cat("Species list (Species_Saxony_Anhalt.xlsx) loaded. Number of entries:", nrow(species_saxony_df), "\n")

# 3c. Load AudioMoth data from all sheets
sheets <- excel_sheets(audiomoth_file)
cat("Sheets in the AudioMoth file:", paste(sheets, collapse = ", "), "\n")

# Function to load and assign date from the sheet name
load_sheet_with_date <- function(sheet_name, file_path) {
  date_extracted <- ymd(sheet_name)
  if (is.na(date_extracted)) {
    stop(paste("Could not extract the date from the sheet name", sheet_name, ". Ensure the sheet name contains the date in 'YYYYMMDD' format."))
  }
  df <- read_excel(file_path, sheet = sheet_name)
  df <- df %>% mutate(Date = as.Date(date_extracted))
  return(df)
}

# Load all sheets and combine them
audiomoth_df_list <- lapply(sheets, load_sheet_with_date, file_path = audiomoth_file)
audiomoth_df <- bind_rows(audiomoth_df_list)
cat("AudioMoth data from all sheets loaded. Total number of entries:", nrow(audiomoth_df), "\n")

# Step 4: Check and adjust column names
# Use `rename_with` for consistent renaming
audiomoth_df <- audiomoth_df %>% rename_with(~ gsub(" ", "_", .), matches("Common Name"))
birdnet_df <- birdnet_df %>% rename_with(~ gsub(" ", "_", .), matches("Common Name"))

# Step 5: Data cleaning before joining
audiomoth_df <- audiomoth_df %>%
  mutate(Common_Name = tolower(trimws(Common_Name)))

birdnet_df <- birdnet_df %>%
  mutate(Common_Name = tolower(trimws(Common_Name)),
         Scientific_Name = trimws(Wissenschaftlicher_Name))

species_saxony_df <- species_saxony_df %>%
  mutate(Scientific_Name = trimws(Wissenschaftlicher_Name))

# Step 6: Add scientific names (merge)
merged_df <- audiomoth_df %>%
  left_join(birdnet_df %>% select(Common_Name, Scientific_Name), by = "Common_Name")

# Step 7: Check if all entries have been assigned a scientific name
num_missing <- merged_df %>% filter(is.na(Scientific_Name)) %>% nrow()
if(num_missing > 0){
  cat("There are", num_missing, "entries without a scientific name.\n")
  # Optional: Save missing entries
  missing_df <- merged_df %>% filter(is.na(Scientific_Name))
  write_xlsx(missing_df, file.path(base_path, "Missing_Scientific_Names.xlsx"))
} else {
  cat("All entries have been assigned a scientific name.\n")
}

# Step 8: Match and filter data based on the species list
filtered_df <- merged_df %>%
  semi_join(species_saxony_df, by = "Scientific_Name")

non_matches_df <- merged_df %>%
  anti_join(species_saxony_df, by = "Scientific_Name")

# Step 9: Save filtered data
#write_xlsx(filtered_df, filtered_output_file)
#write_xlsx(non_matches_df, non_matches_file)
#cat("Filtered data has been saved.\n")

# Step 10: Create DateTime column based on start time at 3:00 AM
filtered_df <- filtered_df %>%
  mutate(
    Date_only = as.Date(Date),
    Start_Time = as.POSIXct(paste(Date_only, "03:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"),
    DateTime = Start_Time + seconds(`Begin Time (s)`)
  )

# Check for failed DateTime creations
num_na_datetime <- sum(is.na(filtered_df$DateTime))
if(num_na_datetime > 0){
  cat("There are", num_na_datetime, "failed DateTime creations.\n")
  print(head(filtered_df %>% filter(is.na(DateTime))))
} else {
  cat("All DateTime values were successfully created.\n")
}

# Step 11: Analyze temporal activity patterns
filtered_df <- filtered_df %>%
  mutate(Begin_Min = floor(`Begin Time (s)` / 60))

# Step 12: Calculate species frequency
species_counts <- filtered_df %>%
  group_by(Scientific_Name, Common_Name) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

# Step 13: Determine the Top 10 species based on total count
top_10_species <- species_counts %>%
  slice_max(order_by = Count, n = 10) %>%
  pull(Common_Name)

cat("Top 10 bird species based on the number of observations:\n")
print(top_10_species)

# Filter data for the Top 10 species
filtered_top_10_df <- filtered_df %>%
  filter(Common_Name %in% top_10_species)

# Step 14: Group data by date and species
activity_by_day_species <- filtered_top_10_df %>%
  group_by(Date, Common_Name) %>%
  summarise(Count = n(), .groups = 'drop')

# Step 15: Plot activity of Top 10 species by date
activity_day_plot <- ggplot(activity_by_day_species, aes(x = Date, y = Count, color = Common_Name)) +
  geom_line() +
  labs(
    title = "Activity of Top 10 Bird Species by Date",
    x = "Date",
    y = "Number of Detections",
    color = "Bird Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot
ggsave(
  filename = file.path(output_directory, "Top10_Activity_By_Date.png"),
  plot = activity_day_plot,
  width = 14,
  height = 8
)
cat("Activity of Top 10 species by date saved.\n")


# Step 16: Visualize confidence distributions
# Filter data for the Top 10 species
top_species_data <- filtered_df %>%
  filter(Common_Name %in% top_10_species)

# Check if the 'Confidence' column exists
if(!"Confidence" %in% colnames(top_species_data)){
  stop("The 'Confidence' column does not exist in the data.")
}

# Create density plots
confidence_density_plot <- ggplot(top_species_data, aes(x = Confidence, fill = Common_Name)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Common_Name, scales = "free_y") +
  scale_fill_viridis_d() +
  labs(
    title = "Confidence Density of the Top 10 Bird Species in Saxony-Anhalt",
    x = "Confidence Value",
    y = "Density",
    fill = "Bird Species"
  ) +
  theme_minimal()

# Display plot
print(confidence_density_plot)

# Save plot
ggsave(
  filename = file.path(output_directory, "Confidence_Density_Top_10_Species.png"),
  plot = confidence_density_plot,
  width = 12,
  height = 10
)
cat("Confidence density plots saved.\n")

# Step 17: Further data aggregation
# Assumption: 'Device' is an actual column. If not, this must be correctly loaded.
if(!"Device" %in% colnames(filtered_df)){
  warning("The 'Device' column does not exist. A temporary column 'Device = 1' will be added.")
  filtered_df <- filtered_df %>% mutate(Device = 1)
}

# Calculate the number of calls per device and species
call_counts <- filtered_df %>%
  group_by(Device, Common_Name) %>%
  summarise(Count = n(), .groups = 'drop')

# Summarize totals and unique species per device
device_summary <- call_counts %>%
  group_by(Device) %>%
  summarise(
    Total_Calls = sum(Count),
    Unique_Species = n_distinct(Common_Name),
    .groups = 'drop'
  )

# Optional: Display or save results
print(device_summary)
#write_xlsx(device_summary, file.path(base_path, "Device_Summary.xlsx"))
#cat("Device summary saved.\n")

# --- Additional plots for Top 10 bird species ---

# Step 18: Bar plot of total number of calls per species
bar_plot <- ggplot(species_counts %>% filter(Common_Name %in% top_10_species), 
                   aes(x = reorder(Common_Name, -Count), y = Count, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(
    title = "Total Number of Calls per Bird Species (Top 10)",
    x = "Bird Species",
    y = "Number of Detections",
    fill = "Bird Species"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save plot
ggsave(
  filename = file.path(output_directory, "Total_Calls_Per_Species_Barplot.png"),
  plot = bar_plot,
  width = 12,
  height = 8
)
cat("Bar plot of total number of calls per species saved.\n")

# Step 19: Boxplot of confidence values per species
boxplot_confidence <- ggplot(top_species_data, aes(x = Common_Name, y = Confidence, fill = Common_Name)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(
    title = "Boxplot of Confidence Values of the Top 10 Bird Species",
    x = "Bird Species",
    y = "Confidence Value",
    fill = "Bird Species"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save plot
ggsave(
  filename = file.path(output_directory, "Confidence_Boxplot_Top10_Species.png"),
  plot = boxplot_confidence,
  width = 12,
  height = 8
)
cat("Boxplot of confidence values per species saved.\n")

# Step 20: Heatmap of calls over time and species
# Aggregate data for the heatmap
heatmap_data <- filtered_df %>%
  mutate(Hour = hour(DateTime)) %>%
  group_by(Hour, Common_Name) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(Common_Name %in% top_10_species)

heatmap_plot <- ggplot(heatmap_data, aes(x = Hour, y = Common_Name, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Number of Detections") +
  labs(
    title = "Heatmap of Calls over Hours and Top 10 Bird Species",
    x = "Hour of the Day",
    y = "Bird Species"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Save plot
ggsave(
  filename = file.path(output_directory, "Heatmap_Calls_Time_Species.png"),
  plot = heatmap_plot,
  width = 14,
  height = 10
)
cat("Heatmap of calls over time and species saved.\n")

# Step 21: Scatter plot of confidence values vs. time
scatter_plot <- ggplot(top_species_data, aes(x = DateTime, y = Confidence, color = Common_Name)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Confidence Values of the Top 10 Bird Species over Time",
    x = "Date and Time",
    y = "Confidence Value",
    color = "Bird Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot
ggsave(
  filename = file.path(output_directory, "Confidence_vs_Time_Scatter.png"),
  plot = scatter_plot,
  width = 14,
  height = 8
)
cat("Scatter plot of confidence values vs. time saved.\n")

# Step 22: Violin plot of confidence values per species
violin_plot <- ggplot(top_species_data, aes(x = Common_Name, y = Confidence, fill = Common_Name)) +
  geom_violin(alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(
    title = "Violin Plot of Confidence Values of the Top 10 Bird Species",
    x = "Bird Species",
    y = "Confidence Value",
    fill = "Bird Species"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save plot
ggsave(
  filename = file.path(output_directory, "Confidence_ViolinPlot_Top10_Species.png"),
  plot = violin_plot,
  width = 12,
  height = 8
)
cat("Violin plot of confidence values per species saved.\n")

# Step 24: Activity patterns per minute and species of the Top 10 species
activity_pattern <- filtered_df %>%
  group_by(Begin_Min, Common_Name) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(Common_Name %in% top_10_species)

# Visualize activity patterns per minute and species
activity_plot <- ggplot(activity_pattern, aes(x = Begin_Min, y = Count, color = Common_Name)) +
  geom_line() +
  labs(
    title = "Temporal Activity Patterns of the Top 10 Bird Species",
    x = "Time (Minutes since Start of Recording)",
    y = "Number of Detections",
    color = "Bird Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot
ggsave(
  filename = file.path(output_directory, "Activity_Pattern_Per_Minute.png"),
  plot = activity_plot,
  width = 12,
  height = 8
)
cat("Activity pattern per minute saved.\n")

# --- End of additional adjustments ---

