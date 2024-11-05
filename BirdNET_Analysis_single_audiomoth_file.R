# Schritt 1: Laden der notwendigen Bibliotheken
# Installieren Sie fehlende Pakete automatisch
required_packages <- c("readxl", "dplyr", "writexl", "lubridate", 
                       "ggplot2", "viridis", "reshape2", "scales", "ggthemes", "stringr")

installed_packages <- rownames(installed.packages())
for(pkg in required_packages){
  if(!pkg %in% installed_packages){
    install.packages(pkg, dependencies = TRUE)
  }
}
# Laden der Pakete
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

# Schritt 2: Definieren der Dateipfade
# Verwenden Sie hier den Pfad mit file.path für plattformübergreifende Kompatibilität
base_path <- "/Users/lucasbeseler/Desktop"
audiomoth_file <- file.path(base_path, "resultsexcel", "1", "1_BirdNET_selection_by_day.xlsx")
birdnet_labels_file <- file.path(base_path, "BirdNET_Labels.xlsx")
species_saxony_file <- file.path(base_path, "Species_Saxony_Anhalt.xlsx")
filtered_output_file <- file.path(base_path, "BirdNET_Saxony_Anhalt_Filtered.xlsx")
non_matches_file <- file.path(base_path, "Arten_nicht_in_Species_Saxony_Anhalt.xlsx")
output_directory <- file.path(base_path, "results", "plots")

# Sicherstellen, dass das Ausgabe-Verzeichnis existiert
if(!dir.exists(output_directory)){
  dir.create(output_directory, recursive = TRUE)
}

# Schritt 3: Laden der Daten
# 3a. Laden der BirdNET Labels
birdnet_df <- tryCatch(
  read_excel(birdnet_labels_file),
  error = function(e) {
    stop("Fehler beim Laden der BirdNET Labels: ", e$message)
  }
)
cat("BirdNET Labels geladen. Anzahl der Einträge:", nrow(birdnet_df), "\n")

# 3b. Laden der Artenliste (Species_Saxony_Anhalt.xlsx)
species_saxony_df <- tryCatch(
  read_excel(species_saxony_file),
  error = function(e) {
    stop("Fehler beim Laden der Artenliste: ", e$message)
  }
)
cat("Artenliste (Species_Saxony_Anhalt.xlsx) geladen. Anzahl der Einträge:", nrow(species_saxony_df), "\n")

# 3c. Laden der Audiomoth-Daten aus allen Blättern
sheets <- excel_sheets(audiomoth_file)
cat("Blätter in der Audiomoth-Datei:", paste(sheets, collapse = ", "), "\n")

# Funktion zum Laden und Zuordnen des Datums aus dem Blattnamen
load_sheet_with_date <- function(sheet_name, file_path) {
  date_extracted <- ymd(sheet_name)
  if (is.na(date_extracted)) {
    stop(paste("Das Datum konnte aus dem Blattnamen", sheet_name, "nicht extrahiert werden. Stellen Sie sicher, dass der Blattname das Datum im Format 'YYYYMMDD' enthält."))
  }
  df <- read_excel(file_path, sheet = sheet_name)
  df <- df %>% mutate(Date = as.Date(date_extracted))
  return(df)
}

# Laden aller Blätter und Zusammenführen
audiomoth_df_list <- lapply(sheets, load_sheet_with_date, file_path = audiomoth_file)
audiomoth_df <- bind_rows(audiomoth_df_list)
cat("Audiomoth Daten aus allen Blättern geladen. Gesamte Anzahl der Einträge:", nrow(audiomoth_df), "\n")

# Schritt 4: Überprüfen und Anpassen der Spaltennamen
# Verwenden Sie `rename_with` für konsistente Umbenennungen
audiomoth_df <- audiomoth_df %>% rename_with(~ gsub(" ", "_", .), matches("Common Name"))
birdnet_df <- birdnet_df %>% rename_with(~ gsub(" ", "_", .), matches("Common Name"))

# Schritt 5: Datenbereinigung vor dem Join
audiomoth_df <- audiomoth_df %>%
  mutate(Common_Name = tolower(trimws(Common_Name)))

birdnet_df <- birdnet_df %>%
  mutate(Common_Name = tolower(trimws(Common_Name)),
         Wissenschaftlicher_Name = trimws(Wissenschaftlicher_Name))

species_saxony_df <- species_saxony_df %>%
  mutate(Wissenschaftlicher_Name = trimws(Wissenschaftlicher_Name))

# Schritt 6: Wissenschaftliche Namen hinzufügen (Mergen)
merged_df <- audiomoth_df %>%
  left_join(birdnet_df %>% select(Common_Name, Wissenschaftlicher_Name), by = "Common_Name")

# Schritt 7: Überprüfen, ob alle Einträge einen wissenschaftlichen Namen erhalten haben
num_missing <- merged_df %>% filter(is.na(Wissenschaftlicher_Name)) %>% nrow()
if(num_missing > 0){
  cat("Es gibt", num_missing, "Einträge ohne wissenschaftlichen Namen.\n")
  # Optional: Speichern der fehlenden Einträge
  missing_df <- merged_df %>% filter(is.na(Wissenschaftlicher_Name))
  write_xlsx(missing_df, file.path(base_path, "Missing_Wissenschaftliche_Namen.xlsx"))
} else {
  cat("Alle Einträge haben einen wissenschaftlichen Namen zugewiesen bekommen.\n")
}

# Schritt 8: Abgleichen und Filtern der Daten auf die Artenliste
filtered_df <- merged_df %>%
  semi_join(species_saxony_df, by = "Wissenschaftlicher_Name")

non_matches_df <- merged_df %>%
  anti_join(species_saxony_df, by = "Wissenschaftlicher_Name")

# Schritt 9: Gefilterte Daten speichern
#write_xlsx(filtered_df, filtered_output_file)
#write_xlsx(non_matches_df, non_matches_file)
#cat("Gefilterte Daten wurden gespeichert.\n")

# Schritt 10: Erstellen der DateTime-Spalte basierend auf Startzeit um 3:00 Uhr
filtered_df <- filtered_df %>%
  mutate(
    Date_only = as.Date(Date),
    Start_Time = as.POSIXct(paste(Date_only, "03:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"),
    DateTime = Start_Time + seconds(`Begin Time (s)`)
  )

# Überprüfen auf fehlgeschlagene DateTime-Erstellungen
num_na_datetime <- sum(is.na(filtered_df$DateTime))
if(num_na_datetime > 0){
  cat("Es gibt", num_na_datetime, "fehlgeschlagene DateTime-Erstellungen.\n")
  print(head(filtered_df %>% filter(is.na(DateTime))))
} else {
  cat("Alle DateTime-Werte wurden erfolgreich erstellt.\n")
}

# Schritt 11: Zeitliche Aktivitätsmuster analysieren
filtered_df <- filtered_df %>%
  mutate(Begin_Min = floor(`Begin Time (s)` / 60))

# Schritt 12: Arthäufigkeit berechnen
species_counts <- filtered_df %>%
  group_by(Wissenschaftlicher_Name, Common_Name) %>%
  summarise(Anzahl = n(), .groups = 'drop') %>%
  arrange(desc(Anzahl))

# Schritt 13: Bestimmen der Top 10 Arten basierend auf Gesamtanzahl
top_10_species <- species_counts %>%
  slice_max(order_by = Anzahl, n = 10) %>%
  pull(Common_Name)

cat("Top 10 Vogelarten basierend auf der Anzahl der Beobachtungen:\n")
print(top_10_species)

# Filtern der Daten für die Top 10 Arten
filtered_top_10_df <- filtered_df %>%
  filter(Common_Name %in% top_10_species)

# Schritt 14: Gruppieren der Daten nach Datum und Art
activity_by_day_species <- filtered_top_10_df %>%
  group_by(Date, Common_Name) %>%
  summarise(Anzahl = n(), .groups = 'drop')

# Schritt 15: Plotten der Aktivität der Top 10 Arten nach Datum
activity_day_plot <- ggplot(activity_by_day_species, aes(x = Date, y = Anzahl, color = Common_Name)) +
  geom_line() +
  labs(
    title = "Aktivität der Top 10 Vogelarten nach Datum",
    x = "Datum",
    y = "Anzahl der Erkennungen",
    color = "Vogelart"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot speichern
ggsave(
  filename = file.path(output_directory, "Top10_Activity_By_Date.png"),
  plot = activity_day_plot,
  width = 14,
  height = 8
)
cat("Aktivität der Top 10 Arten nach Datum gespeichert.\n")

# Schritt 16: Visualisierung der Konfidenzverteilungen
# Daten für die Top 10 Arten filtern
top_species_data <- filtered_df %>%
  filter(Common_Name %in% top_10_species)

# Überprüfen, ob die Spalte 'Confidence' existiert
if(!"Confidence" %in% colnames(top_species_data)){
  stop("Die Spalte 'Confidence' existiert nicht in den Daten.")
}

# Dichteplots erstellen
confidence_density_plot <- ggplot(top_species_data, aes(x = Confidence, fill = Common_Name)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Common_Name, scales = "free_y") +
  scale_fill_viridis_d() +
  labs(
    title = "Konfidenzdichte der Top 10 Vogelarten in Sachsen-Anhalt",
    x = "Konfidenzwert",
    y = "Dichte",
    fill = "Vogelart"
  ) +
  theme_minimal()

# Plot anzeigen
print(confidence_density_plot)

# Plot speichern
ggsave(
  filename = file.path(output_directory, "Confidence_Density_Top_10_Species.png"),
  plot = confidence_density_plot,
  width = 12,
  height = 10
)
cat("Konfidenzdichte-Plots gespeichert.\n")

# Schritt 17: Weitere Datenaggregation
# Annahme: 'Device' ist eine tatsächliche Spalte. Falls nicht, muss diese korrekt geladen werden.
if(!"Device" %in% colnames(filtered_df)){
  warning("Die Spalte 'Device' existiert nicht. Eine temporäre Spalte 'Device = 1' wird hinzugefügt.")
  filtered_df <- filtered_df %>% mutate(Device = 1)
}

# Anzahl der Anrufe pro Gerät und Art berechnen
call_counts <- filtered_df %>%
  group_by(Device, Common_Name) %>%
  summarise(Count = n(), .groups = 'drop')

# Gesamtsummen und eindeutige Arten pro Gerät zusammenfassen
device_summary <- call_counts %>%
  group_by(Device) %>%
  summarise(
    Total_Calls = sum(Count),
    Unique_Species = n_distinct(Common_Name),
    .groups = 'drop'
  )

# Optional: Ergebnisse anzeigen oder speichern
print(device_summary)
#write_xlsx(device_summary, file.path(base_path, "Device_Summary.xlsx"))
#cat("Gerätezusammenfassung gespeichert.\n")

# --- Zusätzliche Plots für Top 10 Vogelarten ---

# Schritt 18: Bar Plot der Gesamtanzahl der Anrufe pro Art
bar_plot <- ggplot(species_counts %>% filter(Common_Name %in% top_10_species), 
                   aes(x = reorder(Common_Name, -Anzahl), y = Anzahl, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(
    title = "Gesamtanzahl der Anrufe pro Vogelart (Top 10)",
    x = "Vogelart",
    y = "Anzahl der Erkennungen",
    fill = "Vogelart"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot speichern
ggsave(
  filename = file.path(output_directory, "Total_Calls_Per_Species_Barplot.png"),
  plot = bar_plot,
  width = 12,
  height = 8
)
cat("Bar Plot der Gesamtanzahl der Anrufe pro Art gespeichert.\n")

# Schritt 19: Boxplot der Konfidenzwerte pro Art
boxplot_confidence <- ggplot(top_species_data, aes(x = Common_Name, y = Confidence, fill = Common_Name)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(
    title = "Boxplot der Konfidenzwerte der Top 10 Vogelarten",
    x = "Vogelart",
    y = "Konfidenzwert",
    fill = "Vogelart"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot speichern
ggsave(
  filename = file.path(output_directory, "Confidence_Boxplot_Top10_Species.png"),
  plot = boxplot_confidence,
  width = 12,
  height = 8
)
cat("Boxplot der Konfidenzwerte pro Art gespeichert.\n")

# Schritt 20: Heatmap der Anrufe über die Zeit und Arten
# Aggregieren der Daten für die Heatmap
heatmap_data <- filtered_df %>%
  mutate(Stunde = hour(DateTime)) %>%
  group_by(Stunde, Common_Name) %>%
  summarise(Anzahl = n(), .groups = 'drop') %>%
  filter(Common_Name %in% top_10_species)

heatmap_plot <- ggplot(heatmap_data, aes(x = Stunde, y = Common_Name, fill = Anzahl)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Anzahl der Erkennungen") +
  labs(
    title = "Heatmap der Anrufe über die Stunden und Top 10 Vogelarten",
    x = "Stunde des Tages",
    y = "Vogelart"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Plot speichern
ggsave(
  filename = file.path(output_directory, "Heatmap_Calls_Time_Species.png"),
  plot = heatmap_plot,
  width = 14,
  height = 10
)
cat("Heatmap der Anrufe über die Zeit und Arten gespeichert.\n")

# Schritt 21: Scatter Plot von Konfidenzwerten vs. Zeit
scatter_plot <- ggplot(top_species_data, aes(x = DateTime, y = Confidence, color = Common_Name)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Konfidenzwerte der Top 10 Vogelarten über die Zeit",
    x = "Datum und Uhrzeit",
    y = "Konfidenzwert",
    color = "Vogelart"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot speichern
ggsave(
  filename = file.path(output_directory, "Confidence_vs_Time_Scatter.png"),
  plot = scatter_plot,
  width = 14,
  height = 8
)
cat("Scatter Plot von Konfidenzwerten vs. Zeit gespeichert.\n")

# Schritt 22: Violin Plot der Konfidenzwerte pro Art
violin_plot <- ggplot(top_species_data, aes(x = Common_Name, y = Confidence, fill = Common_Name)) +
  geom_violin(alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(
    title = "Violin Plot der Konfidenzwerte der Top 10 Vogelarten",
    x = "Vogelart",
    y = "Konfidenzwert",
    fill = "Vogelart"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot speichern
ggsave(
  filename = file.path(output_directory, "Confidence_ViolinPlot_Top10_Species.png"),
  plot = violin_plot,
  width = 12,
  height = 8
)
cat("Violin Plot der Konfidenzwerte pro Art gespeichert.\n")

# Schritt 24: Aktivitätsmuster pro Minute und Art der Top 10 Arten
activity_pattern <- filtered_df %>%
  group_by(Begin_Min, Common_Name) %>%
  summarise(Anzahl = n(), .groups = 'drop') %>%
  filter(Common_Name %in% top_10_species)

# Visualisierung der Aktivitätsmuster pro Minute und Art
activity_plot <- ggplot(activity_pattern, aes(x = Begin_Min, y = Anzahl, color = Common_Name)) +
  geom_line() +
  labs(
    title = "Zeitliche Aktivitätsmuster der Top 10 Vogelarten",
    x = "Zeit (Minuten seit Start der Aufnahme)",
    y = "Anzahl der Erkennungen",
    color = "Vogelart"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot speichern
ggsave(
  filename = file.path(output_directory, "Activity_Pattern_Per_Minute.png"),
  plot = activity_plot,
  width = 12,
  height = 8
)
cat("Aktivitätsmuster pro Minute gespeichert.\n")

# --- Ende der zusätzlichen Anpassungen ---
