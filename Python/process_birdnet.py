import pandas as pd
import glob
import os

# Check if running in a virtual environment
if not os.getenv("VIRTUAL_ENV"):
    print("It looks like you're not in a virtual environment.")
    print("To set up a virtual environment on your Desktop:")
    print("1. Open Terminal.")
    print("2. Run the following commands:")

    desktop_path = os.path.join(os.path.expanduser("~"), "Desktop")
    print(f"\ncd {desktop_path}")
    print("python3 -m venv birdnet_env")  # Create virtual environment on Desktop
    print("source birdnet_env/bin/activate")  # Activate the environment
    print("pip install pandas openpyxl")  # Install the required packages
    print("pip freeze > requirements.txt")  # Save the installed packages

    print("\nAfter setting up the environment, you can run this script with `python process_birdnet.py`.")
    exit(1)

def process_files_in_folder(input_folder, output_folder, folder_number):
    input_pattern = '*.BirdNET.selection.table.txt'
    output_file = os.path.join(output_folder, f'{folder_number}_BirdNET_selection_by_day.xlsx')

    # Alle entsprechenden Dateien im Verzeichnis einlesen
    files = glob.glob(os.path.join(input_folder, input_pattern))

    print(f"Gefundene Dateien zur Verarbeitung in {input_folder}: {files}")

    # Liste zum Sammeln aller Daten
    all_data = []
    header = []

    for file in files:
        with open(file, 'r') as f:
            lines = f.readlines()

        print(f"Verarbeite Datei: {file}, Zeilenanzahl: {len(lines)}")

        # Header verarbeiten (nur einmal setzen)
        if not header:
            header = [h.strip() for h in lines[0].strip().split('\t')]

        # Daten verarbeiten (alle Konfidenzwerte behalten)
        for line in lines[1:]:
            parts = line.strip().split('\t')
            if len(parts) == len(header):  # Sicherstellen, dass die Zeile die gleiche Anzahl von Spalten wie der Header hat
                all_data.append(parts)

    # Daten in pandas DataFrame laden
    if all_data:  # Sicherstellen, dass es Daten gibt
        df = pd.DataFrame(all_data, columns=header)

        # Sicherheitshalber Datentypen anpassen
        numeric_columns = ['Begin Time (s)', 'End Time (s)', 'Low Freq (Hz)', 'High Freq (Hz)', 'Confidence']
        for col in numeric_columns:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col])

        # Datum aus den Datei-Informationen extrahieren
        df['Date'] = df['Begin Path'].apply(lambda x: os.path.basename(x).split('_')[0])

        # Excel-Schreibmaschine initialisieren
        with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
            # Daten nach Datum gruppieren
            grouped = df.groupby('Date')
            
            for date, group in grouped:
                group.to_excel(writer, sheet_name=str(date), index=False)

        print(f'Excel-Datei wurde erfolgreich erstellt: {output_file}')
    else:
        print(f'Keine Daten gefunden in {input_folder}.')

def main(base_input_folder, output_base_folder):
    subfolders = [f for f in os.listdir(base_input_folder) if os.path.isdir(os.path.join(base_input_folder, f))]

    for subfolder in subfolders:
        input_folder = os.path.join(base_input_folder, subfolder)
        subfolder_number = os.path.basename(subfolder)
        output_folder = os.path.join(output_base_folder, "resultsexcel", subfolder_number)
        os.makedirs(output_folder, exist_ok=True)

        print(f'Verarbeite Dateien im Ordner: {input_folder}')
        process_files_in_folder(input_folder, output_folder, subfolder_number)

if __name__ == "__main__":
    base_input_folder = "/Volumes/TRANSFORM/results"
    output_base_folder = "/Volumes/TRANSFORM"

    if not os.path.exists(base_input_folder):
        print(f'Basis-Eingabeordner existiert nicht: {base_input_folder}')
        exit(1)

    os.makedirs(os.path.join(output_base_folder, "resultsexcel"), exist_ok=True)

    main(base_input_folder, output_base_folder)

    print("Verarbeitung abgeschlossen.")
