# DateiLader.R

Datei_laden <- function(pfad, pattern = NULL, namen_vergeben = TRUE) {
  # Lade nötige Pakete
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  
  # Alle csv- und xlsx-Dateien im Pfad (rekursiv)
  alle_dateien <- list.files(
    path = pfad, 
    pattern = "\\.(csv|xlsx)$", 
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Optional: Filter nach Such-Pattern
  if (!is.null(pattern)) {
    dateien_zu_laden <- grep(pattern, alle_dateien, value = TRUE)
  } else {
    dateien_zu_laden <- alle_dateien
  }
  
  # Dateien laden
  geladene_daten <- lapply(dateien_zu_laden, function(datei) {
    if (grepl("\\.csv$", datei, ignore.case = TRUE)) {
      readr::read_csv(datei, show_col_types = FALSE)
    } else if (grepl("\\.xlsx$", datei, ignore.case = TRUE)) {
      readxl::read_excel(datei)
    } else {
      NULL
    }
  })
  
  # Optional: Benenne Liste nach Dateinamen
  if (namen_vergeben) {
    names(geladene_daten) <- basename(dateien_zu_laden)
  }
  
  return(geladene_daten)
}







#source("Pfad/zur/DateiLader.R")

# Dateien laden
#daten_liste <- Datei_laden(
#  pfad = "C:/Users/YAVU061/OneDrive - Bertelsmann Stiftung/[P] Zentrum für Datenmanagement - Data Science Lab/Projektarbeit/Jobmonitor Datensätze",
#  pattern = "Benefits_2019"
#)

# Zugriff z. B. auf die erste Datei:
#benefits_df <- daten_liste[[1]]
