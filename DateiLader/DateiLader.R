# DateiLader.R

Datei_laden <- function(pfad, pattern = NULL, namen_vergeben = TRUE, use_fread = TRUE) {
  # Sicherstellen, dass benötigte Pakete geladen sind
  required_pkgs <- c("data.table", "readxl")
  new_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(new_pkgs) > 0) install.packages(new_pkgs)
  
  # Alle unterstützten Dateitypen
  alle_dateien <- list.files(
    path = pfad,
    pattern = "\\.(csv|xlsx|xls)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Optional: Nach Muster filtern
  if (!is.null(pattern)) {
    alle_dateien <- grep(pattern, alle_dateien, value = TRUE)
  }
  
  if (length(alle_dateien) == 0) {
    warning("Keine passenden Dateien gefunden.")
    return(list())
  }
  
  # Lade-Funktion pro Datei
  lade_einzeldatei <- function(datei) {
    tryCatch({
      if (grepl("\\.csv$", datei, ignore.case = TRUE)) {
        if (use_fread) {
          data.table::fread(datei)
        } else {
          readr::read_csv(datei, show_col_types = FALSE)
        }
      } else if (grepl("\\.xlsx?$", datei, ignore.case = TRUE)) {
        readxl::read_excel(datei)
      } else {
        NULL
      }
    }, error = function(e) {
      warning(paste("Fehler beim Laden von:", datei, "->", e$message))
      return(NULL)
    })
  }
  
  # Daten laden
  geladene_daten <- lapply(alle_dateien, lade_einzeldatei)
  
  # Optional: Liste benennen
  if (namen_vergeben) {
    names(geladene_daten) <- basename(alle_dateien)
  }
  
  # Entferne fehlgeschlagene Ladevorgänge
  geladene_daten <- Filter(Negate(is.null), geladene_daten)
  
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
