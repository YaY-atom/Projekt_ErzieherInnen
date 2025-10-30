# ============================================================
# Libraries.R — Automatisches Installieren, Updaten & Laden
# ============================================================

# ---- Grundeinstellungen ----
options(repos = c(CRAN = "https://cloud.r-project.org"))

# ---- Pfad zum Logfile ----
log_file <- "Libraries_log.txt"

# ---- Liste aller benötigten Pakete ----
required_packages <- c(
  # Datenmanipulation & Bereinigung
  "tidyverse", "data.table", "janitor", "lubridate", "glue", "arrow", "future.apply",
  
  # Datenimport & Export
  "readxl", "openxlsx", "haven", "jsonlite",
  
  #Datenbank-Anbindung / SQL
  "DBI", "RPostgres", "RMySQL", "RSQLite", "odbc", "pool",
  
  # Datenexploration & Statistik
  "summarytools", "skimr", "psych",
  
  # Visualisierung
  "ggplot2", "ggthemes", "plotly", "corrplot",
  
  # Modellierung / Machine Learning
  "caret", "randomForest", "e1071",
  
  # Sonstiges / Nützliches
  "stringr", "forcats", "knitr", "kableExtra", "here", "progressr", "fs", "purrr", "progress"
)

# ============================================================
# Hilfsfunktion: Log-Eintrag schreiben
# ============================================================
write_log <- function(message_text) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", timestamp, "] ", message_text, "\n"), file = log_file, append = TRUE)
}

# ============================================================
# Funktion: installiere, update und lade Pakete
# ============================================================
load_libraries <- function(packages = required_packages, update = FALSE) {
  write_log("---- Starte Paketprüfung ----")
  
  # Fehlende Pakete installieren
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      msg <- paste("📦 Installiere Paket:", pkg)
      message(msg)
      write_log(msg)
      tryCatch(
        install.packages(pkg, dependencies = TRUE),
        error = function(e) write_log(paste("❌ Fehler bei Installation:", pkg, "-", e$message))
      )
    }
  }
  
  # Optional: Pakete aktualisieren
  if (update) {
    write_log("🔄 Überprüfe auf Updates ...")
    message("🔄 Überprüfe auf Paket-Updates ...")
    old_packages <- old.packages()
    if (!is.null(old_packages)) {
      msg <- paste("⬆️  Aktualisiere Pakete:", paste(rownames(old_packages), collapse = ", "))
      message(msg)
      write_log(msg)
      tryCatch(
        update.packages(ask = FALSE, dependencies = TRUE),
        error = function(e) write_log(paste("❌ Fehler beim Update:", e$message))
      )
    } else {
      message("✅ Alle Pakete sind aktuell.")
      write_log("✅ Keine Updates notwendig.")
    }
  }
  
  # Alle Pakete laden
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  
  message("✅ Alle Pakete wurden geladen und sind einsatzbereit.")
  write_log("✅ Alle Pakete wurden geladen.\n")
}

# ============================================================
# Optionale Funktion: Nur prüfen, ob Pakete installiert sind
# ============================================================
check_libraries <- function(packages = required_packages) {
  missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  if (length(missing) == 0) {
    message("✅ Alle Pakete sind installiert.")
    write_log("✅ Alle Pakete sind installiert.")
  } else {
    message("⚠️  Fehlende Pakete:", paste(missing, collapse = ", "))
    write_log(paste("⚠️ Fehlende Pakete:", paste(missing, collapse = ", ")))
  }
}

# ============================================================
# Automatisches Laden beim Import
# ============================================================
load_libraries(update = FALSE)  # Bei Bedarf TRUE setzen



#zum verwenden:
#source("R/Libraries.R")
#load_libraries(update = TRUE)

