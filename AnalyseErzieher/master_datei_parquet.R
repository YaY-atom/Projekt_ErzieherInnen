# ============================================================
# Master-Datei Erstellung – Jobmonitor Erzieher:innen 2019–2024
# ============================================================
source(here::here("R_Libraries", "Libraries.R"))


setDTthreads(0)
options(datatable.optimize = 2)

# --- Basis-Pfad ---
base_path <- "C:/Users/YAVU061/OneDrive - Bertelsmann Stiftung/[P] Zentrum für Datenmanagement - Data Science Lab/Projektarbeit/Jobmonitor Datensätze"
parquet_path <- "C:/Users/YAVU061/OneDrive - Bertelsmann Stiftung/Desktop/Projekte/R"
# --- Parquet Speicherordner  ---
parquet_storage_dir <- file.path(parquet_path, "Parquet_Files")
dir_create(parquet_storage_dir, recurse = TRUE)

# ============================================================
# 1. Hilfsfunktionen
# ============================================================

convert_to_parquet <- function(csv_path, force = FALSE) {
  parquet_filename <- paste0(tools::file_path_sans_ext(basename(csv_path)), ".parquet")
  pq_path <- file.path(parquet_storage_dir, parquet_filename)
  
  if (!file.exists(pq_path) || force) {
    cat("🧩 Konvertiere:", basename(csv_path), "→ Parquet\n")
    dt <- fread(csv_path, encoding = "UTF-8", showProgress = FALSE)
    write_parquet(as_arrow_table(dt), pq_path, compression = "zstd")
    rm(dt); gc()
  }
  return(pq_path)
}

convert_folder_to_parquet <- function(folder, pattern) {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  pq_paths <- map_chr(files, convert_to_parquet)
  return(pq_paths)
}

read_parquet_dt <- function(path) {
  arrow::read_parquet(path, as_data_frame = FALSE) %>% 
    as.data.table()
}

# ============================================================
# 2. Konvertierung aller CSV-Dateien zu Parquet (einmalig)
# ============================================================

cat("📦 Konvertiere große CSVs zu Parquet (nur beim ersten Mal)...\n")

# Ordner: Berufe
berufe_pq  <- convert_folder_to_parquet(file.path(base_path, "Berufe"), "kldb_\\d{4}\\.csv")

# Ordner: Datum, Herkunft, Ort
datum_pq   <- convert_folder_to_parquet(file.path(base_path, "Datum, Herkunft, Ort"), "published_at_source_type_location_\\d{4}\\.csv")

# Ordner: Sprache der Stellenanzeige_Advertiser_type, Art der Stellenausschreibung
sprache_pq <- convert_folder_to_parquet(file.path(base_path, "Sprache der Stellenanzeige_Advertiser_type, Art der Stellenausschreibung"), "sprache_advertiser_contract_type_\\d{4}\\.csv")

# Ordner: NUTS-ID (Region) und Homeoffice
nuts_pq <- convert_to_parquet(file.path(base_path, "NUTS-ID (Region) und Homeoffice", "NUTSRemote.csv"))

# Ordner: Titel des Jobs für Qualitätsprüfung
job_pq  <- convert_to_parquet(file.path(base_path, "Titel des Jobs für Qualitätsprüfung", "Job-Title_2024.csv"))

# Ordner: Transversale Skills (Überfachliche Kompetenzen)
transversal_skills_pq <- convert_to_parquet(file.path(base_path, "Transversale Skills (Überfachliche Kompetenzen)", "enrichment_posting_transversal_skills.csv"))
transversal_skills_label_pq <- convert_to_parquet(file.path(base_path, "Transversale Skills (Überfachliche Kompetenzen)", "enrichment_transversal_skills_Was bedeuten_die_skill_id.csv"))

# Ordner: verbis fachkompetenzen -Skills im Beruf
verbis_fach_pq <- convert_to_parquet(file.path(base_path, "verbis fachkompetenzen -Skills im Beruf", "Verbis_Fachkompetenzen_Erzieher_innen_2019-2024.csv"))
verbis_label_pq <- convert_to_parquet(file.path(base_path, "verbis fachkompetenzen -Skills im Beruf", "Verbis_Fachkompetenzen_Label.csv"))

# Ordner: Zusatzleistungen - Wir bieten
benefits_pq <- convert_folder_to_parquet(file.path(base_path, "Zusatzleistungen - Wir bieten"), "Benefits_\\d{4}\\.csv")
family_compatibility_pq <- convert_to_parquet(file.path(base_path, "Zusatzleistungen - Wir bieten", "family_compatibility_2024.csv"))
tsc_pq <- convert_to_parquet(file.path(base_path, "Zusatzleistungen - Wir bieten", "TSC2024_alle.csv"))

# ============================================================
# 3. Parallele Verarbeitung pro Jahr
# ============================================================

plan(multisession, workers = availableCores() - 1)
cat("💻 Verwende", availableCores() - 1, "Kerne parallel.\n")

# --- Zwischenspeicher für die Masterdateien ---
output_dir <- file.path(base_path, "Masterdaten/tmp_batches")
dir_create(output_dir, recurse = TRUE)

# Alle Jahre aus Datum-Dateien extrahieren
years <- datum_pq %>% 
  str_extract("\\d{4}") %>% 
  as.integer() %>% 
  unique() %>% 
  sort()

future_lapply(years, function(yr) {
  cat("🔄 Verarbeite Jahr:", yr, "\n")
  
  # Daten aus Parquet laden (je Jahr)
  dt_datum   <- read_parquet_dt(datum_pq[str_detect(datum_pq, as.character(yr))])
  dt_berufe  <- read_parquet_dt(berufe_pq[str_detect(berufe_pq, as.character(yr))])
  dt_sprache <- read_parquet_dt(sprache_pq[str_detect(sprache_pq, as.character(yr))])
  dt_benefits <- read_parquet_dt(benefits_pq[str_detect(benefits_pq, as.character(yr))])
  
  # Einzeldateien laden (nicht Jahr-spezifisch)
  dt_nuts    <- read_parquet_dt(nuts_pq)
  dt_job     <- read_parquet_dt(job_pq)
  dt_family_compat <- read_parquet_dt(family_compatibility_pq)
  dt_tsc <- read_parquet_dt(tsc_pq)
  dt_transversal_skills <- read_parquet_dt(transversal_skills_pq)
  dt_transversal_label <- read_parquet_dt(transversal_skills_label_pq)
  dt_verbis_fach <- read_parquet_dt(verbis_fach_pq)
  dt_verbis_label <- read_parquet_dt(verbis_label_pq)
  
  # Spaltennamen anpassen für Joins
  setnames(dt_datum, "id", "posting_id", skip_absent = TRUE)
  setnames(dt_sprache, "id", "posting_id", skip_absent = TRUE)
  setnames(dt_job, "id", "posting_id", skip_absent = TRUE)
  setnames(dt_transversal_label, "id", "skill_id", skip_absent = TRUE) # angepasst, falls benötigt
  setnames(dt_verbis_label, "id", "concept_id", skip_absent = TRUE)
  
  # Keys setzen für Joins
  setkey(dt_datum, posting_id)
  setkey(dt_berufe, posting_id)
  setkey(dt_sprache, posting_id)
  setkey(dt_benefits, posting_id)
  setkey(dt_nuts, posting_id)
  setkey(dt_job, posting_id)
  setkey(dt_family_compat, posting_id)
  setkey(dt_tsc, posting_id)
  setkey(dt_transversal_skills, posting_id)
  setkey(dt_verbis_fach, posting_id)
  
  # Joins Schritt für Schritt (left joins)
  dt <- merge(dt_datum, dt_berufe, by = "posting_id", all.x = TRUE)
  dt <- merge(dt, dt_sprache, by = "posting_id", all.x = TRUE)
  dt <- merge(dt, dt_benefits, by = "posting_id", all.x = TRUE, suffixes = c("", "_benefits"))
  dt <- merge(dt, dt_nuts, by = "posting_id", all.x = TRUE)
  dt <- merge(dt, dt_job, by = "posting_id", all.x = TRUE)
  dt <- merge(dt, dt_family_compat, by = "posting_id", all.x = TRUE)
  dt <- merge(dt, dt_tsc, by = "posting_id", all.x = TRUE)
  dt <- merge(dt, dt_transversal_skills, by = "posting_id", all.x = TRUE)
  dt <- merge(dt, dt_verbis_fach, by = "posting_id", all.x = TRUE)
  
  # Filter für relevante Daten (wie gehabt)
  dt <- dt[
    !contract_type_id %in% c(5, 7, 8, 9, 10) &
      str_starts(kldb_id, "8311")
  ]
  
  # Tätigkeitsniveau extrahieren
  dt[, taetigkeitsniveau := substr(kldb_id, 5, 5)]
  dt[, taetigkeitsniveau := fcase(
    taetigkeitsniveau == "1", "Helfer",
    taetigkeitsniveau == "2", "Fachkraft",
    taetigkeitsniveau == "3", "Spezialist",
    taetigkeitsniveau == "4", "Experte",
    default = NA_character_
  )]
  
  # Zwischenspeichern (Parquet)
  out_path <- file.path(output_dir, glue("master_erzieher_{yr}.parquet"))
  write_parquet(as_arrow_table(dt), out_path, compression = "zstd")
  
  cat("✅ Jahr", yr, "fertig ->", out_path, "\n")
  rm(dt, dt_datum, dt_berufe, dt_sprache, dt_benefits, dt_nuts, dt_job, dt_family_compat, dt_tsc, dt_transversal_skills, dt_verbis_fach)
  gc()
  return(out_path)
})

plan(sequential)

# ============================================================
# 4. Zusammenführen aller Jahresdateien in ein Master-Parquet
# ============================================================

cat("🧩 Kombiniere Jahres-Parquet-Dateien...\n")

all_files <- list.files(output_dir, pattern = "master_erzieher_\\d{4}\\.parquet", full.names = TRUE)
final_tbl <- open_dataset(all_files, format = "parquet")

write_dataset(final_tbl, 
              path = file.path(base_path, "Masterdaten/master_erzieher_2019_2024.parquet"),
              format = "parquet", compression = "zstd")

# Aufräumen
fs::dir_delete(output_dir)

cat("\n✅ Master-Parquet erfolgreich erstellt unter:\n",
    file.path(base_path, "Masterdaten/master_erzieher_2019_2024.parquet"), "\n")

gc()