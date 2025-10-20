library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
analysiere_benefits <- function(data_benefits,
                                   spalten = c("concept", "span", "span_normalized"),
                                   filter_liste = list(
                                     hat_13_monatsgehalt = "Weihnachts_13_Gehalt",
                                     hat_urlaubsgeld = "Urlaubsgeld",
                                     hat_fortbildung = "Fortbildung|Weiterbildung",
                                     hat_gesundheit = "Gesundheit"
                                   )) {
  
  benefit_names <- names(filter_liste)
  
  # Alle Dataframes zusammenführen & 'jahr' extrahieren
  list_dt <- Map(function(df, name) {
    dt <- as.data.table(df)
    dt[, jahr := as.integer(str_extract(name, "\\d{4}"))]
    return(dt)
  }, data_benefits, names(data_benefits))
  
  df <- rbindlist(list_dt, use.names = TRUE, fill = TRUE)
  
  # Relevante Spalten zu character konvertieren
  spalten_vorhanden <- intersect(spalten, names(df))
  df[, (spalten_vorhanden) := lapply(.SD, as.character), .SDcols = spalten_vorhanden]
  
  # Benefit-Flags berechnen
  for (benefit in benefit_names) {
    pattern <- filter_liste[[benefit]]
    df[, (benefit) := rowSums(sapply(.SD, function(spalte) {
      grepl(pattern, spalte, ignore.case = TRUE)
    })) > 0, .SDcols = spalten_vorhanden]
  }
  
  # Gruppieren und aggregieren
  summary_df <- df[, c(.N, lapply(.SD, any)), 
                   by = .(posting_id, jahr), 
                   .SDcols = benefit_names]
  setnames(summary_df, "N", "anzahl_benefits")
  
  # Umformen in langes Format
  daten_long <- melt(summary_df,
                     id.vars = c("posting_id", "jahr", "anzahl_benefits"),
                     measure.vars = benefit_names,
                     variable.name = "Benefit",
                     value.name = "vorhanden")[vorhanden == TRUE]
  
  # Zählen je Jahr und Benefit
  daten_long <- daten_long[, .(Anzahl = .N), by = .(jahr, Benefit)]
  
  # Gesamtzahl pro Jahr
  gesamtanzahl <- summary_df[, .(gesamt = .N), by = jahr]
  
  # Prozent berechnen und sortieren
  daten_long <- merge(daten_long, gesamtanzahl, by = "jahr")
  daten_long[, Prozent := Anzahl / gesamt * 100]
  daten_long[, Benefit := fct_reorder(Benefit, Prozent, .desc = TRUE)]
  
  # Plot erstellen
  benefit_plot <- ggplot(daten_long, aes(x = Benefit, y = Prozent)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(
      label = paste0(round(Prozent, 1), "%"),
      vjust = ifelse(Prozent < 50, -0.3, 1.5),
      color = ifelse(Prozent < 50, "black", "white")
    ), size = 3, show.legend = FALSE) +
    scale_color_identity() +
    facet_wrap(~ jahr, scales = "free_x") +
    labs(
      title = "Verteilung ausgewählter Benefits über Jahre",
      x = "Benefit", y = "Anteil in %"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Rückgabe
  list(
    daten = summary_df,
    daten_long = daten_long,
    plot = benefit_plot
  )
}


#resultate <- analysiere_benefits(data_benefits) <= anwendung im code

# Ergebnis-Daten
#head(resultate$daten)

# Langformat für Auswertung
#head(resultate$daten_long)

# Plot anzeigen
#print(resultate$plot)






