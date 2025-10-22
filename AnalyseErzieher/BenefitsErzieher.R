library(data.table)
library(ggplot2)
library(stringr)
library(forcats)
library(progressr)

analysiere_benefits <- function(data_benefits,
                                spalten = c("concept", "span", "span_normalized"),
                                filter_liste = list(
                                  hat_13_monatsgehalt = "Weihnachts_13_Gehalt",
                                  hat_urlaubsgeld = "Urlaubsgeld",
                                  hat_fortbildung = "Fortbildung|Weiterbildung",
                                  hat_gesundheit = "Gesundheit"
                                )) {
  benefit_names <- names(filter_liste)
  daten_liste <- list()
  jahr_liste <- list()
  
  progressr::with_progress({
    p <- progressr::progressor(steps = length(data_benefits))
    
    for (i in seq_along(data_benefits)) {
      name <- names(data_benefits)[i]
      jahr <- as.integer(stringr::str_extract(name, "\\d{4}"))
      
      df <- data.table::as.data.table(data_benefits[[i]])
      df[, jahr := jahr]
      
      # Relevante Spalten zu character
      spalten_vorhanden <- intersect(spalten, names(df))
      df[, (spalten_vorhanden) := lapply(.SD, as.character), .SDcols = spalten_vorhanden]
      
      # Benefit-Flags berechnen
      for (benefit in benefit_names) {
        pattern <- filter_liste[[benefit]]
        df[, (benefit) := rowSums(sapply(.SD, function(spalte) {
          grepl(pattern, spalte, ignore.case = TRUE)
        })) > 0, .SDcols = spalten_vorhanden]
      }
      
      # Gruppierung und Aggregation
      summary_df <- df[, c(.N, lapply(.SD, any)),
                       by = .(posting_id, jahr),
                       .SDcols = benefit_names]
      data.table::setnames(summary_df, "N", "anzahl_benefits")
      
      # Langformat
      daten_long <- data.table::melt(summary_df,
                                     id.vars = c("posting_id", "jahr", "anzahl_benefits"),
                                     measure.vars = benefit_names,
                                     variable.name = "Benefit",
                                     value.name = "vorhanden")[vorhanden == TRUE]
      
      daten_long <- daten_long[, .(Anzahl = .N), by = .(jahr, Benefit)]
      daten_liste[[i]] <- daten_long
      jahr_liste[[i]] <- summary_df[, .(gesamt = .N), by = jahr]
      
      p(sprintf("Verarbeite Datei %d von %d: %s", i, length(data_benefits), name))
    }
  })
  
  # Ergebnisse zusammenführen
  daten_long_gesamt <- data.table::rbindlist(daten_liste, use.names = TRUE, fill = TRUE)
  gesamtanzahl <- data.table::rbindlist(jahr_liste, use.names = TRUE, fill = TRUE)
  
  # Prozent berechnen
  daten_long_gesamt <- merge(daten_long_gesamt, gesamtanzahl, by = "jahr")
  daten_long_gesamt[, Prozent := Anzahl / gesamt * 100]
  daten_long_gesamt[, Benefit := forcats::fct_reorder(Benefit, Prozent, .desc = TRUE)]
  
  # Plot erstellen
  benefit_plot <- ggplot2::ggplot(daten_long_gesamt, ggplot2::aes(x = Benefit, y = Prozent)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(
      label = paste0(round(Prozent, 1), "%"),
      vjust = ifelse(Prozent < 39, -0.3, 1.5),
      color = ifelse(Prozent < 39, "black", "white")
    ), size = 3, show.legend = FALSE) +
    ggplot2::scale_color_identity() +
    ggplot2::facet_wrap(~ jahr, scales = "free_x") +
    ggplot2::labs(
      title = "Verteilung ausgewählter Benefits über Jahre",
      x = "Benefit", y = "Anteil in %"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  list(
    daten_long = daten_long_gesamt,
    gesamt = gesamtanzahl,
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






