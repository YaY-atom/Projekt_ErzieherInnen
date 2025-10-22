library(data.table)
library(ggplot2)
library(stringr)
library(progressr)

kldb_erzieher <- function(data_benefits) {
  if (is.null(names(data_benefits))) {
    stop("Die Liste data_benefits muss benannte Elemente haben (Dateinamen).")
  }
  
  ergebnisse_liste <- list()
  
  # Fortschrittsanzeige mit progressr (Handlers müssen VORHER gesetzt werden!)
  progressr::with_progress({
    p <- progressr::progressor(steps = length(data_benefits))
    
    for (i in seq_along(data_benefits)) {
      name <- names(data_benefits)[i]
      jahr <- as.integer(stringr::str_extract(name, "\\d{4}"))
      
      df <- data.table::as.data.table(data_benefits[[i]])
      df[, kldb_id := as.character(kldb_id)]
      df[, jahr := jahr]
      
      # Nur relevante Codes behalten
      df <- df[kldb_id %in% as.character(83110:83114)]
      
      if (nrow(df) == 0) {
        p(sprintf("Überspringe %s (keine passenden Codes)", name))
        next
      }
      
      # Zählen pro Jahr und Kldb-Code
      counts <- df[, .(anzahl = .N), by = .(jahr, kldb_id)]
      
      # Prozent pro Jahr berechnen
      counts[, prozent := round(100 * anzahl / sum(anzahl), 2), by = jahr]
      
      # Rollen zuweisen
      counts[, rolle := data.table::fcase(
        kldb_id == "83111", "Aushilfe",
        kldb_id == "83112", "Fachkraft",
        kldb_id == "83113", "Spezialist",
        kldb_id == "83114", "Experte",
        default = NA_character_
      )]
      
      ergebnisse_liste[[i]] <- counts
      p(sprintf("Verarbeitet: %s", name))
    }
  })
  
  # Ergebnisse zusammenführen
  alle_counts <- data.table::rbindlist(ergebnisse_liste, use.names = TRUE, fill = TRUE)
  
  # Plot erstellen
  plot <- ggplot2::ggplot(alle_counts, aes(x = rolle, y = prozent)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_text(aes(
      label = paste0(prozent, "%"),
      vjust = ifelse(prozent < 50, -0.3, 1.5),
      color = ifelse(prozent < 50, "black", "white")
    ), size = 3, show.legend = FALSE) +
    ggplot2::scale_color_identity() +
    ggplot2::facet_wrap(~ jahr) +
    ggplot2::labs(
      title = "Verteilung Erzieher:innen nach KLD-Berufsgruppen (8311X)",
      x = "KLD-Berufsgruppe",
      y = "Prozentanteil"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  list(
    daten = alle_counts,
    plot = plot
  )
}


#zum ausführen:
#ergebnisse <- analysiere_erzieher(data_benefits)


# Optional: Plot speichern
# ggsave("erzieher_verteilung.png", plot = ergebnisse$plot, width = 10, height = 6)

# Optional: Daten exportieren
# write.csv(ergebnisse$daten, "erzieher_analyse.csv", row.names = FALSE)