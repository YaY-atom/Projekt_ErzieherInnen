# KLDBErzieher.R
# KLDBErzieher_pipe.R

library(dplyr)
library(ggplot2)
library(stringr)

library(data.table)
library(stringr)
library(ggplot2)

kldb_erzieher <- function(data_benefits) {
  # --- 1. Validierung ---
  if (is.null(names(data_benefits))) {
    stop("Die Liste data_benefits muss benannte Elemente haben (Dateinamen).")
  }
  
  # --- 2. Daten zusammenführen und Jahr extrahieren ---
  list_dt <- Map(function(df, name) {
    dt <- as.data.table(df)
    dt[, `:=`(
      kldb_id = as.character(kldb_id),
      jahr = as.integer(str_extract(name, "\\d{4}"))
    )]
    return(dt)
  }, data_benefits, names(data_benefits))
  
  df <- rbindlist(list_dt, use.names = TRUE, fill = TRUE)
  
  # --- 3. Filtern auf relevante KldB-Codes (83110 bis 83114) ---
  df <- df[kldb_id %in% as.character(83110:83114)]
  
  # --- 4. Gruppieren und Prozent berechnen ---
  # Anzahl pro Jahr und kldb_id
  counts <- df[, .(anzahl = .N), by = .(jahr, kldb_id)]
  
  # Prozent pro Jahr
  counts[, prozent := round(100 * anzahl / sum(anzahl), 2), by = jahr]
  
  # --- 5. Berufsrollen zuweisen ---
  counts[, rolle := fcase(
    kldb_id == "83111", "Aushilfe",
    kldb_id == "83112", "Fachkraft",
    kldb_id == "83113", "Spezialist",
    kldb_id == "83114", "Experte",
    default = NA_character_
  )]
  
  # --- 6. Plot erzeugen ---
  plot <- ggplot(counts, aes(x = rolle, y = prozent)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(
      label = paste0(prozent, "%"),
      vjust = ifelse(prozent < 50, -0.3, 1.5),
      color = ifelse(prozent < 50, "black", "white")
    ), size = 3, show.legend = FALSE) +
    scale_color_identity() +
    facet_wrap(~ jahr) +
    labs(
      title = "Verteilung Erzieher:innen nach KLD-Berufsgruppen (8311X)",
      x = "KLD-Berufsgruppe",
      y = "Prozentanteil"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # --- 7. Rückgabe ---
  list(
    daten = counts,
    plot = plot
  )
}

#zum ausführen:
#ergebnisse <- analysiere_erzieher(data_benefits)


# Optional: Plot speichern
# ggsave("erzieher_verteilung.png", plot = ergebnisse$plot, width = 10, height = 6)

# Optional: Daten exportieren
# write.csv(ergebnisse$daten, "erzieher_analyse.csv", row.names = FALSE)