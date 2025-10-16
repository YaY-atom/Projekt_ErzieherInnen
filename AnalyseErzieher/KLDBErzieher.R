# KLDBErzieher.R
# KLDBErzieher_pipe.R

library(dplyr)
library(ggplot2)
library(stringr)

kldb_erzieher <- function(data_benefits) {
  # --- 1. Validierung ---
  if (is.null(names(data_benefits))) {
    stop("Die Liste data_benefits muss benannte Elemente haben (Dateinamen).")
  }
  
  # --- 2. Verarbeitung in einem Rutsch ---
  bind_rows(Map(function(df, name) {
    df %>%
      mutate(
        kldb_id = as.character(kldb_id),
        jahr = as.integer(str_extract(name, "\\d{4}"))
      )
  }, data_benefits, names(data_benefits))) %>%
    
    # --- 3. Filtern auf relevante Kldb-Codes ---
    filter(kldb_id %in% as.character(83110:83114)) %>%
    
    # --- 4. Gruppieren und Prozent berechnen ---
    group_by(jahr, kldb_id) %>%
    summarise(anzahl = n(), .groups = "drop") %>%
    group_by(jahr) %>%
    mutate(prozent = round(100 * anzahl / sum(anzahl), 2)) %>%
    ungroup() %>%
    
    # --- 5. Rollenbezeichnungen zuweisen + Plot ---
    mutate(rolle = recode(kldb_id,
                          "83111" = "Aushilfe",
                          "83112" = "Fachkraft",
                          "83113" = "Spezialist",
                          "83114" = "Experte")) %>%
    
    # --- 6. In Plot umwandeln via Pipe-Block ---
    {
      ergebnisse <- .
      
      plot <- ggplot(ergebnisse, aes(x = rolle, y = prozent)) +
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
      
      list(
        daten = ergebnisse,
        plot = plot
      )
    }
}

#zum ausführen:
#ergebnisse <- analysiere_erzieher(data_benefits)


# Optional: Plot speichern
# ggsave("erzieher_verteilung.png", plot = ergebnisse$plot, width = 10, height = 6)

# Optional: Daten exportieren
# write.csv(ergebnisse$daten, "erzieher_analyse.csv", row.names = FALSE)