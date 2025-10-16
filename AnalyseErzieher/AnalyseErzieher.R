# Module/AnalyseErzieher.R

library(dplyr)
library(ggplot2)

# Funktion zur jährlichen Erzieher:innen-Analyse
analysiere_erzieher <- function(data_benefits) {
  
  # Automatisch Jahreszahlen ermitteln (z.B. 2019, 2020, ...)
  anzahl <- length(data_benefits)
  startjahr <- 2019  # Falls du z.B. immer ab 2019 arbeitest
  jahre <- seq(startjahr, by = 1, length.out = anzahl)
  names(data_benefits) <- paste0("kldb", jahre)
  
  # Alle Ergebnisse sammeln
  alle_ergebnisse <- data.frame()
  
  for (jahr in names(data_benefits)) {
    df <- data_benefits[[jahr]]
    jahr_num <- as.numeric(gsub("kldb", "", jahr))
    
    df_filtered <- df %>%
      filter(kldb_id %in% 83110:83114) %>%
      group_by(kldb_id) %>%
      summarise(anzahl = n(), .groups = "drop") %>%
      mutate(
        prozent = round(100 * anzahl / sum(anzahl), 2),
        jahr = jahr_num
      )
    
    alle_ergebnisse <- bind_rows(alle_ergebnisse, df_filtered)
  }
  
  # Rollenbezeichnungen hinzufügen
  alle_ergebnisse$kldb_id <- as.character(alle_ergebnisse$kldb_id)
  alle_ergebnisse$rolle <- recode(alle_ergebnisse$kldb_id,
                                  "83111" = "Aushilfe", 
                                  "83112" = "Fachkraft", 
                                  "83113" = "Spezialist",
                                  "83114" = "Experte"
  )
  
  # Plot erstellen
  plot <- ggplot(alle_ergebnisse, aes(x = rolle, y = prozent)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0(prozent, "%")),
              vjust = -0.3,
              size = 3) +
    facet_wrap(~ jahr) +
    labs(
      x = "KLD-Berufsgruppe",
      y = "Prozentanteil",
      title = "Verteilung Erzieher:innen nach Berufsgruppen (8311X) über die Jahre"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Rückgabe: sowohl Daten als auch Plot
  return(list(
    daten = alle_ergebnisse,
    plot = plot
  ))
}




#zum ausführen:
#ergebnisse <- analysiere_erzieher(data_benefits)