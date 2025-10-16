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
  df <- bind_rows(Map(function(df, name) {
    df$jahr <- as.integer(str_extract(name, "\\d{4}"))
    df
  }, data_benefits, names(data_benefits)))
  
  # Relevante Spalten zu character konvertieren
  df <- df %>%
    mutate(across(all_of(intersect(spalten, names(.))), as.character))
  
  # Benefit-Flags berechnen (außerhalb von mutate)
  benefit_flags <- setNames(
    lapply(filter_liste, function(pattern) {
      rowSums(sapply(intersect(spalten, names(df)), function(sp) {
        grepl(pattern, df[[sp]], ignore.case = TRUE)
      })) > 0
    }),
    benefit_names
  )
  
  # Flags als neue Spalten hinzufügen
  df <- df %>%
    mutate(!!!benefit_flags)
  
  # Gruppieren und aggregieren
  summary_df <- df %>%
    group_by(posting_id, jahr) %>%
    summarise(
      anzahl_benefits = n(),
      across(all_of(benefit_names), any),
      .groups = "drop"
    )
  
  # Umformen und zählen
  daten_long <- summary_df %>%
    pivot_longer(cols = all_of(benefit_names), names_to = "Benefit", values_to = "vorhanden") %>%
    filter(vorhanden) %>%
    count(jahr, Benefit, name = "Anzahl")
  
  # Gesamtzahl je Jahr
  gesamtanzahl <- count(summary_df, jahr, name = "gesamt")
  
  # Prozent berechnen und sortieren
  daten_long <- daten_long %>%
    left_join(gesamtanzahl, by = "jahr") %>%
    mutate(
      Prozent = Anzahl / gesamt * 100,
      Benefit = fct_reorder(Benefit, Prozent, .desc = TRUE)
    )
  
  # Plot erstellen
  benefit_plot <- ggplot(daten_long, aes(x = Benefit, y = Prozent)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0(round(Prozent, 1), "%")),
              vjust = -0.3, size = 3) +
    facet_wrap(~ jahr, scales = "free_x") +
    labs(
      title = "Verteilung ausgewählter Benefits über Jahre",
      x = "Benefit", y = "Anteil in %"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Rückgabe als Liste
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






