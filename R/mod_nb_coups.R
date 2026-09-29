# Module for the average moves per Elo graph

mod_nb_coups_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Nombre de coups",
    h2("Nombre moyen de coups selon l'Elo"),
    mainPanel(
      plotOutput(ns("graphique_temps_elo"), height = "600px")
    )
  )
}

mod_nb_coups_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$graphique_temps_elo <- renderPlot({
      Donnees_Chess$elo_classe <- cut(
        Donnees_Chess$elo_moyen,
        breaks = seq(
          floor(min(Donnees_Chess$elo_moyen, na.rm = TRUE) / 100) * 100,
          ceiling(max(Donnees_Chess$elo_moyen, na.rm = TRUE) / 100) * 100,
          by = 100
        ),
        include.lowest = TRUE
      )

      Donnees_graphique <- Donnees_Chess %>%
        group_by(elo_classe) %>%
        summarise(
          elo_moyen = mean(elo_moyen, na.rm = TRUE),
          coups_moyens = mean(turns, na.rm = TRUE),
          nombre_parties = n()
        )

      ggplot(
        Donnees_graphique,
        aes(
          x = elo_moyen,
          y = coups_moyens
        )
      ) +
        geom_line(
          color = "#2C3E50",
          linewidth = 1
        ) +
        geom_point(
          color = "#3498DB",
          size = 3
        ) +
        labs(
          title = "Nombre moyen de coups selon l'Elo moyen",
          subtitle = "Moyenne du nombre de coups par tranche de 100 points d'Elo",
          x = "Elo moyen de la partie",
          y = "Nombre moyen de coups"
        ) +
        theme_minimal(base_size = 14)
    })
  })
}