# Module for the Elo victory rate graph

mod_elo_victory_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Taux de victoire",
    h2("Proportion de victoire pour les joueurs désavantagés"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          ns("taille_classe"),
          label = "Taille des classes Elo :",
          min = 100,
          max = 1000,
          value = 400,
          step = 100
        ),
        radioButtons(
          ns("couleur_desavantage"),
          label = "Joueur désavantagé :",
          choices = c(
            "Les deux" = "tous",
            "Blancs" = "white",
            "Noirs" = "black"
          ),
          selected = "tous"
        )
      ),
      mainPanel(
        plotOutput(ns("graphique_elo"), height = "600px")
      )
    )
  )
}

mod_elo_victory_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$graphique_elo <- renderPlot({
      donnees_filtrees <- Donnees_Chess %>%
        filter(
          !is.na(victoire_desavantage),
          input$couleur_desavantage == "tous" |
            couleur_desavantage == input$couleur_desavantage
        )

      # Taille des classes choisie par l'utilisateur
      taille <- as.numeric(input$taille_classe)

      # Elo maximum
      max_elo <- max(
        donnees_filtrees$elo_diff_abs,
        na.rm = TRUE
      )

      # Création des classes
      donnees_filtrees <- donnees_filtrees %>%
        mutate(
          classe_elo = cut(
            elo_diff_abs,
            breaks = seq(
              0,
              ceiling(max_elo / taille) * taille,
              by = taille
            ),
            right = FALSE,
            include.lowest = TRUE
          )
        )

      # Calcul du taux de victoire
      resume <- donnees_filtrees %>%
        group_by(classe_elo) %>%
        summarise(
          taux_victoire = mean(
            victoire_desavantage,
            na.rm = TRUE
          ) * 100,
          nombre_parties = n(),
          .groups = "drop"
        )

      # Graphique
      ggplot(
        resume,
        aes(
          x = classe_elo,
          y = taux_victoire
        )
      ) +
        geom_col(
          fill = "black"
        ) +
        geom_text(
          aes(
            label = paste0(
              round(taux_victoire, 1),
              "%",
              "\n(n = ",
              nombre_parties,
              ")"
            )
          ),
          vjust = -0.3
        ) +
        labs(
          title = "Taux de victoire du joueur désavantagé",
          x = "Différence d'Elo",
          y = "Taux de victoire (%)"
        ) +
        theme_minimal()
    })
  })
}