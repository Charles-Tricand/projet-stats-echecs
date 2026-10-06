# Module for the game outcome by Elo class graph

mod_issue_parties_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Issue des parties",
    h2("Issue des parties selon la classe d'Elo"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          ns("taille_classe"),
          label = "Taille des classes Elo :",
          min = 100,
          max = 1000,
          value = 400,
          step = 100
        )
      ),
      mainPanel(
        plotOutput(ns("graphique_elo_test"), height = "600px")
      )
    )
  )
}

mod_issue_parties_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$graphique_elo_test <- renderPlot({
      # Taille des classes
      taille <- input$taille_classe

      # -------------------------------------------------------
      # Création des données
      # -------------------------------------------------------

      donnees_graphique <- Donnees_Chess %>%
        mutate(
          # Issue de la partie
          issue = case_when(
            victory_status == "mate" & winner == "white" ~
              "Victoire Blancs",
            victory_status == "mate" & winner == "black" ~
              "Victoire Noirs",
            victory_status == "draw" | winner == "draw" ~
              "Nulle",
            victory_status == "outoftime" ~
              "Temps",
            victory_status == "resign" ~
              "Abandon",
            TRUE ~ NA_character_
          ),

          # Elo moyen
          elo_moyen = (white_rating + black_rating) / 2
        ) %>%
        filter(
          !is.na(elo_moyen),
          !is.na(issue)
        )

      # -------------------------------------------------------
      # Création des classes d'Elo
      # -------------------------------------------------------

      min_elo <- floor(
        min(donnees_graphique$elo_moyen) / taille
      ) * taille

      max_elo <- ceiling(
        max(donnees_graphique$elo_moyen) / taille
      ) * taille

      # On ajoute une classe supplémentaire pour être sûr
      # que la valeur maximale soit incluse
      bornes <- seq(
        min_elo,
        max_elo + taille,
        by = taille
      )

      donnees_graphique <- donnees_graphique %>%
        mutate(
          classe_elo = cut(
            elo_moyen,
            breaks = bornes,
            right = FALSE,
            include.lowest = TRUE
          )
        )

      # -------------------------------------------------------
      # Comptage
      # -------------------------------------------------------

      resume <- donnees_graphique %>%
        count(
          classe_elo,
          issue,
          name = "nombre_parties"
        )

      # -------------------------------------------------------
      # Graphique
      # -------------------------------------------------------

      ggplot(
        resume,
        aes(
          x = classe_elo,
          y = nombre_parties,
          fill = issue
        )
      ) +
        geom_col(
          position = "fill"
        ) +
        scale_y_continuous(
          labels = scales::label_percent()
        ) +
        scale_fill_manual(
          values = c(
            "Victoire Blancs" = "green",
            "Victoire Noirs" = "red",
            "Nulle" = "grey",
            "Temps" = "blue",
            "Abandon" = "purple"
          )
        ) +
        labs(
          title = "Issue des parties selon la classe d'Elo",
          subtitle = "Proportion des différentes issues",
          x = "Classe d'Elo moyen",
          y = "Proportion",
          fill = "Issue"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            hjust = 1
          ),
          legend.position = "bottom"
        )
    })
  })
}