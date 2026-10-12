# =====================================================================
# Module : évaluation Stockfish de l'opening vs taux de victoire réel
# des Blancs, un point par ouverture.
#
# Lecture : les ouvertures au-dessus de la tendance obtiennent plus de
# victoires que ne le prédit leur évaluation (positions faciles à
# jouer, pièges...) ; celles en dessous en obtiennent moins.
#
# Prérequis : Donnees_Chess doit contenir la colonne eval_opening
# (voir evaluer_openings_stockfish.R), par exemple dans global.R :
#   Donnees_Chess <- readRDS("Donnees_Chess_eval.rds")
#
# Intégration :
#   ui.R     : mod_eval_ouvertures_ui("eval_ouvertures")
#   server.R : mod_eval_ouvertures_server("eval_ouvertures")
# =====================================================================

library(shiny)
library(dplyr)
library(ggplot2)

# Même définition que dans mod_eval_elo.R (ne la redéfinit pas si elle existe)
if (!exists("TRANCHES_ELO")) {
  TRANCHES_ELO <- c("< 1200", "1200-1499", "1500-1799", "1800-2099", "≥ 2100")
}

mod_eval_ouvertures_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Évaluation × Ouvertures",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          ns("tranche"), "Tranche d'Elo",
          choices  = c("Toutes les tranches", TRANCHES_ELO),
          selected = "Toutes les tranches"
        ),
        radioButtons(
          ns("nulles"), "Parties nulles",
          choices  = c("Exclues" = "exclues",
                       "Comptées pour ½ point" = "demi"),
          selected = "exclues"
        ),
        sliderInput(
          ns("n_min"), "Parties minimum par ouverture",
          min = 10, max = 500, value = 50, step = 10
        ),
        sliderInput(
          ns("n_etiquettes"), "Ouvertures étiquetées (les plus jouées)",
          min = 0, max = 20, value = 8, step = 1
        ),
        helpText(
          "Chaque point est une ouverture. ",
          "La taille indique le nombre de parties ; ",
          "la droite est la tendance (pondérée par le nombre de parties)."
        )
      ),
      mainPanel(
        plotOutput(ns("graphique"), height = "600px")
      )
    )
  )
}

mod_eval_ouvertures_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    donnees_base <- reactive({
      Donnees_Chess |>
        filter(!is.na(eval_opening)) |>
        mutate(
          elo_moyen = (white_rating + black_rating) / 2,
          tranche_elo = cut(
            elo_moyen,
            breaks = c(-Inf, 1200, 1500, 1800, 2100, Inf),
            labels = TRANCHES_ELO,
            right  = FALSE
          ),
          # évaluation en pions, bornée à ±3 (écarte les scores de mat)
          eval_pions = pmax(pmin(eval_opening, 300), -300) / 100
        )
    })

    resume <- reactive({
      d <- donnees_base()

      if (input$tranche != "Toutes les tranches") {
        d <- d |> filter(tranche_elo == input$tranche)
      }

      if (input$nulles == "exclues") {
        d <- d |>
          filter(winner != "draw") |>
          mutate(score = as.numeric(winner == "white"))
      } else {
        d <- d |>
          mutate(score = case_when(
            winner == "white" ~ 1,
            winner == "draw"  ~ 0.5,
            TRUE              ~ 0
          ))
      }

      # Une ouverture peut être atteinte par des suites de coups légèrement
      # différentes : on prend l'évaluation médiane.
      d |>
        group_by(opening_name) |>
        summarise(
          n          = n(),
          eval_pions = median(eval_pions),
          taux       = mean(score),
          .groups    = "drop"
        ) |>
        filter(n >= input$n_min)
    })

    output$graphique <- renderPlot({
      res <- resume()
      validate(need(nrow(res) >= 3,
                    "Pas assez d'ouvertures : baisse le nombre minimum de parties."))

      # Écart à la tendance (régression pondérée par le nombre de parties)
      fit <- lm(taux ~ eval_pions, data = res, weights = n)
      res$ecart <- ifelse(residuals(fit) >= 0,
                          "Au-dessus de la tendance",
                          "En dessous de la tendance")

      etiquettes <- slice_max(res, n, n = input$n_etiquettes, with_ties = FALSE)

      ggplot(res, aes(x = eval_pions, y = taux)) +
        geom_hline(yintercept = 0.5, colour = "grey70", linewidth = 0.4) +
        geom_smooth(aes(weight = n), method = "lm", formula = y ~ x,
                    se = FALSE, colour = "grey30", linewidth = 0.6) +
        geom_point(aes(size = n, colour = ecart), alpha = 0.8) +
        geom_text(data = etiquettes, aes(label = opening_name),
                  size = 3, vjust = -1, check_overlap = TRUE) +
        scale_colour_manual(
          values = c("Au-dessus de la tendance" = "#2a9d8f",
                     "En dessous de la tendance" = "#e76f51"),
          name = NULL
        ) +
        scale_size_continuous(range = c(2, 9), name = "Parties") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(
          subtitle = if (input$tranche == "Toutes les tranches") "Toutes les tranches d'Elo"
                     else paste("Elo moyen :", input$tranche),
          x = "Évaluation de l'opening (pions, point de vue des Blancs)",
          y = if (input$nulles == "exclues") "Taux de victoire des Blancs"
              else "Score des Blancs (victoire = 1, nulle = ½)"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom")
    })
  })
}
