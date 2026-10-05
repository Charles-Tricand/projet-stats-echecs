mod_diff_elo_max_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    
    "Différence Elo maximale",
    
    h2("Résultats selon la différence d'Elo"),
    
    sidebarLayout(
      
      sidebarPanel(
        
        sliderInput(
          ns("diff_elo_max"),
          label = "Différence d'Elo maximale :",
          min = 100,
          max = 2000,
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
        
        # Résumé des résultats en pourcentage
        uiOutput(
          ns("resume_resultats")
        ),
        
        # Grille des 100 cases
        plotOutput(
          ns("graphique_resultats"),
          height = "650px"
        )
        
      )
      
    )
    
  )
}


# ==========================================================
# MODULE SERVER
# ==========================================================

mod_diff_elo_max_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # ==========================================================
    # PARTIES FILTREES
    # ==========================================================
    
    parties_filtrees <- reactive({
      
      donnees <- Donnees_Chess[
        Donnees_Chess$elo_diff_abs <= input$diff_elo_max &
          Donnees_Chess$elo_diff_abs > 0,
      ]
      
      # --------------------------------------------------------
      # Sélection du joueur désavantagé
      # --------------------------------------------------------
      
      if (input$couleur_desavantage == "black") {
        
        donnees <- donnees[
          donnees$couleur_desavantage == "black",
        ]
        
      } else if (input$couleur_desavantage == "white") {
        
        donnees <- donnees[
          donnees$couleur_desavantage == "white",
        ]
        
      }
      
      donnees
    })
    
    
    # ==========================================================
    # RESULTATS
    # ==========================================================
    
    resultats <- reactive({
      
      donnees <- parties_filtrees()
      
      # --------------------------------------------------------
      # Aucune donnée
      # --------------------------------------------------------
      
      if (nrow(donnees) == 0) {
        
        return(
          tibble(
            resultat = character(),
            n = integer(),
            proportion = numeric()
          )
        )
      }
      
      # --------------------------------------------------------
      # NOIR
      # --------------------------------------------------------
      
      if (input$couleur_desavantage == "black") {
        
        donnees <- donnees |>
          mutate(
            resultat = case_when(
              winner == "black" ~ "Victoire du joueur désavantagé",
              winner == "draw"  ~ "Partie nulle",
              TRUE              ~ "Défaite du joueur désavantagé"
            )
          )
        
      }
      
      # --------------------------------------------------------
      # BLANC
      # --------------------------------------------------------
      
      else if (input$couleur_desavantage == "white") {
        
        donnees <- donnees |>
          mutate(
            resultat = case_when(
              winner == "white" ~ "Victoire du joueur désavantagé",
              winner == "draw"  ~ "Partie nulle",
              TRUE              ~ "Défaite du joueur désavantagé"
            )
          )
        
      }
      
      # --------------------------------------------------------
      # LES DEUX
      # --------------------------------------------------------
      
      else {
        
        donnees <- donnees |>
          mutate(
            resultat = case_when(
              victoire_desavantage ~ "Victoire du joueur désavantagé",
              winner == "draw"      ~ "Partie nulle",
              TRUE                  ~ "Défaite du joueur désavantagé"
            )
          )
        
      }
      
      # --------------------------------------------------------
      # Proportions
      # --------------------------------------------------------
      
      donnees |>
        count(resultat) |>
        mutate(
          proportion = n / sum(n)
        )
      
    })
    
    
    # ==========================================================
    # 1. RESUME DES POURCENTAGES
    # ==========================================================
    
    output$resume_resultats <- renderUI({
      
      donnees <- resultats()
      
      if (nrow(donnees) == 0) {
        
        return(
          div(
            style = "
              text-align: center;
              margin: 20px;
              color: #777777;
            ",
            "Aucune partie ne correspond aux critères sélectionnés."
          )
        )
        
      }
      
      victoire <- donnees |>
        filter(
          resultat == "Victoire du joueur désavantagé"
        ) |>
        pull(proportion)
      
      defaite <- donnees |>
        filter(
          resultat == "Défaite du joueur désavantagé"
        ) |>
        pull(proportion)
      
      nul <- donnees |>
        filter(
          resultat == "Partie nulle"
        ) |>
        pull(proportion)
      
      if (length(victoire) == 0) victoire <- 0
      if (length(defaite) == 0) defaite <- 0
      if (length(nul) == 0) nul <- 0
      
      if (input$couleur_desavantage == "white") {
        
        victoire_label <- "Victoire des Blancs"
        defaite_label <- "Victoire des Noirs"
        
        couleur_victoire <- "#F5EFE6"
        couleur_defaite <- "#3A3A3A"
        
      } else if (input$couleur_desavantage == "black") {
        
        victoire_label <- "Victoire des Noirs"
        defaite_label <- "Victoire des Blancs"
        
        couleur_victoire <- "#3A3A3A"
        couleur_defaite <- "#F5EFE6"
        
      } else {
        
        victoire_label <- "Victoire du désavantagé"
        defaite_label <- "Défaite du désavantagé"
        
        couleur_victoire <- "#4CAF50"
        couleur_defaite <- "#C0392B"
        
      }
      
      div(
        
        style = "
          display: flex;
          justify-content: center;
          align-items: center;
          gap: 70px;
          margin-top: 15px;
          margin-bottom: 20px;
        ",
        
        # VICTOIRE
        div(
          style = "
            text-align: center;
            min-width: 150px;
          ",
          
          div(
            style = paste0(
              "
              font-size: 30px;
              font-weight: bold;
              color: ",
              couleur_victoire,
              ";
              "
            ),
            
            paste0(
              round(victoire * 100, 1),
              "%"
            )
          ),
          
          div(
            style = "
              font-size: 14px;
              color: #555555;
              margin-top: 3px;
            ",
            victoire_label
          )
        ),
        
        # DEFAITE
        div(
          style = "
            text-align: center;
            min-width: 150px;
          ",
          
          div(
            style = paste0(
              "
              font-size: 30px;
              font-weight: bold;
              color: ",
              couleur_defaite,
              ";
              "
            ),
            
            paste0(
              round(defaite * 100, 1),
              "%"
            )
          ),
          
          div(
            style = "
              font-size: 14px;
              color: #555555;
              margin-top: 3px;
            ",
            defaite_label
          )
        ),
        
        # NULLE
        div(
          style = "
            text-align: center;
            min-width: 150px;
          ",
          
          div(
            style = "
              font-size: 30px;
              font-weight: bold;
              color: #888888;
            ",
            
            paste0(
              round(nul * 100, 1),
              "%"
            )
          ),
          
          div(
            style = "
              font-size: 14px;
              color: #555555;
              margin-top: 3px;
            ",
            "Parties nulles"
          )
        )
        
      )
      
    })
    
    
    # ==========================================================
    # 2. GRAPHIQUE DES 100 CASES
    # ==========================================================
    
    output$graphique_resultats <- renderPlot({
      
      donnees <- resultats()
      
      if (nrow(donnees) == 0) {
        
        plot.new()
        
        text(
          0.5,
          0.5,
          "Aucune partie ne correspond aux critères sélectionnés.",
          cex = 1.3
        )
        
        return()
      }
      
      categories <- c(
        "Victoire du joueur désavantagé",
        "Défaite du joueur désavantagé",
        "Partie nulle"
      )
      
      donnees <- donnees |>
        complete(
          resultat = categories,
          fill = list(
            n = 0,
            proportion = 0
          )
        )
      
      donnees$cases <- floor(
        donnees$proportion * 100
      )
      
      reste <- 100 - sum(donnees$cases)
      
      decimales <- (
        donnees$proportion * 100
      ) - donnees$cases
      
      if (reste > 0) {
        
        ordre <- order(
          decimales,
          decreasing = TRUE
        )
        
        donnees$cases[
          ordre[seq_len(reste)]
        ] <-
          donnees$cases[
            ordre[seq_len(reste)]
          ] + 1
      }
      
      cases <- donnees |>
        slice(
          rep(
            seq_len(nrow(donnees)),
            donnees$cases
          )
        ) |>
        mutate(
          id = row_number()
        )
      
      cases$colonne <- (
        (cases$id - 1) %% 10
      ) + 1
      
      cases$ligne <- 10 - floor(
        (cases$id - 1) / 10
      )
      
      if (input$couleur_desavantage == "white") {
        
        couleurs <- c(
          "Victoire du joueur désavantagé" = "#F5EFE6",
          "Défaite du joueur désavantagé"  = "#3A3A3A",
          "Partie nulle"                   = "#AAA39A"
        )
        
        labels <- c(
          "Victoire du joueur désavantagé" = "Victoire des Blancs",
          "Défaite du joueur désavantagé"  = "Victoire des Noirs",
          "Partie nulle"                   = "Partie nulle"
        )
        
      } else if (input$couleur_desavantage == "black") {
        
        couleurs <- c(
          "Victoire du joueur désavantagé" = "#3A3A3A",
          "Défaite du joueur désavantagé"  = "#F5EFE6",
          "Partie nulle"                   = "#AAA39A"
        )
        
        labels <- c(
          "Victoire du joueur désavantagé" = "Victoire des Noirs",
          "Défaite du joueur désavantagé"  = "Victoire des Blancs",
          "Partie nulle"                   = "Partie nulle"
        )
        
      } else {
        
        couleurs <- c(
          "Victoire du joueur désavantagé" = "#4CAF50",
          "Défaite du joueur désavantagé"  = "#C0392B",
          "Partie nulle"                   = "#AAA39A"
        )
        
        labels <- c(
          "Victoire du joueur désavantagé" = "Victoire du joueur désavantagé",
          "Défaite du joueur désavantagé"  = "Défaite du joueur désavantagé",
          "Partie nulle"                   = "Partie nulle"
        )
      }
      
      titre <- if (
        input$couleur_desavantage == "black"
      ) {
        
        "Résultats lorsque les Noirs sont désavantagés"
        
      } else if (
        input$couleur_desavantage == "white"
      ) {
        
        "Résultats lorsque les Blancs sont désavantagés"
        
      } else {
        
        "Résultats lorsque le joueur est désavantagé"
      }
      
      ggplot(
        cases,
        aes(
          x = colonne,
          y = ligne,
          fill = resultat
        )
      ) +
        
        geom_tile(
          width = 0.9,
          height = 0.9,
          color = "#FFFFFF",
          linewidth = 0.8
        ) +
        
        scale_fill_manual(
          values = couleurs,
          breaks = categories,
          labels = labels,
          name = NULL
        ) +
        
        coord_fixed() +
        
        scale_x_continuous(
          expand = c(0, 0)
        ) +
        
        scale_y_continuous(
          expand = c(0, 0)
        ) +
        
        labs(
          title = titre,
          subtitle = paste0(
            "Parties avec une différence d'Elo maximale de ",
            input$diff_elo_max,
            " points"
          )
        ) +
        
        theme_void(
          base_size = 14
        ) +
        
        theme(
          plot.title = element_text(
            size = 20,
            face = "bold",
            hjust = 0.5,
            margin = margin(b = 5)
          ),
          
          plot.subtitle = element_text(
            size = 13,
            color = "#666666",
            hjust = 0.5,
            margin = margin(b = 20)
          ),
          
          legend.position = "bottom",
          
          legend.text = element_text(
            size = 11
          ),
          
          plot.margin = margin(
            20,
            20,
            10,
            20
          )
        )
      
    })
    
  })
}