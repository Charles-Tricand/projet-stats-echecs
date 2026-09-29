mod_diff_elo_max_ui = function(id){
  tabPanel(
    "Résultat des parties",
    
    h2(
      "Répartition des résultats selon la différence d'Elo"
    ),
    
    sidebarLayout(
      
      sidebarPanel(
        
        sliderInput(
          inputId = "diff_elo_max",
          label = "Différence maximale d'Elo :",
          min = 0,
          max = 1000,
          value = 400,
          step = 50
        )
        
      ),
      
      mainPanel(
        
        plotOutput(
          "graphique_resultats",
          height = "600px"
        )
      )
    )
  )
}

mod_diff_elo_max_server = function(id){
  moduleServer(id, function(input, output, session) {
    output$graphique_resultats <- renderPlot({
      
      # ==========================================================
      # PARTIES FILTREES
      # ==========================================================
      
      parties_filtrees <- reactive({
        Donnees_Chess[Donnees_Chess$elo_diff_abs <= input$diff_elo_max,]
        
      })
      
      
      # ==========================================================
      # REPARTITION DES RESULTATS
      # ==========================================================
      
      resultats <- reactive({
        donnees <- parties_filtrees()
        donnees |>
          count(winner) |>
          mutate(proportion = n / sum(n))
      })
      
      donnees <- resultats()
      
      # --------------------------------------------------------
      # On crée les 100 cases
      # --------------------------------------------------------
      
      donnees$cases <- floor(
        donnees$proportion * 100
      )
      
      # Nombre de cases restantes après arrondi
      reste <- 100 - sum(donnees$cases)
      
      # On distribue les cases restantes aux catégories
      # ayant les plus grandes décimales
      decimales <- donnees$proportion * 100 - donnees$cases
      
      if (reste > 0) {
        ordre <- order(
          decimales,
          decreasing = TRUE)
        
        donnees$cases[ordre[1:reste]] <-
          donnees$cases[ordre[1:reste]] + 1
        
      }
      
      # --------------------------------------------------------
      # Création des 100 cases
      # --------------------------------------------------------
      
      cases = donnees |>
        slice(rep(seq_len(nrow(donnees)),donnees$cases)) |>
        mutate(id = row_number())
      
      # --------------------------------------------------------
      # Position dans une grille 10 x 10
      # --------------------------------------------------------
      
      cases$colonne = ((cases$id - 1) %% 10) + 1
      cases$ligne <- 10 - floor((cases$id - 1) / 10)
      
      # --------------------------------------------------------
      # Couleurs
      # --------------------------------------------------------
      
      couleurs <- c(
        "white" = "#F5EFE6",
        "black" = "#6B4F3A",
        "draw"  = "#AAA39A"
      )
      
      # --------------------------------------------------------
      # Graphique
      # --------------------------------------------------------
      
      ggplot(cases,aes(x = colonne, y = ligne, fill = winner)) +
        
        geom_tile(
          width = 0.9,
          height = 0.9,
          color = "#D8C9B5",
          linewidth = 0.5)+
        
        scale_fill_manual(
          values = couleurs,
          labels = c(
            "white" = "Victoire des Blancs",
            "black" = "Victoire des Noirs",
            "draw"  = "Partie nulle"),
          name = "Résultat")+
        
        coord_fixed()+
        scale_x_continuous( expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))+
        theme_void()+
        theme(legend.position = "bottom")
      
    })
  })
}
  