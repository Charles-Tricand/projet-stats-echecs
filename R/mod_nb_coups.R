mod_nb_coups_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    "Nombre de coups",
    h2("Nombre moyen de coups selon le niveau des joueurs"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = ns("variable_x"),
          label = "Variable étudiée :",
          choices = c(
            "Elo moyen de la partie" = "elo_moyen",
            "Différence d'Elo" = "elo_diff"
          ),
          selected = "elo_moyen"
        ),
        
        sliderInput(
          inputId = ns("diff_elo_max"),
          label = "Différence maximale d'Elo entre les joueurs :",
          min = 0,
          max = 1000,
          value = 400,
          step = 50
        )
      ),
      
      
      mainPanel(
        div(
          class = "graphique-entete",
          
          actionButton(
            inputId = ns("aide_graphique"),
            label = NULL,
            icon = icon("question-circle"),
            title = "Comprendre ce graphique",
            class = "btn-aide-graphique"
          )
        ),
        
        plotlyOutput(
          ns("graphique_temps_elo"),
          height = "600px"
        )
      )
    )
  )
}





mod_nb_coups_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    output$graphique_temps_elo <- renderPlotly({
      
      # ======================================================
      # 1. FILTRAGE DES PARTIES
      # ======================================================
      
      Donnees_filtrees <- Donnees_Chess |>
        filter(elo_diff_abs <= input$diff_elo_max)
      
      
      # ======================================================
      # 2. VERIFICATION
      # ======================================================
      
      if (nrow(Donnees_filtrees) == 0) {
        
        return(
          plotly_empty() |>
            layout(
              annotations = list(
                text = "Aucune partie ne correspond aux critères sélectionnés.",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE
              )
            )
        )
      }
      
      
      # ======================================================
      # 3. MOYENNE GENERALE
      # ======================================================
      
      moyenne_generale <- mean(
        Donnees_filtrees$turns,
        na.rm = TRUE
      )
      
      
      # ======================================================
      # 4. CHOIX DE LA VARIABLE
      # ======================================================
      
      if (input$variable_x == "elo_moyen") {
        
        
        # ----------------------------------------------------
        # MODE : ELO MOYEN
        # ----------------------------------------------------
        
        Donnees_graphique <- Donnees_filtrees |>
          mutate(elo_classe = cut(
            elo_moyen,
            breaks = seq(
              floor(min(elo_moyen,na.rm = TRUE) / 100) * 100, 
              ceiling(max(elo_moyen, na.rm = TRUE) / 100) * 100,
              by = 100),
            include.lowest = TRUE)) |>
          
          group_by(elo_classe) |>
          
          summarise(
            valeur_moyenne = mean(elo_moyen, na.rm = TRUE),
            coups_moyens = mean(turns,na.rm = TRUE),
            nombre_parties = n(),
            .groups = "drop") |>
          
          filter(!is.na(valeur_moyenne))
        titre <- "Nombre moyen de coups selon l'Elo moyen"
        sous_titre <- paste0(
          "Parties avec une différence d'Elo ≤ ",
          input$diff_elo_max,
          " points")
        nom_x <- "Elo moyen de la partie"
        } 
      else {
        
        
        # ====================================================
        # MODE : DIFFERENCE D'ELO
        # ====================================================
        
        Donnees_graphique <- Donnees_filtrees |>
          mutate(
            elo_diff_classe = cut(
              elo_diff_abs,
              breaks = seq(0,ceiling(max(elo_diff_abs,na.rm = TRUE) / 100) * 100,by = 100),
              include.lowest = TRUE)) |>
          group_by(elo_diff_classe) |>
          summarise(
            valeur_moyenne = mean(elo_diff_abs, na.rm = TRUE),
            coups_moyens = mean(turns, na.rm = TRUE),
            nombre_parties = n(),
            .groups = "drop") |>
          filter(!is.na(valeur_moyenne))

        titre <- "Nombre moyen de coups selon la différence d'Elo"
        sous_titre <- paste0("Parties avec une différence d'Elo ≤ ", input$diff_elo_max, " points")
        nom_x <- "Différence d'Elo"
      }
      
      
      # ======================================================
      # 5. INFORMATIONS POUR LE SURVOL
      # ======================================================
      
      nombre_total <- sum(
        Donnees_graphique$nombre_parties
      )
      
      
      Donnees_graphique <- Donnees_graphique |>
        mutate(
          proportion_parties =
            nombre_parties / nombre_total,
          texte_survol = paste0(
            "<b>", nom_x, "</b> : ", round(valeur_moyenne,0), "<br>",
            "<b>Nombre moyen de coups</b> : ", round(coups_moyens, 1), "<br>",
            "<b>Nombre de parties</b> : ", format(nombre_parties, big.mark = " "), "<br>",
            "<b>Part des parties</b> : ", round(proportion_parties * 100, 1), "%"
          )
        )
      
      
      # ======================================================
      # 6. GRAPHIQUE
      # ======================================================
      
      graphique <- ggplot(
        Donnees_graphique,
        aes(
          x = valeur_moyenne,
          y = coups_moyens
        )) +
        
      # ----------------------------------------------------
      # MOYENNE GENERALE
      # ----------------------------------------------------
      
      geom_hline(
        yintercept = moyenne_generale,
        color = "#95A5A6",
        linewidth = 0.9,
        linetype = "dashed") +
        
      # ----------------------------------------------------
      # COURBE
      # ----------------------------------------------------
      
      geom_line(
        color = "#2C3E50",
        linewidth = 1) +

      # ----------------------------------------------------
      # POINTS
      # ----------------------------------------------------
      
      geom_point(
        aes(
          size = nombre_parties,
          text = texte_survol),
        color = "#3498DB",
        alpha = 0.75) +
        
        
      # ----------------------------------------------------
      # TAILLE DES POINTS
      # ----------------------------------------------------
      
      scale_size_area(
        max_size = 10,
        name = "Nombre de parties") +
        
        
      # ----------------------------------------------------
      # ANNOTATION
      # ----------------------------------------------------
      
      annotate(
        "text",
        x = Inf,
        y = moyenne_generale,
        label = paste0("Moyenne générale : ", round(moyenne_generale,1), " coups"),
        hjust = 1.05,
        vjust = -0.7,
        color = "#7F8C8D",
        size = 4) +
        
      # ----------------------------------------------------
      # TITRES
      # ----------------------------------------------------
      
      labs(
        title = titre,
        subtitle = sous_titre,
        x = nom_x,
        y = "Nombre moyen de coups") +
        
      # ----------------------------------------------------
      # THEME
      # ----------------------------------------------------
      
      theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
      
      
      # ======================================================
      # 7. CONVERSION EN PLOTLY
      # ======================================================
      
      ggplotly(graphique, tooltip = "text") |>
        layout(
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = "#3498DB",
            font = list(
              color = "#2C3E50",
              size = 13)
          )
          
        )
      
    })
    
    # ======================================================
    # AIDE A L'INTERPRETATION DU GRAPHIQUE
    # ======================================================
    
    observeEvent(input$aide_graphique, {
      
      showModal(
        
        modalDialog(
          
          title = tagList(
            icon("book-open"),
            " Comprendre le nombre moyen de coups"
          ),
          
          tags$h4("Objectif du graphique"),
          
          p(
            "Ce graphique étudie la relation entre le niveau des joueurs ",
            "et le nombre moyen de coups joués au cours d'une partie."
          ),
          
          tags$h4("Comment lire le graphique ?"),
          
          tags$ul(
            tags$li(
              strong("Axe horizontal : "),
              "le niveau moyen des deux joueurs, ou la différence ",
              "absolue entre leurs classements Elo, selon la variable sélectionnée."
            ),
            
            tags$li(
              strong("Axe vertical : "),
              "le nombre moyen de coups joués dans les parties du groupe."
            ),
            
            tags$li(
              strong("Taille des points : "),
              "le nombre de parties représentées dans chaque groupe. ",
              "Un point plus grand correspond à davantage de parties."
            ),
            
            tags$li(
              strong("Ligne horizontale en pointillés : "),
              "le nombre moyen de coups calculé sur l'ensemble des parties ",
              "retenues après application du filtre."
            )
          ),
          
          tags$h4("Comment interpréter les résultats ?"),
          
          p(
            "Un point situé au-dessus de la ligne correspond à un groupe ",
            "dont les parties durent en moyenne plus longtemps, en nombre ",
            "de coups, que l'ensemble des parties filtrées. ",
            "Un point situé en dessous correspond à des parties plus courtes."
          ),
          
          p(
            "La relation observée décrit une association statistique : ",
            "elle ne permet pas, à elle seule, de conclure à une relation ",
            "de causalité."
          ),
          
          tags$h4("Attention au filtre"),
          
          p(
            "Le curseur limite la différence absolue d'Elo entre les joueurs. ",
            "Lorsque sa valeur change, les parties retenues et la moyenne ",
            "générale peuvent également changer."
          ),
          
          easyClose = TRUE,
          
          footer = modalButton("Fermer"),
          
          size = "m"
        )
      )
      
    })
    
  })
}