# ==========================================================
# MODULE : COMPARAISON DES SCORES ATTENDUS ELO / GLICKO-2
# ==========================================================


# ==========================================================
# 1. INTERFACE UTILISATEUR
# ==========================================================

mod_probabilite_elo_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    
    "Probabilité de victoire",
    
    h2(
      "L'Elo prédit-il réellement le résultat ?"
    ),
    
    p(
      "texteeeeeeeeeeeeeeeeeeeeeeeee"
    ),
    
    sidebarLayout(
      
      sidebarPanel(
        
        # --------------------------------------------------
        # CHOIX DU MODÈLE THÉORIQUE
        # --------------------------------------------------
        
        radioButtons(
          
          inputId = ns("modele"),
          
          label = "Système de classement à afficher :",
          
          choices = c(
            "Elo classique" = "elo",
            "Glicko-2" = "glicko2",
            "Les deux" = "both"
          ),
          
          selected = "both"
        ),
        
        # --------------------------------------------------
        # PARAMÈTRE GLICKO-2
        # --------------------------------------------------
        
        # conditionalPanel(
        #   
        #   condition = "input.modele == 'glicko2' || input.modele == 'both'",
        #   
        #   sliderInput(
        #     
        #     inputId = ns("rd"),
        #     
        #     label = "Incertitude du classement (RD) :",
        #     
        #     min = 30,
        #     max = 350,
        #     value = 100,
        #     step = 10
        #   ),
        #   
        #   helpText(
        #     "Le RD est exprimé en points de classement. ",
        #     "Il s'agit d'une hypothèse commune aux joueurs, ",
        #     "et non de leur RD historique réel."
        #   ),
        #   ns = ns
        # ),
        # 
        # hr(),
        
        # --------------------------------------------------
        # TAILLE DES CLASSES
        # --------------------------------------------------
        
        sliderInput(
          
          inputId = ns("taille_classe"),
          
          label = "Taille des classes de différence d'Elo :",
          
          min = 25,
          max = 200,
          value = 125,
          step = 25
        ),
      ),
      
      mainPanel(
        
        plotlyOutput(
          ns("graphique_probabilite"),
          height = "650px"
        )
        
      )
    )
  )
}


# ==========================================================
# 2. SERVEUR
# ==========================================================

mod_probabilite_elo_server <- function(id) {
  
  moduleServer(
    
    id,
    
    function(input, output, session) {
      
      
      # ====================================================
      # DONNÉES DE BASE
      # ====================================================
      
      donnees <- reactive({
        
        Donnees_Chess %>%
          
          mutate(
            
            # ----------------------------------------------
            # Différence absolue de classement
            # ----------------------------------------------
            
            elo_diff_abs = abs(
              white_rating - black_rating
            ),
            
            # ----------------------------------------------
            # Score obtenu par le joueur le mieux classé
            # ----------------------------------------------
            
            score_meilleur = case_when(
              
              # Partie nulle
              winner == "draw" ~ 0.5,
              
              # Le joueur blanc est mieux classé
              white_rating > black_rating &
                winner == "white" ~ 1,
              
              white_rating > black_rating &
                winner == "black" ~ 0,
              
              # Le joueur noir est mieux classé
              black_rating > white_rating &
                winner == "black" ~ 1,
              
              black_rating > white_rating &
                winner == "white" ~ 0,
              
              # Résultat manquant ou non reconnu
              TRUE ~ NA_real_
            )
          ) %>%
          
          filter(
            
            !is.na(elo_diff_abs),
            
            elo_diff_abs > 0,
            
            !is.na(score_meilleur)
          )
      })
      
      
      # ====================================================
      # 3. DONNÉES EMPIRIQUES
      # ====================================================
      
      donnees_empiriques <- reactive({
        
        data <- donnees()
        
        taille <- input$taille_classe
        
        validate(
          need(
            nrow(data) > 0,
            "Aucune partie exploitable dans les données."
          )
        )
        
        data %>%
          mutate(
            borne_inferieure =
              floor(elo_diff_abs / taille) * taille
          ) %>%
          group_by(borne_inferieure) %>%
          summarise(
            nombre_parties = n(),
            
            score_observe = mean(
              score_meilleur,
              na.rm = TRUE
            ),
            
            .groups = "drop"
          ) %>%
          mutate(
            centre = borne_inferieure + taille / 2,
            
            texte_survol = paste0(
              "<b>Différence de classement</b> : ",
              borne_inferieure, "–",
              borne_inferieure + taille, " points",
              
                                  # "<br><b>Score observé</b> : ",
                                  # scales::percent(
                                  #   score_observe,
                                  #   accuracy = 0.1
                                  # ),
              
              "<br><b>Nombre de parties (n)</b> : ",
              format(
                nombre_parties,
                big.mark = " ",
                scientific = FALSE
              )
            )
          )
      })
      
      
      # ====================================================
      # 4. COURBES THÉORIQUES
      # ====================================================
      
      courbes_theoriques <- reactive({
        
        data <- donnees()
        
        max_diff <- max(
          data$elo_diff_abs,
          na.rm = TRUE
        )
        
        difference <- seq(
          0,
          max_diff,
          by = 1
        )
        
        # --------------------------------------------------
        # ELO CLASSIQUE
        # --------------------------------------------------
        
        courbe_elo <- tibble(
          
          difference_elo = difference,
          
          score_attendu =
            1 / (
              1 + 10 ^ (-difference / 400)
            ),
          
          modele = "Elo classique"
        )
        
        
        # --------------------------------------------------
        # GLICKO-2
        # --------------------------------------------------
        
        # Conversion de l'échelle Elo vers l'échelle Glicko-2
        echelle <- 173.7178
        
        # Différence de classement normalisée
        delta_mu <- difference / echelle
        
        # RD commun supposé pour les deux joueurs
        
        rd_fixe <- 300
        
        phi <- rd_fixe / echelle
        
        # phi <- input$rd / echelle
        
        # Fonction g(phi) de Glicko-2
        facteur_g <- 1 / sqrt(
          1 + 3 * phi^2 / pi^2
        )
        
        courbe_glicko2 <- tibble(
          
          difference_elo = difference,
          
          score_attendu =
            1 / (
              1 + exp(-facteur_g * delta_mu)
            ),
          
          modele = "Glicko-2"
        )
        
        
        # --------------------------------------------------
        # ASSEMBLAGE DES COURBES
        # --------------------------------------------------
        
        bind_rows(
          courbe_elo,
          courbe_glicko2
        )
      })
      
      
      # ====================================================
      # 5. SÉLECTION DES COURBES
      # ====================================================
      
      courbes_a_afficher <- reactive({
        
        courbes <- courbes_theoriques()
        
        if (input$modele == "elo") {
          
          courbes %>%
            filter(
              modele == "Elo classique"
            )
          
        } else if (input$modele == "glicko2") {
          
          courbes %>%
            filter(
              modele == "Glicko-2"
            )
          
        } else {
          
          courbes
          
        }
      })
      
      
      # ====================================================
      # 6. GRAPHIQUE
      # ====================================================
      
      output$graphique_probabilite <- renderPlotly({
        
        empirique <- donnees_empiriques()
        
        theorique <- courbes_a_afficher()
        
        # --------------------------------------------------
        # CONSTRUCTION DU GRAPHIQUE GGPLOT
        # --------------------------------------------------
        
        graphique <- ggplot() +
          
          # ================================================
        # COURBES THÉORIQUES
        # ================================================
        
        geom_line(
          data = theorique,
          
          aes(
            x = difference_elo,
            y = score_attendu,
            color = modele
          ),
          
          linewidth = 1.4
        ) +
          
          # ================================================
        # COURBE EMPIRIQUE
        # ================================================
        
        geom_line(
          data = empirique,
          
          aes(
            x = centre,
            y = score_observe,
            color = "Données observées"
          ),
          
          linewidth = 1.2
        ) +
          
          # ================================================
        # POINTS EMPIRIQUES
        # ================================================
        
        geom_point(
          data = empirique,
          
          aes(
            x = centre,
            y = score_observe,
            size = nombre_parties,
            text = texte_survol
          ),
          
          color = "#C0392B",
          
          alpha = 0.9
        ) +
          
          # ================================================
        # AXE X
        # ================================================
        
        scale_x_continuous(
          
          limits = c(
            0,
            max(courbes_theoriques()$difference_elo)
          ),
          
          expand = expansion(
            mult = c(0, 0.02)
          )
        ) +
          
          # ================================================
        # AXE Y
        # ================================================
        
        scale_y_continuous(
          
          labels = scales::label_percent(
            accuracy = 1
          ),
          
          limits = c(0, 1),
          
          breaks = seq(
            0,
            1,
            by = 0.1
          )
        ) +
          
          # ================================================
        # COULEURS
        # ================================================
        
        scale_color_manual(
          
          values = c(
            "Elo classique" = "#888888",
            "Glicko-2" = "#2471A3",
            "Données observées" = "#C0392B"
          ),
          
          name = "Modèle"
        ) +
          
          # ================================================
        # TAILLE DES POINTS
        # ================================================
        
        scale_size_continuous(
          
          range = c(2, 7),
          
          name = "Nombre de parties",
          
          labels = scales::label_number(
            big.mark = " "
          )
        ) +
          
          # Suppression de la légende des tailles
          guides(
            size = "none"
          ) +
          
          # ================================================
        # TITRES
        # ================================================
        
        labs(
          
          title =
            "Score attendu selon la différence de classement",
          
          subtitle =
            paste(
              "Comparaison des modèles théoriques",
              "et des résultats observés sur Lichess"
            ),
          
          x =
            "Différence de classement entre les joueurs",
          
          y =
            "Score moyen du joueur le mieux classé"
        ) +
          
          # ================================================
        # THÈME
        # ================================================
        
        theme_minimal(
          base_size = 14
        ) +
          
          theme(
            
            panel.grid.minor =
              element_blank(),
            
            panel.grid.major.x =
              element_line(
                color = "#E5E5E5"
              ),
            
            panel.grid.major.y =
              element_line(
                color = "#E5E5E5"
              ),
            
            plot.title =
              element_text(
                size = 19,
                face = "bold",
                hjust = 0.5
              ),
            
            plot.subtitle =
              element_text(
                size = 12,
                color = "#666666",
                hjust = 0.5,
                margin = margin(b = 20)
              ),
            
            axis.title =
              element_text(
                face = "bold"
              ),
            
            legend.position =
              "bottom"
          )
        
        # --------------------------------------------------
        # CONVERSION EN GRAPHIQUE PLOTLY
        # --------------------------------------------------
        
        ggplotly(
          graphique,
          tooltip = "text"
        )
        
      })
    }
  )
}