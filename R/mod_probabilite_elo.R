# ==========================================================
# MODULE : PROBABILITÉ DE VICTOIRE SELON LA DIFFÉRENCE D'ELO
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
      "Comparaison entre la probabilité théorique de victoire du joueur
      le mieux classé et les résultats observés dans les parties du jeu de données."
    ),
    
    sidebarLayout(
      
      sidebarPanel(
        
        sliderInput(
          
          ns("taille_classe"),
          
          label = "Taille des classes de différence d'Elo :",
          
          min = 25,
          
          max = 200,
          
          value = 50,
          
          step = 25
        ),
        
        hr(),
        
        helpText(
          "Les parties nulles ne sont pas prises en compte dans le calcul
          de la probabilité de victoire."
        ),
        
        helpText(
          "La courbe théorique correspond au modèle Elo classique."
        )
        
      ),
      
      mainPanel(
        
        plotOutput(
          
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
            # Différence absolue d'Elo
            # ----------------------------------------------
            
            elo_diff_abs = abs(
              white_rating - black_rating
            ),
            
            
            # ----------------------------------------------
            # Résultat relatif au meilleur joueur
            # ----------------------------------------------
            
            meilleur_gagne = case_when(
              
              # Blanc mieux classé et Blanc gagne
              white_rating > black_rating &
                winner == "white" ~ TRUE,
              
              # Noir mieux classé et Noir gagne
              black_rating > white_rating &
                winner == "black" ~ TRUE,
              
              # Blanc mieux classé et Noir gagne
              white_rating > black_rating &
                winner == "black" ~ FALSE,
              
              # Noir mieux classé et Blanc gagne
              black_rating > white_rating &
                winner == "white" ~ FALSE,
              
              # Égalité Elo ou partie nulle
              TRUE ~ NA
            )
          ) %>%
          
          filter(
            
            !is.na(elo_diff_abs),
            
            elo_diff_abs > 0
          )
      })
      
      
      # ====================================================
      # DONNÉES EMPIRIQUES
      # ====================================================
      
      donnees_empiriques <- reactive({
        
        data <- donnees()
        
        taille <- input$taille_classe
        
        
        # --------------------------------------------------
        # Création de la classe directement à partir de
        # la différence d'Elo
        # --------------------------------------------------
        
        data <- data %>%
          
          mutate(
            
            borne_inferieure =
              floor(
                elo_diff_abs / taille
              ) * taille
            
          )
        
        
        # --------------------------------------------------
        # Résumé par classe
        # --------------------------------------------------
        
        data %>%
          
          group_by(
            borne_inferieure
          ) %>%
          
          summarise(
            
            # Nombre total de parties
            nombre_parties = n(),
            
            # Nombre de parties décisives
            parties_decisives = sum(
              !is.na(meilleur_gagne)
            ),
            
            # Nombre de victoires du meilleur Elo
            victoires_meilleur = sum(
              meilleur_gagne == TRUE,
              na.rm = TRUE
            ),
            
            # Nombre de nulles
            nulles = sum(
              winner == "draw",
              na.rm = TRUE
            ),
            
            .groups = "drop"
          ) %>%
          
          mutate(
            
            # ------------------------------------------------
            # Probabilité empirique
            # ------------------------------------------------
            
            probabilite =
              victoires_meilleur /
              parties_decisives,
            
            
            # ------------------------------------------------
            # Proportion de nulles
            # ------------------------------------------------
            
            proportion_nulle =
              nulles /
              nombre_parties,
            
            
            # ------------------------------------------------
            # Centre de la classe
            # ------------------------------------------------
            
            centre =
              borne_inferieure +
              taille / 2
          ) %>%
          
          filter(
            parties_decisives > 0,
            !is.na(probabilite)
          )
      })
      
      
      # ====================================================
      # COURBE THÉORIQUE ELO
      # ====================================================
      
      courbe_theorique <- reactive({
        
        data <- donnees()
        
        max_diff <- max(
          data$elo_diff_abs,
          na.rm = TRUE
        )
        
        tibble(
          
          difference_elo = seq(
            0,
            max_diff,
            by = 1
          ),
          
          probabilite =
            1 /
            (
              1 +
                10 ^ (
                  -difference_elo / 400
                )
            )
        )
      })
      
      
      # ====================================================
      # GRAPHIQUE
      # ====================================================
      
      output$graphique_probabilite <- renderPlot({
        
        empirique <- donnees_empiriques()
        
        theorique <- courbe_theorique()
        
        
        # --------------------------------------------------
        # Graphique
        # --------------------------------------------------
        
        ggplot() +
          
          # ================================================
        # COURBE THÉORIQUE
        # ================================================
        
        geom_line(
          
          data = theorique,
          
          aes(
            x = difference_elo,
            y = probabilite,
            color = "Modèle Elo théorique"
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
            y = probabilite,
            color = "Données observées"
          ),
          
          linewidth = 1.3
        ) +
          
          
          # ================================================
        # POINTS EMPIRIQUES
        # ================================================
        
        geom_point(
          
          data = empirique,
          
          aes(
            x = centre,
            y = probabilite,
            color = "Données observées",
            
            size = parties_decisives
          ),
          
          alpha = 0.9
        ) +
          
          
          # ================================================
        # AXE X
        # ================================================
        
        scale_x_continuous(
          
          limits = c(
            0,
            max(
              theorique$difference_elo
            )
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
          
          limits = c(
            0.45,
            1
          ),
          
          breaks = seq(
            0.5,
            1,
            by = 0.1
          )
        ) +
          
          
          # ================================================
        # COULEURS
        # ================================================
        
        scale_color_manual(
          
          values = c(
            
            "Modèle Elo théorique" = "#888888",
            
            "Données observées" = "#C0392B"
          ),
          
          name = NULL
        ) +
          
          
          # ================================================
        # TAILLE DES POINTS
        # ================================================
        
        scale_size_continuous(
          
          range = c(
            2,
            7
          ),
          
          name = "Parties décisives",
          
          labels = scales::label_number(
            big.mark = " "
          )
        ) +
          
          
          # ================================================
        # TITRES
        # ================================================
        
        labs(
          
          title =
            "Probabilité de victoire selon la différence d'Elo",
          
          subtitle =
            "Le joueur le mieux classé est-il aussi souvent favori que le prédit le modèle Elo ?",
          
          x =
            "Différence d'Elo entre les joueurs",
          
          y =
            "Probabilité de victoire du joueur le mieux classé"
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
                size = 21,
                face = "bold",
                hjust = 0.5
              ),
            
            plot.subtitle =
              element_text(
                size = 13,
                color = "#666666",
                hjust = 0.5,
                margin = margin(
                  b = 20
                )
              ),
            
            axis.title =
              element_text(
                face = "bold"
              ),
            
            legend.position =
              "bottom",
            
            legend.text =
              element_text(
                size = 11
              )
          )
        
      })
      
    }
  )
}