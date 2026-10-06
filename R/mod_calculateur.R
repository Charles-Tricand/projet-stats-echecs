# ==========================================================
# MODULE : CALCULATEUR DE RESULTAT
# ==========================================================


# ==========================================================
# 1. INTERFACE UTILISATEUR
# ==========================================================

mod_calculateur_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Calculateur",
    h2("Calculateur de résultat"),
    p(
      "Estimez les probabilités de victoire, de partie nulle ",
      "et de défaite à partir de la couleur, de l'Elo des joueurs ",
      "et de l'ouverture."
    ),
    
    sidebarLayout(
      
      # ======================================================
      # PANNEAU DE CONTROLE
      # ======================================================
      
      sidebarPanel(
        h4("Votre partie"),
        
        # ----------------------------------------------------
        # Couleur
        # ----------------------------------------------------
        
        radioButtons(
          ns("couleur"),
          label = "Votre couleur :",
          choices = c(
            "Blancs" = "white",
            "Noirs" = "black"),
          selected = "white"),
        
        # ----------------------------------------------------
        # Elo du joueur
        # ----------------------------------------------------
        
        numericInput(
          ns("elo_joueur"),
          label = "Votre Elo :",
          value = 1500,
          min = 500,
          max = 3000,
          step = 1),
        
        # ----------------------------------------------------
        # Elo de l'adversaire
        # ----------------------------------------------------
        
        numericInput(
          ns("elo_adversaire"),
          label = "Elo de l'adversaire :",
          value = 1500,
          min = 500,
          max = 3000,
          step = 1),
        
        # ----------------------------------------------------
        # Ouverture
        # ----------------------------------------------------
        
        selectInput(
          ns("opening_eco"),
          label = "Code ECO de l'ouverture :",
          choices = NULL,
          selected = NULL),
        
        br(),
        
        # ----------------------------------------------------
        # Bouton de calcul
        # ----------------------------------------------------
        
        actionButton(
          ns("calculer"),
          label = "Calculer",
          class = "btn-primary",
          width = "100%")
        ),
      
      
      # ======================================================
      # RESULTATS
      # ======================================================
      
      mainPanel(
        h3("Résultat estimé"),
        br(),
        
        uiOutput(ns("resume")),
        br(),
        
        plotOutput(ns("graphique_probabilites"), height = "400px"))
    )
  )
}


# ==========================================================
# 2. SERVEUR
# ==========================================================

mod_calculateur_server <- function(id) {
  moduleServer(id, function(input, output, session) {
      
      # ======================================================
      # LISTE DES ECO DISPONIBLES
      # ======================================================
      
      observe({
        ecos <- levels(
          Donnees_modele$opening_eco_modele
        )
        
        updateSelectInput(
          session,
          "opening_eco",
          choices = ecos,
          selected = if ("C50" %in% ecos) {"C50"} else {
            ecos[1]
          }
        )
        
      })
      
      
      # ======================================================
      # CALCUL
      # ======================================================
      
      prediction <- eventReactive(
        
        input$calculer,
        
        {
          req(
            input$couleur,
            input$elo_joueur,
            input$elo_adversaire,
            input$opening_eco
          )
          
          
          # --------------------------------------------------
          # Appel du modèle
          # --------------------------------------------------
          
          resultat <- predire_resultat(
            
            couleur = input$couleur,
            
            elo_joueur = input$elo_joueur,
            
            elo_adversaire = input$elo_adversaire,
            
            opening_eco = input$opening_eco
          )
          
          
          # --------------------------------------------------
          # Mise en forme
          # --------------------------------------------------
          
          resultat
          
        }
      )
      
      
      # ======================================================
      # RESUME DES RESULTATS
      # ======================================================
      
      output$resume <- renderUI({
        
        resultat <- prediction()
        
        req(resultat)
        
        
        div(
          
          style = "
            display: flex;
            justify-content: center;
            align-items: center;
            gap: 50px;
            margin: 20px 0 30px 0;
          ",
          
          
          # --------------------------------------------------
          # VICTOIRE
          # --------------------------------------------------
          
          div(
            
            style = "
              text-align: center;
              min-width: 130px;
            ",
            
            div(
              
              style = "
                font-size: 36px;
                font-weight: bold;
                color: #4CAF50;
              ",
              
              paste0(
                round(
                  resultat$victoire * 100,
                  1
                ),
                "%"
              )
            ),
            
            div(
              
              style = "
                font-size: 15px;
                color: #555555;
              ",
              
              "Victoire"
            )
          ),
          
          
          # --------------------------------------------------
          # NULLE
          # --------------------------------------------------
          
          div(
            
            style = "
              text-align: center;
              min-width: 130px;
            ",
            
            div(
              
              style = "
                font-size: 36px;
                font-weight: bold;
                color: #888888;
              ",
              
              paste0(
                round(
                  resultat$nulle * 100,
                  1
                ),
                "%"
              )
            ),
            
            div(
              
              style = "
                font-size: 15px;
                color: #555555;
              ",
              
              "Partie nulle"
            )
          ),
          
          
          # --------------------------------------------------
          # DEFAITE
          # --------------------------------------------------
          
          div(
            
            style = "
              text-align: center;
              min-width: 130px;
            ",
            
            div(
              
              style = "
                font-size: 36px;
                font-weight: bold;
                color: #C0392B;
              ",
              
              paste0(
                round(
                  resultat$defaite * 100,
                  1
                ),
                "%"
              )
            ),
            
            div(
              
              style = "
                font-size: 15px;
                color: #555555;
              ",
              
              "Défaite"
            )
          )
          
        )
        
      })
      
      
      # ======================================================
      # GRAPHIQUE
      # ======================================================
      
      output$graphique_probabilites <- renderPlot({
        
        resultat <- prediction()
        
        req(resultat)
        
        donnees_graphique <- data.frame(
          resultat = factor(
            c("Victoire","Partie nulle","Défaite"),
            levels = c("Victoire","Partie nulle","Défaite")),
          probabilite = c(resultat$victoire, resultat$nulle, resultat$defaite))
        
        ggplot(
          donnees_graphique,
          aes(
            x = resultat,
            y = probabilite,
            fill = resultat)) +
          geom_col(width = 0.6) +
          geom_text(
            aes(label = paste0(round(probabilite * 100, 1),"%")),
            vjust = -0.4,
            size = 5) +
          scale_fill_manual(
            values = c(
              "Victoire" = "#4CAF50",
              "Partie nulle" = "#888888",
              "Défaite" = "#C0392B")) +
          
          scale_y_continuous(
            limits = c(0, max(1, max(donnees_graphique$probabilite) * 1.15)),
            labels = function(x) {paste0(x * 100,"%")}) +
          labs(
            x = NULL,
            y = "Probabilité",
            title = "Probabilité estimée de chaque résultat") +
          theme_minimal(base_size = 14) +
          theme(
            legend.position = "none",
            plot.title = element_text(
              face = "bold",
              hjust = 0.5)
          )
        
      })
    }
  )
}