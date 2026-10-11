# ==========================================================
# 1. INTERFACE UTILISATEUR
# ==========================================================

mod_issue_parties_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    
    "Issue des parties",
    
    h2(
      "Comment les parties se terminent-elles ?"
    ),
    
    p(
      "Analyse de l'issue des parties selon la différence d'Elo entre les deux joueurs."
    ),
    
    sidebarLayout(
      
      sidebarPanel(
        
        sliderInput(
          
          ns("nombre_classes"),
          
          label = "Nombre de classes de différence d'Elo :",
          
          min = 4,
          
          max = 20,
          
          value = 10,
          
          step = 1
        ),
        
        hr(),
        
        helpText(
          "Les parties sont réparties automatiquement entre 0 et la différence d'Elo maximale observée."
        )
        
      ),
      
      mainPanel(
        
        plotlyOutput(
          
          ns("graphique_issue"),
          
          height = "650px"
        )
        
      )
    )
  )
}


# ==========================================================
# 2. SERVEUR
# ==========================================================

mod_issue_parties_server <- function(id) {
  
  moduleServer(
    
    id,
    
    function(input, output, session) {
      
      # ====================================================
      # DONNÉES
      # ====================================================
      
      donnees <- reactive({
        
        Donnees_Chess %>%
          
          mutate(
            
            issue = case_when(
              
              victory_status == "mate" ~
                "Échec et mat",
              
              victory_status == "resign" ~
                "Abandon",
              
              victory_status == "outoftime" ~
                "Temps écoulé",
              
              victory_status == "draw" ~
                "Nulle",
              
              TRUE ~ NA_character_
            )
          ) %>%
          
          filter(
            !is.na(elo_diff_abs),
            !is.na(issue)
          )
      })
      
      
      # ====================================================
      # CALCUL DES CLASSES
      # ====================================================
      
      donnees_classes <- reactive({
        
        data <- donnees()
        
        nombre_classes <- input$nombre_classes
        
        # --------------------------------------------------
        # Maximum de différence d'Elo
        # --------------------------------------------------
        
        max_diff <- max(
          data$elo_diff_abs,
          na.rm = TRUE
        )
        
        # --------------------------------------------------
        # Bornes
        # --------------------------------------------------
        
        bornes <- seq(
          0,
          max_diff,
          length.out = nombre_classes + 1
        )
        
        # On arrondit les bornes pour avoir
        # des valeurs plus lisibles
        
        bornes <- round(
          bornes
        )
        
        # Évite d'avoir deux bornes identiques
        # après l'arrondi
        
        bornes <- unique(bornes)
        
        # --------------------------------------------------
        # Création des classes
        # --------------------------------------------------
        
        data %>%
          
          mutate(
            
            classe_elo = cut(
              
              elo_diff_abs,
              
              breaks = bornes,
              
              right = FALSE,
              
              include.lowest = TRUE
            )
          )
      })
      
      
      # ====================================================
      # RÉSUMÉ
      # ====================================================
      
      resume <- reactive({
        
        data <- donnees_classes()
        
        # --------------------------------------------------
        # Nombre total de parties par classe
        # --------------------------------------------------
        
        totaux <- data %>%
          
          count(
            classe_elo,
            name = "total_parties"
          )
        
        # --------------------------------------------------
        # Nombre de parties par issue
        # --------------------------------------------------
        
        data %>%
          
          count(
            classe_elo,
            issue,
            name = "nombre_parties"
          ) %>%
          
          left_join(
            totaux,
            by = "classe_elo"
          ) %>%
          
          mutate(
            
            proportion =
              nombre_parties / total_parties
          ) %>%
          
          # ------------------------------------------------
        # On s'assure que toutes les issues existent
        # dans chaque classe
        # ------------------------------------------------
        
        complete(
          
          classe_elo,
          
          issue = c(
            "Échec et mat",
            "Abandon",
            "Temps écoulé",
            "Nulle"
          ),
          
          fill = list(
            nombre_parties = 0,
            proportion = 0
          )
        ) %>%
          
          # Recalcul après complete()
          
          group_by(
            classe_elo
          ) %>%
          
          mutate(
            
            total_parties =
              sum(nombre_parties),
            
            proportion =
              nombre_parties / total_parties
          ) %>%
          
          ungroup()
      })
      
      
      # ====================================================
      # HEATMAP
      # ====================================================
      
      output$graphique_issue <- renderPlotly({
        
        data <- resume()
        
        if (nrow(data) == 0) {
          
          return(
            plot_ly() %>%
              layout(
                title = "Aucune donnée disponible"
              )
          )
        }
        
        # --------------------------------------------------
        # Ordre des issues
        # --------------------------------------------------
        
        data <- data %>%
          
          mutate(
            
            issue = factor(
              
              issue,
              
              levels = c(
                "Échec et mat",
                "Abandon",
                "Temps écoulé",
                "Nulle"
              )
            )
          )
        
        
        # --------------------------------------------------
        # Pourcentage affiché
        # --------------------------------------------------
        
        data <- data %>%
          
          mutate(
            
            pourcentage =
              paste0(
                round(proportion * 100, 1),
                "%"
              )
          )
        
        
        # --------------------------------------------------
        # Texte du tooltip
        # --------------------------------------------------
        
        data <- data %>%
          
          mutate(
            
            tooltip = paste0(
              
              "<b>Différence d'Elo :</b> ",
              classe_elo,
              
              "<br>",
              
              "<b>Issue :</b> ",
              issue,
              
              "<br><br>",
              
              "<b>Proportion :</b> ",
              pourcentage,
              
              "<br>",
              
              "<b>Nombre de parties :</b> ",
              scales::comma(
                nombre_parties,
                big.mark = " "
              ),
              
              "<br>",
              
              "<b>Total dans la classe :</b> ",
              scales::comma(
                total_parties,
                big.mark = " "
              )
            )
          )
        
        
        # --------------------------------------------------
        # Heatmap
        # --------------------------------------------------
        
        graphique <- ggplot(
          
          data,
          
          aes(
            x = issue,
            y = classe_elo,
            fill = proportion,
            
            text = tooltip
          )
        ) +
          
          geom_tile(
            
            color = "white",
            
            linewidth = 1
          ) +
          
          geom_text(
            
            aes(
              label = pourcentage
            ),
            
            color = "#222222",
            
            fontface = "bold",
            
            size = 4
          ) +
          
          scale_fill_gradient(
            
            low = "#F4F7F9",
            
            high = "#1565C0",
            
            labels = scales::label_percent(
              accuracy = 1
            ),
            
            name = "Proportion"
          ) +
          
          labs(
            
            title =
              "Comment les parties se terminent-elles ?",
            
            subtitle =
              "Proportion de chaque issue selon la différence d'Elo entre les joueurs",
            
            x = NULL,
            
            y = "Différence d'Elo"
          ) +
          
          theme_minimal(
            
            base_size = 14
          ) +
          
          theme(
            
            panel.grid = element_blank(),
            
            axis.text.x = element_text(
              
              face = "bold",
              
              size = 12
            ),
            
            axis.text.y = element_text(
              
              size = 11
            ),
            
            axis.title.y = element_text(
              
              face = "bold",
              
              margin = margin(
                r = 10
              )
            ),
            
            plot.title = element_text(
              
              size = 21,
              
              face = "bold",
              
              hjust = 0.5
            ),
            
            plot.subtitle = element_text(
              
              size = 13,
              
              color = "#666666",
              
              hjust = 0.5,
              
              margin = margin(
                b = 20
              )
            ),
            
            legend.position = "right"
          )
        
        
        # --------------------------------------------------
        # Conversion Plotly
        # --------------------------------------------------
        
        ggplotly(
          
          graphique,
          
          tooltip = "text"
        ) %>%
          
          layout(
            
            hoverlabel = list(
              
              bgcolor = "white",
              
              bordercolor = "#333333",
              
              font = list(
                color = "#222222",
                size = 13
              )
            )
          )
      })
      
    }
  )
}