mod_openings_quadrant_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Popularité des ouvertures",
    br(),
    plotOutput(
      ns("quadrant_openings"),
      height = "700px"
    )
  )
}


mod_openings_quadrant_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$quadrant_openings <- renderPlot({
      
      # Correspondance ECO -> nom d'ouverture le plus fréquent
      correspondance_ouvertures <- Donnees_Chess %>%
        filter(
          !is.na(opening_eco),
          !is.na(opening_name)
        ) %>%
        count(
          opening_eco,
          opening_name,
          sort = TRUE
        ) %>%
        group_by(opening_eco) %>%
        slice_max(
          n,
          n = 1,
          with_ties = FALSE
        ) %>%
        ungroup()
      
      
      # 20 ECO les plus joués
      top_20_eco <- Donnees_Chess %>%
        filter(!is.na(opening_eco)) %>%
        count(
          opening_eco,
          sort = TRUE
        ) %>%
        slice_head(n = 20)
      
      
      # Données utilisées pour le graphique
      donnees_graphique <- Donnees_Chess %>%
        filter(
          opening_eco %in% top_20_eco$opening_eco,
          !is.na(winner)
        ) %>%
        group_by(opening_eco) %>%
        summarise(
          nombre_parties = n(),
          taux_victoire = mean(winner == "white"),
          .groups = "drop"
        ) %>%
        left_join(
          top_20_eco,
          by = "opening_eco"
        ) %>%
        left_join(
          correspondance_ouvertures,
          by = "opening_eco"
        ) %>%
        mutate(
          # Popularité par rapport à TOUTES les parties
          popularite = nombre_parties / nrow(Donnees_Chess)
        )
      
      
      # Médiane de la popularité des 20 ouvertures
      mediane_popularite <- median(
        donnees_graphique$popularite
      )
      
      # Médiane du taux de victoire des Blancs
      mediane_victoire <- median(
        donnees_graphique$taux_victoire
      )
      
      
      # Graphique
      ggplot(
        donnees_graphique,
        aes(
          x = popularite,
          y = taux_victoire
        )
      ) +
        
        # Ligne verticale : popularité médiane
        geom_vline(
          xintercept = mediane_popularite,
          linetype = "dashed",
          linewidth = 0.7,
          color = "grey50"
        ) +
        
        # Ligne horizontale : taux de victoire médian
        geom_hline(
          yintercept = mediane_victoire,
          linetype = "dashed",
          linewidth = 0.7,
          color = "grey50"
        ) +
        
        # Pion
        geom_text(
          aes(label = "♟"),
          size = 8,
          family = "DejaVu Sans"
        ) +
        
        # Nom de l'ouverture
        geom_label_repel(
          aes(label = opening_name),
          size = 3.1,
          color = "black",
          fill = "white",
          alpha = 0.85,
          box.padding = 0.7,
          point.padding = 0.6,
          min.segment.length = 0,
          segment.color = "grey60",
          segment.linewidth = 0.3,
          force = 2,
          max.overlaps = Inf
        ) +
        
        # Pourcentages sur les axes
        scale_x_continuous(
          labels = scales::percent_format(
            accuracy = 1
          )
        ) +
        
        scale_y_continuous(
          labels = scales::percent_format(
            accuracy = 1
          )
        ) +
        
        labs(
          title = "Popularité et taux de victoire des ouvertures",
          x = "Popularité de l'ouverture",
          y = "Taux de victoire des Blancs"
        ) +
        
        theme_minimal(
          base_size = 13
        ) +
        
        theme(
          plot.title = element_text(
            hjust = 0.5,
            size = 18,
            face = "bold"
          ),
          panel.grid = element_blank(),
          axis.title = element_text(
            size = 12
          ),
          plot.margin = margin(
            20,
            30,
            20,
            30
          ),
          legend.position = "none"
        )
    })
  })
}

