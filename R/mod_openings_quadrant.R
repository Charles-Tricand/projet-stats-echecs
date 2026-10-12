mod_openings_quadrant_ui <- function(id) {
  ns <- NS(id)
  elo_levels <- c(
    "Toutes les tranches",
    "<1000",
    paste0(seq(1000, 1900, 100), "-", seq(1099, 1999, 100)),
    "2000+"
  )
  
  tabPanel(
    "Popularité et victoire des ouvertures",
    fluidRow(
      column(
        6,
        selectInput(
          ns("elo"),
          "Tranche Elo :",
          choices  = elo_levels,
          selected = "Toutes les tranches"
        )
      ),
      column(6, sliderInput(
        ns("nb_openings"),
        "Nombre d'ouvertures à afficher :",
        min   = 3,
        max   = 50,
        value = 20,
        step  = 1)
      )
    ),
    br(),
    plotOutput(
      ns("quadrant_openings"),
      height = "700px"
    )
  )
}


mod_openings_quadrant_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Correspondance ECO -> nom d'ouverture le plus fréquent
    # (indépendante des filtres : calculée une seule fois)
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
      ungroup() %>%
      select(opening_eco, opening_name)
    
    
    # Parties de la tranche Elo choisie
    donnees_elo <- reactive({
      if (input$elo == "Toutes les tranches") {
        Donnees_Chess
      } else {
        Donnees_Chess %>% filter(elo_bin == input$elo)
      }
    })
    
    
    # Données du graphique : top N ouvertures de la tranche
    donnees_graphique <- reactive({
      donnees <- donnees_elo()
      total_parties <- nrow(donnees)
      
      validate(
        need(total_parties > 0, "Aucune partie dans cette tranche d'Elo.")
      )
      
      # N codes ECO les plus joués dans la tranche
      top_eco <- donnees %>%
        filter(!is.na(opening_eco)) %>%
        count(opening_eco, sort = TRUE) %>%
        slice_head(n = as.numeric(input$nb_openings))
      
      donnees %>%
        filter(
          opening_eco %in% top_eco$opening_eco,
          !is.na(winner)
        ) %>%
        group_by(opening_eco) %>%
        summarise(
          nombre_parties = n(),
          taux_victoire  = mean(winner == "white"),
          .groups = "drop"
        ) %>%
        left_join(correspondance_ouvertures, by = "opening_eco") %>%
        mutate(
          # Popularité par rapport à toutes les parties de la tranche
          popularite = nombre_parties / total_parties
        )
    })
    
    
    output$quadrant_openings <- renderPlot({
      donnees <- donnees_graphique()
      
      titre_elo <- if (input$elo == "Toutes les tranches") {
        "toutes tranches d'Elo"
      } else {
        paste("Elo", input$elo)
      }
      
      ggplot(
        donnees,
        aes(
          x = popularite,
          y = taux_victoire
        )
      ) +
        
        # Ligne horizontale : équilibre Blancs / Noirs (50 % de victoires)
        geom_hline(
          yintercept = 0.5,
          linetype   = "dashed",
          linewidth  = 0.7,
          color      = "grey50"
        ) +
        
        # Pion
        geom_text(
          aes(label = "♟"),
          size   = 8,
          family = "DejaVu Sans"
        ) +
        
        # Nom de l'ouverture
        geom_text_repel(
          aes(label = opening_name),
          size               = 3.1,
          color              = "black",
          alpha              = 0.85,
          box.padding        = 0.7,
          point.padding      = 0.6,
          min.segment.length = 0,
          segment.color      = "grey60",
          segment.linewidth  = 0.3,
          force              = 2,
          max.overlaps       = Inf
        ) +
        
        scale_x_continuous(
          labels = scales::percent_format(accuracy = 1)
        ) +
        
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1)
        ) +
        
        labs(
          title = paste0(
            "Popularité et taux de victoire (des blancs) des ",
            nrow(donnees),
            " ouvertures les plus jouées — ",
            titre_elo
          ),
          x = "Popularité de l'ouverture",
          y = "Taux de victoire des Blancs"
        ) +
        
        theme_minimal(base_size = 13) +
        
        theme(
          plot.title = element_text(
            hjust  = 0.5,
            size   = 18,
            face   = "bold",
            family = "Times"
          ),
          panel.grid      = element_blank(),
          axis.title      = element_text(size = 12),
          plot.margin     = margin(20, 30, 20, 30),
          legend.position = "none"
        )
    })
  })
}
