#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

server <- function(input, output, session) {
  
  # =========================================================
  # 1. TOP 15 POUR LA TRANCHE ELO SELECTIONNEE
  # =========================================================
  
  top15 <- reactive({
    
    donnees_elo <- Donnees_Chess %>%
      filter(elo_bin == input$elo)
    
    total_parties <- nrow(donnees_elo)
    
    # Top 15 ouvertures
    top <- donnees_elo %>%
      mutate(
        opening_eco = as.character(opening_eco),
        opening_name = as.character(opening_name),
        opening_ply = as.integer(as.character(opening_ply))
      ) %>%
      count(
        opening_eco,
        sort = TRUE,
        name = "nombre"
      ) %>%
      slice_head(n = 15) %>%
      mutate(
        proportion = nombre / total_parties
      )
    
    # Pour chaque ouverture, on prend une partie représentative
    # avec une séquence de coups correspondant à cette ouverture
    representation <- donnees_elo %>%
      mutate(
        opening_eco = as.character(opening_eco),
        opening_name = as.character(opening_name),
        opening_ply = as.integer(as.character(opening_ply))
      ) %>%
      filter(opening_eco %in% top$opening_eco) %>%
      count(
        opening_eco,
        opening_name,
        moves,
        opening_ply,
        sort = TRUE,
        name = "frequence"
      ) %>%
      group_by(opening_eco) %>%
      slice_head(n = 1) %>%
      ungroup()
    
    top %>%
      left_join(
        representation,
        by = "opening_eco"
      )
  })
  
  
  # =========================================================
  # 2. HISTOGRAMME
  # =========================================================
  
  output$top_openings <- renderPlotly({
    
    df <- top15()
    
    plot_ly(
      data = df,
      
      # IMPORTANT pour récupérer le clic
      source = "graphique",
      
      x = ~proportion,
      y = ~reorder(opening_name, proportion),
      
      type = "bar",
      orientation = "h",
      
      # Identifiant de la barre
      key = ~opening_eco,
      
      text = ~nombre,
      
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Nombre de parties : %{text}<br>",
        "Proportion : %{x:.2%}",
        "<extra></extra>"
      )
    ) %>%
      
      layout(
        
        xaxis = list(
          title = "Proportion des parties",
          tickformat = ".1%"
        ),
        
        yaxis = list(
          title = ""
        )
      )
  })
  
  
  # =========================================================
  # 3. CLIQUE SUR UNE BARRE
  # =========================================================
  
  ouverture_cliquee <- reactive({
    
    event <- event_data(
      "plotly_click",
      source = "graphique"
    )
    
    req(event)
    
    # ECO de l'ouverture sélectionnée
    eco <- as.character(event$key[1])
    
    top15() %>%
      filter(opening_eco == eco) %>%
      slice(1)
  })
  
  
  # =========================================================
  # 4. NOM DE L'OUVERTURE
  # =========================================================
  
  output$opening_name <- renderText({
    
    ouverture_cliquee()$opening_name
    
  })
  
  
  # =========================================================
  # 5. CODE ECO
  # =========================================================
  
  output$opening_eco <- renderText({
    
    paste(
      "Code ECO :",
      ouverture_cliquee()$opening_eco
    )
    
  })
  
  
  # =========================================================
  # 6. ENVOI DES COUPS AU JAVASCRIPT
  # =========================================================
  
  observe({
    
    ouverture <- ouverture_cliquee()
    
    session$sendCustomMessage(
      "update_chessboard",
      
      list(
        
        moves = ouverture$moves,
        
        opening_ply =
          ouverture$opening_ply
        
      )
    )
  })
}