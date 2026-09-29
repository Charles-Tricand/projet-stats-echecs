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
  
  output$top_openings <- renderPlotly({
    
    # Données de la tranche Elo sélectionnée
    donnees_elo <- Donnees_Chess %>%
      filter(elo_bin == input$elo)
    
    # Nombre total de parties dans la tranche
    total_parties <- nrow(donnees_elo)
    
    # Nombre d'ouvertures choisi par l'utilisateur
    n <- as.numeric(input$nb_openings)
    
    # Top N ouvertures
    top_openings <- donnees_elo %>%
      count(
        opening_eco,
        sort = TRUE,
        name = "nombre"
      ) %>%
      slice_head(n = n) %>%
      mutate(
        proportion = nombre / total_parties
      )
    
    # Graphique
    plot_ly(
      data = top_openings,
      x = ~proportion,
      y = ~reorder(opening_eco, proportion),
      type = "bar",
      orientation = "h",
      
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Nombre de parties : %{text}<br>",
        "Proportion : %{x:.2%}",
        "<extra></extra>"
      ),
      
      text = ~nombre
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
  
  output$coups_elo <- renderPlotly({
    
    donnees <- Donnees_Chess %>%
      filter(turns<=100) %>%
      mutate(
        elo_blanc = white_rating,
        elo_noir = black_rating
      )
    
    plot_ly(
      data = donnees,
      x = ~elo_blanc,
      y = ~elo_noir,
      type = "scatter",
      mode = "markers",
      
      marker = list(
        size = 7,
        color = ~turns,
        colorscale = "Viridis",
        showscale = TRUE,
        colorbar = list(
          title = "Nombre de coups"
        ),
        opacity = 0.6
      ),
      
      hovertemplate = paste0(
        "<b>Elo blancs :</b> %{x}<br>",
        "<b>Elo noirs :</b> %{y}<br>",
        "<b>Nombre de coups :</b> %{marker.color}",
        "<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(
          title = "Elo des blancs",
          showgrid = TRUE,
          gridcolor = "#E5E7EB"
        ),
        
        yaxis = list(
          title = "Elo des noirs",
          showgrid = TRUE,
          gridcolor = "#E5E7EB"
        ),
        
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        
        margin = list(
          l = 70,
          r = 80,
          t = 30,
          b = 70
        )
      )
  })
}


