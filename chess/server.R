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
    
    taille <- input$taille_elo
    
    donnees <- Donnees_Chess %>%
      mutate(
        diff_elo = abs(white_rating - black_rating),
        categorie_elo = floor(diff_elo / taille) * taille
      ) %>%
      group_by(categorie_elo) %>%
      summarise(
        nombre_coups_moyen = mean(turns, na.rm = TRUE),
        nombre_parties = n(),
        .groups = "drop"
      ) %>%
      arrange(categorie_elo) %>%
      mutate(
        categorie = paste0(
          categorie_elo,
          "-",
          categorie_elo + taille - 1
        ),
        categorie = factor(
          categorie,
          levels = categorie
        )
      )
    
    plot_ly(
      data = donnees,
      x = ~categorie,
      y = ~nombre_coups_moyen,
      type = "bar",
      hovertemplate = paste0(
        "<b>Écart Elo : %{x}</b><br>",
        "Nombre moyen de coups : %{y:.1f}<br>",
        "Nombre de parties : %{customdata}<br>",
        "<extra></extra>"
      ),
      customdata = ~nombre_parties
    ) %>%
      layout(
        xaxis = list(
          title = "Différence de classement Elo"
        ),
        yaxis = list(
          title = "Nombre moyen de coups"
        )
      )
  })
}


