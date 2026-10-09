# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

source("global.R")

fluidPage(
  
  tags$head(
    
    # --------------------------------------------------------------
    
    # Première proposition de css :
    
    # tags$link(
    #   rel = "stylesheet",
    #   type = "text/css",
    #   href = "style.css"
    # ),
    
    # Deuxième proposition de css :
    
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style_2.css"
    ),
    
    # -------------------------------------------------------------------
    
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "guide-echecs.css"
    )
  ),
  
  titlePanel("Jeu d'échecs"),
  
  tabsetPanel(
    
    tabPanel(
      "Statistiques Descriptives de base",
      tabsetPanel(
        #mod_elo_victory_ui("elo_victory_tab"),
        mod_nb_coups_ui("nb_coups_tab"),
        mod_issue_parties_ui("issue_parties_tab"),
        mod_diff_elo_max_ui("diff_elo_max_tab"),
        mod_calculateur_ui("calculateur_tab"),
        mod_echiquier_interactif_ui("echiquier_interactif_tab"),
        mod_probabilite_elo_ui("probabilite_elo_tab")
      )
    ),
    
    tabPanel(
      "Statistiques de plus haut niveau",
      h2("Statistiques de plus haut niveau")
    ),
    
    tabPanel(
      "Autres résultats",
      h2("Autres résultats")
    ),
    tabPanel(
      "Apprendre les échecs",
      mod_guide_echecs_ui("guide_echecs_tab")
    )
  )
)