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
  
  titlePanel(
    title = actionLink(
      inputId = "lien_accueil",
      label = "Jeu d'échecs",
      class = "titre-accueil"
    ),
    windowTitle = "Jeu d'échecs"   # nécessaire car le titre n'est plus un simple texte
  ),
  
  tabsetPanel(
    
    id = "onglets_principaux",
    selected = "accueil",          # l'appli s'ouvre sur l'accueil
    
    tabPanel(
      "Accueil",
      value = "accueil",
      h1("Initiation aux statistiques sur les échecs"),
      p(class = "sous-titre-accueil", "Comment l’opening, l’elo et la couleur déterminent le déroulement et l’issue d’une partie d'echec ?"),
      div(style = "text-align: center;",
          img(src = "echecs.png", width = "400px"))
    ),
    
    tabPanel(
      "Apprendre les échecs",
      tabsetPanel(
        mod_guide_echecs_ui("guide_echecs_tab"),
        mod_echiquier_interactif_ui("echiquier_interactif_tab")
      )
    ),
    
    tabPanel(
      "Statistiques Descriptives de base",
      navlistPanel(
        id = "nav_stats_base",
        widths = c(2, 10),          # 2 colonnes pour le menu, 10 pour le contenu
        well = FALSE,  
        mod_issue_parties_ui("issue_parties_tab"),
        mod_taux_victoires_ui("taux_victoires_tab"),
        mod_elo_victory_ui("elo_victory_tab"),
        mod_nb_coups_ui("nb_coups_tab"),
        mod_openings_quadrant_ui("openings_quadrant_tab")
      )
    ),
    
    tabPanel(
      "Pour aller plus loin",
      h2("Pour aller plus loin"),
      navlistPanel(
        widths = c(2, 10),  
        mod_probabilite_elo_ui("probabilite_elo_tab"),
        mod_calculateur_ui("calculateur_tab"),
      )
    ),
    
    tabPanel(
      "Autres résultats",
      h2("Autres résultats")
    )
  )
)