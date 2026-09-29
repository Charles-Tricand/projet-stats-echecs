# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

source("global.R")

fluidPage(
  titlePanel("Jeu d'échecs"),

  tabsetPanel(

    tabPanel(
      "Statistiques Descriptives de base",
      tabsetPanel(
        mod_elo_victory_ui("elo_victory_tab"),
        mod_nb_coups_ui("nb_coups_tab"),
        mod_issue_parties_ui("issue_parties_tab")
      )
    ),

    tabPanel(
      "Statistiques de plus haut niveau",
      h2("Statistiques de plus haut niveau")
    ),

    tabPanel(
      "Autres résultats",
      h2("Autres résultats")
    )
  )
)