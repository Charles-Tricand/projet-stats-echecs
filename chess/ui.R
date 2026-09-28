#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(
  
  titlePanel("Les ouvertures les plus jouées selon le niveau Elo"),
  
  fluidRow(
    
    column(
      width = 6,
      
      selectInput(
        inputId = "elo",
        label = "Tranche Elo :",
        choices = elo_levels,
        selected = "1500-1599"
      )
    ),
    
    column(
      width = 6,
      
      sliderInput(
        inputId = "nb_openings",
        label = "Nombre d'ouvertures à afficher :",
        min = 1,
        max = 20,
        value = 3,
        step=1
      )
    )
  ),
  
  br(),
  
  plotlyOutput(
    "top_openings",
    height = "600px"
  ),
  tabPanel(
    "Coups selon différence d'Elo",
    
    sliderInput(
      "taille_elo",
      "Taille des catégories d'écart Elo :",
      min = 50,
      max = 500,
      value = 100,
      step = 50
    ),
    
    plotlyOutput("coups_elo", height = "600px")
  )
)


