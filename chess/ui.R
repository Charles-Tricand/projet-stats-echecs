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
  
  titlePanel("Top 15 des ouvertures selon le niveau Elo"),
  
  selectInput(
    inputId = "elo",
    label = "Choisissez une tranche Elo :",
    choices = elo_levels,
    selected = "1500-1599"
  ),
  
  fluidRow(
    
    column(
      width = 8,
      
      plotlyOutput(
        "top_openings",
        height = "600px"
      )
    ),
    
    column(
      width = 4,
      
      h3("Position à la fin de l'ouverture"),
      
      h4(textOutput("opening_name")),
      
      p(textOutput("opening_eco")),
      
      div(
        id = "chessboard"
      )
    )
  )
)