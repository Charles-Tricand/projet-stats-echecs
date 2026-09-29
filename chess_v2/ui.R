#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(shiny)
library(dplyr)
library(readr)
library(plotly)

ui <- fluidPage(
  
  # =========================
  # STYLE
  # =========================
  
  tags$head(
    tags$style(HTML("
      
      body {
        background-color: #F5F6F8;
        font-family: Arial, sans-serif;
        color: #263238;
      }
      
      .container-fluid {
        max-width: 1400px;
        margin: auto;
        padding: 30px 45px;
      }
      
      .main-title {
        font-size: 32px;
        font-weight: 700;
        margin-bottom: 30px;
        color: #263238;
      }
      
      .nav-tabs {
        border-bottom: 1px solid #DDE1E6;
        margin-bottom: 25px;
      }
      
      .nav-tabs > li > a {
        color: #6B7280;
        font-weight: 500;
        border: none;
        padding: 14px 20px;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #263238;
        background-color: transparent;
        border: none;
        border-bottom: 3px solid #263238;
      }
      
      .control-panel {
        background: white;
        border-radius: 14px;
        padding: 22px 25px 10px 25px;
        margin-bottom: 25px;
        box-shadow: 0 3px 15px rgba(0,0,0,0.05);
      }
      
      .graph-card {
        background: white;
        border-radius: 16px;
        padding: 25px;
        box-shadow: 0 3px 18px rgba(0,0,0,0.06);
      }
      
      .section-title {
        font-size: 21px;
        font-weight: 600;
        margin-bottom: 5px;
      }
      
      .section-subtitle {
        color: #7A8088;
        font-size: 14px;
        margin-bottom: 25px;
      }
      
    "))
  ),
  
  
  # =========================
  # TITRE
  # =========================
  
  div(
    class = "main-title",
    "♟ Analyse des parties d'échecs"
  ),
  
  
  # =========================
  # ONGLETS
  # =========================
  
  tabsetPanel(
    
    
    # =====================================================
    # ONGLET 1 : OUVERTURES
    # =====================================================
    
    tabPanel(
      
      title = "Ouvertures selon l'Elo",
      
      br(),
      
      div(
        class = "control-panel",
        
        fluidRow(
          
          column(
            width = 6,
            
            selectInput(
              inputId = "elo",
              label = "Tranche Elo",
              choices = elo_levels,
              selected = "1500-1599"
            )
          ),
          
          column(
            width = 6,
            
            sliderInput(
              inputId = "nb_openings",
              label = "Nombre d'ouvertures",
              min = 1,
              max = 20,
              value = 10,
              step = 1
            )
          )
        )
      ),
      
      div(
        class = "graph-card",
        
        div(
          class = "section-title",
          "Les ouvertures les plus jouées"
        ),
        
        div(
          class = "section-subtitle",
          "La taille de chaque pièce représente la proportion de parties."
        ),
        
        uiOutput("top_openings")
      )
    ),
    
    
    # =====================================================
    # ONGLET 2 : COUPS / ÉCART ELO
    # =====================================================
    
    tabPanel(
      
      title = "Coups selon l'écart d'Elo",
      
      br(),
      
      div(
        class = "control-panel",
        
        sliderInput(
          inputId = "taille_elo",
          label = "Largeur des catégories d'écart Elo",
          min = 50,
          max = 500,
          value = 100,
          step = 50
        )
      ),
      
      div(
        class = "graph-card",
        
        div(
          class = "section-title",
          "Nombre moyen de coups selon l'écart d'Elo"
        ),
        
        div(
          class = "section-subtitle",
          "Chaque barre représente une catégorie de différence de classement."
        ),
        
        plotlyOutput(
          "coups_elo",
          height = "600px"
        )
      )
    )
  )
)