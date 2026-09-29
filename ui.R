#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# -------- IMPORTATION DES DONNEES ------------

library(readr)
library(ggplot2)
Donnees_Chess <- read_csv("Donnees_Chess.csv")

# ---------- Présentation du jeu de données ----------

# Le jeu de données "Donnees_Chess", se compose de 16 variables pour 20058 lignes.
# Chaque ligne représente une rencontre entre deux joureurs et les variables représentent un certain nombre de caractéristique
# de la partie. On retrouve 3 varibles quantitatives :
# "Turns" qui indique le nombre de Tour qu'a durée la partie.
# "white_rating" / "black_rating" renseigne l'élo (ou le classement) du joueur noir ou du joueur blanc.
# Il y a 2 variables temporelles : (FAIRE LE LIEN AVEC SON COURS : catégorielle unilatérale ??)
# "created_at" et "last_move_at" qui informe de l'heure de debut et de fin de partie.
# Ce jeu de données se compose également de varibles qualitatives :
# "id" qui correspond au numéro de la partie, ce numéro est unique pour chaque partie (il y a moins de numéro de que ligne => normalement ça devrait être unique, faudrait  vérifier si c'est pas lier aux report de certaines partie, les joueurs finissent leurs partie un autre jour là où ils l'ont arrétées)
# "rated" il s'agit d'un boolean qui permet de savoir si la rencontre est classée (menera à l'augmentation / diminution de l'élo des deux adversaires) ou non.
# "victory_status" concerne toutes les issues possibles d'une partie d'échecs, soit victoire, defaite, égalité ou encore abandon (resign / mate ??)
# "winner" disigne le gagnant de la rencontre le cas échant (black - white ou draw)
# "increment_code" fait référence à une catégorie de partie disputée par les joueurs (leur chrono de départ, l'ajout de temps pour chaque coups...) 
# "black_id" / "white_id" renseigne le numéro d'identifiant du joueur blanc / noir
# "moves" correspond à une suite de coups symbolisées par des diminutifs (ex Nc6 : est le diminutif du coup : Cavalier va en c6), je crois que quand il y a un x ça signifie qu'une pièce à étée prise)
# "opening_eco" fait référence au nom standardisé pour chaque opening (ou familles d'opening car des fois quand il y a deux fois le même eco il y a différents noms)
# "opening_name" est le nom de l'opening comme nommé dans la littérature
# "opening_ply" fait quant à elle allusion au nombre de coup qui composent chaque ouverture.

# ---------- Pré - Traitement des données ------------

# Il va dans un premier être nécessaire de procéder à un prétraitement des données avant de pouvoir poursuivre l'analyse.

summary(Donnees_Chess)

# La fonction summary nous indique que les variables facteurs n'ont pas étées bien typées, de même que les dates.
# On peut également noter que dans ce jeu de données, aucune valeur n'est manquante.

Donnees_Chess$victory_status <- as.factor(Donnees_Chess$victory_status)
Donnees_Chess$winner <- as.factor(Donnees_Chess$winner)
Donnees_Chess$increment_code <- as.factor(Donnees_Chess$increment_code)
Donnees_Chess$opening_eco <- as.factor(Donnees_Chess$opening_eco)
Donnees_Chess$opening_name <- as.factor(Donnees_Chess$opening_name)
Donnees_Chess$opening_ply <- as.factor(Donnees_Chess$opening_ply)

Donnees_Chess$created_at <- as.POSIXct(
  Donnees_Chess$created_at / 1000,
  origin = "1970-01-01",
  tz = "UTC"
)

Donnees_Chess$last_move_at <- as.POSIXct(
  Donnees_Chess$last_move_at / 1000,
  origin = "1970-01-01",
  tz = "UTC"
)

# ============================================================
# VARIABLES ELO
# ============================================================

# Différence d'Elo :
# positif = les Blancs ont un Elo supérieur
# négatif = les Noirs ont un Elo supérieur

Donnees_Chess$elo_diff <- Donnees_Chess$white_rating - Donnees_Chess$black_rating


# Différence absolue d'Elo

Donnees_Chess$elo_diff_abs <- abs(Donnees_Chess$elo_diff)


# Elo moyen de la partie

Donnees_Chess$elo_moyen <- (Donnees_Chess$white_rating + Donnees_Chess$black_rating) / 2


# ============================================================
# IDENTIFICATION DU JOUEUR DÉSAVANTAGÉ
# ============================================================

Donnees_Chess$couleur_desavantage <- ifelse(
  Donnees_Chess$elo_diff > 0,
  "black",
  ifelse(
    Donnees_Chess$elo_diff < 0,
    "white",
    "egalite"
  )
)


# ============================================================
# VICTOIRE DU JOUEUR DÉSAVANTAGÉ
# ============================================================

Donnees_Chess$victoire_desavantage <- ifelse(
  Donnees_Chess$couleur_desavantage == "white",
  Donnees_Chess$winner == "white",
  ifelse(
    Donnees_Chess$couleur_desavantage == "black",
    Donnees_Chess$winner == "black",
    NA
  )
)

# Conversion en numérique/logique

Donnees_Chess$victoire_desavantage <- as.logical(Donnees_Chess$victoire_desavantage)



str(Donnees_Chess)
remotes::install_github("jbkunst/rchess")
library(rchess)
library(htmltools)
library(shiny)

fluidPage(
  
  titlePanel("Jeu d'échecs"),
  
  tabsetPanel(
    
    tabPanel(
      
      "Statistiques Descriptives de base",
      
      tabsetPanel(
        
        # -------------------------------------------------------
        # Onglet 1
        # -------------------------------------------------------
        
        tabPanel(
          "Taux de victoire",
          
          h2(
            "Proportion de victoire pour les joueurs désavantagés"
          ),
          
          sidebarLayout(
            
            sidebarPanel(
              
              sliderInput(
                inputId = "taille_classe",
                label = "Taille des classes Elo :",
                min = 100,
                max = 1000,
                value = 400,
                step = 100
              ),
              
              radioButtons(
                inputId = "couleur_desavantage",
                label = "Joueur désavantagé :",
                choices = c(
                  "Les deux" = "tous",
                  "Blancs" = "white",
                  "Noirs" = "black"
                ),
                selected = "tous"
              )
              
            ),
            
            mainPanel(
              
              plotOutput(
                "graphique_elo",
                height = "600px"
              )
              
            )
          )
        ),
        
        
        # -------------------------------------------------------
        # Onglet 2
        # -------------------------------------------------------
        
        tabPanel(
          "Nombre de coups",
          
          h2(
            "Nombre moyen de coups selon l'Elo"
          ),
          
          mainPanel(
            
            plotOutput(
              "graphique_temps_elo",
              height = "600px"
            )
            
          )
        ),
        
        
        # -------------------------------------------------------
        # Onglet 3
        # -------------------------------------------------------
        
        tabPanel(
          "Issue des parties",
          
          h2(
            "Issue des parties selon la classe d'Elo"
          ),
          
          sidebarLayout(
            
            sidebarPanel(
              
              sliderInput(
                inputId = "taille_classe",
                label = "Taille des classes Elo :",
                min = 100,
                max = 1000,
                value = 400,
                step = 100
              )
              
            ),
            
            mainPanel(
              
              plotOutput(
                "graphique_elo_test",
                height = "600px"
              )
              
            )
          )
        )
      )
    ),
    tabPanel(
      "Statistiques de plus haut niveau",
      h2("Statistiques de plus haut niveau"),
    ),
    tabPanel(
      "Autres résultats",
      h2("Autres résultats")
    ),
    
    tabPanel(
      "Jeu d'echec",
      h2("Jeu d'echec"),
      fluidRow(
        
        # ==========================================================
        # ECHIQUIER
        # ==========================================================
        
        column(
          width = 8,
          
          uiOutput("echiquier")
        ),
        
        # ==========================================================
        # PANNEAU DE DROITE
        # ==========================================================
        
        column(
          width = 4,
          
          h3("Partie"),
          
          hr(),
          
          strong("Trait :"),
          textOutput("tour"),
          
          br(),
          
          strong("Case sélectionnée :"),
          textOutput("selection"),
          
          br(),
          
          strong("Historique :"),
          textOutput("historique"),
          
          br(),
          
          strong("Message :"),
          textOutput("message"),
          
          uiOutput("promotion"),
          
          br(),
          
          actionButton(
            "nouvelle_partie",
            "Nouvelle partie",
            class = "btn-primary"
          )
        )
      )
    )
  )
)
