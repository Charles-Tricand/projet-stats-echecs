# Global.R - Preprocessing and shared objects for the Shiny app

# Load libraries
library(shiny)
library(rchess)
library(tidyverse)
library(ggplot2)
library(readr)
library(htmltools)

# Install rchess from GitHub if not already installed
# remotes::install_github("jbkunst/rchess")

# Load and preprocess the chess dataset
Donnees_Chess <- read_csv("Donnees_Chess.csv")

# Convert character columns to factors
Donnees_Chess$victory_status <- as.factor(Donnees_Chess$victory_status)
Donnees_Chess$winner <- as.factor(Donnees_Chess$winner)
Donnees_Chess$increment_code <- as.factor(Donnees_Chess$increment_code)
Donnees_Chess$opening_eco <- as.factor(Donnees_Chess$opening_eco)
Donnees_Chess$opening_name <- as.factor(Donnees_Chess$opening_name)
Donnees_Chess$opening_ply <- as.factor(Donnees_Chess$opening_ply)

# Convert timestamps from milliseconds to POSIXct
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

# Conversion en logique
Donnees_Chess$victoire_desavantage <- as.logical(Donnees_Chess$victoire_desavantage)

# Source all R files in the R directory (modules)
lapply(list.files(path="R", pattern="\\.R$", full.names=TRUE), source)