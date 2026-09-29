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

Donnees_Chess = Donnees_Chess |>
  mutate(
    across(c(id, white_id, black_id, moves, opening_name), as.character),
    rated = as.logical(toupper(rated)),
    created_at = as.POSIXct(created_at / 1000, origin = "1970-01-01", tz = "UTC"),
    last_move_at = as.POSIXct(last_move_at / 1000, origin = "1970-01-01", tz = "UTC"),
    elo_diff = white_rating - black_rating,
    elo_diff_abs = abs(elo_diff),
    elo_moyen = (white_rating + black_rating) / 2,
    couleur_desavantage = ifelse(elo_diff > 0,"black",ifelse(elo_diff < 0,"white","egalite")),
    victoire_desavantage = couleur_desavantage == winner
  )

# Source all R files in the R directory (modules)
lapply(list.files(path="R", pattern="\\.R$", full.names=TRUE), source)