# -------- IMPORTATION DES DONNEES ------------

library(readr)
library(tidyverse)
library(stockfish)
library(chess)
Donnees_Chess <- read.csv("Donnees_Chess.csv", stringsAsFactors = TRUE)

# ---------- Présentation du jeu de données ------------

# Le jeu de données "Donnees_Chess", se compose de 16 variables pour 20058 lignes.
# Chaque ligne représente une rencontre entre deux joureurs et les variables représentent un certain nombre de caractéristique
# de la partie. On retrouve 3 varibles quantitatives :
# "Turns" qui indique le nombre de Tour qu'a durée la partie.
# "white_rating" / "black_rating" renseigne l'élo (ou le classement) du joueur noir ou du joueur blanc.
# Il y a 2 variables temporelles : (FAIRE LE LIEN AVEC SON COURS : catégorielle unilatérale ??)
# "created_at" et "last_move_at" qui informe de l'heure de debut et de fin de partie.
# Ce jeu de données se compose également de varibles qualitatives :
# "id" qui correspond au numéro de la partie, ce numéro est unique pour chaque partie (il y a moins de numéro de que ligne => normalement ça devrait être unique, faudrait  vérifier si c'est pas lier aux report de certaine partie, les joueurs finissent leur partie un autre jour là où ils l'ont arrétées)
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

# La fonction summary nous indique que les variables facteurs n'ont pas été bien typées, de même que les dates.
# On peut également noter que dans ce jeu de données, aucune valeur n'est manquante.

Donnees_Chess_processed = Donnees_Chess |>
  mutate(across(c(id, white_id, black_id, moves, opening_name), as.character),
         rated = as.logical(toupper(rated)),
         created_at = as.POSIXct(created_at / 1000, origin = "1970-01-01", tz = "UTC"),
         last_move_at = as.POSIXct(last_move_at / 1000, origin = "1970-01-01", tz = "UTC"))


stockfish_path <- "stockfish/stockfish-linux-x86-64-universal"

file.exists(stockfish_path)
file.access(stockfish_path, mode = 1)


# Fonction pour analyser une séquence de coups avec Stockfish
# Retourne l'évaluation (score en centipawns) après chaque coup
analyser_partie_stockfish <- function(moves_san, profondeur = 10) {
  # Initialiser une nouvelle partie
  game_obj <- game()

  # Séparer les coups
  moves_list <- strsplit(moves_san, " ")[[1]]

  # Stock pour les résultats
  evaluations <- list()

  for (i in seq_along(moves_list)) {
    move_san <- moves_list[i]

    tryCatch({
      # Parser le coup en notation SAN dans le contexte de la position actuelle
      parsed_move <- parse_move(game_obj, move_san)

      if (is.null(parsed_move)) {
        # Coup invalide
        evaluations[[i]] <- list(
          move_number = i,
          san = move_san,
          uci = NA,
          evaluation = NA,
          error = "Coup invalide"
        )
        next
      }

      # Appliquer le coup à la partie
      game_obj <- move(game_obj, parsed_move)

      # Obtenir la position actuelle en format FEN
      fen_position <- fen(game_obj)

      # Créer une nouvelle instance de Stockfish pour cette position
      # (Ou réutiliser la même instance en définissant la position)
      fish <- stockfish::fish$new(stockfish_path)

      # Définir la position Stockfish using FEN
      fish$position(position = fen_position, type = "fen")

      # Obtenir l'évaluation de Stockfish
      result <- fish$go(depth = profondeur)

      # Extraire le score centipawns de la sortie
      score_cp <- NA
      if (length(result) > 0) {
        # Chercher la ligne avec le score
        score_line <- grep("score cp", result, value = TRUE)
        if (length(score_line) > 0) {
          # Extraire le nombre après "score cp"
          score_parts <- unlist(strsplit(score_line, " "))
          cp_index <- which(score_parts == "cp")
          if (length(cp_index) > 0 && cp_index < length(score_parts)) {
            score_cp <- as.numeric(score_parts[cp_index + 1])
          }
        }
      }

      evaluations[[i]] <- list(
        move_number = i,
        san = move_san,
        uci = as.uci(parsed_move),  # Convertir le coup parsé en UCI
        evaluation = score_cp,
        error = NA
      )

    }, error = function(e) {
      evaluations[[i]] <- list(
        move_number = i,
        san = move_san,
        uci = NA,
        evaluation = NA,
        error = as.character(e)
      )
    })
  }

  # Convertir en data frame
  results_df <- bind_rows(lapply(evaluations, as.data.frame))
  return(results_df)
}

# Exemple d'utilisation:
# Pour analyser les premières parties du dataset:
#
exemple_moves <- Donnees_Chess_processed$moves[1]
print(paste("Coups de la première partie:", exemple_moves))
#
analyse <- analyser_partie_stockfish(exemple_moves, profondeur = 5)
print(analyse)
#
# Pour ajouter une colonne d'évaluation au dataset (attention: peut être lent):
# Donnees_Chess_processed <- Donnees_Chess_processed %>%
#   rowwise() %>%
#   mutate(
#     eval_list = list(analyser_partie_stockfish(moves, profondeur = 5)),
#     final_evaluation = ifelse(nrow(eval_list) > 0,
#                               eval_list$evaluation[nrow(eval_list)],
#                               NA)
#   ) %>%
#   ungroup()