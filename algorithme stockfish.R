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
