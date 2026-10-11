library(stockfish)
library(rchess)

stockfish_path <- "stockfish/stockfish-linux-x86-64-universal"

file.exists(stockfish_path)
file.access(stockfish_path, mode = 1)

engine <- fish$new()

# 1. Séquence de coups de l'ouverture pour chaque partie
Donnees_Chess <- Donnees_Chess |>
  mutate(
    moves_opening = vapply(seq_len(n()), function(i) {
      coups <- strsplit(trimws(moves[i]), "\\s+")[[1]]
      paste(coups[seq_len(min(opening_ply[i], length(coups)))], collapse = " ")
    }, character(1))
  )

# 2. Séquences distinctes uniquement
ouvertures <- distinct(Donnees_Chess, moves_opening) #environ 10 fois moins d'entrées

# 3. FEN de la position finale de chaque séquence
fen_finale <- function(seq_coups) {
  ch <- Chess$new()
  for (m in strsplit(seq_coups, " ")[[1]]) ch$move(m) # on joue les coups de la séquence pour obtenir le plateau final
  ch$fen()
}

ouvertures$fen <- vapply(ouvertures$moves_opening, fen_finale, character(1))

# 4. Évaluation Stockfish

score_position <- function(fen, depth = 6) {
  i <<- i+1
  print(paste(i, "/", nrow(ouvertures)))
  engine$run(paste("position fen", fen))
  engine$run(paste("go depth", depth))
  out <- engine$output
  # dernière ligne "info ... score cp X" ou "score mate N"
  info <- tail(grep("score (cp|mate)", out, value = TRUE), 1)
  if (grepl("score mate", info)) {
    n <- as.integer(sub(".*score mate (-?[0-9]+).*", "\\1", info))
    val <- sign(n) * 10000          # convention à ajuster pour les mats
  } else {
    val <- as.integer(sub(".*score cp (-?[0-9]+).*", "\\1", info))
  }
  # point de vue des blancs
  if (grepl(" b ", fen)) val <- -val
  val
}

i=0
ouvertures$eval_opening <- vapply(ouvertures$fen, score_position,
                                  numeric(1), depth = 6)

# 5. Report sur toutes les parties
Donnees_Chess <- left_join(Donnees_Chess, ouvertures, by = "moves_opening")

write.csv(Donnees_Chess, "Donnees_Chess.csv", row.names = FALSE)
