# ==========================================================
# MODELE : PREDICTION DU RESULTAT D'UNE PARTIE
# ==========================================================

library(nnet)


# ==========================================================
# 1. PARAMETRES
# ==========================================================

# Nombre minimum de parties nécessaire pour qu'une ouverture
# soit conservée comme catégorie propre dans le modèle.
#
# Les ouvertures apparaissant moins souvent seront regroupées
# dans la catégorie "Autres".

SEUIL_OPENING_NAME <- 50


# ==========================================================
# 2. IDENTIFICATION DES OUVERTURES SUFFISAMMENT REPRESENTEES
# ==========================================================

openings_frequentes <- Donnees_Chess |>
  
  filter(!is.na(opening_name)) |>
  count(opening_name) |>
  filter(n >= SEUIL_OPENING_NAME) |>
  pull(opening_name)

cat(
  "\nNombre d'ouvertures suffisamment représentées :",
  length(openings_frequentes),
  "\n")

cat(
  "Nombre total d'ouvertures dans les données :",
  n_distinct(
    Donnees_Chess$opening_name,
    na.rm = TRUE),
  "\n")

# ==========================================================
# 3. CONSTRUCTION DE LA BASE DE MODELISATION
# ==========================================================
#
# Chaque partie est transformée en deux observations :
#
#   - une observation du point de vue des Blancs
#   - une observation du point de vue des Noirs
#
# Ainsi, le modèle répond à la question :
#
# "Quelle est la probabilité que CE joueur gagne ?"
#
# plutôt que :
#
# "Quelle est la probabilité que les Blancs gagnent ?"

Donnees_modele <- bind_rows(
  
  # ========================================================
  # JOUEUR = BLANCS
  # ========================================================
  
  Donnees_Chess |>
    transmute(
      elo_joueur = white_rating,
      elo_adversaire = black_rating,
      elo_diff = white_rating - black_rating,
      couleur = "white",
      opening_name = opening_name,
      resultat = case_when(
        winner == "white" ~ "win",
        winner == "draw" ~ "draw",
        winner == "black" ~ "loss",
        TRUE ~ NA_character_
      )
    ),
  
  # ========================================================
  # JOUEUR = NOIRS
  # ========================================================
  
  Donnees_Chess |>
    transmute(
      elo_joueur = black_rating,
      elo_adversaire = white_rating,
      elo_diff = black_rating - white_rating,
      couleur = "black",
      opening_name = opening_name,
      resultat = case_when(
        winner == "black" ~ "win",
        winner == "draw" ~ "draw",
        winner == "white" ~ "loss",
        TRUE ~ NA_character_))) |>
  
    # ========================================================
    # 4. REGROUPEMENT DES OUVERTURES RARES
    # ========================================================
    
    mutate(
      opening_name_modele = if_else(opening_name %in% openings_frequentes, opening_name, "Autres"),
      couleur = factor(couleur,levels = c("white","black")),
      resultat = factor(resultat,levels = c("loss","draw","win")),
      opening_name_modele = factor(opening_name_modele)) |>
  
  # ========================================================
# 5. SUPPRESSION DES OBSERVATIONS INCOMPLETES
# ========================================================

    filter(
      !is.na(elo_joueur),
      !is.na(elo_adversaire),
      !is.na(elo_diff),
      !is.na(opening_name_modele),
      !is.na(resultat))


    # ==========================================================
    # 6. INFORMATIONS SUR LES DONNEES
    # ==========================================================

cat("\n------------------------------------------\n")
cat("DONNEES DU MODELE\n")
cat("------------------------------------------\n")
cat("Nombre d'observations :", nrow(Donnees_modele), "\n")
cat("Nombre de parties originales :", nrow(Donnees_modele) / 2, "\n")
cat("Nombre de catégories d'ouvertures utilisées :", nlevels(Donnees_modele$opening_name_modele), "\n")

# ==========================================================
# 7. REPARTITION DES RESULTATS
# ==========================================================

cat("\nRépartition des résultats :\n")
print(table(Donnees_modele$resultat))
cat("\nRépartition des résultats selon la couleur :\n")
print(table(Donnees_modele$resultat, Donnees_modele$couleur))

# ==========================================================
# 8. REPARTITION DES OUVERTURES DU MODELE
# ==========================================================

cat("\nNombre de parties par catégorie d'ouverture :\n")
print(
  Donnees_modele |>
    count(opening_name_modele,sort = TRUE) |>
    head(20))

# ==========================================================
# 9. MODELE MULTINOMIAL
# ==========================================================

cat("\n------------------------------------------\n")
cat("ENTRAINEMENT DU MODELE\n")
cat("------------------------------------------\n")

modele_complet <- multinom(
  resultat ~ elo_diff + couleur + opening_name_modele,
  data = Donnees_modele,
  trace = FALSE)

cat("\nModèle multinomial créé avec succès.\n")

# ==========================================================
# 10. FONCTION DE PREDICTION
# ==========================================================

predire_resultat <- function(couleur, elo_joueur, elo_adversaire, opening_name) {
  
  # --------------------------------------------------------
  # Vérification de la couleur
  # --------------------------------------------------------
  
  if (!couleur %in% c("white","black")) {
    stop("La couleur doit être 'white' ou 'black'.")}
  
  
  # --------------------------------------------------------
  # Calcul de la différence Elo
  # --------------------------------------------------------
  
  elo_diff <- elo_joueur - elo_adversaire
  
  # --------------------------------------------------------
  # Vérification du nom de l'ouverture
  # --------------------------------------------------------
  
  if (is.null(opening_name) || length(opening_name) != 1 || is.na(opening_name)) {
    stop("Le nom de l'ouverture doit être renseigné.")}
  
  # --------------------------------------------------------
  # Détermination de la catégorie utilisée par le modèle
  # --------------------------------------------------------
  #
  # Si l'ouverture est suffisamment représentée dans les
  # données, elle possède sa propre catégorie.
  #
  # Sinon, elle est envoyée dans "Autres".
  #
  
  if (opening_name %in% levels(Donnees_modele$opening_name_modele)) {
    ouverture_modele <- opening_name
  } else {
    ouverture_modele <- "Autres"
  }
  
  
  # --------------------------------------------------------
  # Création de la nouvelle observation
  # --------------------------------------------------------
  
  nouvelle_partie <- data.frame(
    elo_diff = elo_diff,
    couleur = factor(
      couleur,
      levels = levels(
        Donnees_modele$couleur
      )
    ),
    
    opening_name_modele = factor(
      ouverture_modele,
      levels = levels(
        Donnees_modele$opening_name_modele
      )
    )
  )
  
  
  # --------------------------------------------------------
  # Prédiction
  # --------------------------------------------------------
  
  prediction <- predict(
    modele_complet,
    nouvelle_partie,
    type = "probs"
  )
  
  
  # --------------------------------------------------------
  # Conversion robuste en valeurs numériques
  # --------------------------------------------------------
  
  prediction <- as.numeric(
    prediction
  )
  
  
  # --------------------------------------------------------
  # Vérification
  # --------------------------------------------------------
  
  if (length(prediction) != 3) {
    stop(paste0("Le modèle n'a pas renvoyé 3 probabilités. ", "Nombre obtenu : ", length(prediction)))
  }
  
  # --------------------------------------------------------
  # Résultat final
  # --------------------------------------------------------
  
  data.frame(
    defaite = prediction[1],
    nulle = prediction[2],
    victoire = prediction[3])
}


# ==========================================================
# 11. EXEMPLE DE TEST
# ==========================================================
#
# Cette partie permet de vérifier que le modèle fonctionne.
#
# Elle peut être supprimée plus tard si nécessaire.


ouverture_test <- "King's Pawn Game: Leonardis Variation"


if (ouverture_test %in% levels(Donnees_modele$opening_name_modele)) {
  
  test_prediction <- predire_resultat(
    couleur = "white",
    elo_joueur = 1500,
    elo_adversaire = 1600,
    opening_name = ouverture_test
  )
  
  
  cat("\n------------------------------------------\n")
  cat("EXEMPLE DE PREDICTION\n")
  cat("------------------------------------------\n")
  
  print(test_prediction)
  
}