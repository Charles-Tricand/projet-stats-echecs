modele_nul <- multinom(
  resultat ~ 1,
  data = Donnees_modele,
  trace = FALSE
)

classes <- levels(Donnees_modele$resultat)

P_complet <- predict(
  modele_complet,
  newdata = Donnees_modele,
  type = "probs"
)

P_nul <- predict(
  modele_nul,
  newdata = Donnees_modele,
  type = "probs"
)

# Garantir le même ordre des colonnes
P_complet <- as.matrix(P_complet)[, classes, drop = FALSE]
P_nul <- as.matrix(P_nul)[, classes, drop = FALSE]


# ----------------------------------------------------------
# 3. INDICATRICES DES RESULTATS OBSERVES
# ----------------------------------------------------------

Y <- sapply(
  classes,
  function(classe) {
    as.numeric(Donnees_modele$resultat == classe)
  }
)

Y <- as.matrix(Y)
colnames(Y) <- classes


# ----------------------------------------------------------
# 4. CALCUL DES CRITERES DE PERFORMANCE
# ----------------------------------------------------------

# Eviter log(0)
epsilon <- 1e-15

P_complet <- pmax(
  pmin(P_complet, 1 - epsilon),
  epsilon
)

P_nul <- pmax(
  pmin(P_nul, 1 - epsilon),
  epsilon
)

# Log-loss moyenne
logloss <- function(P, Y) {
  -mean(rowSums(Y * log(P)))
}

# Brier Score multiclasses
brier <- function(P, Y) {
  mean(rowSums((P - Y)^2))
}

# Taux de bonnes classifications
accuracy <- function(P, Y) {
  mean(
    classes[max.col(P, ties.method = "first")] ==
      classes[max.col(Y, ties.method = "first")]
  )
}

# Log-loss
LL_complet <- logloss(P_complet, Y)
LL_nul <- logloss(P_nul, Y)

# Brier Score
BS_complet <- brier(P_complet, Y)
BS_nul <- brier(P_nul, Y)

# Taux de bonnes classifications
ACC_complet <- accuracy(P_complet, Y)
ACC_nul <- accuracy(P_nul, Y)

# Pseudo-R² de McFadden
R2_McFadden <- 1 - LL_complet / LL_nul

# Amélioration relative de la log-loss
amelioration_LL <- 100 * (LL_nul - LL_complet) / LL_nul

# Deviances calculées à partir des probabilités
deviance_complet <- 2 * nrow(Y) * LL_complet
deviance_nul <- 2 * nrow(Y) * LL_nul


# ----------------------------------------------------------
# 5. TABLEAU COMPARATIF
# ----------------------------------------------------------

comparaison <- data.frame(
  Critere = c(
    "Log-loss moyenne",
    "Brier Score",
    "Taux de bonnes classifications",
    "Deviance"
  ),
  
  Modele_nul = c(
    LL_nul,
    BS_nul,
    ACC_nul,
    deviance_nul
  ),
  
  Modele_complet = c(
    LL_complet,
    BS_complet,
    ACC_complet,
    deviance_complet
  )
)

comparaison$Difference <-
  comparaison$Modele_complet - comparaison$Modele_nul

cat("\n============================================\n")
cat("COMPARAISON DU MODELE NUL ET DU MODELE COMPLET\n")
cat("============================================\n\n")

print(comparaison, digits = 5, row.names = FALSE)

cat(
  "\nPseudo-R² de McFadden :",
  round(R2_McFadden, 5),
  "\n"
)

cat(
  "Amelioration relative de la log-loss :",
  round(amelioration_LL, 2),
  "%\n"
)

# ==========================================================
# 12. COMPARAISON PAR ACCURACY AVEC VALIDATION CROISEE
# ==========================================================

library(nnet)
library(dplyr)

set.seed(123)

# ----------------------------------------------------------
# 1. PARAMETRES
# ----------------------------------------------------------

K <- 10  # Nombre de plis de validation croisee

# ----------------------------------------------------------
# 2. IDENTIFICATION DES PARTIES ORIGINALES
# ----------------------------------------------------------
# Dans Donnees_modele, les observations sont ordonnees ainsi :
# d'abord les Blancs, puis les Noirs.
#
# On reconstruit un identifiant commun aux deux observations
# d'une meme partie.

n_parties <- nrow(Donnees_modele) / 2

if (n_parties != floor(n_parties)) {
  stop("Le nombre d'observations doit etre pair.")
}

Donnees_modele <- Donnees_modele |>
  mutate(
    id_partie = rep(seq_len(n_parties), each = 2)
  )

# ----------------------------------------------------------
# 3. CREATION DES PLIS
# ----------------------------------------------------------
# Chaque partie est affectee a un seul pli.
# Les deux observations d'une partie restent ensemble.

ids_parties <- sample(seq_len(n_parties))

plis_parties <- split(
  ids_parties,
  rep(seq_len(K), length.out = n_parties)
)

# ----------------------------------------------------------
# 4. FONCTION DE CALCUL DE L'ACCURACY
# ----------------------------------------------------------

accuracy <- function(observations, predictions) {
  mean(observations == predictions)
}

# Stockage des resultats
resultats_cv <- data.frame(
  pli = seq_len(K),
  accuracy_nul = NA_real_,
  accuracy_complet = NA_real_
)

# ----------------------------------------------------------
# 5. VALIDATION CROISEE
# ----------------------------------------------------------

for (k in seq_len(K)) {
  
  cat("\nValidation croisee : pli", k, "/", K, "\n")
  
  # Parties de test
  ids_test <- plis_parties[[k]]
  
  # Separation apprentissage / test
  train <- Donnees_modele |>
    filter(!id_partie %in% ids_test)
  
  test <- Donnees_modele |>
    filter(id_partie %in% ids_test)
  
  # Conserver les niveaux des facteurs du jeu complet
  train <- train |>
    mutate(
      resultat = factor(
        resultat,
        levels = levels(Donnees_modele$resultat)
      ),
      couleur = factor(
        couleur,
        levels = levels(Donnees_modele$couleur)
      ),
      opening_name_modele = factor(
        opening_name_modele,
        levels = levels(Donnees_modele$opening_name_modele)
      )
    )
  
  test <- test |>
    mutate(
      resultat = factor(
        resultat,
        levels = levels(Donnees_modele$resultat)
      ),
      couleur = factor(
        couleur,
        levels = levels(Donnees_modele$couleur)
      ),
      opening_name_modele = factor(
        opening_name_modele,
        levels = levels(Donnees_modele$opening_name_modele)
      )
    )
  
  # --------------------------------------------------------
  # MODELE NUL
  # --------------------------------------------------------
  
  modele_nul_cv <- multinom(
    resultat ~ 1,
    data = train,
    trace = FALSE
  )
  
  # --------------------------------------------------------
  # MODELE COMPLET
  # --------------------------------------------------------
  
  modele_complet_cv <- multinom(
    resultat ~ elo_diff + couleur + opening_name_modele,
    data = train,
    trace = FALSE
  )
  
  # --------------------------------------------------------
  # PREDICTIONS
  # --------------------------------------------------------
  
  pred_nul <- predict(
    modele_nul_cv,
    newdata = test,
    type = "class"
  )
  
  pred_complet <- predict(
    modele_complet_cv,
    newdata = test,
    type = "class"
  )
  
  # --------------------------------------------------------
  # ACCURACY SUR LE PLI DE TEST
  # --------------------------------------------------------
  
  resultats_cv$accuracy_nul[k] <- accuracy(
    test$resultat,
    pred_nul
  )
  
  resultats_cv$accuracy_complet[k] <- accuracy(
    test$resultat,
    pred_complet
  )
}

# ==========================================================
# 6. SYNTHESE DES RESULTATS
# ==========================================================

cat("\n============================================\n")
cat("RESULTATS DE LA VALIDATION CROISEE\n")
cat("============================================\n\n")

print(resultats_cv, digits = 4, row.names = FALSE)

# Moyennes des accuracies par pli
moyenne_nul <- mean(resultats_cv$accuracy_nul)
moyenne_complet <- mean(resultats_cv$accuracy_complet)

# Ecart-type entre les plis
sd_nul <- sd(resultats_cv$accuracy_nul)
sd_complet <- sd(resultats_cv$accuracy_complet)

# Difference d'accuracy
difference <- moyenne_complet - moyenne_nul

cat("\nAccuracy moyenne du modele nul :",
    round(moyenne_nul, 4), "\n")

cat("Ecart-type du modele nul :",
    round(sd_nul, 4), "\n")

cat("\nAccuracy moyenne du modele complet :",
    round(moyenne_complet, 4), "\n")

cat("Ecart-type du modele complet :",
    round(sd_complet, 4), "\n")

cat("\nDifference d'accuracy (complet - nul) :",
    round(difference, 4), "\n")

cat("Difference en points de pourcentage :",
    round(100 * difference, 2), "points\n")

