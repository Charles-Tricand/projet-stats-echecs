# -------- IMPORTATION DES DONNEES ------------

library(readr)
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

