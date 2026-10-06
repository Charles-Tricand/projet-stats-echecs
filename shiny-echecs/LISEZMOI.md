# Visualisation d'une partie d'échecs — application Shiny

Preuve de faisabilité : lecture d'une partie au format PGN et rejeu animé sur un
échiquier interactif.

## Installation

Testé avec R 4.5.1 et shiny 1.11.1.

```r
install.packages(c("shiny", "remotes"))
remotes::install_github("jbkunst/rchess")   # rchess n'est plus sur CRAN
```

## Lancement

```r
shiny::runApp("shiny-echecs")
```

## Utilisation

- Collez une partie au format PGN puis cliquez sur « Charger la partie ».
- Naviguez avec les boutons, le curseur ou un clic sur un coup de la liste.
- Clavier : flèches gauche / droite, Début / Fin, espace pour la lecture automatique.
- Le bouton de rotation retourne l'échiquier.

## Fonctionnement

- rchess lit le PGN et calcule la position (FEN) après chaque demi-coup.
- L'échiquier chessboard.js est créé une seule fois dans le navigateur ; le
  serveur ne lui envoie que la nouvelle position, ce qui permet l'animation des
  pièces.
- chessboard.js et les images des pièces sont fournis par rchess : aucune
  connexion internet n'est nécessaire.
