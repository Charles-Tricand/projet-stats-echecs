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

library(dplyr)
mm_id<-Donnees_Chess |> group_by(id) |> filter(n() > 1) |> ungroup() 

id_pareil <- Donnees_Chess |> filter(id == "7NKtCk3B")


library(rchess)

library(dplyr)

top_100 <- Donnees_Chess %>%
  count(opening_eco, sort = TRUE) %>%
  slice_head(n = 100)

df_top100 <- Donnees_Chess %>%
  filter(opening_eco %in% top_100$opening_eco)


bottom_100 <- Donnees_Chess %>%
  count(opening_eco, sort = FALSE) %>%
  slice_min(n, n = 100)

df_bottom100 <- Donnees_Chess %>%
  filter(opening_eco %in% bottom_100$opening_eco)


library(ggplot2)

ggplot(df_bottom100, aes(x = white_rating, fill = opening_eco))+
  geom_histogram(binwidth = 100, position = "dodge") +
  labs(
    title = "Répartition des ouvertures selon l'Elo",
    x = "Elo",
    y = "Nombre de parties",
    fill = "Ouverture"
  ) +
  theme_minimal()


ggplot(df_top100, aes(x = white_rating, fill = opening_eco)) +
  geom_histogram(binwidth = 100, position = "dodge") +
  labs(
    title = "Répartition des ouvertures selon l'Elo",
    x = "Elo",
    y = "Nombre de parties",
    fill = "Ouverture"
  ) +
  theme_minimal()

top_20<- Donnees_Chess %>%
  count(opening_eco, sort = TRUE) %>%
  slice_head(n = 20)

df_top20 <- Donnees_Chess %>%
  filter(opening_eco %in% top_20$opening_eco)


ggplot(df_top20, aes(x = white_rating, fill = opening_eco)) +
  geom_histogram(binwidth = 100, position = "dodge") +
  labs(
    title = "Répartition des ouvertures selon l'Elo",
    x = "Elo",
    y = "Nombre de parties",
    fill = "Ouverture"
  ) +
  theme_minimal()


top<- Donnees_Chess %>%
  count(opening_eco, sort = TRUE)


ggplot(top)

top_20 <- Donnees_Chess %>%
  count(opening_eco, sort = TRUE) %>%
  slice_head(n = 20)

df_proportion <- Donnees_Chess %>%
  mutate(
    elo_bin = floor(white_rating / 100) * 100
  ) %>%
  count(elo_bin, opening_eco) %>%
  group_by(elo_bin) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  ungroup() %>%
  filter(opening_eco %in% top_20$opening_eco)

ggplot(df_proportion, aes(x = elo_bin, y = proportion, color = opening_eco)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Proportion des ouvertures selon l'Elo des Blancs",
    x = "Elo des Blancs",
    y = "Proportion des parties",
    color = "Ouverture"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


top_20 <- Donnees_Chess %>%
  count(opening_eco, sort = TRUE) %>%
  slice_head(n = 20)

df_proportion <- Donnees_Chess %>%
  mutate(
    elo_bin = floor(white_rating / 100) * 100
  ) %>%
  count(elo_bin, opening_eco) %>%
  group_by(elo_bin) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  ungroup() %>%
  filter(opening_eco %in% top_20$opening_eco)

ggplot(df_proportion, aes(x = elo_bin, y = proportion)) +
  geom_col() +
  facet_wrap(~ opening_eco, ncol = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion des ouvertures selon l'Elo des Blancs",
    x = "Elo des Blancs",
    y = "Proportion des parties"
  ) +
  theme_minimal()

top_20 <- Donnees_Chess %>%
  count(opening_eco, sort = TRUE) %>%
  slice_head(n = 20)

df_proportion <- Donnees_Chess %>%
  mutate(
    elo_bin = floor(white_rating / 100) * 100
  ) %>%
  count(elo_bin, opening_eco) %>%
  group_by(elo_bin) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  ungroup() %>%
  filter(opening_eco %in% top_20$opening_eco)

ggplot(df_proportion, aes(
  x = elo_bin,
  y = proportion,
  fill = opening_eco
)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion des ouvertures selon l'Elo des Blancs",
    x = "Elo des Blancs",
    y = "Proportion des parties",
    fill = "Ouverture"
  ) +
  theme_minimal()


top_20 <- Donnees_Chess %>%
  count(opening_eco, sort = TRUE) %>%
  slice_head(n = 20)

df_proportion <- Donnees_Chess %>%
  mutate(
    elo_bin = case_when(
      white_rating < 1000 ~ "<1000",
      white_rating >= 2000 ~ "2000+",
      TRUE ~ paste0(
        floor(white_rating / 100) * 100,
        "-",
        floor(white_rating / 100) * 100 + 99
      )
    )
  ) %>%
  count(elo_bin, opening_eco) %>%
  group_by(elo_bin) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  ungroup() %>%
  filter(opening_eco %in% top_20$opening_eco)

ggplot(df_proportion, aes(
  x = elo_bin,
  y = proportion,
  fill = opening_eco
)) +
  geom_col(position = "dodge") +
  
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion des ouvertures selon l'Elo des Blancs",
    x = "Elo des Blancs",
    y = "Proportion des parties",
    fill = "Ouverture"
  ) +
  theme_minimal()




top_20 <- Donnees_Chess %>%
  count(opening_eco, sort = TRUE) %>%
  slice_head(n = 20)

df_proportion <- Donnees_Chess %>%
  mutate(
    elo_bin = case_when(
      white_rating < 1000 ~ "<1000",
      white_rating >= 2000 ~ "2000+",
      TRUE ~ paste0(
        floor(white_rating / 100) * 100,
        "-",
        floor(white_rating / 100) * 100 + 99
      )
    )
  ) %>%
  count(elo_bin, opening_eco) %>%
  group_by(elo_bin) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  ungroup() %>%
  filter(opening_eco %in% top_20$opening_eco)

# Ordonner correctement les tranches d'Elo
df_proportion <- df_proportion %>%
  mutate(
    elo_bin = factor(
      elo_bin,
      levels = c(
        "<1000",
        paste0(seq(1000, 1900, 100), "-", seq(1099, 1999, 100)),
        "2000+"
      )
    )
  )

ggplot(
  df_proportion,
  aes(
    x = elo_bin,
    y = proportion,
    group = opening_eco,
    color = opening_eco
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    title = "Évolution de la popularité des ouvertures selon l'Elo",
    subtitle = "Proportion des parties jouées par les Blancs",
    x = "Elo des Blancs",
    y = "Proportion des parties",
    color = "Ouverture"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


library(dplyr)
library(ggplot2)
library(scales)

# ============================================================
# 1. CREATION DES TRANCHES D'ELO
# ============================================================

df_top10 <- Donnees_Chess %>%
  mutate(
    elo_bin = case_when(
      white_rating < 1000 ~ "<1000",
      white_rating >= 2000 ~ "2000+",
      TRUE ~ paste0(
        floor(white_rating / 100) * 100,
        "-",
        floor(white_rating / 100) * 100 + 99
      )
    )
  ) %>%
  
  # Comptage des parties par tranche d'Elo et par ouverture
  count(elo_bin, opening_eco, name = "n") %>%
  
  # Proportion de chaque ouverture dans sa tranche d'Elo
  group_by(elo_bin) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  
  # Trier les ouvertures de la plus populaire à la moins populaire
  arrange(desc(n), .by_group = TRUE) %>%
  
  # Garder exactement les 10 premières dans CHAQUE tranche
  slice_head(n = 10) %>%
  
  ungroup()


# ============================================================
# 2. ORDONNER LES TRANCHES D'ELO
# ============================================================

df_top10 <- df_top10 %>%
  mutate(
    elo_bin = factor(
      elo_bin,
      levels = c(
        "<1000",
        paste0(
          seq(1000, 1900, 100),
          "-",
          seq(1099, 1999, 100)
        ),
        "2000+"
      )
    )
  )


# ============================================================
# 3. VERIFICATION
# ============================================================

# Cette commande doit afficher 10 pour chaque tranche d'Elo
df_top10 %>%
  count(elo_bin)


# ============================================================
# 4. GRAPHIQUE
# ============================================================

ggplot(
  df_top10,
  aes(
    x = reorder(opening_eco, proportion),
    y = proportion
  )
) +
  geom_col() +
  
  facet_wrap(
    ~ elo_bin,
    ncol = 3,
    scales = "free_x"
  ) +
  
  scale_y_continuous(
    labels = scales::percent
  ) +
  
  labs(
    title = "Les 10 ouvertures les plus populaires selon le niveau Elo",
    subtitle = "Proportion des parties dans chaque tranche d'Elo",
    x = "Ouverture (ECO)",
    y = "Proportion des parties"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid.minor = element_blank()
  )

# ============================================================
# VERIFIER LES OUVERTURES DU GRAPHIQUE
# ============================================================

# Les 20 ouvertures les plus populaires toutes tranches d'Elo confondues
top20_global <- Donnees_Chess %>%
  count(opening_eco, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(opening_eco)


# Toutes les ouvertures présentes dans les top 10 par tranche d'Elo
ouvertures_graphique <- df_top10 %>%
  distinct(opening_eco) %>%
  pull(opening_eco)


# Ouvertures du graphique qui font partie du top 20 global
ouvertures_dans_top20 <- intersect(
  ouvertures_graphique,
  top20_global
)


# Ouvertures du graphique qui NE font PAS partie du top 20 global
ouvertures_hors_top20 <- setdiff(
  ouvertures_graphique,
  top20_global
)


# ============================================================
# RESULTATS
# ============================================================

cat("Nombre d'ouvertures différentes dans le graphique :",
    length(ouvertures_graphique), "\n\n")

cat("Ouvertures du graphique appartenant au TOP 20 global :\n")
print(ouvertures_dans_top20)

cat("\nOuvertures du graphique qui ne sont PAS dans le TOP 20 global :\n")
print(ouvertures_hors_top20)


# ============================================================
# GRAPHIQUE INTERACTIF : TOP 15 OUVERTURES PAR TRANCHE D'ELO
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# 1. CREATION DES TRANCHES D'ELO
# ------------------------------------------------------------

Donnees_Chess <- Donnees_Chess %>%
  mutate(
    elo_bin = case_when(
      white_rating < 1000 ~ "<1000",
      white_rating >= 2000 ~ "2000+",
      TRUE ~ paste0(
        floor(white_rating / 100) * 100,
        "-",
        floor(white_rating / 100) * 100 + 99
      )
    )
  )

# Ordre des tranches
elo_levels <- c(
  "<1000",
  paste0(
    seq(1000, 1900, 100),
    "-",
    seq(1099, 1999, 100)
  ),
  "2000+"
)

Donnees_Chess$elo_bin <- factor(
  Donnees_Chess$elo_bin,
  levels = elo_levels
)

# ------------------------------------------------------------
# 2. INTERFACE
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Popularité des ouvertures selon le niveau Elo"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId = "elo",
        label = "Choisissez une tranche d'Elo :",
        choices = elo_levels,
        selected = "1500-1599"
      )
      
    ),
    
    mainPanel(
      
      plotOutput(
        outputId = "top_openings",
        height = "600px"
      )
      
    )
  )
)

# ------------------------------------------------------------
# 3. SERVEUR
# ------------------------------------------------------------

server <- function(input, output) {
  
  output$top_openings <- renderPlot({
    
    # Sélection de la tranche d'Elo choisie
    df <- Donnees_Chess %>%
      filter(elo_bin == input$elo) %>%
      
      # Comptage des parties par ouverture
      count(opening_eco, sort = TRUE) %>%
      
      # Garder les 15 ouvertures les plus jouées
      slice_head(n = 15) %>%
      
      # Calcul de la proportion dans la tranche d'Elo
      mutate(
        proportion = n / sum(n),
        opening_eco = reorder(opening_eco, proportion)
      )
    
    # --------------------------------------------------------
    # GRAPHIQUE
    # --------------------------------------------------------
    
    ggplot(
      df,
      aes(
        x = opening_eco,
        y = proportion
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      scale_y_continuous(
        labels = percent
      ) +
      
      labs(
        title = paste(
          "Les 15 ouvertures les plus jouées",
          "pour les joueurs Elo", input$elo
        ),
        subtitle = "Proportion des parties dans la tranche d'Elo sélectionnée",
        x = "Ouverture (code ECO)",
        y = "Proportion des parties"
      ) +
      
      theme_minimal() +
      
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10)
      )
  })
}

# ------------------------------------------------------------
# 4. LANCER L'APPLICATION
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)





