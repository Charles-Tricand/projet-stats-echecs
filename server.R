#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(rchess)
library(tidyverse)


# ============================================================
# SYMBOLES DES PIECES
# ============================================================

symboles <- c(
  "K" = "♔",
  "Q" = "♕",
  "R" = "♖",
  "B" = "♗",
  "N" = "♘",
  "P" = "♙",
  "k" = "♚",
  "q" = "♛",
  "r" = "♜",
  "b" = "♝",
  "n" = "♞",
  "p" = "♟"
)


# ============================================================
# OUTILS
# ============================================================

colonnes <- letters[1:8]
lignes <- 8:1


coordonnees <- function(case) {
  
  c(
    ligne = 9 - as.numeric(substr(case, 2, 2)),
    colonne = match(
      substr(case, 1, 1),
      colonnes
    )
  )
}


est_piece_blanche <- function(piece) {
  
  piece %in% c(
    "P", "R", "N", "B", "Q", "K"
  )
}


est_piece_noire <- function(piece) {
  
  piece %in% c(
    "p", "r", "n", "b", "q", "k"
  )
}

position_depuis_jeu <- function(game) {
  
  fen <- game$fen()
  
  placement <- strsplit(
    strsplit(fen, " ")[[1]][1],
    "/"
  )[[1]]
  
  board <- matrix(
    "",
    nrow = 8,
    ncol = 8
  )
  
  for (ligne in 1:8) {
    
    colonne <- 1
    
    caracteres <- strsplit(
      placement[ligne],
      ""
    )[[1]]
    
    for (caractere in caracteres) {
      
      if (grepl("[1-8]", caractere)) {
        
        colonne <- colonne +
          as.numeric(caractere)
        
      } else {
        
        board[ligne, colonne] <- caractere
        
        colonne <- colonne + 1
      }
    }
  }
  
  board
}

joueur_courant <- function(game) {
  
  fen <- game$fen()
  
  informations <- strsplit(
    fen,
    " "
  )[[1]]
  
  if (informations[2] == "w") {
    return("Blancs")
  } else {
    return("Noirs")
  }
}

etat_partie <- function(game) {
  
  if (game$in_checkmate()) {
    
    return("mat")
  }
  
  
  if (game$in_stalemate()) {
    
    return("pat")
  }
  
  
  if (game$insufficient_material()) {
    
    return("nulle_materiel")
  }
  
  
  if (game$in_check()) {
    
    return("echec")
  }
  
  
  return("en_cours")
}

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  output$graphique_elo <- renderPlot({
    
    donnees_filtrees <- Donnees_Chess %>%
      filter(
        !is.na(victoire_desavantage),
        input$couleur_desavantage == "tous" |
          couleur_desavantage == input$couleur_desavantage
      )
    
    
    # Taille des classes choisie par l'utilisateur
    taille <- as.numeric(input$taille_classe)
    
    
    # Elo maximum
    max_elo <- max(
      donnees_filtrees$elo_diff_abs,
      na.rm = TRUE
    )
    
    
    # Création des classes
    donnees_filtrees <- donnees_filtrees %>%
      mutate(
        classe_elo = cut(
          elo_diff_abs,
          breaks = seq(
            0,
            ceiling(max_elo / taille) * taille,
            by = taille
          ),
          right = FALSE,
          include.lowest = TRUE
        )
      )
    
    
    # Calcul du taux de victoire
    resume <- donnees_filtrees %>%
      group_by(classe_elo) %>%
      summarise(
        
        taux_victoire = mean(
          victoire_desavantage,
          na.rm = TRUE
        ) * 100,
        
        nombre_parties = n(),
        
        .groups = "drop"
      )
    
    
    # Graphique
    ggplot(
      resume,
      aes(
        x = classe_elo,
        y = taux_victoire
      )
    ) +
      
      geom_col(
        fill = "black"
      ) +
      
      geom_text(
        aes(
          label = paste0(
            round(taux_victoire, 1),
            "%",
            "\n(n = ",
            nombre_parties,
            ")"
          )
        ),
        vjust = -0.3
      ) +
      
      labs(
        title = "Taux de victoire du joueur désavantagé",
        x = "Différence d'Elo",
        y = "Taux de victoire (%)"
      ) +
      
      theme_minimal()
    
  })
  
  output$graphique_temps_elo <- renderPlot({
    
    Donnees_Chess$elo_classe <- cut(
      Donnees_Chess$elo_moyen,
      breaks = seq(
        floor(min(Donnees_Chess$elo_moyen, na.rm = TRUE) / 100) * 100,
        ceiling(max(Donnees_Chess$elo_moyen, na.rm = TRUE) / 100) * 100,
        by = 100
      ),
      include.lowest = TRUE
    )
    
    Donnees_graphique <- Donnees_Chess %>%
      group_by(elo_classe) %>%
      summarise(
        elo_moyen = mean(elo_moyen, na.rm = TRUE),
        coups_moyens = mean(turns, na.rm = TRUE),
        nombre_parties = n()
      )
    
    ggplot(
      Donnees_graphique,
      aes(
        x = elo_moyen,
        y = coups_moyens
      )
    ) +
      geom_line(
        color = "#2C3E50",
        linewidth = 1
      ) +
      geom_point(
        color = "#3498DB",
        size = 3
      ) +
      labs(
        title = "Nombre moyen de coups selon l'Elo moyen",
        subtitle = "Moyenne du nombre de coups par tranche de 100 points d'Elo",
        x = "Elo moyen de la partie",
        y = "Nombre moyen de coups"
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$graphique_elo_test <- renderPlot({

    # Taille des classes
    taille <- input$taille_classe


    # -------------------------------------------------------
    # Création des données
    # -------------------------------------------------------

    donnees_graphique <- Donnees_Chess %>%
      mutate(

        # Issue de la partie
        issue = case_when(

          victory_status == "mate" & winner == "white" ~
            "Victoire Blancs",

          victory_status == "mate" & winner == "black" ~
            "Victoire Noirs",

          victory_status == "draw" | winner == "draw" ~
            "Nulle",

          victory_status == "outoftime" ~
            "Temps",

          victory_status == "resign" ~
            "Abandon",

          TRUE ~ NA_character_
        ),

        # Elo moyen
        elo_moyen = (white_rating + black_rating) / 2
      ) %>%

      filter(
        !is.na(elo_moyen),
        !is.na(issue)
      )


    # -------------------------------------------------------
    # Création des classes d'Elo
    # -------------------------------------------------------

    min_elo <- floor(
      min(donnees_graphique$elo_moyen) / taille
    ) * taille

    max_elo <- ceiling(
      max(donnees_graphique$elo_moyen) / taille
    ) * taille


    # On ajoute une classe supplémentaire pour être sûr
    # que la valeur maximale soit incluse
    bornes <- seq(
      min_elo,
      max_elo + taille,
      by = taille
    )


    donnees_graphique <- donnees_graphique %>%
      mutate(
        classe_elo = cut(
          elo_moyen,
          breaks = bornes,
          right = FALSE,
          include.lowest = TRUE
        )
      )


    # -------------------------------------------------------
    # Comptage
    # -------------------------------------------------------

    resume <- donnees_graphique %>%
      count(
        classe_elo,
        issue,
        name = "nombre_parties"
      )


    # -------------------------------------------------------
    # Graphique
    # -------------------------------------------------------

    ggplot(
      resume,
      aes(
        x = classe_elo,
        y = nombre_parties,
        fill = issue
      )
    ) +

      geom_col(
        position = "fill"
      ) +

      scale_y_continuous(
        labels = scales::label_percent()
      ) +

      scale_fill_manual(
        values = c(
          "Victoire Blancs" = "green",
          "Victoire Noirs" = "red",
          "Nulle" = "grey",
          "Temps" = "blue",
          "Abandon" = "purple"
        )
      ) +

      labs(
        title = "Issue des parties selon la classe d'Elo",
        subtitle = "Proportion des différentes issues",
        x = "Classe d'Elo moyen",
        y = "Proportion",
        fill = "Issue"
      ) +

      theme_minimal(base_size = 14) +

      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        legend.position = "bottom"
      )

  })
  
  
  # ==========================================================
  # ETAT DE LA PARTIE
  # ==========================================================
  
  jeu <- reactiveVal(
    Chess$new()
  )
  
  selection <- reactiveVal(
    NULL
  )
  
  coups_possibles <- reactiveVal(
    character(0)
  )
  
  historique <- reactiveVal(
    character(0)
  )
  
  partie_terminee <- reactiveVal(
    FALSE
  )
  
  promotion_en_cours <- reactiveVal(
    NULL
  )
  
  rafraichissement <- reactiveVal(0)
  
  message <- reactiveVal(
    "Cliquez sur une pièce pour commencer."
  )
  
  # ==========================================================
  # AFFICHAGE DE L'ECHIQUIER
  # ==========================================================
  
  
  output$echiquier <- renderUI({
    
    rafraichissement()
    
    board <- position_depuis_jeu(jeu())
    
    case_selectionnee <- selection()
    
    cases <- list()
    
    
    for (ligne in 1:8) {
      
      for (colonne in 1:8) {
        
        case <- paste0(
          colonnes[colonne],
          lignes[ligne]
        )
        
        
        # ----------------------------------------------------
        # Couleur de la case
        # ----------------------------------------------------
        
        if ((ligne + colonne) %% 2 == 0) {
          
          couleur_case <- "#F0D9B5"
          
        } else {
          
          couleur_case <- "#B58863"
        }
        
        # ----------------------------------------------------
        # Coup possible
        # ----------------------------------------------------
        
        case_possible <- case %in% coups_possibles()
        
        if (case_possible) {
          
          couleur_case <- "#A9D18E"
        }
        
        
        # ----------------------------------------------------
        # Case sélectionnée
        # ----------------------------------------------------
        
        if (!is.null(case_selectionnee) &&
            case == case_selectionnee) {
          
          couleur_case <- "#F6F669"
        }
        
        
        # ----------------------------------------------------
        # Pièce
        # ----------------------------------------------------
        
        piece <- board[ligne, colonne]
        
        symbole <- ""
        
        if (piece != "") {
          symbole <- symboles[piece]
        }
        
        
        # ----------------------------------------------------
        # Bouton
        # ----------------------------------------------------
        
        cases[[length(cases) + 1]] <- actionButton(
          
          inputId = paste0("case_", case),
          
          label = symbole,
          
          width = "70px",
          
          height = "70px",
          
          onclick = sprintf(
            "Shiny.setInputValue('case_cliquee', '%s', {priority: 'event'});",
            case
          ),
          
          style = paste0(
            
            "background-color:", couleur_case, ";",
            
            "border:none;",
            
            "border-radius:0;",
            
            "padding:0;",
            
            "font-size:52px;",
            
            "line-height:70px;",
            
            "text-align:center;",
            
            "font-family:'Segoe UI Symbol',",
            "'Noto Sans Symbols 2',",
            "sans-serif;",
            
            "color:#111;",
            
            "box-shadow:none;"
          )
        )
      }
    }
    
    
    # ========================================================
    # CONTENEUR
    # ========================================================
    
    tags$div(
      
      style = paste0(
        
        "width:560px;",
        
        "height:560px;",
        
        "display:grid;",
        
        "grid-template-columns:repeat(8, 70px);",
        
        "grid-template-rows:repeat(8, 70px);",
        
        "border:4px solid #333;",
        
        "box-shadow:0 5px 15px rgba(0,0,0,0.3);"
      ),
      
      cases
    )
  })
  
  
  # ==========================================================
  # AFFICHAGE DU TOUR
  # ==========================================================
  
  output$tour <- renderText({
    
    rafraichissement()
    
    joueur_courant(
      jeu()
    )
  })
  
  
  # ==========================================================
  # AFFICHAGE DE LA SELECTION
  # ==========================================================
  
  output$selection <- renderText({
    
    if (is.null(selection())) {
      
      "Aucune"
      
    } else {
      
      selection()
    }
  })
  
  
  # ==========================================================
  # AFFICHAGE DU DERNIER COUP
  # ==========================================================
  
  output$historique <- renderText({
    
    coups <- historique()
    
    if (length(coups) == 0) {
      return("Aucun coup")
    }
    
    paste(
      coups,
      collapse = "\n"
    )
  })
  
  
  # ==========================================================
  # AFFICHAGE DU MESSAGE
  # ==========================================================
  
  output$message <- renderText({
    message()
  })
  
  # ==========================================================
  # CHOIX DE PROMOTION
  # ==========================================================
  
  output$promotion <- renderUI({
    
    promotion <- promotion_en_cours()
    
    if (is.null(promotion)) {
      return(NULL)
    }
    
    joueur <- promotion$joueur
    
    symboles_promotion <- if (joueur == "Blancs") {
      
      c(
        Q = "♕",
        R = "♖",
        B = "♗",
        N = "♘"
      )
      
    } else {
      
      c(
        Q = "♛",
        R = "♜",
        B = "♝",
        N = "♞"
      )
    }
    
    tags$div(
      
      style = paste0(
        "margin-top:15px;",
        "padding:15px;",
        "border:2px solid #333;",
        "border-radius:10px;",
        "background-color:#f5f5f5;",
        "text-align:center;"
      ),
      
      tags$div(
        style = "font-size:18px; margin-bottom:10px;",
        "Choisissez la pièce de promotion :"
      ),
      
      actionButton(
        "promotion_Q",
        symboles_promotion["Q"],
        class = "btn btn-light",
        style = "font-size:35px; margin:5px;"
      ),
      
      actionButton(
        "promotion_R",
        symboles_promotion["R"],
        class = "btn btn-light",
        style = "font-size:35px; margin:5px;"
      ),
      
      actionButton(
        "promotion_B",
        symboles_promotion["B"],
        class = "btn btn-light",
        style = "font-size:35px; margin:5px;"
      ),
      
      actionButton(
        "promotion_N",
        symboles_promotion["N"],
        class = "btn btn-light",
        style = "font-size:35px; margin:5px;"
      )
    )
  })
  
  # ==========================================================
  # GESTION DES CLICS
  # ==========================================================
  
  observeEvent(
    
    input$case_cliquee,
    
    {
      
      # ========================================================
      # CASE CLIQUEE
      # ========================================================
      
      case <- input$case_cliquee
      
      if (partie_terminee()) {
        
        return()
      }
      
      if (!is.null(promotion_en_cours())) {
        
        return()
      }
      
      # ========================================================
      # COORDONNEES
      # ========================================================
      
      coord <- coordonnees(case)
      
      ligne <- coord["ligne"]
      colonne <- coord["colonne"]
      
      
      # ========================================================
      # POSITION ACTUELLE
      # ========================================================
      
      board <- position_depuis_jeu(
        jeu()
      )
      
      
      piece <- board[
        ligne,
        colonne
      ]
      
      
      # ========================================================
      # PREMIER CLIC
      # ========================================================
      
      if (is.null(selection())) {
        
        
        # ------------------------------------------------------
        # Case vide
        # ------------------------------------------------------
        
        if (piece == "") {
          
          message(
            "Cette case est vide."
          )
          
          return()
        }
        
        
        # ------------------------------------------------------
        # Couleur de la pièce
        # ------------------------------------------------------
        
        piece_blanche <- est_piece_blanche(
          piece
        )
        
        
        # ------------------------------------------------------
        # Joueur dont c'est le tour
        # ------------------------------------------------------
        
        game <- jeu()
        
        joueur <- joueur_courant(
          game
        )
        
        
        # ------------------------------------------------------
        # Vérification du tour
        # ------------------------------------------------------
        
        if (
          (joueur == "Blancs" && !piece_blanche) ||
          (joueur == "Noirs" && piece_blanche)
        ) {
          
          message(
            paste(
              "C'est aux",
              joueur,
              "de jouer."
            )
          )
          
          return()
        }
        
        # ------------------------------------------------------
        # Recherche des coups légaux de cette pièce
        # ------------------------------------------------------
        
        game <- jeu()
        
        coups_detail <- game$moves(
          verbose = TRUE
        )
        
        coups_piece <- character(0)
        
        if (nrow(coups_detail) > 0) {
          
          coups_piece <- coups_detail$to[
            coups_detail$from == case
          ]
          
          coups_piece <- unique(
            coups_piece
          )
        }
        
        # ------------------------------------------------------
        # Aucun coup légal
        # ------------------------------------------------------
        
        if (length(coups_piece) == 0) {
          
          message(
            paste(
              "La pièce en",
              case,
              "n'a aucun coup légal."
            )
          )
          
          return()
        }
        
        # ------------------------------------------------------
        # Sauvegarde de la sélection
        # ------------------------------------------------------
        
        selection(
          case
        )
        
        coups_possibles(
          coups_piece
        )
        
        
        message(
          paste(
            "Pièce sélectionnée en",
            case,
            ". Cliquez sur la case d'arrivée."
          )
        )
        
        return()
      }
      
      # ========================================================
      # DEUXIEME CLIC
      # ========================================================
      
      depart <- selection()
      arrivee <- case
      
      # --------------------------------------------------------
      # Nouvelle sélection d'une pièce de sa propre couleur
      # --------------------------------------------------------
      
      piece_blanche <- est_piece_blanche(piece)
      
      joueur <- joueur_courant(jeu())
      
      bonne_couleur <- (
        (joueur == "Blancs" && piece_blanche) ||
          (joueur == "Noirs" && est_piece_noire(piece))
      )
      
      if (
        piece != "" &&
        bonne_couleur
      ) {
        
        game <- jeu()
        
        coups_detail <- game$moves(
          verbose = TRUE
        )
        
        coups_piece <- character(0)
        
        if (nrow(coups_detail) > 0) {
          
          coups_piece <- unique(
            coups_detail$to[
              coups_detail$from == arrivee
            ]
          )
        }
        
        if (length(coups_piece) > 0) {
          
          selection(arrivee)
          
          coups_possibles(coups_piece)
          
          message(
            paste(
              "Nouvelle pièce sélectionnée en",
              arrivee,
              "."
            )
          )
          
          return()
        }
      }
      
      # --------------------------------------------------------
      # Même case
      # --------------------------------------------------------
      
      if (depart == arrivee) {
        
        selection(NULL)
        
        coups_possibles(
          character(0)
        )
        
        message(
          "Sélection annulée."
        )
        
        return()
      }
      
      
      # ========================================================
      # RECHERCHE DU COUP LEGAL
      # ========================================================
      
      game <- jeu()
      
      coups_detail <- game$moves(
        verbose = TRUE
      )
      
      
      coup_legal <- FALSE
      coup_san <- NULL
      
      
      if (nrow(coups_detail) > 0) {
        
        correspondance <- coups_detail[
          coups_detail$from == depart &
            coups_detail$to == arrivee,
          ,
          drop = FALSE
        ]
        
        
        if (nrow(correspondance) > 0) {
          
          coup_legal <- TRUE
          
          coups_promotion <- correspondance[
            grepl("=", correspondance$san),
            ,
            drop = FALSE
          ]
          
          if (nrow(coups_promotion) > 0) {
            
            promotion_en_cours(
              list(
                depart = depart,
                arrivee = arrivee,
                joueur = joueur
              )
            )
            
            selection(NULL)
            
            coups_possibles(
              character(0)
            )
            
            message(
              "Choisissez une pièce pour la promotion."
            )
            
            return()
          }
          
          coup_san <- correspondance$san[1]
        }
        
      }
      
      
      # ========================================================
      # COUP ILLEGAL
      # ========================================================
      
      if (!coup_legal) {
        
        message(
          paste(
            "Coup illégal :",
            depart,
            "→",
            arrivee,
            ". Choisissez une case en surbrillance."
          )
        )
        
        return()
      }
      
      
      # ========================================================
      # JOUER LE COUP
      # ========================================================
      
      resultat <- game$move(
        coup_san
      )
      
      
      if (is.null(resultat)) {
        
        selection(NULL)
        
        message(
          "Impossible de jouer ce coup."
        )
        
        return()
      }
      
      
      # ========================================================
      # SAUVEGARDE DU JEU
      # ========================================================
      
      jeu(
        game
      )
      
      # ========================================================
      # INFORMATIONS
      # ========================================================
      
      historique(
        c(
          historique(),
          coup_san
        )
      )
      
      # ========================================================
      # NETTOYAGE
      # ========================================================
      
      selection(NULL)
      
      coups_possibles(
        character(0)
      )
      
      etat <- etat_partie(game)
      
      joueur <- joueur_courant(game)
      
      
      if (etat == "mat") {
        
        gagnant <- if (joueur == "Blancs") {
          "Noirs"
        } else {
          "Blancs"
        }
        
        partie_terminee(TRUE)
        
        message(
          paste(
            "Échec et mat !",
            gagnant,
            "gagnent."
          )
        )
        
        
      } else if (etat == "pat") {
        
        partie_terminee(TRUE)
        
        message(
          "Partie nulle : pat."
        )
        
        
      } else if (etat == "nulle_materiel") {
        
        partie_terminee(TRUE)
        
        message(
          "Partie nulle : matériel insuffisant."
        )
        
        
      } else if (etat == "echec") {
        
        message(
          paste(
            "Coup joué :",
            coup_san,
            "— Échec !"
          )
        )
        
        
      } else {
        
        message(
          paste(
            "Coup joué :",
            coup_san
          )
        )
      }
    },
    
    ignoreInit = TRUE
  )
  
  # ==========================================================
  # PROMOTION
  # ==========================================================
  
  jouer_promotion <- function(piece_promotion) {
    
    promotion <- promotion_en_cours()
    
    if (is.null(promotion)) {
      return()
    }
    
    game <- jeu()
    
    coups_detail <- game$moves(
      verbose = TRUE
    )
    
    correspondance <- coups_detail[
      coups_detail$from == promotion$depart &
        coups_detail$to == promotion$arrivee &
        grepl(
          paste0("=", piece_promotion),
          coups_detail$san
        ),
      ,
      drop = FALSE
    ]
    
    if (nrow(correspondance) == 0) {
      
      message(
        "Promotion impossible."
      )
      
      return()
    }
    
    coup_san <- correspondance$san[1]
    
    resultat <- game$move(
      coup_san
    )
    
    if (is.null(resultat)) {
      
      message(
        "Impossible de jouer la promotion."
      )
      
      return()
    }
    
    jeu(
      game
    )
    
    rafraichissement(
      rafraichissement() + 1
    )
    
    historique(
      c(
        historique(),
        coup_san
      )
    )
    
    promotion_en_cours(NULL)
    
    etat <- etat_partie(game)
    joueur <- joueur_courant(game)
    
    if (etat == "mat") {
      
      gagnant <- if (joueur == "Blancs") {
        "Noirs"
      } else {
        "Blancs"
      }
      
      partie_terminee(TRUE)
      
      message(
        paste(
          "Échec et mat !",
          gagnant,
          "gagnent."
        )
      )
      
    } else if (etat == "pat") {
      
      partie_terminee(TRUE)
      
      message(
        "Partie nulle : pat."
      )
      
    } else if (etat == "nulle_materiel") {
      
      partie_terminee(TRUE)
      
      message(
        "Partie nulle : matériel insuffisant."
      )
      
    } else if (etat == "echec") {
      
      message(
        paste(
          "Coup joué :",
          coup_san,
          "— Échec !"
        )
      )
      
    } else {
      
      message(
        paste(
          "Coup joué :",
          coup_san
        )
      )
    }
  }
  
  observeEvent(
    input$promotion_Q,
    {
      jouer_promotion("Q")
    }
  )
  
  observeEvent(
    input$promotion_R,
    {
      jouer_promotion("R")
    }
  )
  
  observeEvent(
    input$promotion_B,
    {
      jouer_promotion("B")
    }
  )
  
  observeEvent(
    input$promotion_N,
    {
      jouer_promotion("N")
    }
  )
  
  # ==========================================================
  # NOUVELLE PARTIE
  # ==========================================================
  
  observeEvent(
    
    input$nouvelle_partie,
    
    {
      
      jeu(
        Chess$new()
      )
      
      partie_terminee(FALSE)
      
      promotion_en_cours(NULL)
      
      selection(
        NULL
      )
      
      coups_possibles(
        character(0)
      )
      
      historique(
        character(0)
      )
      
      message(
        "Nouvelle partie. Aux Blancs de jouer."
      )
    }
  )
}