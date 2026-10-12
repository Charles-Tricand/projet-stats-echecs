# ==========================================================
# MODULE : GUIDE D'APPRENTISSAGE DES ECHECS
# Fichier : R/mod_guide_echecs.R
# ==========================================================


# ==========================================================
# 1. INTERFACE UTILISATEUR
# ==========================================================

mod_guide_echecs_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    title = "Guide des échecs",
    
    div(
      class = "guide-echecs",
      
      # ----------------------------------------------------
      # EN-TETE
      # ----------------------------------------------------
      
      div(
        class = "guide-hero",
        
        div(
          class = "guide-eyebrow",
          icon("book-open"),
          span("LE GUIDE DES ECHECS")
        ),
        
        h1("Comprendre les échecs"),
        
        p(
          class = "guide-intro",
          "Ce guide permet de comprendre les notions étudiées au dans cette application"
        )
      ),
      
      # ----------------------------------------------------
      # BARRE DE RECHERCHE
      # ----------------------------------------------------
      
      div(
        class = "guide-search",
        
        textInput(
          ns("recherche"),
          label = NULL,
          placeholder = "Rechercher une notion, un terme...",
          width = "100%"
        )
      ),
      
      # ----------------------------------------------------
      # INDICATIONS DE NAVIGATION
      # ----------------------------------------------------
      
      div(
        class = "guide-navigation",
        
        h4("Explorer le guide"),
        
        p(
          "Sélectionne une rubrique pour consulter son contenu."
        ),
        
        fluidRow(
          
          column(
            6,
            actionLink(
              ns("tout_ouvrir"),
              label = tagList(
                icon("chevrons-down"),
                " Tout développer"
              )
            )
          ),
          
          column(
            6,
            actionLink(
              ns("tout_fermer"),
              label = tagList(
                icon("chevrons-up"),
                " Tout réduire"
              )
            )
          )
        )
      ),
      
      # ----------------------------------------------------
      # CONTENU PEDAGOGIQUE
      # ----------------------------------------------------
      
      withMathJax(
        uiOutput(ns("contenu"))
      )
    )
  )
}


# ==========================================================
# 2. SERVEUR
# ==========================================================

mod_guide_echecs_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # ------------------------------------------------------
    # ETAT D'OUVERTURE DES RUBRIQUES
    # ------------------------------------------------------
    
    ouvert <- reactiveVal(character(0))
    
    observeEvent(input$tout_ouvrir, {
      
      ouvert(names(contenu_guide))
      
    })
    
    observeEvent(input$tout_fermer, {
      
      ouvert(character(0))
      
    })
    
    contenu_guide <- list(
      
      regles = list(
        
        titre = "Les règles des échecs",
        
        description =
          "Les échecs sont un jeu de stratégie opposant deux joueurs. Le but est de mettre le Roi adverse échec et mat, tout en évitant que son propre Roi soit capturé.",
        
        icone = "chess-pawn",
        
        couleur = "sable",
        
        sections = list(
          
          list(
            titre = "Le plateau et les pièces",
            texte = paste(
              "Un échiquier comporte 64 cases, réparties en 8 colonnes et 8 rangées.",
              "Les couleurs des cases qui composent l'échiquier alternent entre le noir et le blanc sur toute sa surface.
              Chacun des deux joueurs se voit attribuer une couleur entre le blanc ou le noir ainsi qu'un total de 16 pièces
              On distingue 6 types de pièces : 8 Pions, 2 Tours, 2 Cavaliers, 2 Fous, 1 Dame et enfin 1 Roi.
              Ces différents types de pièces ont toutes des déplacements qui leurs sont prores et permettent de priver l'adversaire de ses propres pièces.
              Les pièces occupent les deux premières rangées de chaque camp. Sur la rangée arrière, on trouve les tours dans les coins, puis les cavaliers et les fous. La dame se place sur une case de sa propre couleur et le roi sur l'autre case centrale. Les huit pions se placent devant ces pièces.
              Les blancs jouent toujours le premier coup, puis les joueurs jouent à tour de rôle."
            )
          ),
          
          list(
            titre = "Les déplacements",
            texte = paste(
              "Chaque pièce possède ses propres règles de déplacement :
                  Le pion avance d'une case vers l'avant. Lors de son premier déplacement, il peut avancer de deux cases si le chemin est libre. Il capture une pièce adverse en diagonale, d'une case. Lorsqu'il atteint la dernière rangée, il est promu et remplacé par une dame, une tour, un fou ou un cavalier.
                  La tour se déplace horizontalement ou verticalement, sur autant de cases que possible, tant qu'aucune pièce ne bloque son chemin.
                  Le cavalier se déplace en forme de L : deux cases dans une direction, puis une case perpendiculairement. Il est le seul à pouvoir sauter par-dessus les autres pièces.
                  Le fou se déplace en diagonale, sans limite de distance, tant que son chemin est libre.
                  La dame combine les déplacements de la tour et du fou : elle se déplace en ligne droite ou en diagonale.
                  Le roi se déplace d'une seule case dans n'importe quelle direction, à condition de ne pas se mettre en échec.
              
              Une pièce ne peut pas terminer son déplacement sur une case occupée par une pièce de son propre camp. Lorsqu'elle arrive sur une case occupée par une pièce adverse, elle la capture et la retire de l'échiquier."
            )
          ),
          
          list(
            titre = "L'échec, l'échec et mat et le pat",
            texte = paste(
              "Un roi est en échec lorsqu'une pièce adverse menace de le capturer. Le joueur doit alors obligatoirement supprimer cette menace, par exemple en déplaçant son roi, en capturant la pièce attaquante ou en interposant une pièce lorsque cela est possible.
               Lorsque le roi est en échec et qu'aucun coup légal ne permet de le sauver, il y a échec et mat : la partie est gagnée par l'adversaire.
               Le pat est une situation différente : un joueur n'a aucun coup légal à jouer, mais son roi n'est pas en échec. La partie est alors nulle. Une partie peut également se terminer par un abandon ou par d'autres situations de nullité."
            )
          )
        )
      ),
      
      ouvertures = list(
        
        titre = "Les ouvertures",
        
        description =
          "L'ouverture est la première phase d'une partie d'échecs. Elle correspond aux premiers coups joués par les blancs et les noirs. Elle permet de développer ses pièces, de prendre position au centre de l'échiquier et de préparer la suite de la partie.",
        
        icone = "book-open",
        
        couleur = "vert",
        
        sections = list(
          
          list(
            titre = "Les grands principes",
            texte = paste(
              "Au début d'une partie, les joueurs cherchent généralement à développer leurs pièces, à contrôler le centre et à mettre leur roi en sécurité.
               Certaines ouvertures privilégient un développement rapide et une partie tactique, tandis que d'autres conduisent à des positions plus stratégiques. Le choix de l'ouverture influence les positions que les joueurs devront ensuite comprendre et jouer."
            )
          ),
          
          list(
            titre = "Les grandes familles d'ouvertures",
            texte = paste(
              "Il existe de très nombreuses ouvertures, que l'on peut regrouper selon les premiers coups joués.
              On distingue notamment :
              
              Les ouvertures ouvertes, qui commencent par 1. e4 e5. Elles conduisent souvent à des positions où le jeu peut s'ouvrir rapidement.
              Les ouvertures semi-ouvertes, dans lesquelles les blancs jouent 1. e4 et les noirs répondent autrement que par 1... e5. La défense sicilienne, qui commence par 1. e4 c5, en est un exemple.
              Les ouvertures fermées, souvent associées au premier coup 1. d4 et à des positions où les pions peuvent se retrouver face à face.
              Les ouvertures de flanc, qui commencent par un coup tel que 1. c4 ou 1. Cf3, plutôt que par l'avancée immédiate d'un pion central."
            )
          ),
          
          list(
            titre = "Comprendre la notation",
            texte = paste(
              "La notation algébrique permet d'enregistrer les coups d'une partie et de rejouer une succession de déplacements.

              Chaque case est identifiée par une lettre, de a à h, pour la colonne, et un chiffre, de 1 à 8, pour la rangée.
              Pour un pion, on écrit simplement la case d'arrivée, le coup e4 indique donc qu'un pion se déplace en e4.
              
              Chaque pièce est identifiée par une lettre sauf pour les pions.
              'N' pour les Cavaliers (Knight), 'R' pour les Tours (Rook), 'B' pour les Fous (Bishop), 'Q' pour la Dame (Queen) et 'K' pour le Roi (King).
              
              Le signe x indique une capture, + indique un échec et # indique un échec et mat.

              Les ouvertures peuvent également être répertoriées à l'aide du code ECO, composé d'une lettre de A à E et de deux chiffres, par exemple B01 pour la défense scandinave. Ce code permet de classer les ouvertures et leurs variantes."
            )
          )
        )
      ),
      
      elo = list(
        
        titre = "Le classement Elo",
        
        description =
          "Le classement Elo est un indicateur numérique permettant d'estimer le niveau relatif des joueurs d'échecs. En général, un classement plus élevé correspond à un niveau de jeu plus fort. Il évolue au fil des résultats obtenus face à d'autres joueurs.",
        
        icone = "chart-no-axes-combined",
        
        couleur = "bleu",
        
        sections = list(
          
          list(
            titre = "Comment déterminer l'élo d'un joueur ?",
            
            texte = c(
              "Le système Elo classique repose sur la comparaison entre le résultat attendu d'une partie et son résultat réel.",
              
              "Avant une partie, le classement des deux joueurs permet d'estimer leur score attendu.",
              
              HTML(
                "La différence de classement entre les deux joueurs est notée \\(D\\) :"
              ),
              
              "\\[ D = R_A - R_B \\]",
              
              "Le score attendu du joueur \\(A\\) se calcule avec la formule :",
               
              "\\[ E_A = \\frac{1}{1 + 10^{-D/400}} \\]",
              
              "Après la partie, le classement est actualisé selon la formule :",
              
              "\\[ R'_A = R_A + K(S_A - E_A) \\]",
              
              HTML(
                "où \\(K\\) détermine l'ampleur de la variation, \\(S_A\\) est le résultat réel et \\(E_A\\) le score attendu."
              )
            )
          ),
          
          list(
            titre = "Comment interpréter une différence Elo ?",
            
            texte = c(
              "La différence de classement entre deux joueurs contribue à l'estimation du score attendu avant une partie.
                    Aux échecs, une partie peut se terminer par une victoire, une défaite ou une nulle. Le score attendu correspond à la probabilité de victoire à laquelle on ajoute la moitié de la probabilité de nulle.",
              
              "Avec Glicko-2, le score attendu dépend également de l'incertitude associée au classement des adversaires.",
              
              "Le facteur qui tient compte de l'incertitude de l'adversaire est :",
              
              "\\[ g(\\phi_B) = \\frac{1}{\\sqrt{1 + \\frac{3\\phi_B^2}{\\pi^2}}} \\]",
              
              HTML(
                "Le score attendu du joueur \\(A\\) est alors :"
              ),
              
              "\\[ E_A = \\frac{1}{1 + \\exp[-g(\\phi_B)(\\mu_A - \\mu_B)]} \\]",
              
              "Dans notre application, le graphique de la page « Probabilité de victoire » permet de comparer les estimations théoriques aux résultats observés dans le jeu de données."
            )
          )
        )
      )
    )
    
    
    # ------------------------------------------------------
    # RECHERCHE DANS LE GUIDE
    # ------------------------------------------------------
    
    contenu_filtre <- reactive({
      
      terme <- trimws(
        tolower(input$recherche %||% "")
      )
      
      if (terme == "") {
        return(contenu_guide)
      }
      
      resultat <- list()
      
      for (id_rubrique in names(contenu_guide)) {
        
        rubrique <- contenu_guide[[id_rubrique]]
        
        sections <- Filter(
          function(section) {
            
            texte <- paste(
              rubrique$titre,
              rubrique$description,
              section$titre,
              section$texte
            )
            
            grepl(
              terme,
              tolower(texte),
              fixed = TRUE
            )
          },
          rubrique$sections
        )
        
        if (
          grepl(
            terme,
            tolower(rubrique$titre),
            fixed = TRUE
          ) ||
          length(sections) > 0
        ) {
          
          rubrique$sections <- sections
          
          resultat[[id_rubrique]] <- rubrique
        }
      }
      
      resultat
    })
    
    
    # ------------------------------------------------------
    # AFFICHAGE DU CONTENU
    # ------------------------------------------------------
    
    output$contenu <- renderUI({
      
      rubriques <- contenu_filtre()
      
      if (length(rubriques) == 0) {
        
        return(
          div(
            class = "guide-empty",
            
            icon("search-x", class = "fa-3x"),
            
            h4("Aucun résultat"),
            
            p(
              "Essaie avec un autre mot-clé."
            )
          )
        )
      }
      
      lapply(names(rubriques), function(id_rubrique) {
        
        rubrique <- rubriques[[id_rubrique]]
        
        est_ouverte <- (
          id_rubrique %in% ouvert() ||
            nzchar(input$recherche %||% "")
        )
        
        div(
          class = paste(
            "guide-card",
            paste0("guide-", rubrique$couleur)
          ),
          
          div(
            class = "guide-card-header",
            
            div(
              class = "guide-card-icon",
              icon(rubrique$icone)
            ),
            
            div(
              class = "guide-card-heading",
              
              h3(rubrique$titre),
              
              p(rubrique$description)
            ),
            
            actionLink(
              session$ns(
                paste0("ouvrir_", id_rubrique)
              ),
              label = NULL,
              icon(
                if (est_ouverte) {
                  "chevron-up"
                } else {
                  "chevron-down"
                }
              )
            )
          ),
          
          if (est_ouverte) {
            
            div(
              class = "guide-card-body",
              
              lapply(
                seq_along(rubrique$sections),
                function(i) {
                  
                  section <- rubrique$sections[[i]]
                  
                  div(
                    class = "guide-section",
                    
                    h4(section$titre),
                    
                    lapply(section$texte, function(element) {
                      
                      if (startsWith(trimws(element), "\\[")) {
                        
                        # Formule mathématique LaTeX
                        div(
                          class = "guide-formule",
                          withMathJax(
                            HTML(
                              paste0(
                                "<div> ",
                                element,
                                " </div>"
                              )
                            )
                          )
                        )
                        
                      } else {
                        
                        # Texte normal, conservé tel quel
                        div(
                          class = "guide-texte",
                          HTML(element)
                        )
                        
                      }
                      
                    })
                    
                  )
                }
              )
            )
          }
        )
      })
    })
    
    
    # ------------------------------------------------------
    # OUVERTURE INDIVIDUELLE DES RUBRIQUES
    # ------------------------------------------------------
    
    lapply(names(contenu_guide), function(id_rubrique) {
      
      local({
        
        id_local <- id_rubrique
        
        observeEvent(
          input[[paste0("ouvrir_", id_local)]],
          {
            
            courant <- ouvert()
            
            if (id_local %in% courant) {
              
              ouvert(
                setdiff(courant, id_local)
              )
              
            } else {
              
              ouvert(
                union(courant, id_local)
              )
            }
            
          },
          ignoreInit = TRUE
        )
      })
    })
    
  })
}