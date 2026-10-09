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
          "Remplace ce texte par une courte présentation ",
          "de ton guide et de son objectif."
        ),
        
        div(
          class = "guide-tags",
          
          span("Les règles"),
          span("Les ouvertures"),
          span("Les stratégies")
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
      
      uiOutput(ns("contenu")),
      
      # ----------------------------------------------------
      # PIED DE PAGE
      # ----------------------------------------------------
      
      div(
        class = "guide-footer",
        icon("graduation-cap"),
        span("Un espace de référence à compléter.")
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
    
    
    # ------------------------------------------------------
    # CONTENU DU GUIDE
    #
    # C'est ici que tu ajouteras tes textes.
    # Chaque rubrique contient un titre, une introduction
    # et des sections de contenu.
    # ------------------------------------------------------
    
    contenu_guide <- list(
      
      regles = list(
        
        titre = "Les règles des échecs",
        
        description =
          "Ajoute ici une introduction sur les règles du jeu.",
        
        icone = "chess-pawn",
        
        couleur = "sable",
        
        sections = list(
          
          list(
            titre = "Le plateau et les pièces",
            texte = paste(
              "Rédige ici ton texte sur le plateau,",
              "les pièces et leur position initiale."
            )
          ),
          
          list(
            titre = "Les déplacements",
            texte = paste(
              "Explique ici comment se déplacent",
              "le roi, la dame, les tours, les fous,",
              "les cavaliers et les pions."
            )
          ),
          
          list(
            titre = "Échec, échec et mat",
            texte = paste(
              "Ajoute ici tes explications sur",
              "l'échec, le mat et les conditions de victoire."
            )
          ),
          
          list(
            titre = "Les règles particulières",
            texte = paste(
              "Présente ici le roque, la prise en passant",
              "et la promotion du pion."
            )
          )
        )
      ),
      
      ouvertures = list(
        
        titre = "Les ouvertures",
        
        description =
          "Ajoute ici une introduction aux premiers coups d'une partie.",
        
        icone = "book-open",
        
        couleur = "vert",
        
        sections = list(
          
          list(
            titre = "Qu'est-ce qu'une ouverture ?",
            texte = paste(
              "Rédige ici ta définition d'une ouverture",
              "et explique son rôle dans une partie."
            )
          ),
          
          list(
            titre = "Les grands principes",
            texte = paste(
              "Présente ici le développement des pièces,",
              "le contrôle du centre et la sécurité du roi."
            )
          ),
          
          list(
            titre = "Les grandes familles d'ouvertures",
            texte = paste(
              "Ajoute ici les ouvertures que tu souhaites",
              "présenter et leurs caractéristiques."
            )
          ),
          
          list(
            titre = "Comprendre la notation",
            texte = paste(
              "Explique ici comment lire une suite de coups",
              "comme e4, Cf3 ou F b5, selon la notation choisie."
            )
          )
        )
      ),
      
      strategie = list(
        
        titre = "Stratégie et tactique",
        
        description =
          "Ajoute ici une introduction aux idées qui guident les joueurs.",
        
        icone = "brain",
        
        couleur = "violet",
        
        sections = list(
          
          list(
            titre = "Matériel et valeur des pièces",
            texte = "Rédige ici ton contenu."
          ),
          
          list(
            titre = "Les tactiques essentielles",
            texte = "Rédige ici ton contenu."
          ),
          
          list(
            titre = "Les plans stratégiques",
            texte = "Rédige ici ton contenu."
          ),
          
          list(
            titre = "Les finales",
            texte = "Rédige ici ton contenu."
          )
        )
      ),
      
      elo = list(
        
        titre = "Le classement Elo",
        
        description =
          "Ajoute ici une introduction au système de classement.",
        
        icone = "chart-no-axes-combined",
        
        couleur = "bleu",
        
        sections = list(
          
          list(
            titre = "Qu'est-ce que le classement Elo ?",
            texte = "Rédige ici ton contenu."
          ),
          
          list(
            titre = "Comment interpréter une différence Elo ?",
            texte = "Rédige ici ton contenu."
          ),
          
          list(
            titre = "Probabilités et résultats",
            texte = "Rédige ici ton contenu."
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
                    
                    p(section$texte)
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