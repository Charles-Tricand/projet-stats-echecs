# Visualisation dynamique d'une partie d'échecs — preuve de faisabilité
# Dépendances : shiny, rchess (remotes::install_github("jbkunst/rchess"))
#
# rchess sert à lire le PGN et à rejouer les coups. L'échiquier est un
# chessboard.js créé une seule fois côté navigateur, puis piloté par messages :
# les pièces glissent d'une case à l'autre au lieu d'être redessinées.

library(shiny)
library(rchess)

# Partie de démonstration : Morphy – Duc de Brunswick & Comte Isouard, Paris 1858
pgn_defaut <- '[Event "Opéra de Paris"]
[White "Paul Morphy"]
[Black "Duc de Brunswick et Comte Isouard"]
[Result "1-0"]

1. e4 e5 2. Nf3 d6 3. d4 Bg4 4. dxe5 Bxf3 5. Qxf3 dxe5 6. Bc4 Nf6 7. Qb3 Qe7
8. Nc3 c6 9. Bg5 b5 10. Nxb5 cxb5 11. Bxb5+ Nbd7 12. O-O-O Rd8 13. Rxd7 Rxd7
14. Rd1 Qe6 15. Bxd7+ Nxd7 16. Qb8+ Nxb8 17. Rd8# 1-0'

# chessboard.js et ses pièces (en data URI) sont fournis par rchess : pas
# besoin de connexion internet. Shiny apporte déjà jQuery.
dep_echiquier <- htmltools::htmlDependency(
  "chessboardjs", "0.3.0",
  src = system.file("htmlwidgets/lib", package = "rchess"),
  script = c("chessboard-0.3.0.min.js", "chessboardjs.themes.js",
             "chessboardjs.themes.data.js"),
  stylesheet = "chessboard-0.3.0.min.css"
)

js_echiquier <- "
var echiquier = null, dernier = null;

function obtenirPlateau() {
  if (!echiquier) {
    echiquier = ChessBoard('plateau', {
      position: 'start', pieceTheme: chess24_theme,
      moveSpeed: 350, showNotation: true
    });
    $(window).resize(function() { echiquier.resize(); surligner(); });
  }
  return echiquier;
}

function surligner() {
  $('#plateau .square-55d63').removeClass('surbrillance');
  if (dernier && dernier.de) {
    $('#plateau .square-' + dernier.de + ', #plateau .square-' + dernier.vers)
      .addClass('surbrillance');
  }
  $('.coup').removeClass('actif');
  if (dernier) $('.coup[data-ply=' + dernier.ply + ']').addClass('actif');
}

Shiny.addCustomMessageHandler('position', function(m) {
  dernier = m;
  obtenirPlateau().position(m.fen, m.anime);
  surligner();
});

Shiny.addCustomMessageHandler('retourner', function(m) {
  obtenirPlateau().flip();
  surligner();
});

$(document).on('click', '.coup', function() {
  Shiny.setInputValue('clic_coup', +$(this).data('ply'), {priority: 'event'});
});

// Flèches du clavier : navigation ; espace : lecture / pause
$(document).on('keydown', function(e) {
  if ($(e.target).is('textarea, input')) return;
  var action = {ArrowLeft: 'prec', ArrowRight: 'suiv', Home: 'debut',
                End: 'fin', ' ': 'lecture'}[e.key];
  if (action) {
    e.preventDefault();
    Shiny.setInputValue('touche', action, {priority: 'event'});
  }
});
"

css_echiquier <- "
#plateau { width: 440px; }
.surbrillance { box-shadow: inset 0 0 0 100px rgba(255, 205, 0, 0.45); }
.liste-coups { max-height: 440px; overflow-y: auto; font-family: monospace; }
.liste-coups td { padding: 2px 8px; }
.coup { cursor: pointer; padding: 1px 4px; border-radius: 3px; }
.coup:hover { background: #e8e8e8; }
.coup.actif { background: #337ab7; color: white; }
.statut { font-size: 1.2em; font-weight: bold; min-height: 1.5em; }
.prises { font-size: 1.6em; min-height: 1.4em; letter-spacing: 2px; }
"

# Rejoue la partie coup par coup et renvoie les positions successives
analyser_pgn <- function(pgn) {
  jeu <- Chess$new()
  ok <- jeu$load_pgn(pgn)
  if (!isTRUE(ok)) stop("PGN invalide")
  coups <- jeu$history(verbose = TRUE)
  plateau <- Chess$new()
  fens <- plateau$fen()
  for (san in coups$san) {
    plateau$move(san)
    fens <- c(fens, plateau$fen())
  }
  list(coups = coups, fens = fens, entetes = jeu$get_header())
}

# Pièces capturées : différence entre la dotation initiale et la position
symboles <- c(P = "♙", N = "♘", B = "♗", R = "♖", Q = "♕",
              p = "♟", n = "♞", b = "♝", r = "♜", q = "♛")
dotation <- c(P = 8, N = 2, B = 2, R = 2, Q = 1, p = 8, n = 2, b = 2, r = 2, q = 1)

pieces_prises <- function(fen) {
  lettres <- strsplit(sub(" .*", "", fen), "")[[1]]
  presentes <- table(factor(lettres, levels = names(dotation)))
  manquantes <- pmax(dotation - as.integer(presentes), 0)
  noms <- rep(names(dotation), manquantes)
  blanc <- noms %in% LETTERS
  list(blanches = paste(symboles[noms[blanc]], collapse = ""),
       noires = paste(symboles[noms[!blanc]], collapse = ""))
}

ui <- fluidPage(
  dep_echiquier,
  tags$head(tags$style(HTML(css_echiquier)), tags$script(HTML(js_echiquier))),
  titlePanel("Visualisation d'une partie d'échecs"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textAreaInput("pgn", "PGN de la partie", value = pgn_defaut,
                    rows = 10, width = "100%"),
      actionButton("charger", "Charger la partie", class = "btn-primary"),
      hr(),
      uiOutput("curseur"),
      div(
        actionButton("debut", icon("backward-fast")),
        actionButton("prec", icon("backward-step")),
        actionButton("lecture", icon("play")),
        actionButton("suiv", icon("forward-step")),
        actionButton("fin", icon("forward-fast")),
        actionButton("retourner", icon("rotate"), title = "Retourner l'échiquier")
      ),
      br(),
      sliderInput("vitesse", "Délai entre deux coups (s)",
                  min = 0.5, max = 3, value = 1, step = 0.1),
      helpText("Clavier : flèches gauche / droite, Début / Fin, espace pour la lecture.",
               "Cliquez sur un coup de la liste pour y aller.")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(7,
               h4(textOutput("joueurs")),
               div(class = "prises", textOutput("prises_noires")),
               div(id = "plateau"),
               div(class = "prises", textOutput("prises_blanches")),
               div(class = "statut", textOutput("titre_coup"))),
        column(5,
               h4("Coups joués"),
               div(class = "liste-coups", uiOutput("liste_coups")))
      ),
      verbatimTextOutput("fen")
    )
  )
)

server <- function(input, output, session) {
  partie <- reactiveVal(analyser_pgn(pgn_defaut))
  ply <- reactiveVal(0)          # 0 = position initiale
  en_lecture <- reactiveVal(FALSE)
  n_plies <- reactive(nrow(partie()$coups))

  observeEvent(input$charger, {
    res <- tryCatch(analyser_pgn(input$pgn), error = function(e) NULL)
    if (is.null(res)) {
      showNotification("PGN non reconnu", type = "error")
    } else {
      partie(res); ply(0); en_lecture(FALSE)
    }
  })

  # Curseur recréé à chaque nouvelle partie (borne max variable)
  output$curseur <- renderUI({
    sliderInput("ply_slider", "Demi-coup", min = 0, max = n_plies(),
                value = isolate(ply()), step = 1, width = "100%")
  })
  observeEvent(input$ply_slider, ply(input$ply_slider), ignoreInit = TRUE)
  observeEvent(ply(), {
    if (!identical(input$ply_slider, ply()))
      updateSliderInput(session, "ply_slider", value = ply())
  })

  aller <- function(i) ply(max(0, min(n_plies(), i)))
  basculer_lecture <- function() {
    if (!en_lecture() && ply() >= n_plies()) ply(0)
    en_lecture(!en_lecture())
  }
  observeEvent(input$debut, aller(0))
  observeEvent(input$prec,  aller(ply() - 1))
  observeEvent(input$suiv,  aller(ply() + 1))
  observeEvent(input$fin,   aller(n_plies()))
  observeEvent(input$lecture, basculer_lecture())
  observeEvent(input$clic_coup, { en_lecture(FALSE); aller(input$clic_coup) })
  observeEvent(input$retourner, session$sendCustomMessage("retourner", list()))
  observeEvent(input$touche, switch(input$touche,
    prec = aller(ply() - 1), suiv = aller(ply() + 1),
    debut = aller(0), fin = aller(n_plies()), lecture = basculer_lecture()))

  # Icône du bouton lecture selon l'état
  observeEvent(en_lecture(), updateActionButton(
    session, "lecture", icon = icon(if (en_lecture()) "pause" else "play")))

  # Lecture automatique
  observe({
    req(en_lecture())
    invalidateLater(input$vitesse * 1000)
    isolate({
      if (ply() < n_plies()) ply(ply() + 1) else en_lecture(FALSE)
    })
  })

  fen_courante <- reactive(partie()$fens[ply() + 1])

  # Envoi de la position au navigateur, qui anime le déplacement
  observe({
    i <- ply()
    cp <- if (i > 0) partie()$coups[i, ] else NULL
    session$sendCustomMessage("position", list(
      fen = fen_courante(), anime = TRUE, ply = i,
      de = if (!is.null(cp)) cp$from, vers = if (!is.null(cp)) cp$to))
  })

  output$fen <- renderText(paste("FEN :", fen_courante()))

  output$joueurs <- renderText({
    e <- partie()$entetes
    if (is.null(e$White)) return("")
    paste(e$White, "–", e$Black, if (!is.null(e$Result)) paste0("(", e$Result, ")"))
  })

  prises <- reactive(pieces_prises(fen_courante()))
  output$prises_noires <- renderText(prises()$blanches)   # prises par les Noirs
  output$prises_blanches <- renderText(prises()$noires)   # prises par les Blancs

  output$titre_coup <- renderText({
    i <- ply()
    if (i == 0) return("Position initiale")
    cp <- partie()$coups[i, ]
    etat <- if (grepl("#", cp$san)) " — échec et mat"
            else if (grepl("\\+", cp$san)) " — échec"
            else if (grepl("c", cp$flags)) " — prise" else ""
    sprintf("%d.%s %s%s", ceiling(i / 2),
            if (cp$color == "w") "" else "..", cp$san, etat)
  })

  output$liste_coups <- renderUI({
    san <- partie()$coups$san
    if (length(san) == 0) return(NULL)
    actif <- isolate(ply())
    cellule <- function(k) {
      if (k > length(san)) return(tags$td())
      tags$td(tags$span(class = paste("coup", if (k == actif) "actif"),
                        `data-ply` = k, san[k]))
    }
    lignes <- lapply(seq(1, length(san), by = 2), function(k)
      tags$tr(tags$td((k + 1) / 2, "."), cellule(k), cellule(k + 1)))
    tags$table(lignes)
  })
}

shinyApp(ui, server)
