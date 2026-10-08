# Module : échiquier interactif (pièces blanches seules, sans pièces noires)
#
# Principe
# - L'échiquier est un chessboard.js (fourni par rchess, donc sans connexion
#   internet) créé une seule fois côté navigateur.
# - Le joueur glisse-dépose une pièce blanche. Le navigateur envoie le coup
#   (case de départ, case d'arrivée) au serveur.
# - Le serveur vérifie la légalité du coup avec les règles de déplacement ci-dessous
#   puis répond toujours par la position faisant foi : si le coup est illégal,
#   la pièce revient à sa place.
# - Comme il n'y a aucune pièce noire, il n'y a ni prise, ni échec, ni prise en
#   passant : un coup est légal si la pièce se déplace comme elle en a le droit,
#   sans sauter de pièce, vers une case vide. Le roque est géré, et un pion
#   arrivé sur la dernière rangée devient une dame.
#
# Dépendance : rchess, uniquement pour les fichiers chessboard.js et les images
# des pièces  (remotes::install_github("jbkunst/rchess"))

# ---- Constantes --------------------------------------------------------------

# Position de départ sans les pièces noires. Le 3e champ donne les roques encore
# possibles (K = petit roque, Q = grand roque, - = aucun).
fen_depart <- "8/8/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 0 1"

dep_echiquier_interactif <- function() {
  htmltools::htmlDependency(
    "chessboardjs", "0.3.0",
    src = system.file("htmlwidgets/lib", package = "rchess"),
    script = c("chessboard-0.3.0.min.js", "chessboardjs.themes.js",
               "chessboardjs.themes.data.js"),
    stylesheet = "chessboard-0.3.0.min.css"
  )
}

css_echiquier_interactif <- "
.plateau-echecs { width: 440px; }
.plateau-echecs .surbrillance { box-shadow: inset 0 0 0 100px rgba(255, 205, 0, 0.45); }
.echiquier-coups { font-family: monospace; max-height: 300px; overflow-y: auto; }
.echiquier-coups .coup { display: inline-block; padding: 1px 6px; margin: 1px; background: #fff; border-radius: 3px; }
"

js_echiquier_interactif <- r"---(
(function() {
  var plateaux = {};   // un état par échiquier (identifiant = id du div)

  function surligner(id) {
    var e = plateaux[id], $p = $('#' + id);
    $p.find('.square-55d63').removeClass('surbrillance');
    if (e.dernier && e.dernier.de) {
      $p.find('.square-' + e.dernier.de + ', .square-' + e.dernier.vers)
        .addClass('surbrillance');
    }
  }

  function obtenir(id) {
    if (plateaux[id]) return plateaux[id];
    var etat = { board: null, dernier: null, verrou: false };
    var champ = $('#' + id).attr('data-input');   // nom (avec espace de noms) de l'input Shiny

    etat.board = ChessBoard(id, {
      pieceTheme: chess24_theme,
      draggable: true,
      dropOffBoard: 'snapback',
      moveSpeed: 250, snapbackSpeed: 250, snapSpeed: 60,
      showNotation: true,
      onDragStart: function(source, piece) {
        return !etat.verrou && piece.charAt(0) === 'w';
      },
      onDrop: function(source, target) {
        if (target === 'offboard' || target === source) return 'snapback';
        etat.verrou = true;   // levé à la réponse du serveur
        Shiny.setInputValue(champ,
          { de: source, vers: target, nonce: Math.random() },
          { priority: 'event' });
      }
    });
    plateaux[id] = etat;
    return etat;
  }

  // Réponse du serveur : position faisant foi (coup accepté, refusé, annulé...)
  Shiny.addCustomMessageHandler('echiquier_position', function(m) {
    var e = obtenir(m.id);
    e.dernier = m;
    e.verrou = false;
    e.board.position(m.fen, true);
    surligner(m.id);
  });

  // L'échiquier se trouve dans un onglet : le redimensionner à l'affichage
  function redimensionner() {
    $.each(plateaux, function(id, e) { e.board.resize(); surligner(id); });
  }
  $(window).on('resize', redimensionner);
  $(document).on('shown.bs.tab', redimensionner);
})();
)---"

# ---- Règles de déplacement (Blancs seuls) --------------------------------------

# Lecture d'un FEN : matrice m[rangée, colonne], rangée 1 = en bas, "." = vide
fen_vers_plateau <- function(fen) {
  rangs <- strsplit(sub(" .*", "", fen), "/")[[1]]   # rangs[1] = 8e rangée
  m <- matrix(".", nrow = 8, ncol = 8)
  for (i in 1:8) {
    col <- 1
    for (ch in strsplit(rangs[i], "")[[1]]) {
      if (grepl("[1-8]", ch)) {
        col <- col + as.integer(ch)
      } else {
        m[9 - i, col] <- ch
        col <- col + 1
      }
    }
  }
  m
}

plateau_vers_fen <- function(m, droits) {
  rangs <- vapply(8:1, function(r) {
    sortie <- ""
    vides <- 0
    for (ch in m[r, ]) {
      if (ch == ".") {
        vides <- vides + 1
      } else {
        if (vides > 0) sortie <- paste0(sortie, vides)
        vides <- 0
        sortie <- paste0(sortie, ch)
      }
    }
    if (vides > 0) sortie <- paste0(sortie, vides)
    sortie
  }, character(1))
  paste0(paste(rangs, collapse = "/"), " w ", droits, " - 0 1")
}

droits_du_fen <- function(fen) strsplit(fen, " ")[[1]][3]

nom_case <- function(col, rang) paste0(letters[col], rang)

# Teste le déplacement (col0, rang0) -> (col1, rang1).
# Renvoie NULL s'il est illégal, sinon list(roque = NULL, "K" ou "Q").
deplacement_legal <- function(m, droits, col0, rang0, col1, rang1) {
  piece <- m[rang0, col0]
  if (!piece %in% c("P", "N", "B", "R", "Q", "K")) return(NULL)
  if (col0 == col1 && rang0 == rang1) return(NULL)
  if (m[rang1, col1] != ".") return(NULL)       # pas de pièce noire : pas de prise

  dc <- col1 - col0
  dr <- rang1 - rang0
  adc <- abs(dc)
  adr <- abs(dr)

  # Les cases situées entre départ et arrivée sont-elles vides ?
  voie_libre <- function() {
    n <- max(adc, adr)
    if (n <= 1) return(TRUE)
    all(vapply(seq_len(n - 1), function(k) {
      m[rang0 + k * sign(dr), col0 + k * sign(dc)] == "."
    }, logical(1)))
  }

  ok <- switch(
    piece,
    P = dc == 0 && (dr == 1 || (dr == 2 && rang0 == 2 && m[3, col0] == ".")),
    N = (adc == 1 && adr == 2) || (adc == 2 && adr == 1),
    B = adc == adr && voie_libre(),
    R = (adc == 0 || adr == 0) && voie_libre(),
    Q = (adc == adr || adc == 0 || adr == 0) && voie_libre(),
    K = max(adc, adr) == 1
  )

  roque <- NULL
  if (!ok && piece == "K" && rang0 == 1 && col0 == 5 && rang1 == 1) {
    if (col1 == 7 && grepl("K", droits, fixed = TRUE) &&
        m[1, 6] == "." && m[1, 8] == "R") {
      ok <- TRUE
      roque <- "K"
    }
    if (col1 == 3 && grepl("Q", droits, fixed = TRUE) &&
        all(m[1, 2:4] == ".") && m[1, 1] == "R") {
      ok <- TRUE
      roque <- "Q"
    }
  }

  if (!isTRUE(ok)) return(NULL)
  list(roque = roque)
}

# Notation algébrique (SAN) du coup, avant qu'il soit joué
notation_san <- function(m, droits, col0, rang0, col1, rang1, coup) {
  piece <- m[rang0, col0]
  if (!is.null(coup$roque)) return(if (coup$roque == "K") "O-O" else "O-O-O")

  cible <- nom_case(col1, rang1)
  if (piece == "P") return(paste0(cible, if (rang1 == 8) "=Q" else ""))

  # Autres pièces identiques pouvant aussi aller sur la case d'arrivée
  autres <- which(m == piece, arr.ind = TRUE)      # colonnes : row, col
  autres <- autres[!(autres[, 1] == rang0 & autres[, 2] == col0), , drop = FALSE]
  rivaux <- autres[vapply(seq_len(nrow(autres)), function(i) {
    !is.null(deplacement_legal(m, droits, autres[i, 2], autres[i, 1], col1, rang1))
  }, logical(1)), , drop = FALSE]

  prefixe <- ""
  if (nrow(rivaux) > 0) {
    prefixe <- if (!any(rivaux[, 2] == col0)) letters[col0]
               else if (!any(rivaux[, 1] == rang0)) as.character(rang0)
               else nom_case(col0, rang0)
  }
  paste0(piece, prefixe, cible)
}

# Joue le coup de..vers (ex. "e2", "e4") dans la position `fen`.
# Renvoie list(san, fen) ou NULL si le coup est illégal.
jouer_coup <- function(fen, de, vers) {
  if (!is.character(de) || !is.character(vers) ||
      nchar(de) != 2 || nchar(vers) != 2) return(NULL)
  col0 <- match(substr(de, 1, 1), letters[1:8])
  col1 <- match(substr(vers, 1, 1), letters[1:8])
  rang0 <- suppressWarnings(as.integer(substr(de, 2, 2)))
  rang1 <- suppressWarnings(as.integer(substr(vers, 2, 2)))
  if (anyNA(c(col0, col1, rang0, rang1)) ||
      any(c(rang0, rang1) < 1) || any(c(rang0, rang1) > 8)) return(NULL)

  m <- fen_vers_plateau(fen)
  droits <- droits_du_fen(fen)
  coup <- deplacement_legal(m, droits, col0, rang0, col1, rang1)
  if (is.null(coup)) return(NULL)

  piece <- m[rang0, col0]
  san <- notation_san(m, droits, col0, rang0, col1, rang1, coup)

  m[rang0, col0] <- "."
  m[rang1, col1] <- if (piece == "P" && rang1 == 8) "Q" else piece   # promotion en dame
  if (identical(coup$roque, "K")) { m[1, 8] <- "."; m[1, 6] <- "R" }
  if (identical(coup$roque, "Q")) { m[1, 1] <- "."; m[1, 4] <- "R" }

  # Droits de roque perdus si le roi ou la tour concernée a bougé
  if (piece == "K") droits <- gsub("[KQ]", "", droits)
  if (piece == "R" && rang0 == 1 && col0 == 8) droits <- sub("K", "", droits, fixed = TRUE)
  if (piece == "R" && rang0 == 1 && col0 == 1) droits <- sub("Q", "", droits, fixed = TRUE)
  if (!nzchar(droits)) droits <- "-"

  list(san = san, fen = plateau_vers_fen(m, droits))
}

partie_initiale <- function() {
  list(fens = fen_depart, sans = character(0),
       de = character(0), vers = character(0))
}

# ---- Interface ---------------------------------------------------------------

mod_echiquier_interactif_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Echiquier ouvertures",
    dep_echiquier_interactif(),
    tags$head(tags$style(HTML(css_echiquier_interactif))),
    h2("Echiquier interactif"),
    p(
      "Jouez un véritable début de partie et observez ",
      "l'ouverture que vous êtes en train de réaliser"
    ),
    fluidRow(
      column(
        3,
        wellPanel(
          actionButton(ns("annuler"), "Annuler", icon = icon("rotate-left")),
          actionButton(ns("recommencer"), "Recommencer",
                       icon = icon("arrows-rotate")),
          hr(),
          h4("Coups joués"),
          div(class = "echiquier-coups", uiOutput(ns("liste_coups")))
        )
      ),
      column(
        9,
        div(id = ns("plateau"), class = "plateau-echecs",
            `data-input` = ns("coup"))
      )
    ),
    # Le script est dans le corps de la page, après le chargement de Shiny
    tags$script(HTML(js_echiquier_interactif))
  )
}

# ---- Serveur -----------------------------------------------------------------

mod_echiquier_interactif_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    id_plateau <- session$ns("plateau")
    hist <- reactiveVal(partie_initiale())

    # Envoie au navigateur la position faisant foi
    envoyer <- function(h) {
      n <- length(h$sans)
      session$sendCustomMessage("echiquier_position", list(
        id = id_plateau,
        fen = h$fens[n + 1],
        de = if (n > 0) h$de[n] else "",
        vers = if (n > 0) h$vers[n] else ""
      ))
    }

    # Position initiale, annulation, remise à zéro : tout changement de
    # l'historique est répercuté sur l'échiquier
    observe(envoyer(hist()))

    observeEvent(input$coup, {
      h <- hist()
      n <- length(h$sans)

      res <- tryCatch(
        jouer_coup(h$fens[n + 1], input$coup$de, input$coup$vers),
        error = function(e) NULL
      )

      if (is.null(res)) {
        showNotification("Coup illégal.", type = "warning", duration = 2)
        envoyer(h)   # la pièce revient à sa place
      } else {
        hist(list(
          fens = c(h$fens, res$fen),
          sans = c(h$sans, res$san),
          de   = c(h$de, input$coup$de),
          vers = c(h$vers, input$coup$vers)
        ))
      }
    })

    observeEvent(input$annuler, {
      h <- hist()
      n <- length(h$sans)
      req(n > 0)
      hist(list(
        fens = h$fens[seq_len(n)],
        sans = h$sans[seq_len(n - 1)],
        de   = h$de[seq_len(n - 1)],
        vers = h$vers[seq_len(n - 1)]
      ))
    })

    observeEvent(input$recommencer, hist(partie_initiale()))

    output$liste_coups <- renderUI({
      san <- hist()$sans
      if (length(san) == 0) return(helpText("Aucun coup joué pour l'instant."))
      lapply(seq_along(san), function(i) span(class = "coup", paste0(i, ". ", san[i])))
    })
  })
}
