# Module : échiquier interactif + arbre des ouvertures
#
# Échiquier
# - chessboard.js (fourni par rchess, donc sans connexion internet), créé une
#   seule fois côté navigateur.
# - Le joueur déplace les pièces par glisser-déposer, Blancs puis Noirs
#   alternativement. Chaque coup est envoyé au serveur, qui vérifie sa légalité
#   avec rchess (chess.js) et répond toujours par la position faisant foi :
#   un coup illégal ramène la pièce à sa place. Roque, prise en passant et
#   échecs sont gérés ; un pion promu devient une dame.
#
# Arbre des ouvertures
# - Construit une fois depuis `Donnees_Chess` : chaque partie contribue par les
#   `opening_ply` premiers demi-coups (coups blancs ET noirs) de son ouverture.
# - Les ouvertures sont regroupées en grandes familles ("Sicilian Defense",
#   "Italian Game"...) : on retire les suffixes "#2" et ce qui suit ":" ou "|".
# - Un nœud dont toutes les parties appartiennent à une seule famille n'est
#   pas développé davantage : l'arbre reste lisible.
# - L'arbre affiché part de la position courante et se restreint à chaque coup.
# - Les noms d'ouverture sont des liens : un clic remet l'échiquier au départ et
#   rejoue la ligne correspondante (celle de la branche pour une famille unique,
#   sinon la ligne la plus fréquente de la famille parmi les parties restantes).
#
# Dépendances : rchess (remotes::install_github("jbkunst/rchess")) et l'objet
# `Donnees_Chess` (colonnes moves, opening_eco, opening_name, opening_ply).

# ---- Constantes --------------------------------------------------------------

fen_depart <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
RACINE <- "racine"   # clé du nœud « aucun coup joué »

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
.echiquier-coups { font-family: monospace; max-height: 420px; overflow-y: auto; }
.echiquier-table td { padding: 1px 8px 1px 0; }

.arbre-panneau { max-height: 560px; overflow-y: auto; font-size: 0.9em; }
.arbre-ouverture { background: #eaf4ea; border-radius: 4px; padding: 6px 10px; margin-bottom: 8px; }
.arbre-info { color: #666; margin-bottom: 8px; }
.arbre-panneau ul { list-style: none; margin: 0; padding-left: 16px; border-left: 1px solid #ccc; }
.arbre-racine > ul { border-left: none; padding-left: 0; }
.arbre-panneau li { padding: 2px 0; }
.arbre-coup { font-family: monospace; font-weight: bold; }
.arbre-n { color: #888; margin: 0 6px; font-size: 0.85em; }
.arbre-fam { color: #337ab7; }
.arbre-fam.pur { color: #2e7d32; font-weight: bold; }
.arbre-reste { color: #999; font-style: italic; }
.arbre-panneau a.lien-ouverture { color: inherit; cursor: pointer; text-decoration: underline dotted; }
.arbre-panneau a.lien-ouverture:hover { text-decoration: underline; }
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
    var etat = { board: null, dernier: null, verrou: false, fin: false, trait: 'w' };
    var champ = $('#' + id).attr('data-input');   // nom (avec espace de noms) de l'input Shiny

    etat.board = ChessBoard(id, {
      pieceTheme: chess24_theme,
      draggable: true,
      dropOffBoard: 'snapback',
      moveSpeed: 250, snapbackSpeed: 250, snapSpeed: 60,
      showNotation: true,
      // Seules les pièces du camp qui a le trait sont déplaçables
      onDragStart: function(source, piece) {
        return !etat.verrou && !etat.fin && piece.charAt(0) === etat.trait;
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
    e.trait = m.trait;
    e.fin = !!m.fin;
    e.verrou = false;
    e.board.position(m.fen, true);
    surligner(m.id);
  });

  // L'échiquier se trouve dans un onglet : le redimensionner à l'affichage
  function redimensionner() {
    $.each(plateaux, function(id, e) { e.board.resize(); surligner(id); });
  }
  // Clic sur un nom d'ouverture : on envoie la ligne de coups au serveur
  $(document).on('click', '.lien-ouverture', function(ev) {
    ev.preventDefault();
    var champ = $(this).closest('.arbre-panneau').attr('data-input-ligne');
    var ligne = $(this).attr('data-ligne');
    if (champ && ligne) {
      Shiny.setInputValue(champ, { ligne: ligne, nonce: Math.random() },
                          { priority: 'event' });
    }
  });

  $(window).on('resize', redimensionner);
  $(document).on('shown.bs.tab', redimensionner);
})();
)---"

# ---- Partie : historique et coups ----------------------------------------------

partie_initiale <- function() {
  list(fens = fen_depart, sans = character(0),
       de = character(0), vers = character(0))
}

trait_du_fen <- function(fen) strsplit(fen, " ")[[1]][2]

# Plus aucun coup légal (mat ou pat) ?
partie_finie <- function(jeu, fen) {
  jeu$load(fen)
  length(jeu$moves()) == 0
}

# Joue le coup de..vers dans la position `fen`.
# Renvoie list(san, fen) ou NULL si le coup est illégal.
jouer_coup <- function(jeu, fen, de, vers) {
  jeu$load(fen)
  legaux <- jeu$moves(verbose = TRUE)
  if (is.null(legaux) || NROW(legaux) == 0) return(NULL)

  candidats <- legaux[legaux$from == de & legaux$to == vers, , drop = FALSE]
  if (nrow(candidats) == 0) return(NULL)

  # Plusieurs candidats = promotion : toujours en dame
  if (nrow(candidats) > 1) {
    dames <- candidats[grepl("=Q", candidats$san, fixed = TRUE), , drop = FALSE]
    if (nrow(dames) > 0) candidats <- dames
  }

  san <- as.character(candidats$san[1])
  jeu$move(san)
  list(san = san, fen = jeu$fen())
}

# Joue le coup `san` (ex. "Nf3") dans la position `fen`.
# Renvoie list(san, fen, de, vers) ou NULL si ce coup n'est pas légal.
jouer_san <- function(jeu, fen, san) {
  jeu$load(fen)
  legaux <- jeu$moves(verbose = TRUE)
  if (is.null(legaux) || NROW(legaux) == 0) return(NULL)
  nettoyer <- function(x) sub("[+#]$", "", as.character(x))
  candidats <- legaux[nettoyer(legaux$san) == nettoyer(san), , drop = FALSE]
  if (nrow(candidats) == 0) return(NULL)
  coup <- as.character(candidats$san[1])
  jeu$move(coup)
  list(san = coup, fen = jeu$fen(),
       de = as.character(candidats$from[1]), vers = as.character(candidats$to[1]))
}

# ---- Arbre des ouvertures --------------------------------------------------------

nom_propre <- function(noms) trimws(gsub("\\s+", " ", gsub("\\s*#[0-9]+", "", noms)))

# Grande famille : "Sicilian Defense: Najdorf Variation #2" -> "Sicilian Defense"
famille_ouverture <- function(noms) {
  f <- gsub("\\s*#[0-9]+", "", noms)
  trimws(sub("\\s*[:|].*$", "", f))
}

# Table triée des effectifs, sous forme de vecteur d'entiers nommé
compter <- function(x) {
  t <- sort(table(x), decreasing = TRUE)
  stats::setNames(as.integer(t), names(t))
}

construire_arbre <- function(donnees) {
  ok <- !is.na(donnees$moves) & !is.na(donnees$opening_ply) &
    !is.na(donnees$opening_name) & donnees$opening_ply > 0
  donnees <- donnees[ok, , drop = FALSE]

  coups    <- strsplit(as.character(donnees$moves), " ", fixed = TRUE)
  ply      <- as.integer(donnees$opening_ply)
  noms_brut <- as.character(donnees$opening_name)
  noms     <- nom_propre(noms_brut)
  familles <- famille_ouverture(noms_brut)
  eco      <- as.character(donnees$opening_eco)

  # Coups de l'ouverture de chaque partie (les `opening_ply` premiers demi-coups)
  ops <- Map(function(mv, p) mv[seq_len(min(p, length(mv)))], coups, ply)
  longueurs <- lengths(ops)

  # Table « une ligne par partie et par demi-coup »
  jeu      <- rep(seq_along(ops), longueurs)
  coup     <- unlist(ops, use.names = FALSE)
  prof     <- sequence(longueurs)
  cle      <- unlist(lapply(ops, function(o) {
    Reduce(function(a, b) paste(a, b), o, accumulate = TRUE)
  }), use.names = FALSE)
  parent   <- ifelse(prof == 1, RACINE, sub(" [^ ]+$", "", cle))
  terminal <- prof == longueurs[jeu]
  fam      <- familles[jeu]

  # Nombre de parties passant par chaque nœud
  tab <- table(cle)
  n <- stats::setNames(as.integer(tab), names(tab))
  n <- c(n, stats::setNames(length(ops), RACINE))

  # Familles possibles à chaque nœud
  fam_noeud <- lapply(split(fam, cle), compter)
  fam_noeud[[RACINE]] <- compter(familles)

  # Enfants de chaque nœud
  premiere <- !duplicated(cle)
  E <- data.frame(parent = parent[premiere], cle = cle[premiere],
                  coup = coup[premiere], stringsAsFactors = FALSE)
  E$n <- unname(n[E$cle])
  enfants <- split(E, E$parent)

  # Ouvertures reconnues exactement à un nœud (la partie y termine son ouverture)
  idx <- which(terminal)
  terminaux <- lapply(split(noms[jeu[idx]], cle[idx]), compter)
  eco_nom <- vapply(split(eco, noms), function(x) x[1], character(1))

  # Lignes d'ouverture : une par nœud terminal, avec sa famille et sa fréquence
  tt <- table(cle[idx])
  premier <- match(names(tt), cle[idx])
  lignes <- data.frame(cle = names(tt), n = as.integer(tt),
                       famille = fam[idx][premier], stringsAsFactors = FALSE)

  list(n = n, cles = names(n), familles = fam_noeud, enfants = enfants,
       terminaux = terminaux, eco = eco_nom, lignes = lignes)
}

# Le calcul se fait une seule fois pour toutes les sessions
cache_echiquier <- new.env()

obtenir_arbre <- function(donnees) {
  if (is.null(cache_echiquier$arbre)) {
    cache_echiquier$arbre <- construire_arbre(donnees)
  }
  cache_echiquier$arbre
}

# "e4" au demi-coup 1 -> "1. e4" ; "e5" au demi-coup 2 -> "1... e5"
etiquette_coup <- function(ply, coup) {
  if (ply %% 2 == 1) sprintf("%d. %s", (ply + 1) %/% 2, coup)
  else sprintf("%d... %s", ply %/% 2, coup)
}

# "e4 e5 Nf3" -> "1. e4 e5 2. Nf3"
ligne_texte <- function(ligne) {
  coups <- strsplit(ligne, " ", fixed = TRUE)[[1]]
  i <- seq_along(coups)
  num <- ifelse(i %% 2 == 1, paste0((i + 1) %/% 2, "."), "")
  paste(trimws(paste(num, coups)), collapse = " ")
}

# Nom cliquable : un clic joue `ligne` depuis la position de départ
lien_ouverture <- function(texte, ligne) {
  if (is.null(ligne) || length(ligne) == 0 || !nzchar(ligne)) return(texte)
  tags$a(href = "#", class = "lien-ouverture", `data-ligne` = ligne,
         title = paste("Jouer :", ligne_texte(ligne)), texte)
}

# Ligne la plus jouée d'une famille parmi les parties passant par le nœud `cle`
ligne_famille <- function(arbre, cle, famille) {
  t <- arbre$lignes
  ok <- t$famille == famille
  if (cle != RACINE) ok <- ok & (t$cle == cle | startsWith(t$cle, paste0(cle, " ")))
  if (!any(ok)) return(NULL)
  t$cle[ok][which.max(t$n[ok])]
}

# Familles encore possibles sous le nœud `cle`, chacune cliquable
liens_familles <- function(arbre, cle, fam, max = 3) {
  tete <- head(fam, max)
  sortie <- list()
  for (i in seq_along(tete)) {
    if (i > 1) sortie <- c(sortie, list(", "))
    f <- names(tete)[i]
    sortie <- c(sortie, list(
      lien_ouverture(f, ligne_famille(arbre, cle, f)),
      paste0(" (", tete[[i]], ")")))
  }
  if (length(fam) > max) sortie <- c(sortie, list(paste0(", … +", length(fam) - max)))
  tagList(sortie)
}

# Branches sous le nœud `cle`. Un nœud dont toutes les parties relèvent d'une
# même famille n'est pas développé. `ply_base` = nombre de demi-coups déjà joués.
rendre_branches <- function(arbre, cle, ply_base, niveau = 1, profondeur_max = 3) {
  enf <- arbre$enfants[[cle]]
  if (is.null(enf) || niveau > profondeur_max) return(NULL)

  enf <- enf[order(-enf$n), , drop = FALSE]
  vus <- head(enf, c(8, 5, 3)[niveau])

  items <- lapply(seq_len(nrow(vus)), function(i) {
    fam <- arbre$familles[[vus$cle[i]]]
    pur <- length(fam) == 1
    tags$li(
      tags$span(class = "arbre-coup", etiquette_coup(ply_base + niveau, vus$coup[i])),
      tags$span(class = "arbre-n", vus$n[i]),
      tags$span(
        class = paste("arbre-fam", if (pur) "pur"),
        if (pur) lien_ouverture(names(fam), vus$cle[i])   # ligne menant à ce nœud
        else liens_familles(arbre, vus$cle[i], fam)
      ),
      if (!pur) rendre_branches(arbre, vus$cle[i], ply_base, niveau + 1, profondeur_max)
    )
  })

  reste <- nrow(enf) - nrow(vus)
  if (reste > 0) {
    items <- c(items, list(tags$li(
      class = "arbre-reste",
      sprintf("… %d autre%s coup%s", reste, if (reste > 1) "s" else "",
              if (reste > 1) "s" else ""))))
  }
  tags$ul(items)
}

# ---- Interface ---------------------------------------------------------------

mod_echiquier_interactif_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Echiquier interactif",
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
        5,
        div(id = ns("plateau"), class = "plateau-echecs",
            `data-input` = ns("coup"))
      ),
      column(
        4,
        h4("Arbre des ouvertures"),
        div(class = "arbre-panneau", `data-input-ligne` = ns("ligne"),
            uiOutput(ns("arbre")))
      )
    ),
    # Le script est dans le corps de la page, après le chargement de Shiny
    tags$script(HTML(js_echiquier_interactif))
  )
}

# ---- Serveur -----------------------------------------------------------------

mod_echiquier_interactif_server <- function(id, donnees = Donnees_Chess) {
  moduleServer(id, function(input, output, session) {

    id_plateau <- session$ns("plateau")
    arbre <- obtenir_arbre(donnees)
    jeu <- rchess::Chess$new()
    hist <- reactiveVal(partie_initiale())

    # Envoie au navigateur la position faisant foi
    envoyer <- function(h) {
      n <- length(h$sans)
      fen <- h$fens[n + 1]
      session$sendCustomMessage("echiquier_position", list(
        id = id_plateau,
        fen = fen,
        de = if (n > 0) h$de[n] else "",
        vers = if (n > 0) h$vers[n] else "",
        trait = trait_du_fen(fen),
        fin = partie_finie(jeu, fen)
      ))
    }

    # Position initiale, annulation, remise à zéro : tout changement de
    # l'historique est répercuté sur l'échiquier
    observe(envoyer(hist()))

    observeEvent(input$coup, {
      h <- hist()
      n <- length(h$sans)

      res <- tryCatch(
        jouer_coup(jeu, h$fens[n + 1], input$coup$de, input$coup$vers),
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

    # Clic sur un nom d'ouverture : on repart du début et on rejoue sa ligne
    observeEvent(input$ligne, {
      coups <- strsplit(input$ligne$ligne, " ", fixed = TRUE)[[1]]
      req(length(coups) > 0)

      h <- partie_initiale()
      fen <- fen_depart
      for (san in coups) {
        res <- tryCatch(jouer_san(jeu, fen, san), error = function(e) NULL)
        if (is.null(res)) {
          showNotification("Cette ligne n'a pas pu être jouée en entier.",
                           type = "warning", duration = 3)
          break
        }
        fen <- res$fen
        h$fens <- c(h$fens, res$fen)
        h$sans <- c(h$sans, res$san)
        h$de   <- c(h$de, res$de)
        h$vers <- c(h$vers, res$vers)
      }
      hist(h)
    })

    output$liste_coups <- renderUI({
      san <- hist()$sans
      if (length(san) == 0) return(helpText("Aucun coup joué pour l'instant."))
      lignes <- lapply(seq(1, length(san), by = 2), function(k) {
        tags$tr(tags$td(paste0((k + 1) / 2, ".")),
                tags$td(san[k]),
                tags$td(if (k + 1 <= length(san)) san[k + 1]))
      })
      tags$table(class = "echiquier-table", lignes)
    })

    output$arbre <- renderUI({
      sans <- hist()$sans
      n_coups <- length(sans)
      cle_de <- function(k) paste(sans[seq_len(k)], collapse = " ")

      # Plus long début de partie présent dans la base
      connus <- 0
      for (k in seq_len(n_coups)) {
        if (cle_de(k) %in% arbre$cles) connus <- k else break
      }

      # Dernière ouverture reconnue sur le chemin joué
      reconnue <- NULL
      for (k in rev(seq_len(connus))) {
        term <- arbre$terminaux[[cle_de(k)]]
        if (!is.null(term)) {
          reconnue <- list(k = k, noms = term)
          break
        }
      }

      bloc_ouverture <- if (!is.null(reconnue)) {
        nom <- names(reconnue$noms)[1]
        autres <- names(reconnue$noms)[-1]
        div(class = "arbre-ouverture",
            tags$strong(lien_ouverture(nom, cle_de(reconnue$k))),
            sprintf(" (ECO %s)", arbre$eco[[nom]]),
            if (reconnue$k < n_coups)
              tags$div(sprintf("reconnue après %s", etiquette_coup(reconnue$k, sans[reconnue$k]))),
            if (length(autres) > 0)
              tags$div(class = "arbre-info",
                       paste("Autres noms possibles ici :", paste(head(autres, 3), collapse = ", "))))
      } else if (n_coups > 0) {
        div(class = "arbre-info", "Aucune ouverture reconnue pour l'instant.")
      }

      if (connus < n_coups) {
        return(tagList(
          bloc_ouverture,
          div(class = "arbre-info",
              sprintf("La suite à partir de %s n'apparaît dans aucune partie de la base.",
                      etiquette_coup(connus + 1, sans[connus + 1])))
        ))
      }

      cle <- if (connus == 0) RACINE else cle_de(connus)
      fam <- arbre$familles[[cle]]
      branches <- rendre_branches(arbre, cle, n_coups)

      tagList(
        bloc_ouverture,
        div(class = "arbre-info",
            sprintf("%d parties, %d famille%s d'ouvertures encore possible%s",
                    arbre$n[[cle]], length(fam),
                    if (length(fam) > 1) "s" else "", if (length(fam) > 1) "s" else "")),
        if (is.null(branches)) {
          helpText("Fin de l'arbre : aucune partie de la base ne poursuit l'ouverture au-delà de ce coup.")
        } else {
          div(class = "arbre-racine", branches)
        }
      )
    })
  })
}
