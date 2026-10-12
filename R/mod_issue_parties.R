# ==========================================================
# MODULE : Issue des parties
# Histogramme empilé : nombre de parties par issue selon la
# différence d'Elo absolue entre les deux joueurs.
# ==========================================================

NB_CLASSES_ELO <- 20

LIBELLES_ISSUES <- c(
  mate     = "Échec et mat",
  resign   = "Abandon",
  outoftime = "Temps écoulé",
  draw     = "Nulle"
)
COULEURS_ISSUES <- setNames(
  c("#1565C0", "#E69F00", "#009E73", "#9E9E9E"),
  LIBELLES_ISSUES
)


# ==========================================================
# PRÉPARATION DES DONNÉES
# (indépendante de l'interface : calculée une seule fois)
# ==========================================================

preparer_issues <- function(donnees, nb_classes = NB_CLASSES_ELO) {

  donnees <- donnees %>%
    transmute(
      elo_diff_abs,
      issue = factor(
        unname(LIBELLES_ISSUES[victory_status]),
        levels = LIBELLES_ISSUES
      )
    ) %>%
    filter(!is.na(elo_diff_abs), !is.na(issue))

  # Bornes arrondies et uniques, de 0 à la différence maximale observée
  bornes <- unique(
    round(seq(0, ceiling(max(donnees$elo_diff_abs)), length.out = nb_classes + 1))
  )

  # Libellés « début–fin » (la dernière classe inclut sa borne haute)
  debuts  <- head(bornes, -1)
  fins    <- tail(bornes, -1) - 1
  fins[length(fins)] <- tail(bornes, 1)
  libelles <- paste0(debuts, "–", fins)
  milieux  <- (head(bornes, -1) + tail(bornes, -1)) / 2

  donnees %>%
    mutate(
      classe_elo = cut(
        elo_diff_abs,
        breaks         = bornes,
        labels         = libelles,
        right          = FALSE,
        include.lowest = TRUE
      )
    ) %>%
    count(classe_elo, issue, name = "nombre_parties", .drop = FALSE) %>%
    group_by(classe_elo) %>%
    mutate(
      # Milieu de la classe : abscisse du point dans l'aire empilée
      milieu_elo    = milieux[as.integer(classe_elo)],
      total_parties = sum(nombre_parties),
      proportion    = if_else(total_parties > 0, nombre_parties / total_parties, 0)
    ) %>%
    ungroup() %>%
    mutate(
      tooltip = paste0(
        "<b>Différence d'Elo :</b> ", classe_elo,
        "<br><b>Issue :</b> ", issue,
        "<br><br><b>Nombre de parties :</b> ",
        format(nombre_parties, big.mark = " "),
        "<br><b>Part de la classe :</b> ",
        scales::percent(proportion, accuracy = 0.1),
        "<br><b>Total dans la classe :</b> ",
        format(total_parties, big.mark = " ")
      )
    )
}


# ==========================================================
# 1. INTERFACE UTILISATEUR
# ==========================================================

mod_issue_parties_ui <- function(id) {

  ns <- NS(id)

  tabPanel(
    "Issue des parties",

    h2("Comment les parties se terminent-elles ?"),

    p("Analyse de l'issue des parties selon la différence d'Elo entre les deux joueurs."),

    plotlyOutput(ns("graphique_issue"), height = "650px")
  )
}


# ==========================================================
# 2. SERVEUR
# ==========================================================

mod_issue_parties_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    # Les classes sont fixes : pas besoin de reactive()
    resume <- preparer_issues(Donnees_Chess)

    output$graphique_issue <- renderPlotly({

      if (nrow(resume) == 0) {
        return(plot_ly() %>% layout(title = "Aucune donnée disponible"))
      }

      plot_ly(
        data       = resume,
        x          = ~milieu_elo,
        y          = ~nombre_parties,
        color      = ~issue,
        colors     = COULEURS_ISSUES,
        type       = "scatter",
        mode       = "lines+markers",
        stackgroup = "one",            # aires empilées et continues
        groupnorm  = "percent",        # chaque classe totalise 100 %
        line       = list(width = 1),
        marker     = list(size = 5),
        hovertext  = ~tooltip,
        hoverinfo  = "text"
      ) %>%
        layout(
          title   = list(
            text = paste0(
              "<br><sup>Part de chaque issue selon la différence d'Elo entre les joueurs</sup>"
            )
          ),
          xaxis = list(
            title     = "Différence d'Elo entre les deux joueurs",
            rangemode = "tozero"
          ),
          yaxis = list(
            title      = "Part des parties",
            ticksuffix = " %",
            range      = c(0, 100)
          ),
          margin = list(t = 110),   # espace entre le sous-titre et le graphique
          legend = list(title = list(text = "<b>Issue</b>")),
          hoverlabel = list(
            bgcolor     = "white",
            bordercolor = "#333333",
            font        = list(color = "#222222", size = 13)
          )
        )
    })
  })
}
