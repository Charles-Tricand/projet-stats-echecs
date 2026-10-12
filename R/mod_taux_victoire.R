# ==========================================================
# MODULE : Taux de victoire
# Aires empilées : Blancs, Noirs et matchs nuls
# ==========================================================

NB_CLASSES_ELO <- 20

LIBELLES_RESULTATS <- c(
  white = "Victoire des Blancs",
  draw  = "Match nul",
  black = "Victoire des Noirs"
)


COULEURS_RESULTATS <- setNames(
  c("#F3E8D2", "#D2BCA5", "#966B4F"),
  unname(LIBELLES_RESULTATS)
)



# ==========================================================
# PRÉPARATION DES DONNÉES
# ==========================================================

preparer_taux_victoires <- function(donnees, nb_classes = NB_CLASSES_ELO) {
  
  donnees <- donnees %>%
    transmute(
      elo_diff_abs,
      resultat = factor(
        unname(LIBELLES_RESULTATS[winner]),
        levels = unname(LIBELLES_RESULTATS)
      )
    ) %>%
    filter(!is.na(elo_diff_abs), !is.na(resultat))
  
  bornes <- unique(
    round(seq(
      0,
      ceiling(max(donnees$elo_diff_abs)),
      length.out = nb_classes + 1
    ))
  )
  
  debuts <- head(bornes, -1)
  fins <- tail(bornes, -1) - 1
  fins[length(fins)] <- tail(bornes, 1)
  
  libelles <- paste0(debuts, "–", fins)
  milieux <- (head(bornes, -1) + tail(bornes, -1)) / 2
  
  donnees %>%
    mutate(
      classe_elo = cut(
        elo_diff_abs,
        breaks = bornes,
        labels = libelles,
        right = FALSE,
        include.lowest = TRUE
      )
    ) %>%
    count(classe_elo, resultat, name = "nombre_parties", .drop = FALSE) %>%
    group_by(classe_elo) %>%
    mutate(
      milieu_elo = milieux[as.integer(classe_elo)],
      total_parties = sum(nombre_parties),
      proportion = if_else(
        total_parties > 0,
        nombre_parties / total_parties,
        0
      ),
      tooltip = paste0(
        "<b>Différence d'Elo :</b> ", classe_elo,
        "<br><b>Résultat :</b> ", resultat,
        "<br><br><b>Nombre de parties :</b> ",
        format(nombre_parties, big.mark = " "),
        "<br><b>Proportion :</b> ",
        scales::percent(proportion, accuracy = 0.1),
        "<br><b>Total dans la classe :</b> ",
        format(total_parties, big.mark = " ")
      )
    ) %>%
    ungroup()
}


# ==========================================================
# 1. INTERFACE UTILISATEUR
# ==========================================================

mod_taux_victoires_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    "Taux de victoire selon la couleur",
    h2("Est ce que la couleur offre un avantage à un joueur ?"),
    
    p("Proportion des victoires des Blancs, des victoires des Noirs ",
      "et des matchs nuls selon la différence d'Elo."),
    
    plotlyOutput(ns("graphique_taux"), height = "650px")
  )
}


# ==========================================================
# 2. SERVEUR
# ==========================================================

mod_taux_victoires_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    resume <- preparer_taux_victoires(Donnees_Chess)
    output$graphique_taux <- renderPlotly({
      if (nrow(resume) == 0) {
        return(plot_ly() %>% layout(title = "Aucune donnée disponible"))
      }
      
      plot_ly(
        data = resume,
        x = ~milieu_elo,
        y = ~nombre_parties,
        color = ~resultat,
        colors = COULEURS_RESULTATS,
        type = "scatter",
        mode = "lines+markers",
        stackgroup = "one",
        groupnorm = "percent",
        line = list(width = 1),
        marker = list(size = 5),
        hovertext = ~tooltip,
        hoverinfo = "text"
      ) %>%
        add_segments(
          x = min(resume$milieu_elo, na.rm = TRUE),
          xend = max(resume$milieu_elo, na.rm = TRUE),
          y = 50,
          yend = 50,
          inherit = FALSE,
          line = list(
            color = "#555555",
            width = 2,
            dash = "dot"
          ),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        layout(
          title = list(
            text = paste0(
              "<br><sup>Proportion de chaque résultat selon la différence d'Elo</sup>"
            )
          ),
          xaxis = list(
            title = "Différence d'Elo entre les deux joueurs",
            rangemode = "tozero"
          ),
          yaxis = list(
            title = "Part des parties",
            ticksuffix = " %",
            range = c(0, 100)
          ),
          
          margin = list(t = 110),
          legend = list(title = list(text = "<b>Résultat</b>")),
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = "#333333",
            font = list(color = "#222222", size = 13)
          )
        )
    })
  })
}

