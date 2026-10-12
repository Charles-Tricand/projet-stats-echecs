# Module for the Elo victory rate graph

mod_elo_victory_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Victoire avec désavantage",
    h2("Proportion de victoire pour les joueurs désavantagés"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          ns("nb_classe"),
          label = "Nombre de classes d'Elo :",
          min = 5,
          max = 20,
          value = 10,
          step = 1
        ),
        radioButtons(
          ns("couleur_desavantage"),
          label = "Joueur désavantagé :",
          choices = c(
            "Les deux" = "tous",
            "Blancs" = "white",
            "Noirs" = "black"),
          selected = "tous")
      ),
      mainPanel(
        plotOutput(ns("graphique_elo"), height = "600px")
      )
    )
  )
}


mod_elo_victory_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$graphique_elo <- renderPlot({
      
      
      # Calcul du taux de victoire
      resume <- Donnees_Chess |> 
        filter(
          !is.na(victoire_desavantage),
          input$couleur_desavantage == "tous" | couleur_desavantage == input$couleur_desavantage)
      
      bornes = floor(seq(
        from = 0,
        to = max(resume$elo_diff_abs, na.rm = TRUE),
        length.out = input$nb_classe + 1))
      
      resume = resume |> 
        mutate(elo_diff_class = cut(
          elo_diff_abs,
          breaks = bornes,
          labels = format(bornes[-1], scientific = FALSE, nsmall = 0, trim = TRUE)))|> 
        group_by(elo_diff_class) |> 
        summarise(
          taux_victoire = mean(victoire_desavantage, na.rm = TRUE) * 100,
          nombre_parties = n(),
          .groups = "drop")

      # Graphique
      ggplot(
        resume,
        aes(x = elo_diff_class, y = taux_victoire)) +
        geom_col(fill = "grey") +
        geom_text(
          aes(label = paste0(round(taux_victoire, 1),"%","\n(n = ",nombre_parties,")")),
          vjust = -0.3) +
        labs(
          title = "Taux de victoire du joueur désavantagé",
          x = "Différence d'Elo",
          y = "Taux de victoire (%)") +
        theme_minimal()+
        theme(
          panel.grid.major.x = element_blank())
    })
  })
}