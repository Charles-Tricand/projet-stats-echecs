source("global.R")

server <- function(input, output, session) {

  # ==========================================================
  # MODULES SERVEUR
  # ==========================================================

  #mod_elo_victory_server("elo_victory_tab")
  mod_nb_coups_server("nb_coups_tab")
  mod_issue_parties_server("issue_parties_tab")
  mod_diff_elo_max_server("diff_elo_max_tab")
  mod_calculateur_server("calculateur_tab")
  mod_probabilite_elo_server("probabilite_elo_tab")
  mod_guide_echecs_server("guide_echecs_tab")
}