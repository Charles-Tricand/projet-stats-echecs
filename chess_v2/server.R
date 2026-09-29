#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
server <- function(input, output, session) {
  
  
  # =====================================================
  # GRAPHIQUE 1 : OUVERTURES
  # =====================================================
  
  output$top_openings <- renderUI({
    
    donnees_elo <- Donnees_Chess %>%
      filter(elo_bin == input$elo)
    
    total_parties <- nrow(donnees_elo)
    
    n <- as.numeric(input$nb_openings)
    
    
    # -----------------------------------------------------
    # TOP DES OUVERTURES
    # -----------------------------------------------------
    
    top_openings <- donnees_elo %>%
      count(
        opening_name,
        sort = TRUE,
        name = "nombre"
      ) %>%
      slice_head(n = n) %>%
      mutate(
        proportion = nombre / total_parties
      )
    
    
    # -----------------------------------------------------
    # TAILLE DES PIÈCES
    # -----------------------------------------------------
    
    taille_min <- 45
    taille_max <- 125
    
    p_min <- min(top_openings$proportion)
    p_max <- max(top_openings$proportion)
    
    if (p_max == p_min) {
      
      top_openings$taille <- 
        (taille_min + taille_max) / 2
      
    } else {
      
      top_openings$taille <- taille_min +
        (top_openings$proportion - p_min) /
        (p_max - p_min) *
        (taille_max - taille_min)
    }
    
    
    # -----------------------------------------------------
    # CRÉATION DES PIÈCES
    # -----------------------------------------------------
    
    lignes <- lapply(
      seq_len(nrow(top_openings)),
      function(i) {
        
        ouverture <- top_openings$opening_name[i]
        proportion <- top_openings$proportion[i]
        nombre <- top_openings$nombre[i]
        taille <- top_openings$taille[i]
        
        y <- 145 - taille
        
        
        HTML(
          paste0(
            
            '<div style="
              display:flex;
              align-items:center;
              margin-bottom:18px;
              width:100%;
            ">',
            
            
            # NOM DE L'OUVERTURE
            
            '<div style="
              width:230px;
              font-size:15px;
              font-weight:500;
              color:#263238;
              padding-right:20px;
              text-align:right;
            ">',
            
            ouverture,
            
            '</div>',
            
            
            # ZONE DE LA PIÈCE
            
            '<div style="
              flex:1;
              height:150px;
              position:relative;
              border-bottom:1px solid #E5E7EB;
            ">',
            
            
            # SVG
            
            '<svg
              width="100%"
              height="150"
              viewBox="0 0 180 150"
              preserveAspectRatio="xMinYMid meet"
              style="overflow:visible;"
            >',
            
            
            # POSITION + TAILLE
            
            '<g transform="translate(70,',
            y,
            ') scale(',
            taille / 100,
            ')">',
            
            
            # =================================================
            # TOUR
            # =================================================
            
            '<path
              d="
                M 22 100
                L 28 82
                L 28 67
                L 22 62
                L 22 53
                L 30 53
                L 30 42
                L 38 42
                L 38 53
                L 46 53
                L 46 42
                L 54 42
                L 54 53
                L 62 53
                L 62 42
                L 70 42
                L 70 62
                L 64 67
                L 64 82
                L 70 100
                Z
              "
              fill="#263238"
            />',
            
            
            # =================================================
            # SOCLE
            # =================================================
            
            '<path
              d="
                M 18 100
                L 74 100
                L 82 108
                L 82 116
                L 10 116
                L 10 108
                Z
              "
              fill="#263238"
            />',
            
            
            '</g>',
            
            
            # POURCENTAGE
            
            '<text
              x="125"
              y="35"
              font-size="13"
              fill="#6B7280"
            >',
            
            scales::percent(
              proportion,
              accuracy = 0.1
            ),
            
            '</text>',
            
            
            '</svg>',
            
            '</div>',
            
            
            # NOMBRE DE PARTIES
            
            '<div style="
              width:110px;
              font-size:13px;
              color:#6B7280;
              padding-left:15px;
            ">',
            
            format(
              nombre,
              big.mark = " "
            ),
            
            ' parties',
            
            '</div>',
            
            
            '</div>'
          )
        )
      }
    )
    
    
    # -----------------------------------------------------
    # AFFICHAGE
    # -----------------------------------------------------
    
    tagList(
      lignes
    )
  })
  
  
  
  # =====================================================
  # GRAPHIQUE 2 : COUPS SELON ÉCART D'ELO
  # =====================================================
  
  output$coups_elo <- renderPlotly({
    
    taille <- input$taille_elo
    
    
    donnees <- Donnees_Chess %>%
      
      mutate(
        diff_elo = abs(
          white_rating - black_rating
        ),
        
        categorie_elo = floor(
          diff_elo / taille
        ) * taille
      ) %>%
      
      group_by(categorie_elo) %>%
      
      summarise(
        
        nombre_coups_moyen = mean(
          turns,
          na.rm = TRUE
        ),
        
        nombre_parties = n(),
        
        .groups = "drop"
      ) %>%
      
      arrange(categorie_elo) %>%
      
      mutate(
        
        categorie = paste0(
          categorie_elo,
          "-",
          categorie_elo + taille - 1
        ),
        
        categorie = factor(
          categorie,
          levels = categorie
        )
      )
    
    
    # -----------------------------------------------------
    # GRAPHIQUE
    # -----------------------------------------------------
    
    plot_ly(
      
      data = donnees,
      
      x = ~categorie,
      
      y = ~nombre_coups_moyen,
      
      type = "bar",
      
      marker = list(
        color = "#263238"
      ),
      
      hovertemplate = paste0(
        
        "<b>Écart Elo : %{x}</b><br>",
        
        "Nombre moyen de coups : %{y:.1f}<br>",
        
        "Nombre de parties : %{customdata}<br>",
        
        "<extra></extra>"
      ),
      
      customdata = ~nombre_parties
      
    ) %>%
      
      layout(
        
        xaxis = list(
          title = "Différence de classement Elo",
          showgrid = FALSE
        ),
        
        yaxis = list(
          title = "Nombre moyen de coups",
          showgrid = TRUE,
          gridcolor = "#E5E7EB",
          zeroline = FALSE
        ),
        
        plot_bgcolor = "#FFFFFF",
        
        paper_bgcolor = "#FFFFFF",
        
        margin = list(
          l = 70,
          r = 30,
          t = 30,
          b = 70
        ),
        
        hoverlabel = list(
          bgcolor = "white",
          font = list(
            size = 13
          )
        )
      )
  })
}