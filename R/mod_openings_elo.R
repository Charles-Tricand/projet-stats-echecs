mod_openings_elo_ui <- function(id) {
  ns <- NS(id)
  elo_levels <- c("<1000",paste0(seq(1000,1900,100),"-",seq(1099,1999,100)),"2000+")
  tabPanel(
    "Ouvertures selon Elo",
    fluidRow(
      column(6,selectInput(ns("elo"),"Tranche Elo :",choices=elo_levels,selected="1500-1599")),
      column(6,sliderInput(ns("nb_openings"),"Nombre d'ouvertures à afficher :",min=1,max=20,value=3,step=1))
    ),
    br(),
    plotlyOutput(ns("top_openings"),height="600px")
  )
}

mod_openings_elo_server <- function(id) {
  moduleServer(id,function(input,output,session) {
    output$top_openings <- renderPlotly({
      correspondance_ouvertures <- Donnees_Chess %>%
        filter(!is.na(opening_eco),!is.na(opening_name)) %>%
        count(opening_eco,opening_name,sort=TRUE) %>%
        group_by(opening_eco) %>%
        slice_max(n,n=1,with_ties=FALSE) %>%
        ungroup()
      
      donnees_elo <- Donnees_Chess %>%
        filter(elo_bin==input$elo) %>%
        select(-opening_name) %>%
        left_join(correspondance_ouvertures %>% select(opening_eco,opening_name),by="opening_eco")
      
      total_parties <- nrow(donnees_elo)
      n <- as.numeric(input$nb_openings)
      
      top_openings <- donnees_elo %>%
        count(opening_name,sort=TRUE,name="nombre") %>%
        slice_head(n=n) %>%
        mutate(proportion=nombre/total_parties)
      
      plot_ly(
        data=top_openings,
        x=~reorder(opening_name,-proportion),
        y=~proportion,
        type="bar",
        marker=list(color="white"),
        text=~opening_name,
        textposition="inside",
        insidetextfont=list(color="black"),
        hovertemplate=paste0(
          "<b>%{x}</b><br>",
          "Nombre de parties : %{text}<br>",
          "Proportion : %{y:.2%}",
          "<extra></extra>"
        )
      ) %>%
        layout(
          xaxis=list(title="",tickangle=-45,color="white",showgrid=FALSE,showticklabels=FALSE),
          yaxis=list(title="Proportion des parties",tickformat=".1%",color="white",showgrid=FALSE),
          plot_bgcolor="black",
          paper_bgcolor="black",
          font=list(color="white")
        )
    })
  })
}
