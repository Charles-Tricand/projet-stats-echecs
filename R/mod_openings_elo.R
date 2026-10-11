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
        marker=list(color="black",line=list(width=0)),
        text=~opening_name,
        textposition="inside",
        insidetextfont=list(color="white",size=10),
        hovertemplate=paste0(
          "<b>%{x}</b><br>",
          "Proportion : %{y:.2%}",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title=list(
            text=paste0(
              "Les ",input$nb_openings,
              " ouvertures les plus jouées — Elo ",input$elo
            ),
            font=list(size=20,color="white"),
            x=0.5,
            xanchor="center"
          ),
          xaxis=list(
            title="",
            showgrid=FALSE,
            zeroline=FALSE,
            showline=FALSE
          ),
          yaxis=list(
            title="",
            showticklabels=FALSE,
            showgrid=FALSE,
            zeroline=FALSE,
            showline=FALSE
          ),
          plot_bgcolor="white",
          paper_bgcolor="white",
          font=list(color="black"),
          margin=list(l=20,r=20,t=80,b=30)
        )
    })
  })
}
