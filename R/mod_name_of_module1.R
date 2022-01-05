#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
load("./data/spdfGEO.Rdata")
load("./data/spdfDPT.Rdata")
load("./data/thesoc_data.Rdata")
load("./data/thesoc_spdf.Rdata")
mod_name_of_module1_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      
      pageWithSidebar(
        headerPanel('GeoMatique'),
        sidebarPanel(
          selectInput(NS(id,"notionUI"),label = "Notion",choices = c("abeille")),
          selectInput(inputId = NS(id,'decoupageID'),label = 'Découpage Géographique',choices =  c("Partition linguistique","departement (1min de chargement)")),
          sliderInput(inputId = NS(id,'clusterLevel'),label = 'Niveau de cluster (for dendrogram but unfinished)',min=1,max=68,value=6)
          
        ),
        mainPanel(
          plotOutput(NS(id,'plotAffichage')),
          textOutput(NS(id,"stats")),
          
          # affichage carte clust (eurreur)
          #plotOutput(outputId = 'clusterMap'),
          
          
          column(12,
                 gt_output(NS(id,'table'))
          )
          
          
        )
      )
    )
    
  )
}

#' name_of_module1 Server Functions
#'
#' @noRd 
mod_name_of_module1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    listeNotions<-colnames(DATA)[5:239]
    
    updateSelectInput(session,'notionUI',choices=listeNotions,selected = "jardin")
    # affichage stat de base
    
    output$stats = renderText({paste("Statistiques pour la notion :",input$notionUI,sep = " ")})
    
    
    # affichage(notion,découpage)
    output$plotAffichage<-renderPlot({
      if (input$decoupageID=="Partition linguistique") {
        affichage(input$notionUI,partition = spdf.geo)
      } else {
        affichage(input$notionUI,partition = spdf.dpt)
      }
    })
    
    
    output$table<-render_gt({
      stat<-Calcule_statistique_base(mot=input$notionUI,spdf.geo= if (input$decoupageID=="Partition linguistique") {partitionChoisi<-spdf.geo} else {partitionChoisi <- spdf.dpt})
      
      nameIndicateur<-c()
      valIndicateur<-c()
      for (i in c(2,3,4,5,8)) {
        nameIndicateur<-append(nameIndicateur,labels(stat[i]))
        valIndicateur<-append(valIndicateur,signif(round(stat[[i]],digits = 4),digits = 8))
      }
      dataStats<-c(nameIndicateur,valIndicateur)
      dfStat<-data.frame(matrix(data=dataStats,nrow = 5))
      colnames(dfStat)<-c("indicateur","valeur")
      gt(dfStat)

    })

  })
}

    
    
## To be copied in the UI (done)
# mod_name_of_module1_ui("name_of_module1_ui_1")
    
## To be copied in the server (done)
# mod_name_of_module1_server("name_of_module1_ui_1")
