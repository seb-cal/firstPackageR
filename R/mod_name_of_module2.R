#' name_of_module2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_name_of_module2_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Plot intéractif (1min de chargement)"),
    dygraphOutput(ns("plot"))
 
  )
}
    
#' name_of_module2 Server Functions
#'
#' @noRd 
mod_name_of_module2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$plot = renderDygraph({
      dailyAcc()
    })
 
  })
}
    
## To be copied in the UI (done)
# mod_name_of_module2_ui("name_of_module2_ui_1")
    
## To be copied in the server (done)
# mod_name_of_module2_server("name_of_module2_ui_1")
