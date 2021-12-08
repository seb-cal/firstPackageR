#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import sp
#' @import rgdal
#' @import raster
#' @import rgeos
#' @import terra
#' @noRd
app_server <- function( input, output, session ) {
 
  
  # Your application server logic 
  mod_name_of_module1_server("name_of_module1_ui_1")
  mod_name_of_module2_server("name_of_module2_ui_1")
  
  observeEvent(input$alert,{
    golem::invoke_js("alert","hi")
    
  })
  
}
