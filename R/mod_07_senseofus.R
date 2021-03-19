#' 07_senseofus UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_07_senseofus_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::splitLayout(

    )
  )
}
    
#' 07_senseofus Server Function
#'
#' @noRd 
mod_07_senseofus_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns


 })
}
    
## To be copied in the UI
# mod_07_senseofus_ui("07_senseofus_ui_1")
    
## To be copied in the server
# callModule(mod_07_senseofus_server, "07_senseofus_ui_1")
 
