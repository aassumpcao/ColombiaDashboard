#' 06_return UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_06_return_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::splitLayout(

    )
  )
}
    
#' 06_return Server Function
#'
#' @noRd 
mod_06_return_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns


 })
}
    
## To be copied in the UI
# mod_06_return_ui("06_return_ui_1")
    
## To be copied in the server
# callModule(mod_06_return_server, "06_return_ui_1")
 
