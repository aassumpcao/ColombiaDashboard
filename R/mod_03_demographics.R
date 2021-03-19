#' 03_demographics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_03_demographics_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::splitLayout(

    )
  )
}
    
#' 03_demographics Server Function
#'
#' @noRd 
mod_03_demographics_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns


 })
}
    
## To be copied in the UI
# mod_03_demographics_ui("03_demographics_ui_1")
    
## To be copied in the server
# callModule(mod_03_demographics_server, "03_demographics_ui_1")
 
