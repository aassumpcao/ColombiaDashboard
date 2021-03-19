#' 04_experience UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_04_experience_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::splitLayout(

    )
  )
}
    
#' 04_experience Server Function
#'
#' @noRd 
mod_04_experience_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns


 })
}
    
## To be copied in the UI
# mod_04_experience_ui("04_experience_ui_1")
    
## To be copied in the server
# callModule(mod_04_experience_server, "04_experience_ui_1")
 
