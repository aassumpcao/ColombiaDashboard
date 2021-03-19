#' 05_attachment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_05_attachment_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::splitLayout(

    )
  )
}
    
#' 05_attachment Server Function
#'
#' @noRd 
mod_05_attachment_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns


 })
}
    
## To be copied in the UI
# mod_05_attachment_ui("05_attachment_ui_1")
    
## To be copied in the server
# callModule(mod_05_attachment_server, "05_attachment_ui_1")
 
