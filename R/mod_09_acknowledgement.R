#' 09_acknowledgement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_09_acknowledgement_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 09_acknowledgement Server Functions
#'
#' @noRd 
mod_09_acknowledgement_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_09_acknowledgement_ui("09_acknowledgement_ui_1")
    
## To be copied in the server
# mod_09_acknowledgement_server("09_acknowledgement_ui_1")
