#' 08_compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_08_compare_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 08_compare Server Functions
#'
#' @noRd 
mod_08_compare_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_08_compare_ui("08_compare_ui_1")
    
## To be copied in the server
# mod_08_compare_server("08_compare_ui_1")
