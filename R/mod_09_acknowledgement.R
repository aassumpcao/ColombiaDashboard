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

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(12, shiny::htmlOutput(ns('acknowledgment')))
      )
    )
  )
}
    
#' 09_acknowledgement Server Functions
#'
#' @noRd 
mod_09_acknowledgement_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$acknowledgment <- shiny::renderUI({
      list(
        shiny::h2('Acknowledgements'),
        shiny::p('NA'),
        shiny::br(),
        shiny::h2('Authors:'),
        shiny::p('Ana Grisanti'),
        shiny::p('Andre Assumpcao'),
        shiny::p('Daniela Muhaj'),
        shiny::p('Jessie Lu'),
        shiny::p('Ljubica Nedelkoska'),
        shiny::br()
      )
    })
  })
}
    
## To be copied in the UI
# mod_09_acknowledgement_ui("09_acknowledgement_ui_1")
    
## To be copied in the server
# mod_09_acknowledgement_server("09_acknowledgement_ui_1")
