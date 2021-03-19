#' 02_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02_selection_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::verticalLayout(
        shiny::splitLayout(

          # named arguments
          cellWidths = c('49%', '49%'),
          style = 'border: 1px solid silver;',
          cellArgs = list(
            style = paste(
              'white-space: normal',
              'text-align: justify',
              'align: center',
              'padding: 10px',
              sep = '; '
            )
          ),

          # content
          shiny::plotOutput(ns('plot01')),
          shiny::htmlOutput(ns('text01'))
        ),
        shiny::br(),
        shiny::splitLayout(

          # named arguments
          cellWidths = c('49%', '49%'),
          style = 'border: 1px solid silver;',
          cellArgs = list(
            style = paste(
              'white-space: normal',
              'text-align: justify',
              'align: center',
              'padding: 10px',
              sep = '; '
            )
          ),

          # content
          shiny::htmlOutput(ns('text02')),
          shiny::plotOutput(ns('plot02'))
        )
      )
    )
  )
}

#' 02_selection Server Function
#'
#' @noRd 
#'
#' @import ggplot2 stringr
mod_02_selection_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$plot01 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q2_3(app_data())
    })

    output$text01 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q2_3')
    })

    output$plot02 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q3_2()

    })
    output$text02 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q3_2')
    })
  })
}
    
## To be copied in the UI
# mod_02_selection_ui("02_selection_ui_1")
    
## To be copied in the server
# callModule(mod_02_selection_server, "02_selection_ui_1")
 
