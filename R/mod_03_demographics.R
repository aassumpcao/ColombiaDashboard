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

  # produce content
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
          shiny::plotOutput(ns('plot03')),
          shiny::htmlOutput(ns('text03'))
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
          shiny::htmlOutput(ns('text04')),
          shiny::plotOutput(ns('plot04'))
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
          shiny::plotOutput(ns('plot05')),
          shiny::htmlOutput(ns('text05'))
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
          shiny::htmlOutput(ns('text06')),
          shiny::plotOutput(ns('plot06'))
        )
      )
    )
  )
}
    
#' 03_demographics Server Function
#'
#' @noRd 
mod_03_demographics_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$plot01 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q3_3(app_data())
    })

    output$text01 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q3_3')
    })

    output$plot02 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q3_4(app_data())
    })

    output$text02 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q3_4')
    })

    output$plot03 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q3_5(app_data())
    })

    output$text03 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q3_5')
    })

    output$plot04 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q3_6(app_data())
    })

    output$text04 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q3_6')
    })

    output$plot05 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q3_7(app_data())
    })

    output$text05 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q3_7')
    })

    output$plot06 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q3_8(app_data())
    })

    output$text06 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q3_8')
    })

 })
}
    
## To be copied in the UI
# mod_03_demographics_ui("03_demographics_ui_1")
    
## To be copied in the server
# callModule(mod_03_demographics_server, "03_demographics_ui_1")
 
