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

  ns <- shiny::NS(id)

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
          shiny::plotOutput(ns('plot07')),
          shiny::htmlOutput(ns('text07'))
        ),
        shiny::br()
      )
    )
  )
}
    
#' 06_return Server Function
#'
#' @noRd 
mod_06_return_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$plot01 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q7_2(app_data())
    })

    output$text01 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q7_2')
    })

    output$plot02 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q2_5(app_data())
    })

    output$text02 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q2_5')
    })

    output$plot03 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q7_8(app_data())
    })

    output$text03 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q7_8')
    })

    output$plot04 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q7_11(app_data())
    })

    output$text04 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q7_11')
    })

    output$plot05 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q7_14(app_data())
    })

    output$text05 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q7_14')
    })

    output$plot06 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q7_15(app_data())
    })

    output$text06 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q7_15')
    })

    output$plot07 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q135(app_data())
    })

    output$text07 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q135')
    })
 })
}
    
## To be copied in the UI
# mod_06_return_ui("06_return_ui_1")
    
## To be copied in the server
# callModule(mod_06_return_server, "06_return_ui_1")
 
