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
        shiny::br()
      )
    )
  )
}
    
#' 05_attachment Server Function
#'
#' @noRd 
mod_05_attachment_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$plot01 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q6_2(app_data())
    })

    output$text01 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q6_2')
    })

    output$plot02 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q6_3(app_data())
    })

    output$text02 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q6_3')
    })

    output$plot03 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q6_5(app_data())
    })

    output$text03 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q6_5')
    })

    output$plot04 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q6_6(app_data())
    })

    output$text04 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q6_6')
    })

    output$plot05 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q6_7(app_data())
    })

    output$text05 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q6_7')
    })

 })
}
    
## To be copied in the UI
# mod_05_attachment_ui("05_attachment_ui_1")
    
## To be copied in the server
# callModule(mod_05_attachment_server, "05_attachment_ui_1")
 
