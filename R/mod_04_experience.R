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
          shiny::htmlOutput(ns('text08')),
          shiny::plotOutput(ns('plot08'))
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
          shiny::plotOutput(ns('plot09')),
          shiny::htmlOutput(ns('text09'))
        ),
        shiny::br()
      )
    )
  )
}
    
#' 04_experience Server Function
#'
#' @noRd 
mod_04_experience_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$plot01 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q5_3(app_data())
    })

    output$text01 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q5_3')
    })

    output$plot02 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q3_9(app_data())
    })

    output$text02 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q3_9')
    })

    output$plot03 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q4_4(app_data())
    })

    output$text03 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q4_4')
    })

    output$plot04 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q5_11(app_data())
    })

    output$text04 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q5_11')
    })

    output$plot05 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q5_8(app_data())
    })

    output$text05 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q5_8')
    })

    output$plot06 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q5_9(app_data())
    })

    output$text06 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q5_9')
    })

    output$plot07 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q5_12(app_data())
    })

    output$text07 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q5_12')
    })

    output$plot08 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q5_13(app_data())
    })

    output$text08 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q5_13')
    })

    output$plot09 <- shiny::renderPlot({
      DiasporaSurveyResults::plot_q5_4(app_data())
    })

    output$text09 <- shiny::renderUI({
      DiasporaSurveyResults::load_text('q5_4')
    })

 })
}
    
## To be copied in the UI
# mod_04_experience_ui("04_experience_ui_1")
    
## To be copied in the server
# callModule(mod_04_experience_server, "04_experience_ui_1")
 
