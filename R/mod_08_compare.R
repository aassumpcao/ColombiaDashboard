#' 08_compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom rlang .data
mod_08_compare_ui <- function(id){

  ns <- NS(id)

  # ingest list of countries
  choices <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(.data$q3_2) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$q3_2) %>%
    dplyr::filter(.data$n > 100)

  # add all sample as choice
  choices <- c('All', choices$q3_2)

  # produce list of questions
  questions <- DiasporaSurveyResults::analysis_text %>%
    dplyr::filter(
      !is.na(.data$include_comparison) & .data$include_comparison == 1
    ) %>%
    dplyr::transmute(
      questions = paste(.data$section, .data$h2, sep = ': '), .data$question
    ) %>%
    tibble::deframe() %>%
    as.list(questions)

  # produce content
  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(12,
          shiny::selectInput(
            inputId = ns('compare'),
            label = 'Choose a question to compare survey results:',
            choices = questions,
            width = '100%'
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(6,
          shiny::selectInput(
            inputId = ns('country01'),
            label = 'Country 1:',
            choices = choices
          )
        ),
        shiny::column(6,
          shiny::selectInput(
            inputId = ns('country02'),
            label = 'Country 2:',
            choices = choices
          ) #,
          # offset = 5
        )
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
        shiny::plotOutput(ns('compare01')),
        shiny::plotOutput(ns('compare02'))
      ),
      shiny::br()
    )
  )
}
    
#' 08_compare Server Functions
#'
#' @noRd 
mod_08_compare_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$compare01 <- shiny::renderPlot({
      func <- paste0('plot_', input$compare)
      do.call(func, list(country = input$country01))
    })

    output$compare02 <- shiny::renderPlot({
      func <- paste0('plot_', input$compare)
      do.call(func, list(country = input$country02))
    })
  })
}
    
## To be copied in the UI
# mod_08_compare_ui("08_compare_ui_1")
    
## To be copied in the server
# mod_08_compare_server("08_compare_ui_1")
