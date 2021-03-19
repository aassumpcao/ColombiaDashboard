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
    dplyr::filter(.data$n > 100) %>%
    dplyr::pull(.data$q3_2)

  # add all sample as choice
  choices <- c('All', choices)


  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(12,
          shiny::selectInput(
            inputId = ns('compare'),
            label = 'Choose a question to compare survey results:',
            choices = list(`What's your connection to Colombia?` = 'q2_3'),
            width = '100%'
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(6,
          shiny::selectInput(
            inputId = ns('country01'),
            label = 'Country (or Region) 1:',
            choices = choices
          )
        ),
        shiny::column(6,
          shiny::selectInput(
            inputId = ns('country02'),
            label = 'Country (or Region) 2:',
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
      )
    )
  )
}
    
#' 08_compare Server Functions
#'
#' @noRd 
mod_08_compare_server <- function(id){
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
