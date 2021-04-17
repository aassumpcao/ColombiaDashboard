#' 01_welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_01_welcome_ui <- function(id){

  ns <- shiny::NS(id)

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
      tags$head(
        tags$style(
          paste0(
            '#fixed .selectize-control .selectize-dropdown',
            ' {position: static !important;}'
          )
        )
      ),
      shiny::splitLayout(

        # # named arguments
        cellWidths = c('30%', '70%'),
        cellArgs = list(
          style = paste(
            'white-space: normal',
            'text-align: left',
            'align: center',
            'padding: 10px',
            'overflow-y: hidden; overflow-x: hidden;',
            sep = '; '
          )
        ),

        # content
        shiny::imageOutput(ns('welcome_image')),
        shiny::fluidRow(
          shiny::column(12, shiny::htmlOutput(ns('welcome_text'))),
          shiny::column(12,
            shiny::div(
              id = 'fixed',
              shiny::selectizeInput(
                inputId = ns('app_data'),
                label = 'Choose a country for analysis:',
                choices = choices,
                width = '50%'
              )
            )
          )
        )
      )
    )
  )
}

#' 01_welcome Server Function
#'
#' @noRd 
mod_01_welcome_server <- function(id, app_data){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$welcome_image <- shiny::renderImage({

      # load colombia flag()
      grDevices::png(app_sys('app/www/flag.png'))
      grDevices::dev.off()

      # create list to return image
      list(
        src = app_sys('app/www/flag.png'),
        width = '100%',
        height = '100%',
        align = 'center',
        contentType = 'image/png'
      )

    }, deleteFile = FALSE)

    output$welcome_text <- shiny::renderUI({
      list(
        shiny::h2('Welcome!'),
        shiny::p(
          paste0(
            DiasporaSurveyResults::analysis_text %>%
              dplyr::filter(.data$question == 'welcome') %>%
              dplyr::pull(p),
            ' '
          ),
          shiny::a(
            'aassumpcao@hks.harvard.edu',
            href='mailto:aassumpcao@hks.harvard.edu?subject=Colombia Dashboard',
            .noWS = 'outside'
          ),
          '.',
          .noWS = c('after-begin', 'before-end')
        )
      )
    })

    # return the analysis value
    return(shiny::reactive({input$app_data}))
  })
}

## To be copied in the UI
# mod_01_welcome_ui("01_welcome_ui_1")

## To be copied in the server
# callModule(mod_01_welcome_server, "01_welcome_ui_1")
