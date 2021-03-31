#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rlang .data
#' @noRd
app_server <- function(input, output, session) {

  # # create object with app data
  # app_data <- shiny::reactive({

  #   # load data
  #   analysis_text <- DiasporaSurveyResults::analysis_text
  #   survey_data <- DiasporaSurveyResults::survey_data

  #   # ingest list of countries
  #   choices <- survey_data %>%
  #     dplyr::select(.data$q3_2) %>%
  #     tidyr::drop_na() %>%
  #     dplyr::count(.data$q3_2) %>%
  #     dplyr::filter(.data$n > 100)

  #   # add all sample as choice
  #   choices <- c('All', choices)

  #   # produce list of questions
  #   questions <- analysis_text %>%
  #     dplyr::filter(
  #       !is.na(.data$include_comparison) & .data$include_comparison == 1
  #     ) %>%
  #     dplyr::select(h2, question) %>%
  #     tibble::deframe() %>%
  #     as.list(questions)

  #   # return call
  #   list(
  #     analysis_text = analysis_text,
  #     survey_data = survey_data,
  #     choices = choices,
  #     questions = questions
  #   )
  # })

  # call module 1
  app_data <- mod_01_welcome_server('01_welcome_ui_1')

  # call module 2
  mod_02_selection_server('02_selection_ui_1', app_data)

  # call module 3
  mod_03_demographics_server('03_demographics_ui_1', app_data)

  # call module 4
  mod_04_experience_server('04_experience_ui_1', app_data)

  # call module 5
  mod_05_attachment_server('05_attachment_ui_1', app_data)

  # call module 6
  mod_06_return_server('06_return_ui_1', app_data)

  # call module 7
  mod_07_senseofus_server('07_senseofus_ui_1', app_data)

  # call module 8
  mod_08_compare_server('08_compare_ui_1', app_data)

  # call module 9
  mod_09_acknowledgement_server('09_acknowledgement_ui_1')

}
