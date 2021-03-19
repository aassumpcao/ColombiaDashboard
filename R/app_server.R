#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # call modules
  mod_01_welcome_server('01_welcome_ui_1')
  mod_02_selection_server('02_selection_ui_1')
  mod_08_compare_server("08_compare_ui_1")

}
