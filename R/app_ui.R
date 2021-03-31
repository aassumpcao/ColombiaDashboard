#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  shiny::tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # List the first level UI elements here 
    shiny::navbarPage(
      title = 'Colombia Survey Results',
      theme = bslib::bs_theme(
        bootswatch = 'journal',
        'navbar-bg' = '#fff',
        base_font = bslib::font_google('Merriweather'),
        code_font = bslib::font_google('JetBrains Mono'),
        'enable-shadows' = TRUE,
        'enable-rounded' = TRUE,
        'enable-gradients' = TRUE,
        'enable-transitions' = TRUE
      ),
      shiny::tabsetPanel(
        shiny::tabPanel(
          title = 'Welcome',
          shiny::br(),
          mod_01_welcome_ui('01_welcome_ui_1')
        ),
        shiny::tabPanel(
          title = 'Selection',
          shiny::br(),
          mod_02_selection_ui('02_selection_ui_1')
        ),
        shiny::tabPanel(
          title = 'Demographics',
          shiny::br(),
          mod_03_demographics_ui('03_demographics_ui_1')
        ),
        shiny::tabPanel(
          title = 'Experience',
          shiny::br(),
          mod_04_experience_ui('04_experience_ui_1')
        ),
        shiny::tabPanel(
          title = 'Attachment',
          shiny::br(),
          mod_05_attachment_ui('05_attachment_ui_1')
        ),
        shiny::tabPanel(
          title = 'Intent to Return',
          shiny::br(),
          mod_06_return_ui('06_return_ui_1')
        ),
        shiny::tabPanel(
          title = 'Sense of Us',
          shiny::br(),
          mod_07_senseofus_ui('07_senseofus_ui_1')
        ),
        shiny::tabPanel(
          title = 'Compare Countries',
          shiny::br(),
          mod_08_compare_ui('08_compare_ui_1')
        ),
        shiny::tabPanel(
          title = 'Acknowledgements',
          shiny::br(),
          mod_09_acknowledgement_ui('09_acknowledgement_ui_1')
        )
      )
    )#,

    # create footer
    # footer = bs4Dash::dashboardFooter(
    #   copyrights = shiny::a(
    #     href = 'https://growthlab.cid.harvard.edu',
    #     target = '_blank', 'Harvard Growth Lab'
    #   ),
    #   right_text = '2021 | Developed by Harvard\'s CID Growth Lab'
    # )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'DiasporaSurveyResults'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

