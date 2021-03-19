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
  shiny::tagList(
    shiny::fluidPage(
      shiny::splitLayout(

        # # named arguments
        cellWidths = c('30%', '70%'),
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
        shiny::imageOutput(ns('welcome_image')),
        shiny::htmlOutput(ns('welcome_text'))
      )
    )
  )
}
    
#' 01_welcome Server Function
#'
#' @noRd 
mod_01_welcome_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$welcome_image <- shiny::renderImage({

      # shinipsum::random_image()
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

    output$welcome_text <- shiny::renderText({
      shinipsum::random_text(nwords = 200)
    })
  })
}
    
## To be copied in the UI
# mod_01_welcome_ui("01_welcome_ui_1")
    
## To be copied in the server
# callModule(mod_01_welcome_server, "01_welcome_ui_1")
