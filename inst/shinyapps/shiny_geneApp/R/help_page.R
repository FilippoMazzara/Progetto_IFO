#' help_page_ui
#' @description
#' HELP PAGE UI MODULE
#' @param id
#' module's id
#' @examples
#' help_page_ui("modulename")
#'
help_page_ui <- function(id) {

  ns <- shiny::NS(id)

  ### HELP TAB ###
  shiny::tabPanel(
    title = "Help",
    value = "help_page",
    id = "help_page",
    class = "topchoice",

    # CONTENTS OF HELP PAGE
    shiny::fluidRow(
      style = "margin-right: 4em; margin-left: 4em; display: flex; justify-content: center;",
      shiny::tags$br(),

      shiny::tags$br()
    )
  )
}



#' help_page_server
#' @description
#' server module for the helpt page
#' @param id
#' modules'id
#' @examples
#' help_page_server("modulename")
#'
help_page_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      #POSSIBLE SERVER FUNCTIONS

    }
  )
}
