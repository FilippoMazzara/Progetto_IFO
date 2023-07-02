
#' enterDisable_ui
#' @description
#' ui module to deactivate enter key in order to prevent erroneous inputs activation
#' @param id
#' module's id
#' @examples
#' enterDisable_ui("modulename")
#'
enterDisable_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::uiOutput(ns("enterDis"))
}

#' enterDisable_server
#' @description
#' server module to deactivate enter key in order to prevent erroneous inputs activation
#' @param id
#' module's id
#' @examples
#' enterDisable_server("modulename")
#'
enterDisable_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      #useless button
      output$enterDis <- shiny::renderUI(htmltools::HTML( '<button type = "submit" disabled style = "display: none" aria-hidden = "true"></button>'))

    }
  )
}
