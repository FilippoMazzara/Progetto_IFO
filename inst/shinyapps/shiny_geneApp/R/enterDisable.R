
#' enterDisable_ui
#' @description
#' modulo ui per l'inserimento del bottone che disabilita il tasto enter
#' @param id
#' l'id assegnato al modulo
#' @examples
#' enterDisable_ui("nomemodulo")
enterDisable_ui <- function(id) {
  ns <- shiny::NS(id) #id del modulo
  shiny::uiOutput(ns("enterDis"))
}

#' enterDisable_server
#' @description
#' modulo server per l'inserimento del bottone che disabilita il tasto enter
#' @param id
#' l'id assegnato al modulo
#' @examples
#' enterDisable_server("nomemodulo")
enterDisable_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      #bottone inutile
      output$enterDis <- shiny::renderUI(htmltools::HTML( '<button type="submit" disabled style="display: none" aria-hidden="true"></button>'))
      #
    }
  )
}
