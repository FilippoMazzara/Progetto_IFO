#' about_ui
#' @description
#' ABOUT PAGE UI MODULE
#' @param id
#' module's id
#' @examples
#' about_ui("modulename")
#'
about_ui <- function(id) {

  ns <- shiny::NS(id)

  ### ABOUT TAB ###
  shiny::tabPanel(
    title = "About us",
    value = "about",
    id = "about",
    class = "topchoice",

    # CONTENTS OF ABOUT PAGE
    shiny::fluidRow(
      style = "margin-right:4em; margin-left:4em; display:flex; justify-content:center;",
      shiny::tags$br(),
      shiny::includeHTML(rmarkdown::render('www/about.Rmd')),
      shiny::tags$br()
    )
  )
}



#' about_server
#' @description
#' server module for the about page
#' @param id
#' modules'id
#' @examples
#' about_server("modulename")
#'
about_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      #POSSIBLE SERVER FUNCTIONS

    }
  )
}
