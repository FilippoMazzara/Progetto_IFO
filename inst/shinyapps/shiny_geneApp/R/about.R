#' about_ui
#' @description
#' modulo ui per la pagina about
#' @param id
#' l'id assegnato al modulo
#' @examples
#' about_ui("nomemodulo")
about_ui <- function(id) {
  ns <- shiny::NS(id) #id del modulo
  #### TABPANEL DELLA NAVBAR PRINCIPALE ####
  shiny::tabPanel(
    title = "About us",
    value = shiny::NS(id,"about"),
    id = "about",
    class = "topchoice",
    shiny::fluidRow(
      style = "margin-right:4em;margin-left:4em;display:flex;justify-content:center;",
      shiny::tags$br(),
      shiny::includeHTML(rmarkdown::render('www/about.Rmd')),
      shiny::tags$br()
    )
  )
}



#' about_server
#' @description
#' modulo server per la pagina about
#' @param id
#' l'id assegnato al modulo
#' @examples
#' about_server("nomemodulo")
about_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

    }
  )
}
