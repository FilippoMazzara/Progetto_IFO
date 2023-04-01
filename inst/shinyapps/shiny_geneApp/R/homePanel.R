#' homePanel_ui
#' @description
#' modulo ui per la home Page
#' @param id
#' l'id assegnato al modulo
#' @examples
#' homePanel_ui("nomemodulo")
homePanel_ui <- function(id) {
  ns <- shiny::NS(id) #id del modulo
  #### TABPANEL DELLA NAVBAR PRINCIPALE ####
  shiny::tabPanel(
    title = "Home",
    value = shiny::NS(id,"home"),
    id = "home",
    class = "topchoice",
    shiny::tags$br(),
    shiny::tags$br(),
    shiny::fluidRow(
      style = "justify-content: center;
        display: flex;
        align-items: center;",
      shiny::tags$h1(
        id = "home_title",
        style = "line-height: 1.5;",
        htmltools::HTML("BENVENUTI NELLA <strong>GENEAPP</strong>")
      )
    ),
    shiny::tags$br(),
    shiny::tags$br(),
    shiny::fluidRow(
      style = "margin-right:4em;margin-left:4em;display:flex;justify-content:center;",
      shiny::tags$br(),
      shiny::wellPanel(
        style = "margin-bottom:0;width:100%",
        shiny::includeHTML(rmarkdown::render('www/homepage_insert1.Rmd'))
      ),
      shiny::tags$br()
    ),
    shiny::tags$br(),
    shiny::actionLink("linkapp",shiny::tags$span(("> > > VAI ALL'APP < < <"))),
    shiny::tags$br(),

    shiny::tags$br(),
    shiny::fluidRow(
      style = "margin-right:4em;margin-left:4em;display:flex;justify-content:center;",
      shiny::tags$br(),
      shiny::wellPanel(
        style = "margin-bottom:0;width:100%;",
        shiny::includeHTML(rmarkdown::render('www/homepage_insert2.Rmd'))
      ),
      shiny::tags$br()
    ),
    shiny::tags$br(),
    shiny::tags$br(),
    shiny::fluidRow(
      style = "margin-right:4em;margin-left:4em;display:flex;justify-content:center;",
      shiny::tags$br(),
      shiny::wellPanel(
        style = "margin-bottom:0;width:100%;",
        shiny::includeHTML(rmarkdown::render('www/homepage_insert2.Rmd'))
      ),
      shiny::tags$br()
    ),
    shiny::tags$br(),
    shiny::tags$br()
  )
}



#' homePanel_server
#' @description
#' modulo server per la homepage
#' @param id
#' l'id assegnato al modulo
#' @examples
#' homePanel_server("nomemodulo")
homePanel_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

    }
  )
}
