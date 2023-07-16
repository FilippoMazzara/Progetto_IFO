#' homePanel_ui
#' @description
#' homepage ui module
#' @param id
#' module's id
#' @examples
#' homePanel_ui("modulename")
homePanel_ui <- function(id) {

  ns <- shiny::NS(id)

  #### HOMEPAGE TAB ####
  shiny::tabPanel(
    title = "Home",
    value = "homepage",
    id = "home",
    class = "topchoice",

    shiny::tags$br(),
    shiny::tags$br(),

    ## TITLE
    shiny::fluidRow(
      id="home_title_cont",
      shiny::tags$h1(
        id = "home_title",
        htmltools::HTML("Welcome to <strong>GENEAPP</strong>")
      )
    ),

    shiny::tags$br(),

    ## FIRST WELL PANEL
    shiny::fluidRow(
      class="home_wellpanel",
      shiny::tags$br(),
      shiny::wellPanel(
        style = "margin-bottom: 0; width: 80%; text-align: center; font-size: large;",
        shiny::includeHTML(rmarkdown::render('www/homepage_insert1.Rmd'))
      ),
      shiny::tags$br()
    ),

    shiny::tags$br(),

    #LINK TO SECOND PAGE
    shiny::actionLink("linkapp", shiny::tags$span(("> > > Start the App < < <"))),

    shiny::tags$br(),

    ## SECOND WELL PANEL
    shiny::fluidRow(
      class="home_wellpanel",
      shiny::tags$br(),
      shiny::wellPanel(
        style = "margin-bottom: 0; width: 80%; text-align: left; font-size: medium;",
        shiny::includeHTML(rmarkdown::render('www/homepage_insert2.Rmd'))
      ),
      shiny::tags$br()
    ),
    shiny::tags$br()
  )
}

#' homePanel_server
#' @description
#' server module for the homepage
#' @param id
#' module's id
#' @examples
#' homePanel_server("modulename")
homePanel_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      #POSSIBLE SERVER FUNCTIONS

    }
  )
}
