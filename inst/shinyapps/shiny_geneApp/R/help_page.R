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
    class = "topchoice_help",
    shiny::fluidRow(

      shiny::sidebarLayout(

        ### HELP SIDEBAR ###
        shiny::sidebarPanel(
          style = "align-self: center;",
          width = 3,
          shiny::tags$nav(
            id = "help_nav",
            `data-toggle` = "help_nav",
            shiny::tags$h2(
              "Page contents"
            ),
            shiny::tags$ul(
              class = "nav navbar-nav shiny-tab-input shiny-bound-input",
              id = "help_nav_ul",
              shiny::tags$li(
                class = "nav-item",
                shiny::tags$a(
                  class = "nav-link",
                  href = "#overview",
                  "Overview"
                )
              ),
              shiny::tags$li(
                class = "nav-item",
                shiny::tags$a(
                  class = "nav-link",
                  href = "#key-features",
                  "Key Features"
                )
              ),
              shiny::tags$li(
                class = "nav-item",
                shiny::tags$a(
                  class = "nav-link",
                  href = "#installation",
                  "Installation"
                )
              ),
              shiny::tags$li(
                class = "nav-item",
                shiny::tags$a(
                  class = "nav-link",
                  href = "#basic-usage",
                  "Basic Usage"
                )
              ),
              shiny::tags$li(
                class = "nav-item",
                shiny::tags$a(
                  class = "nav-link",
                  href = "#advanced-usage",
                  "Advanced Usage"
                )
              )
            ),
          ),
        ),
        shiny::mainPanel(
          width = 9,
          ### HELP CONTENT ###
          shiny::includeHTML(rmarkdown::render('www/help_page.Rmd')),
          shiny::tags$br()
        )
      )
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
