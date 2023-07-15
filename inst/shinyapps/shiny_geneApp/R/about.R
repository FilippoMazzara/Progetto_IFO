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
    class = "topchoice_about",

    # CONTENTS OF ABOUT PAGE
    shiny::fluidRow(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          style = "align-self: center;",
          width = 3,
          shiny::tags$div(
            class = "about_links",
            shiny::tags$h2(
              class = "about_side_titles",
              "Links"
            ),
            shiny::tags$a(
              target = "_blank",
              href = "https://github.com/FilippoMazzara/Progetto_IFO",
              style = "font-size: large;",
              "GitHub - Source"
            ),
            shiny::tags$hr(style = "width: 40%; margin-top: 5px; margin-bottom: 5px;"),
            shiny::tags$a(
              target = "_blank",
              href = "https://github.com/FilippoMazzara/Progetto_IFO/issues",
              style = "font-size: large;",
              "Report Issues"
            )
          ),
          shiny::tags$div(
            class = "about_license",
            shiny::tags$h2(
              class = "about_side_titles",
              "License"
            ),
            shiny::tags$a(
              target = "_blank",
              href = "LICENSE.md",
              style = "font-size: large;",
              "Full license"
            ),
            shiny::tags$hr(style = "margin-top: 5px; width: 40%; margin-bottom: 5px;"),
            shiny::tags$div(
              shiny::tags$a(
                target = "_blank",
                style = "font-size: large;",
                href = "https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0)",
                "AGPL"
              ),

              "+",

              shiny::tags$a(
                target = "_blank",
                href = "LICENSE.html",
                style = "font-size: large;",
                "File license"
              )
            )
          ),
          shiny::tags$div(
            class = "about_dev",
            shiny::tags$h2(
              class = "about_side_titles",
              "Developers"
            ),
            shiny::tags$div(
              shiny::tags$a(
                target = "_blank",
                href = "https://github.com/FilippoMazzara/Progetto_IFO",
                style = "font-size: large;",
                "Filippo Mazzara"
              ),
              shiny::tags$br(),
              shiny::tags$span(
                "Author, maintainer"
              )
            ),
            shiny::tags$div(
              shiny::tags$a(
                target = "_blank",
                href = "https://www.rstudio.com",
                shiny::tags$img(
                  src = "https://www.tidyverse.org/rstudio-logo.svg",
                  alt = "RStudio",
                  width = "72"
                ),
              ),
              shiny::tags$br(),
              shiny::tags$span(
                "Copyright holder, funder"
              )
            )
          )
        ),
        shiny::mainPanel(
          width = 9,
          shiny::includeHTML(rmarkdown::render('www/about.Rmd')),
          shiny::tags$br()
        )
      )
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
