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
        shiny::sidebarPanel(
          style = "align-self: center;",
          width = 3,
          shiny::HTML(
          '<div class="links">
            <h2 data-toc-skip>Links</h2>
            <ul class="list-unstyled">
            <li><a target="_blank" href="https://github.com/FilippoMazzara/Progetto_IFO" class="external-link">Browse source code</a></li>
            <li><a target="_blank" href="https://github.com/FilippoMazzara/Progetto_IFO/issues" class="external-link">Report a bug</a></li>
            </ul>
            </div>
            <div class="license">
            <h2 data-toc-skip>License</h2>
            <ul class="list-unstyled">
            <li><a target="_blank" href="LICENSE.md">Full license</a></li>
            <li><small><a target="_blank" href="https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0)" class="external-link">AGPL</a> + file <a target="_blank" href="LICENSE.html">LICENSE</a></small></li>
            </ul>
            </div>
            <div class="developers">
            <h2 data-toc-skip>Developers</h2>
            <ul class="list-unstyled">
            <li>
            <a target="_blank" href="https://github.com/FilippoMazzara/Progetto_IFO" class="external-link">Filippo Mazzara</a> <br><small class="roles"> Author, maintainer </small> </li>
            <li>
            <a target="_blank" href="https://www.rstudio.com" class="external-link"><img src="https://www.tidyverse.org/rstudio-logo.svg" alt="RStudio" width="72"></a> <br><small class="roles"> Copyright holder, funder </small>  </li>
            </ul>
          </div>
            <hr>
            <nav id="toc" data-toggle="toc"><h2>On this page</h2>
          <div class = "navlinks_help_container">
            <a class="nav-link" href="#home-page">Home page
            </a>
          <a class="nav-link" href="#reference">Reference
            </a>
          <a class="nav-link" href="#articles">Articles
            </a>
          <a class="nav-link" href="#news">News
            </a>
          <a class="nav-link" href="#publishing">Publishing
            </a>
          <a class="nav-link" href="#promoting">Promoting
            </a>
            </div>
          </nav>
            '
          )
        ),
        shiny::mainPanel(
          width = 9,
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
