ui <- shiny::fluidPage(
  id = "mainpage",
  #Styling
  shiny::includeCSS("www/style.css"),
  shiny::includeScript("www/myscript.js"),
  # title header (hidden)
  shiny::tags$div(
    style = "display: none",
    shiny::titlePanel(title = "", windowTitle = "geneApp")
  ),
  #top header
  shiny::navbarPage(
    position = "static-top",
    header = NULL,
    footer = NULL,
    collapsible = TRUE,
    id = "topnavbar",
    fluid = TRUE,
    #logo
    title = shiny::tags$div(
      id = "logo1",
      shiny::tags$a(
        href = "/",
        shiny::tags$img(
          src = "img/logo.jpg",
          height = "75px"
        )
      )
    ),
    #### HOME ####
    homePanel_ui("homepage"),
    #### OVERVIEW ####
    mainGenePanel_ui("overview"),
    #### ABOUT ####
    about_ui("about")
  #END UI
  ),
  shiny::tags$div(
    class = "footer",
    shiny::includeHTML("www/footer.html")
  )
)

#' Main geneApp content
#' @description
#' Contains the server, the ui and the launcher function.
#' The app should not be launched through this file.
#' @param input
#' the input vector
#' @param output
#' the output vector
#' @return the running gene app
server <- function(input, output, session) {
  #modulo home
  homePanel_server("homepage")
  #modulo overview
  mainGenePanel_server("overview")
  shiny::observeEvent(input$linkapp, {
    shiny::updateNavbarPage(session, "topnavbar", "overview-gersom")
  })
  about_server("about")
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
