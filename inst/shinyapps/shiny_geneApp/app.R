ui <- fluidPage(
  id = "mainpage",
  #Styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # title header (hidden)
  div(
    style = "display: none",
    titlePanel(title = "", windowTitle = "geneApp")
  ),
  #top header
  navbarPage(
    #logo
    title = div(id = "logo1", a(href = "/", img(src = "img/logo.jpg", height = "75px"))),
    position = "static-top",
    header = NULL,
    footer = NULL,
    collapsible = TRUE,
    id = "topnavbar",
    fluid = TRUE,
    tabPanel(title = "Home",id = "home", class = "topchoice"),
    tabPanel(title = "GERSOM",id = "gersom",class = "topchoice",
    # Application body
    # navbar page start
      navbarPage(
        "GERSOM",
        position = c("static-top"), #cambia qui se vuoi che solo la navbar sia sticky
        #panel1 contains graph and it's menu
        ##### FIRST TAB #####
        homepanel_ui("overview"),
        ##### END FIRST TAB #####
        # second and third set of nav tabs
        navbarMenu(title = "tab1", menuName = "tab1", "panel 1.1", tabPanel("1.1"), "panel 1.2", tabPanel("1.2")),
        navbarMenu(title = "tab2", menuName = "tab2", tabPanel("2.1"), tabPanel("2.2")),
        #END FLUID ROW AND NAV PAG
      )
    ),
    tabPanel(title = "About us", id = "about", class = "topchoice"),
  )
  #END UI
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
server <- function(input, output) {
  #modulo overview
  homepanel_server("overview")
}

# Run the application
shinyApp(ui = ui, server = server)
