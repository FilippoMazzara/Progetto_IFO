ui <- shiny::fluidPage(
  id = "mainpage",
  #Styling
  shinyjs::useShinyjs(),
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
      shiny::tags$a(
        href = "/",
        shiny::tags$img(
          id = "logo1",
          src = "img/logo.jpg"
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
  br(),
  shiny::tags$div(
    class = "footer",
    shiny::includeHTML("www/footer.html")
  )
)

#' Main geneApp content
#' @description
#' Contains the server, the ui and the launcher function.
#' The app should not be launched through this file.
#' @importFrom magrittr "%>%"
#' @param input
#' the input vector
#' @param output
#' the output vector
#' @return the running gene app
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  #modulo home
  homePanel_server("homepage")
  #modulo overview
  mainGenePanel_server("overview")
  #link
  shiny::observeEvent(
    input$linkapp,
    {shiny::updateNavbarPage(session, "topnavbar", "overview-gersom")}
  )
  shinyjs::onclick(id= "toggleSidebar", {
    shinyjs::runjs('var x = document.querySelector("#mainpage > div > div.tab-content > div > nav > div > div");
                   var y = document.querySelector("#colcol")
                    if (x.style.width == "93px") {x.style.width="24.5%";y.style.width="75%";} else {x.style.width="93px";y.style.width="100%";}
                    ')
    shinyjs::toggle(
      id = "sidebar",
      anim = TRUE,
      animType = "fade",
      time = 0.1)
  })
  #modulo about
  about_server("about")

}

# Run the application
shiny::shinyApp(ui = ui, server = server)
