
#' mainGenePanel_ui
#' @description
#' modulo ui per la pagina principale della geneApp
#' @param id
#' l'id assegnato al modulo
#' @examples
#' mainGenePanel_ui("nomemodulo")
mainGenePanel_ui<-function(id){
  ns <- shiny::NS(id) #id del modulo
  #### TABPANEL DELLA NAVBAR PRINCIPALE ####
  shiny::tabPanel(
    title = "GERSOM",
    value = shiny::NS(id,"gersom"),
    id = "gersom",
    class = "topchoice",
    # navbar page start
    #TUTTA QUESTA PARTE PUO ESSERE RIDOTTA SOLO AL PRIMO TAB
    shiny::navbarPage(
      position = "static-top",
      collapsible = TRUE,
      id = "topnavbar2",
        shiny::tags$a(
          id = "toggleSidebar",
          class = "toggleSidebar",
          shiny::tags$span(class="toggleLine"),
          shiny::tags$span(class="toggleLine"),
          shiny::tags$span(class="toggleLine")
        ),
      #cambia qui se vuoi che solo la navbar sia sticky
      #panel1 contains graph and it's menu
      ##### FIRST TAB #####

      geneOverview_ui(ns("overview")),
      # second and third set of nav tabs
      #PER ORA NON FANNO NULLA
      shiny::navbarMenu(title = "tab1", menuName = "tab1", "panel 1.1", shiny::tabPanel("1.1"), "panel 1.2", shiny::tabPanel("1.2")),
      shiny::navbarMenu(title = "tab2", menuName = "tab2", shiny::tabPanel("2.1"), shiny::tabPanel("2.2")),
      #END FLUID ROW AND NAV PAG
    )
  )
}

#' mainGenePanel_server
#' @description
#' modulo server per la pagina principale della geneApp
#' @param id
#' l'id assegnato al modulo
#' @examples
#' mainGenePanel_server("nomemodulo")
mainGenePanel_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      geneOverview_server("overview")
    }
  )
}
