#' geneOverview_ui
#' @description
#' modulo ui per la main overview
#' @param id
#' l'id assegnato al modulo
#' @examples
#' geneOverview_ui("nomemodulo")
geneOverview_ui<-function(id){
  ns <- shiny::NS(id) #id del modulo
  shiny::tabPanel(
    title = "Graph",
    value = shiny::NS(id, "graph"),
    #declare sidebars layout
    shiny::sidebarLayout(
      #side menu start
      #this menu could be floating or shared across navbar options or unique to this
      shiny::sidebarPanel(
        #inputs panel
        #option to hide the panel,hide only one of the inputs ,only allow from server,
        #manage the inputs from file in another location, or by making user choose in advance
        shiny::wellPanel(
          tooltip_inutile(),
          #input from filesystem
          shiny::fileInput(
            ns("fromfile"),
            label=htmltools::HTML(
              'Carica un file .tsv </label><span data-toggle="tooltip" style="float:right" data-placement="right" title="" data-original-title="A tooltip">
              <i class="far fa-circle-question" role="presentation" aria-label="circle-question icon"></i></span>'),
            accept = ".tsv"),
          #bottone per disabilitare il tasto enter fuori da i widget
          #una pecionata pero funziona (attento a possibili errori)
          #non lo puoi mettere statico senno non fa il render finche non lo premi
          enterDisable_ui(ns("inutile")),
          #input from DB/server
          #guarda le opzioni di selectize
          #anche questo panello puo essere reso interattivo e dinamico,compare diverso e compare solo per una certa condizione
        )
      ),
    # Show a plot of the generated distribution, main panel
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("pannello1"),
          shiny::tabPanel("pannello2")
        )
      )
    #END SIDEBAR LAYOUT
    )
  #END TAB
  )
}

#' geneOverview_server
#' @description
#' modulo server per la main overview
#' @param id
#' l'id assegnato al modulo
#' @examples
#' geneOverview_server("nomemodulo")
geneOverview_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      #bottone inutile
      enterDisable_server("inutile")
    }
  )
}
