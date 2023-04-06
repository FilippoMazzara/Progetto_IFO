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
    id = "graph",
    value = shiny::NS(id, "graph"),
    shiny::tags$div(id = "overview_container",
    #declare sidebars layout
        shiny::sidebarLayout(
          #side menu start
          #this menu could be floating or shared across navbar options or unique to this
          shiny::tags$div( id = "sidebar", class="col-sm-3",
          shiny::sidebarPanel(
            width = 3,
            #inputs panel
            #option to hide the panel,hide only one of the inputs ,only allow from server,
            #manage the inputs from file in another location, or by making user choose in advance
            shiny::wellPanel(
              id = "well_1",
              shiny::tags$div(
                class = "toggle_panel",
              shiny::tags$span(
                class = "toggle_panel_text",
                "Seleziona il campione:"
              ),
                shiny::tags$a(
                  id = "toggle_well_input",
                  class="toggle_well_link",
                  `data-toggle`="collapse",
                  `data-target`="#well_input",
                shiny::tags$span(class="toggle_panel_line"))

              ),
              shiny::tags$div( id = "well_input", class = "collapse in",

              #input from filesystem
              shiny::fileInput(
                ns("fromfile"),
                label=htmltools::HTML(
                  'Carica un file .tsv </label><span data-toggle="tooltip" style="float:right" data-placement="right" title="" data-original-title="A tooltip">
              <i class="far fa-circle-question" role="presentation" aria-label="circle-question icon"></i></span>'),
              accept = c(".tsv",".csv")
              ),
              #bottone per disabilitare il tasto enter fuori da i widget
              #una pecionata pero funziona (attento a possibili errori)
              #non lo puoi mettere statico senno non fa il render finche non lo premi

              #input from DB/server
              #guarda le opzioni di selectize
              #anche questo panello puo essere reso interattivo e dinamico,compare diverso e compare solo per una certa condizione
            )
            ),
            shiny::wellPanel(),
            enterDisable_ui(ns("inutile")),
            tooltip_inutile()
          )),
          # Show a plot of the generated distribution, main panel
          shiny::tags$div(id = "colcol",class="col-sm-9",
          shiny::mainPanel(
            width = 9,
            shiny::tabsetPanel(
              shiny::tabPanel(
                "pannello1",
                shiny::tags$h1("Il file che hai caricato:"),
                shiny::fluidRow(
                  id = "tabcontainer",
                  style="display:flex;justify-content:center;",
                  DT::dataTableOutput(ns("table"))
                )
              ),
              shiny::tabPanel("pannello2")
            )
          ))
          #END SIDEBAR LAYOUT
      ))
    )
  #END TAB
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
      data <- shiny::reactive({
        shiny::req(input$fromfile)
        data.table::fread(input$fromfile$datapath, data.table=T)
      })
      cols <- shiny::reactive(names(data()))
      selected <- shiny::reactive({
        each_var <- purrr::map(cols(), ~ filter_var(data()[[.x]], input[[.x]]))
        purrr::reduce(each_var, `&`)
      })
      output$table <- DT::renderDataTable(data() ,
        rownames = FALSE,
        filter = 'top',
        selection = list(mode = "multiple"),
        editable = T,
        class = 'display',
        extensions = 'Buttons',
        options = list(
          autoWidth = TRUE,
          pageLength = 50,
          pagingType = 'full_numbers',
          scrollX = TRUE,
          scrollCollapse = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'excel', 'pdf'),
          lengthMenu = list(c(10,25,50,100,1000,-1),c(10,25,50,100,1000,"All")))
      )
      #bottone inutile
      enterDisable_server("inutile")
    }
  )
}
