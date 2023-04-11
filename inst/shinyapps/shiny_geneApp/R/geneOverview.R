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
            shiny::tags$div( style = "display:flex;justify-content: center;",
            shiny::actionButton(inputId = "sidebar_somatic", label="SOMATIC", class = "btn btn-default action-button shiny-bound-input active" ),
            shiny::actionButton(inputId = "sidebar_germ", label="GERMLINE")),
            shiny::tags$br(),

            #inputs panel
            #option to hide the panel,hide only one of the inputs ,only allow from server,
            #manage the inputs from file in another location, or by making user choose in advance
            shiny::tags$div( id = "file1_sidebar", class= "file_side",
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
              shinyFiles::shinyFilesButton(
                id = ns("dataset_files"),
                label= "Seleziona un campione dai datasets...",
                title = "Pick a file:" ,
                viewtype = "detail",
                multiple = FALSE,
                style= "overflow: hidden;
                        width: auto;
                        max-width: 100%;"
              ),
              shiny::tags$div(shiny::textOutput(ns("titolo12")), style="white-space: nowrap;overflow: auto;")
              #bottone per disabilitare il tasto enter fuori da i widget
              #una pecionata pero funziona (attento a possibili errori)
              #non lo puoi mettere statico senno non fa il render finche non lo premi

              #input from DB/server
              #guarda le opzioni di selectize
              #anche questo panello puo essere reso interattivo e dinamico,compare diverso e compare solo per una certa condizione
            )
            ),

            shiny::uiOutput(ns("checkbox1")),

            shiny::uiOutput(ns("filter")),

            enterDisable_ui(ns("inutile")),
            tooltip_inutile()
          ),
          #---------------------------
          shiny::tags$div(id = "file2_sidebar", class= "file_side",
                          shiny::wellPanel(
                            id = "well_2",
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
                                `data-target`="#well_input2",
                                shiny::tags$span(class="toggle_panel_line"))

                            ),
                            shiny::tags$div( id = "well_input2", class = "collapse in",

                                             #input from filesystem
                                             shiny::fileInput(
                                               ns("fromfile2"),
                                               label=htmltools::HTML(
                                                 'Carica un file .tsv </label><span data-toggle="tooltip" style="float:right" data-placement="right" title="" data-original-title="A tooltip">
              <i class="far fa-circle-question" role="presentation" aria-label="circle-question icon"></i></span>'),
              accept = c(".tsv",".csv")
                                             ),
              shinyFiles::shinyFilesButton(
                id = ns("dataset_files2"),
                label= "Seleziona un campione dai datasets...",
                title = "Pick a file:" ,
                viewtype = "detail",
                multiple = FALSE,
                style= "overflow: hidden;
                        width: auto;
                        max-width: 100%;"
              ),
              shiny::tags$div(shiny::textOutput(ns("titolo22")), style="white-space: nowrap;overflow: auto;")
              #bottone per disabilitare il tasto enter fuori da i widget
              #una pecionata pero funziona (attento a possibili errori)
              #non lo puoi mettere statico senno non fa il render finche non lo premi

              #input from DB/server
              #guarda le opzioni di selectize
              #anche questo panello puo essere reso interattivo e dinamico,compare diverso e compare solo per una certa condizione
                            )
                          ),
                shiny::uiOutput(ns("checkbox2")),
          )
          )

          ),
          # Show a plot of the generated distribution, main panel
          shiny::tags$div(id = "colcol",class="col-sm-9",
          shiny::mainPanel(
            width = 9,
            shiny::tabsetPanel(
              shiny::tabPanel(
                "pannello1",
                shiny::tags$h1(shiny::textOutput(ns("titolo11")), style ="white-space: nowrap;overflow: hidden;"),
                shiny::fluidRow(
                  id = "tabcontainer",
                  style="display:flex;justify-content:center;",
                  DT::dataTableOutput(ns("table"))
                )
              ),
              shiny::tabPanel("pannello2",
              shiny::tags$h1(shiny::textOutput(ns("titolo21")), style ="white-space: nowrap;overflow: hidden;"),
                shiny::fluidRow(
                  id = "tabcontainer2",
                  style="display:flex;justify-content:center;",
                  DT::dataTableOutput(ns("table2"))
                )


              )
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
#' @importFrom magrittr "%>%"
#' @examples
#' geneOverview_server("nomemodulo")
geneOverview_server <- function(id, ch2 =NULL, ch = NULL, selected = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      titolo11 <- shiny::reactiveVal("CARICA IL FILE CON il somatico")
      titolo12 <- shiny::reactiveVal("")
      titolo21 <- shiny::reactiveVal("CARICA IL FILE CON il germinale")
      titolo22 <- shiny::reactiveVal("")
      data1 <- shiny::reactiveVal(NULL)
      data2 <- shiny::reactiveVal(NULL)
      allFiles <- shiny::reactiveValues( data1 = shiny::reactiveVal(), data2= shiny::reactiveVal())



      shiny::observeEvent(input$fromfile,{
        #shiny::req(input$fromfile)
        titolo11(input$fromfile$name)
        titolo12("")
        data1(data.table::fread(input$fromfile$datapath, data.table=F,
                                select= c("V1","vaf")
                                ))
      })

      shinyFiles::shinyFileChoose(input = input, id = "dataset_files", session = session, roots=c(wd = "C:/Users/facke/Desktop/datasets"), defaultPath="/")

      shiny::observeEvent(input$dataset_files, {
        #shiny::req(input$dataset_files)
        inFile <- shinyFiles::parseFilePaths(roots=c(wd="C:/Users/facke/Desktop/datasets"), input$dataset_files)
        if (length(inFile$datapath) != 0 ){
          titolo11(inFile$name)
          titolo12(inFile$name)
          data1(data.table::fread(as.character(inFile$datapath), data.table=F))}
      })
      #---------------------
      shiny::observeEvent(input$fromfile2,{
        #shiny::req(input$fromfile)
        titolo21(input$fromfile2$name)
        titolo22("")
        data2(data.table::fread(input$fromfile2$datapath, data.table=F))

      })

      shinyFiles::shinyFileChoose(input = input, id = "dataset_files2", session = session, roots=c(wd = "C:/Users/facke/Desktop/datasets"), defaultPath="/")

      shiny::observeEvent(input$dataset_files2, {
        #shiny::req(input$dataset_files)
        inFile <- shinyFiles::parseFilePaths(roots=c(wd="C:/Users/facke/Desktop/datasets"), input$dataset_files2)
        if (length(inFile$datapath) != 0 ){
          titolo21(inFile$name)
          titolo22(inFile$name)
          data2(data.table::fread(as.character(inFile$datapath), data.table=F))
          }
      })
      #--------------------


      output$checkbox1 <- shiny::renderUI({
        #shiny::req(!is.null(data()))
        if (!is.null(data1())){
        shiny::wellPanel(
        shiny::checkboxGroupInput(
          inputId = "checkbox1",
          label = "seleziona le colonne: ",
          choices = c(names(data1())),
          selected = c(names(data1()))
        ))}
      })

      output$checkbox2 <- shiny::renderUI({
        #shiny::req(!is.null(data2()))
        #if (!is.null(data2())){
        shiny::wellPanel(
        shiny::checkboxGroupInput(
          inputId = "checkbox2",
          label = "",
          choices = c(names(data2())),
          selected = c(names(data2()))
        ))
      })


      #ch <- shiny::reactive({input$checkbox1})



      filteredData2 <- shiny::reactive({

        d2 <- data2()
        if(is.null(d2)){return(NULL)}
        if(!is.null(ch2())){
          d2 <- d2 %>% dplyr::select(dplyr::any_of(ch2()))
        }
        return(d2)
      })
      vars <- shiny::reactive(names(data1()))

      output$filter <- shiny::renderUI(
        if (!is.null(data1())){
          shiny::wellPanel(
            purrr::map(vars(), ~ make_ui(data1()[[.x]], .x)))}
      )

      sel <- shiny::reactive({
        if (!is.null(selected())){
          each_var <- purrr::map(vars(), ~ filter_var(data1()[[.x]], selected()[[.x]]))
          purrr::reduce(each_var, `&`)}
      })

      fd <-  shiny::reactive({

        d <- data1()
        if(is.null(d)){return(NULL)}
        if (!is.null(sel())){
        d <- d %>% filter(sel())}
        return(d)
      })


      filteredData <- shiny::reactive({

        d <- fd()
        if(is.null(d)){return(NULL)}
        if(!is.null(ch())){

          d <- d %>% dplyr::select(dplyr::any_of(ch()))
        }
        return(d)
      })

      output$titolo11 <- shiny::renderText(titolo11())
      output$titolo12 <- shiny::renderText(titolo12())
      output$titolo21 <- shiny::renderText(titolo21())
      output$titolo22 <- shiny::renderText(titolo22())

      output$table <- DT::renderDataTable({
        DT::datatable(
          filteredData(),
          extensions = 'Buttons',
          rownames = FALSE,
          filter = 'top',
          selection = list(mode = "multiple"),
          editable = F,
          class = 'display',
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
      },server =T)

      output$table2 <- DT::renderDataTable({
        DT::datatable(
          filteredData2(),
          extensions = 'Buttons',
          rownames = FALSE,
          filter = 'top',
          selection = list(mode = "multiple"),
          editable = F,
          class = 'display',
          options = list(
            autoWidth = TRUE,
            pageLength = 50,
            pagingType = 'full_numbers',
            scrollX = TRUE,
            scrollCollapse = TRUE,
            dom = 'Blfrtip',
            buttons = c('copy', 'excel', 'pdf'),
            lengthMenu = list(c(10,25,50,100,1000,-1),c(10,25,50,100,1000,"All"))))
        },server=T)
      #bottone inutile
      enterDisable_server("inutile")

    }
  )
}
