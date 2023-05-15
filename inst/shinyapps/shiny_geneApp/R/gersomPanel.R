#' gersomPanel module ui
#' @description
#' ui module for the comparison of somatic and germ files
#' @param id the module id
#'
#' @examples gersomPanel_ui("modulename")
#'
gersomPanel_ui <- function(id){
  ns <- shiny::NS(id)

  # ------ FIRST TAB -------
  shiny::tabPanel(
    title = "Graph",
    id = "graph",
    value =  "graph",

    #LAYOUT CONTAINER
    shiny::tags$div(
      id = "overview_container",

      # ------ SIDEBAR LAYOUT -------
      shiny::sidebarLayout(

        #SIDEBAR CONTAINER
        shiny::tags$div(
          id = "sidebar",
          class ="col-sm-3",

          # ------ FIRST TAB - SIDEBAR -------

          shiny::sidebarPanel(
            width = 3,
            # SIDEBAR SWITCH
            shiny::tags$div(
              style = "display:flex;justify-content: center;",
              shiny::actionButton(inputId = shiny::NS(id,"sidebar_somatic"), label = "SOMATIC", class = "btn btn-default action-button shiny-bound-input active" ),
              shiny::actionButton(inputId = shiny::NS(id,"sidebar_germ"), label = "GERMLINE")
            ),

            shiny::tags$br(),

            shiny::tabsetPanel(
              id = shiny::NS(id,"sidebar_tabset"),
              shiny::tabPanel(
                title = "",
                value = "som_nav",

                # ------ FILE INPUT - SOMATIC -------
                # SIDEBAR INTERNAL CONTAINER (FILE1)
                shiny::tags$div(
                  id = "file1_sidebar",
                  class = "file_side",

                  shiny::wellPanel(
                    id = "well_1",
                    # DIV TOGGLER
                    toggle_panel("toggle_well_input", "well_input","Seleziona il campione:"),

                    shiny::tags$div(
                      # CONTAINER TOGGLER INPUT ID + CLASS
                      id = "well_input",
                      class = "collapse in",

                      # ------ FILE INPUT - SOMATIC FROM USER FILESYSTEM -------
                      shiny::fileInput(
                        inputId = shiny::NS(id,"fromfile"),
                        # LABEL + TOOLTIP (WORKAROUND)
                        label = htmltools::HTML('Carica un file .tsv </label><span data-toggle="tooltip" style="float:right" data-placement="right" title="" data-original-title="A tooltip"><i class="far fa-circle-question" role="presentation" aria-label="circle-question icon"></i></span>'),
                        # LIST HERE THE ALLOWED FILE FORMATS
                        accept = c(".tsv",".csv",".maf",".xlsx",".xls")
                      ),

                      # ------ FILE INPUT - SOMATIC FROM SERVER FILESYSTEM -------
                      ##### NO BUILT IN SEARCH BAR, LOOK FOR OTHER SOLUTIONS ####
                      shinyFiles::shinyFilesButton(
                        id = shiny::NS(id,"dataset_files"),
                        label = "Seleziona un campione dai datasets...",
                        title = "Pick a file:" ,
                        viewtype = "detail",
                        multiple = FALSE,
                        style = "overflow: hidden; width: auto; max-width: 100%;"
                      ),

                      # FILE NAME SELECTED FROM SERVER (FILE1)
                      shiny::tags$div(
                        shiny::textOutput(shiny::NS(id,"titolo12")),
                        style = "white-space: nowrap;overflow: auto;"
                      )
                    )
                  ), # WELL_1 END (FILE1)

                  # WELL_2 COL SELECTION CHECKBOX (FILE1)
                  shiny::uiOutput(shiny::NS(id,"checkbox1")),

                  # WELL_3 ROW FILTERS PANEL (FILE1)
                  shiny::uiOutput(shiny::NS(id,"filter1")),

                  # DISABLE ENTER KEY PUSH (WORKAROUND)
                  enterDisable_ui("inutile"),
                  # ENABLE TOOLTIPS (WORKAROUND)
                  tooltip_inutile()

                ), # CONTAINER END (FILE1)
              ),
              shiny::tabPanel(
                title = "",
                value = "germ_nav",

                # ------ FILE INPUT - GERM -------
                # SIDEBAR INTERNAL CONTAINER (FILE2)
                shiny::tags$div(
                  id = "file2_sidebar",
                  class = "file_side",

                  shiny::wellPanel(
                    id = "well_2",
                    # DIV TOGGLER
                    toggle_panel("toggle_well_input", "well_input2","Seleziona il campione:"),

                    shiny::tags$div(
                      # CONTAINER TOGGLER INPUT ID + CLASS
                      id = "well_input2",
                      class = "collapse in",

                      # ------ FILE INPUT - GERM FROM USER FILESYSTEM -------
                      shiny::fileInput(
                        inputId = shiny::NS(id,"fromfile2"),
                        # LABEL + TOOLTIP (WORKAROUND)
                        label = htmltools::HTML('Carica un file .tsv </label><span data-toggle="tooltip" style="float:right" data-placement="right" title="" data-original-title="A tooltip"><i class="far fa-circle-question" role="presentation" aria-label="circle-question icon"></i></span>'),
                        # LIST HERE THE ALLOWED FILE FORMATS
                        accept = c(".tsv",".csv")
                      ),

                      # ------ FILE INPUT - GERM FROM SERVER FILESYSTEM -------
                      ##### NO BUILT IN SEARCH BAR, LOOK FOR OTHER SOLUTIONS ####
                      shinyFiles::shinyFilesButton(
                        id = shiny::NS(id,"dataset_files2"),
                        label = "Seleziona un campione dai datasets...",
                        title = "Pick a file:" ,
                        viewtype = "detail",
                        multiple = FALSE,
                        style = "overflow: hidden; width: auto; max-width: 100%;"
                      ),

                      # FILE NAME SELECTED FROM SERVER (FILE2)
                      shiny::tags$div(
                        shiny::textOutput(shiny::NS(id,"titolo22")),
                        style = "white-space: nowrap; overflow: auto;"
                      )

                    )
                  ), # WELL_1 END (FILE2)

                  # WELL_2 COL SELECTION CHECKBOX (FILE2)
                  shiny::uiOutput(shiny::NS(id,"checkbox2")),
                  shiny::uiOutput(shiny::NS(id,"filter2"))
                  # WELL_3 ROW FILTERS PANEL (FILE2)

                  #### MISSING ####

                ) # CONTAINER END (FILE2)
              )
            ),


          ) # FIRST TAB - SIDEBAR END
        ), # FIRST TAB - SIDEBAR CONTAINER END
        # MAIN CONTAINER
        shiny::tags$div(
          id = "colcol",
          class = "col-sm-9",

          # ------ FIRST TAB - MAIN -------
          shiny::mainPanel(
            width = 9,

            shiny::tabsetPanel(
              id = shiny::NS(id,"main_overview_files"),
              # ------ FIRST TAB - MAIN SOMATIC-------
              shiny::tabPanel(
                id = "som_panel",
                title = "pannello1",
                value = "pannello1",

                # TITLE MAIN SOMATIC
                shiny::tags$h1(
                  shiny::textOutput(shiny::NS(id,"titolo11")),
                  style = "white-space: nowrap; overflow: hidden;"
                ),

                # TABLE MAIN SOMATIC
                shiny::fluidRow(
                  id = "tabcontainer",
                  style = "display: flex; justify-content: center;",
                  DT::DTOutput(shiny::NS(id,"som_table"))
                )
              ),

              # ------ FIRST TAB - MAIN GERM-------
              shiny::tabPanel(
                id = "germ_panel",
                title = "pannello2",
                value = "pannello2",

                # TITLE MAIN GERM
                shiny::tags$h1(
                  shiny::textOutput(shiny::NS(id,"titolo21")),
                  style = "white-space: nowrap; overflow: hidden;"
                ),

                # TABLE MAIN GERM
                shiny::fluidRow(
                  id = "tabcontainer2",
                  style = "display: flex; justify-content: center;",
                  DT::dataTableOutput(shiny::NS(id,"germ_table"))
                )
              )
            )
          )
        )
      )
    )
  )
}


#' gersomPanel module server
#' @description
#' server module for the comparison of somatic and germ files
#' @param id the module id
#'
#' @examples gersomPanel_server("modulename")
#'
gersomPanel_server <- function(id){ #oltre id puoi passare altri parametri
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # ------ OVERVIEW DATA -------
      # REACTIVE TITLES
      titolo11 <- shiny::reactiveVal("CARICA IL FILE CON il somatico")
      titolo12 <- shiny::reactiveVal("")
      titolo21 <- shiny::reactiveVal("CARICA IL FILE CON il germinale")
      titolo22 <- shiny::reactiveVal("")

      # REACTIVE DATA VALUES
      data1 <- shiny::reactiveVal()
      data2 <- shiny::reactiveVal()
      proc_data1 <- shiny::reactiveVal()
      proc_data2 <- shiny::reactiveVal()

      # REACTIVE FILTERS
      filter_vars1 <- shiny::reactiveValues(l = list())
      filter_vars2 <- shiny::reactiveValues(l = list())

      # REACTIVE OBSERVERS FO INPUTS
      obs1 <- shiny::reactiveVal(list())
      obs2 <- shiny::reactiveVal(list())

      # ------ FILE INPUT 1 -------

      #INPUT FROM CLIENT (SINGLE FILE)
      shiny::observeEvent(input$fromfile,{    #forse ci va un if(!is.null(input$fromfile))
        titolo11(input$fromfile$name)
        titolo12("")
        data1(
          data.table::fread(
            input$fromfile$datapath,
            data.table = FALSE,
            ### TROVA UN MODO MEGLIO DE FA STA' COSA  ###
            select = c("Actionable.O","Actionable.M","Actionable.C","Moderate.risk","azionabile","depth","High.risk","actionable","Amino_acids","Protein_position","cancervar_tier","tiering","POS","pos","SYMBOL","Symbol","symbol","hugo_symbol" ,"Hugo_Symbol","HUGO_SYMBOL","Gene.refGene","Gene","gene","CHROM", "Chromosome","Chr","chrom","chromosome","Ref","REF","Tumor_Seq_Allele1","Alt","ALT","Tumor_Seq_Allele2","alt","vaf","VAF","Vaf","Consequence","Variant_Classification","Func.refGene","consequence","VARIANT_CLASS","Variant_Type","ExonicFunc.refGene","variant_class","CLIN_SIG","clinvar","Clinvar","Start_Position","start","Start","End_Position","end","End","Existing_variation","AAChange.refGene","Variation","Var","variation","HGVSp_Short","HGVSp","hgvsp","Exon_Number","EXON","exon","Exon"),  #vedi per il select delle variabili
            na.strings = base::getOption("NA")
          )
        )
      })

      #CONNECTION BETWEEN SERVER AND CLIENT TO ACCESS FILESYSTEM
      #con fileGetter puoi mettere delle restrizioni ed implementare una sorta di ricerca files
      #altrimenti vedi altre soluzioni
      #fileGetter(roots, restrictions, filetypes, pattern, hidden = FALSE)
      shinyFiles::shinyFileChoose(
        input = input,
        id = "dataset_files",
        session = session,
        roots = c(wd = "C:/Users/facke/Desktop/datasets"),
        defaultPath="/"
      )

      #INPUT FROM SERVER (SINGLE FILE)
      shiny::observeEvent(input$dataset_files, {
        if (!is.null(input$dataset_files)){
          inFile <- shinyFiles::parseFilePaths(roots = c(wd = "C:/Users/facke/Desktop/datasets"), input$dataset_files)
          if (length(inFile$datapath) != 0 ){
            titolo11(inFile$name)
            titolo12(inFile$name)
            data1(
              data.table::fread(
                as.character(inFile$datapath),
                data.table = F,
                na.strings = base::getOption("NA")
              )
            )
          }}
      })

      # ------ FILE INPUT 2 -------
      #INPUT FROM CLIENT (SINGLE FILE)
      shiny::observeEvent(input$fromfile2,{
        titolo21(input$fromfile2$name)
        titolo22("")
        data2(
          data.table::fread(
            input$fromfile2$datapath,
            data.table = FALSE,
            na.strings = base::getOption("NA")
          )
        )
      })

      #CONNECTION BETWEEN SERVER AND CLIENT TO ACCESS FILESYSTEM
      shinyFiles::shinyFileChoose(
        input = input,
        id = "dataset_files2",
        session = session,
        roots = c(wd = "C:/Users/facke/Desktop/datasets"),
        defaultPath="/")

      #INPUT FROM SERVER (SINGLE FILE)
      shiny::observeEvent(input$dataset_files2, {
        inFile <- shinyFiles::parseFilePaths(roots = c(wd = "C:/Users/facke/Desktop/datasets"), input$dataset_files2)
        if (length(inFile$datapath) != 0 ){
          titolo21(inFile$name)
          titolo22(inFile$name)
          data2(
            data.table::fread(
              as.character(inFile$datapath),
              data.table = F,
              na.strings = base::getOption("NA")
            )
          )
        }
      })

      #------- FILE  PRE PROCESSING --------

      #PROCESS FILE 1
      shiny::observeEvent(data1(),{
        #initialize filter list
        filter_vars1$l <- list()
        #destroy old reactive observers
        if (length(obs1()) > 0){
          for (o in obs1()){
            if (!is.null(o)){
              o$destroy()
              rm(o)
            }
          }
          #reset observers
          obs1(list())
        }
        #pre process data
        d <- data1()
        for (n in names(d)){
          #if table is a numerical range with NA values convert the column to char
          #find another way to implement it
          #here can be implemented other pre processing parameters
          if (anyNA(d[[n]]) && is.numeric(d[[n]])){
            #here converts "" to "NA"
            x1 <- lapply(d[[n]], function(y) {ifelse(is.na(y), "NA", y)})
            d[[n]] <- as.character(x1)
          }
          #here can be implemented other pre processing parameters
        }
        proc_data1(d)
      })

      #PROCESS FILE 2
      shiny::observeEvent(data2(),{
        #initialize filter list
        filter_vars2$l <- list()
        #destroy old reactive observers
        if (length(obs2()) > 0 ){
          for (o in obs2()){
            if (!is.null(o)){
              o$destroy()
              rm(o)
            }
          }
          #reset observers
          obs2(list())
        }
        #pre process data
        d <- data2()
        for (n in names(d)){
          #if table is a numerical range with NA values convert the column to char
          #find another way to implement it
          #here can be implemented other pre processing parameters
          if (anyNA(d[[n]]) && is.numeric(d[[n]])){
            #here converts "" to "NA"
            x1 <- lapply(d[[n]], function(y) {ifelse(is.na(y), "NA", y)})
            d[[n]] <- as.character(x1)
          }
          #here can be implemented other pre processing parameters
        }
        proc_data2(d)
      })

      #DYNAMICALLY CREATE OBSERVERS FOR SOMATIC FILTERS' INPUTS
      shiny::observeEvent(proc_data1(),{
        if(!is.null(proc_data1())){
          ##### per non creare tutti gli observer
          li <- list()
          for(n in names(proc_data1())){
            if( !is.null(check_ui(proc_data1()[[n]]))){
              li <- append(li,n)
            }
          }
          ##### names(proc_data1())
          #create observers fo inputs
          res <- lapply(li, function (x) {
            shiny::observeEvent(input[[x]], {
              #questionable if, check for redundant initialization of observers
              if(!is.null(input[[x]]) && !is.null(proc_data1())){
                f <- filter_var(proc_data1()[[x]], input[[x]])
                if (all(f)){
                  if (x %in% names(filter_vars1$l)){
                    filter_vars1$l[[x]] <- f
                  }
                }
                else if (!all(f)){
                  filter_vars1$l[[x]] <- f
                }
              }
            },ignoreInit = T ) #inner observer end
          }) #lapply end
          obs1(res)
        }
      }, priority =  10) #end outer observer

      #DYNAMICALLY CREATE OBSERVERS FOR GERM FILTERS' INPUTS
      shiny::observeEvent(proc_data2(),{
        if(!is.null(proc_data2())){
          #####
          li <- list()
          for(n in names(proc_data2())){
            if( !is.null(check_ui(proc_data2()[[n]]))){
              li <- append(li,n)
            }
          }
          ##### names(proc_data2())
          #create observers fo inputs
          res <- lapply(li, function (x) {
            cx <- paste(x,"2",sep="")
            shiny::observeEvent(input[[cx]], {
              #questionable if, check for redundant initialization of observers
              if(!is.null(input[[cx]]) && !is.null(proc_data2())){
                f <- filter_var(proc_data2()[[x]], input[[cx]])
                if (all(f)){
                  if (x %in% names(filter_vars2$l)){
                    filter_vars2$l[[x]] <- f
                  }
                }
                else {
                  filter_vars2$l[[x]] <- f
                }
              }
            },ignoreInit = T ) #inner observer end
          }) #lapply end
          obs2(res)
        }
      }, priority =  10) #end outer observer

      #FIRST CHECK BOX
      output$checkbox1 <- shiny::renderUI({
        shiny::req(proc_data1())
        shiny::wellPanel(
          id = "chk1",
          toggle_panel("toggle_chk1", "well_chk_container1","Seleziona le colonne:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_chk_container1",
            class = "collapse in",
            shinyWidgets::pickerInput(
              shiny::NS(id,"checkbox1"),
              "",
              choices = c(names(proc_data1())),
              selected = c(),
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                actionsBox= TRUE,
                size = 10
              ),
              choicesOpt = list(content = stringr::str_trunc(c(names(proc_data1())), width = 40))
            )
          )
        )
      })


      #SECOND CHECK BOX
      output$checkbox2 <- shiny::renderUI({
        shiny::req(proc_data2())
        shiny::wellPanel(
          id = "chk2",
          toggle_panel("toggle_chk2", "well_chk_container2","Seleziona le colonne:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_chk_container2",
            class = "collapse in",
            shinyWidgets::pickerInput(
              shiny::NS(id,"checkbox2"),
              "",
              choices = c(names(proc_data2())),
              selected = c(names(proc_data2())),
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                actionsBox= TRUE,
                size = 10
              ),
              choicesOpt = list(content = stringr::str_trunc(c(names(proc_data2())), width = 40))
            )
          )
        )
      })


      #RENDER FILTERS FOR SOMATIC SIDEBAR
      output$filter1 <- shiny::renderUI({
        shiny::req(proc_data1())
        shiny::wellPanel(
          id = "well_filter1",
          toggle_panel("toggle_filter1", "well_filter_container1","Seleziona i filtri:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_filter_container1",
            class = "collapse in",
            purrr::map(names(proc_data1()), ~ make_ui(proc_data1()[[.x]], .x , id))
          )
        )
      })

      #RENDER FILTERS FOR GERM SIDEBAR
      output$filter2 <- shiny::renderUI({
        shiny::req(proc_data2())
        shiny::wellPanel(
          id = "well_filter2",
          toggle_panel("toggle_filter2", "well_filter_container2","Seleziona i filtri:"),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_filter_container2",
            class = "collapse in",
            purrr::map(names(proc_data2()), ~ make_ui2(proc_data2()[[.x]], .x, id))
          )
        )
      })

      # RENDER OF TITLES
      output$titolo11 <- shiny::renderText(titolo11())
      output$titolo12 <- shiny::renderText(titolo12())
      output$titolo21 <- shiny::renderText(titolo21())
      output$titolo22 <- shiny::renderText(titolo22())



      #------ RENDER SOMATIC TABLE ------
      output$som_table <- DT::renderDT({
        shiny::req(proc_data1())
        DT::datatable(
          proc_data1(),
          extensions = c('Buttons','FixedHeader'),
          rownames = FALSE,
          filter = 'top',
          #fillContainer = T,
          container = htmltools::withTags(table(DT::tableHeader(names(proc_data1())),DT::tableFooter(names(proc_data1())))),
          selection = list(mode = "multiple"),
          editable = FALSE,
          class = 'display',
          options = list(
            order = list(0,"asc"),
            serverSide = TRUE,
            paging = T,
            processing = TRUE,
            autoWidth = F,
            fixedHeader = list(header = F, footer = F),
            pageLength = 25,
            pagingType = 'full_numbers',
            #scrollY = 944,
            #scrollX = T,
            scrollCollapse = T,
            dom = '<"row_b" B><"row_i" fl><"row_i" pi>rt<"#rowsc1.row_sc"<"row_sc_i" >><"row_i" pi><"row_e" >',
            buttons = list('copy', 'excel',#'colvis','colvisRestore',
                           list(extend = "pdf", pageSize = "A3", orientation = "landscape", exportOptions = list(columns = ":visible"))
            )
            ,lengthMenu = list(c(10,25,50,100,1000,-1),c(10,25,50,100,1000,"All"))
          )
        )
        #%>% DT::formatStyle(names(proc_data1()),"text-align"= "center", 'min-width' = '250px','width' = '250px','max-width' = '250px')
      },
      server = T
      )



      ### SOMATIC TABLE PROXY SETUP ###
      ds <- shiny::reactiveVal()
      rec_val <- shiny::reactiveValues(df = NULL)
      proxy1 <- DT::dataTableProxy('som_table')

      ### SOMATIC TABLE FILTERS PROXY ###
      shiny::observe({
        shiny::req(proc_data1())
        shiny::req(filter_vars1$l)
        fv1 <- filter_vars1$l
        if (length(filter_vars1$l) != 0 && !identical(length(filter_vars1$l[[1]]),nrow(proc_data1()))){fv1 <- list()}
        rec_val$df <<- proc_data1()%>% dplyr::filter(purrr::reduce(fv1,`&`,.init = TRUE))
        proxy1 %>% DT::replaceData(rec_val$df, resetPaging = T,rownames = FALSE)
      })

      ### SOMATIC TABLE CHECKBOX PROXY ###
      shiny::observe({
        shiny::req(rec_val$df)
        shiny::req(input$checkbox1)
        chk1 <- input$checkbox1
        if (!identical(union(names(proc_data1()),input$checkbox1), names(proc_data1()))){ chk1 <- c()}
        if (length(ds()) != length(chk1)){
          d <- c()
          for (n in chk1){
            d <- append(d, (which(names(proc_data1()) == n))-1)
          }
          ds(d)
        }
        proxy1 %>% DT::showCols(ds(),reset = T)
      })

      #------ RENDER GERM TABLE ------
      output$germ_table <- DT::renderDataTable({
        shiny::req(proc_data2())
        DT::datatable(
          proc_data2(),
          extensions = c('Buttons','FixedHeader'),
          rownames = FALSE,
          filter = 'top',
          #fillContainer = T,
          container = htmltools::withTags(table(DT::tableHeader(names(proc_data2())),DT::tableFooter(names(proc_data2())))),
          selection = list(mode = "multiple"),
          editable = FALSE,
          class = 'display',
          options = list(
            order = list(0,"asc"),
            serverSide = TRUE,
            paging = T,
            processing = TRUE,
            autoWidth = F,
            fixedHeader = list(header = F, footer = F),
            pageLength = 25,
            pagingType = 'full_numbers',
            #scrollY = 944,
            #scrollX = T,
            scrollCollapse = T,
            dom = '<"row_b" B><"row_i" fl><"row_i" pi>rt<"row_sc"<"row_sc_i" >><"row_i" pi><"row_e" >',
            buttons = list('copy', 'excel',#'colvis','colvisRestore',
                           list(extend = "pdf", pageSize = "A3", orientation = "landscape", exportOptions = list(columns = ":visible"))
            )
            ,lengthMenu = list(c(10,25,50,100,1000,-1),c(10,25,50,100,1000,"All"))
          )
        )
        #%>% DT::formatStyle(names(proc_data2()),"text-align"= "center", 'min-width' = '250px','width' = '250px','max-width' = '250px')
      },
      server = T
      )

      ### GERM TABLE PROXY SETUP ###
      ds2 <- shiny::reactiveVal()
      rec_val2 <- shiny::reactiveValues(df = NULL)
      proxy2 <- DT::dataTableProxy('germ_table')

      ### GERM TABLE FILTERS2 PROXY ###
      shiny::observe({
        shiny::req(proc_data2())
        shiny::req(filter_vars2$l)
        fv2 <- filter_vars2$l
        if (length(filter_vars2$l) != 0 && !identical(length(filter_vars2$l[[1]]),nrow(proc_data2()))){fv2 <- list()}
        rec_val2$df <<- proc_data2()%>% dplyr::filter(purrr::reduce(fv2,`&`,.init = TRUE))
        proxy2 %>% DT::replaceData(rec_val2$df, resetPaging = FALSE,rownames = FALSE) #%>% DT::showCols(ds2(),reset = T)
      })

      ### GERM TABLE CHECKBOX2 PROXY ###
      shiny::observe({
        shiny::req(rec_val2$df)
        shiny::req(input$checkbox2)
        chk2 <- input$checkbox2
        if (!identical(union(names(proc_data2()),input$checkbox2), names(proc_data2()))){ chk2 <- c()}
        if (length(ds2()) != length(chk2)){
          d <- c()
          for (n in chk2){
            d <- append(d, (which(names(proc_data2()) == n))-1)
          }
          ds2(d)
        }
        proxy2 %>% DT::showCols(ds2(),reset = T)
      })

      #BUTTON TO DISABLE ENTER KEY (OVERVIEW TAB)
      enterDisable_server("inutile")

      #JS ON SOMATIC BUTTON PUSH
      shiny::observeEvent(input$sidebar_somatic, {
        shinyjs::runjs(
          'document.querySelector("#GSP-sidebar_somatic").className += " active";
          document.querySelector("#GSP-sidebar_germ").className= "btn btn-default action-button shiny-bound-input";'
        )
        shiny::updateTabsetPanel(session, "main_overview_files", "pannello1")
      })

      #JS ON GERM BUTTON PUSH
      shiny::observeEvent(input$sidebar_germ, {
        shinyjs::runjs(
          'document.querySelector("#GSP-sidebar_germ").className += " active";
          document.querySelector("#GSP-sidebar_somatic").className = "btn btn-default action-button shiny-bound-input";'
        )
        shiny::updateTabsetPanel(session, "main_overview_files", "pannello2")
      })


      ### OVERVIEW PANEL SWITCH BUTTONS + TABS ###
      shiny::observeEvent(input$main_overview_files, {

        if(input$main_overview_files == "pannello1"){
          shinyjs::runjs(
            'document.querySelector("#GSP-sidebar_somatic").className += " active";
            document.querySelector("#GSP-sidebar_germ").className= "btn btn-default action-button shiny-bound-input";'
          )
          shiny::updateTabsetPanel(session, "sidebar_tabset", "som_nav")
        }
        else if (input$main_overview_files == "pannello2"){
          shinyjs::runjs(
            'document.querySelector("#GSP-sidebar_germ").className += " active";
            document.querySelector("#GSP-sidebar_somatic").className = "btn btn-default action-button shiny-bound-input";'
          )
          shiny::updateTabsetPanel(session, "sidebar_tabset", "germ_nav")
        }
      })
    }

  )
}
