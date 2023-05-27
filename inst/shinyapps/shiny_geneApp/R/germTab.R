germTab_ui_sidebar <- function(id){
  ns <- shiny::NS(id)
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
}
germTab_ui_table <- function(id){
  ns <- shiny::NS(id)
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
}

germTab_server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # ------ OVERVIEW DATA -------
      # REACTIVE TITLES

      titolo21 <- shiny::reactiveVal("CARICA IL FILE CON il germinale")
      titolo22 <- shiny::reactiveVal("")

      # REACTIVE DATA VALUES

      data2 <- shiny::reactiveVal()

      proc_data2 <- shiny::reactiveVal()

      nomi2 <- shiny::reactiveVal()

      # REACTIVE FILTERS

      filter_vars2 <- shiny::reactiveValues(l = list())

      # REACTIVE OBSERVERS FOR INPUTS

      obs2 <- shiny::reactiveVal(list())

      # ------ FILE INPUT 1 -------

      #INPUT FROM CLIENT (SINGLE FILE)

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
              paste("GSP-",shiny::NS(id,"checkbox2"),sep=""),
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
            purrr::map(names(proc_data2()), ~ make_ui(proc_data2()[[.x]], .x, id, "2"))
          )
        )
      })


      output$titolo21 <- shiny::renderText(titolo21())
      output$titolo22 <- shiny::renderText(titolo22())





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
            serverSide = F,
            paging = T,
            processing = TRUE,
            autoWidth = F,
            fixedHeader = list(header = F, footer = F),
            pageLength = 25,
            pagingType = 'full_numbers',
            #scrollY = 944,
            #scrollX = T,
            scrollCollapse = T,
            dom = '<"row_b" B><"row_i" fl><"row_i" pi>rt<"#rowsc2.row_sc"<"row_sc_i" >><"row_i" pi><"row_e" >',
            buttons = list('copy', 'excel',#'colvis','colvisRestore',
                           list(extend = "pdf", pageSize = "A3", orientation = "landscape", exportOptions = list(columns = ":visible"))
            )
            ,lengthMenu = list(c(10,25,50,100,1000,-1),c(10,25,50,100,1000,"All"))
          )
        )
        #%>% DT::formatStyle(names(proc_data2()),"text-align"= "center", 'min-width' = '250px','width' = '250px','max-width' = '250px')
        #%>% DT::formatRound(columns = names(proc_data2()))
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


    })
}
