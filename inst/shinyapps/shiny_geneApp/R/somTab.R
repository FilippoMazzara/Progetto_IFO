somTab_ui_sidebar <- function(id){
  ns <- shiny::NS(id)
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
          ),
          shiny::tags$div(
            id = "warning_1",
            shiny::textOutput(shiny::NS(id,"warning1")),

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
  )
}
somTab_ui_table <- function(id){
  ns <- shiny::NS(id)
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
  )
}

somTab_server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # REACTIVE TITLES
      titolo11 <- shiny::reactiveVal("CARICA IL FILE CON il somatico")
      titolo12 <- shiny::reactiveVal("")

      # REACTIVE DATA VALUES
      data1 <- shiny::reactiveVal()
      proc_data1 <- shiny::reactiveVal()
      nomi1 <- shiny::reactiveVal()
      nomi1_f <- shiny::reactiveVal()
      nomi_w <- shiny::reactiveVal()
      nomi_format <- shiny::reactiveVal()

      # REACTIVE FILTERS
      filter_vars1 <- shiny::reactiveValues(l = list())

      # REACTIVE OBSERVERS FOR INPUTS
      obs1 <- shiny::reactiveVal(list())

      # ------ FILE INPUT 1 -------
      shiny::observeEvent(input$fromfile,{    #forse ci va un if(!is.null(input$fromfile))
        titolo11(input$fromfile$name)
        titolo12("")

        d <-  data.table::fread(
          input$fromfile$datapath,
          data.table = FALSE,
          ### TROVA UN MODO MEGLIO DE FA STA' COSA  ###
          #select = c("Actionable.O","Actionable.M","Actionable.C","Moderate.risk","azionabile","depth","High.risk","actionable","Amino_acids","Protein_position","cancervar_tier","tiering","POS","pos","SYMBOL","Symbol","symbol","hugo_symbol" ,"Hugo_Symbol","HUGO_SYMBOL","Gene.refGene","Gene","gene","CHROM", "Chromosome","Chr","chrom","chromosome","Ref","REF","Tumor_Seq_Allele1","Alt","ALT","Tumor_Seq_Allele2","alt","vaf","VAF","Vaf","Consequence","Variant_Classification","Func.refGene","consequence","VARIANT_CLASS","Variant_Type","ExonicFunc.refGene","variant_class","CLIN_SIG","clinvar","Clinvar","Start_Position","start","Start","End_Position","end","End","Existing_variation","AAChange.refGene","Variation","Var","variation","HGVSp_Short","HGVSp","hgvsp","Exon_Number","EXON","exon","Exon"),  #vedi per il select delle variabili
          na.strings = base::getOption("NA")
        )
        data1(d)
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
        #select = c("Actionable.O","Actionable.M","Actionable.C","Moderate.risk","azionabile","depth","High.risk","actionable","Amino_acids","Protein_position","cancervar_tier","tiering","POS","pos","SYMBOL","Symbol","symbol","hugo_symbol" ,"Hugo_Symbol","HUGO_SYMBOL","Gene.refGene","Gene","gene","CHROM", "Chromosome","Chr","chrom","chromosome","Ref","REF","Tumor_Seq_Allele1","Alt","ALT","Tumor_Seq_Allele2","alt","vaf","VAF","Vaf","Consequence","Variant_Classification","Func.refGene","consequence","VARIANT_CLASS","Variant_Type","ExonicFunc.refGene","variant_class","CLIN_SIG","clinvar","Clinvar","Start_Position","start","Start","End_Position","end","End","Existing_variation","AAChange.refGene","Variation","Var","variation","HGVSp_Short","HGVSp","hgvsp","Exon_Number","EXON","exon","Exon")
        col_yes_y <- list()
        col_yes <- list()
        col_no <- list()
        d2 <- NULL
        for (n in names(d)){
          #if table is a numerical range with NA values convert the column to char
          #find another way to implement it
          #here can be implemented other pre processing parameters
          y <- check_names(n)
          if (is.null(y)){col_no <- append(col_no,n)}
          else {
            if(y %in% col_yes){col_no <- append(col_no,n)}
            else{col_yes <- append(col_yes,y)
            col_yes_y <- append(col_yes_y,n)
            }
          }
          #if (anyNA(d[[n]]) && is.numeric(d[[n]])){
          #here converts "" to "NA"
          #x1 <- lapply(d[[n]], function(y) {ifelse(is.na(y), "NA", y)})
          #d[[n]] <- as.character(x1)
          #}
          #here can be implemented other pre processing parameters
        }
        pos <- c("Gene","Chromosome","VAF","Consequence","Variant_Type","Clinvar","Depth","Ref","Alt","Start","End","Variation","HGVSp","Exon")
        for (n in pos){
          l <- which(col_yes == n)

          if(length(l)> 0){

            cl <- col_yes_y[[l]]
            if(is.null(d2)){d2 <- data.frame(nome = d[[cl]])
            data.table::setnames(d2,n)
            }
            else{
              d2[[n]] <- d[[cl]]}
          }
        }
        for(n in col_no){
          if(is.null(d2)){d2 <- data.frame(nome = d[[n]])
          data.table::setnames(d2,n)
          }
          else{
            d2[[n]] <- d[[n]]}
        }
        pos2 <- c("Chromosome","VAF","Consequence","Variant_Type","Clinvar","Depth","Start","End")
        c <- intersect(pos2,col_yes)
        c2 <- setdiff(pos,col_yes)
        form <- c("VAF","Start","End","Depth")
        c3 <- intersect(form,col_yes)

        nomi_format(unlist(c3))
        nomi1(col_yes)
        nomi1_f(c)
        if (length(c2) > 0){
          nomi_w(paste("Le colonne mancanti sono:",paste(c2,collapse = ", "), sep=" "))
        } else {nomi_w("Non ci sono colonne mancanti") }

        proc_data1(d2)
      })

      #DYNAMICALLY CREATE OBSERVERS FOR SOMATIC FILTERS' INPUTS
      shiny::observeEvent(proc_data1(),{
        if(!is.null(proc_data1())){
          ##### per non creare tutti gli observer
          #li <- list()
          #for(n in names(proc_data1())){
          #if( !is.null(check_ui(proc_data1()[[n]]))){
          # li <- append(li,n)
          #}
          #}
          ##### names(proc_data1())
          #create observers fo inputs
          #res <- lapply(li, function (x) {

          li <- nomi1_f()
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
              paste("GSP-",shiny::NS(id,"checkbox1"),sep=""),
              "",
              choices = c(names(proc_data1())),
              selected = nomi1(),#c("Gene","Chromosome","VAF","Consequence","Variant_Type","Clinvar","Depth","Ref","Alt","Start","End","Variation","HGVSp","Exon"),
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                actionsBox= TRUE,
                size = 10,
                liveSearch = T
              ),
              choicesOpt = list(content = stringr::str_trunc(c(names(proc_data1())), width = 40))
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
            #purrr::map(names(proc_data1()), ~ make_ui(proc_data1()[[.x]], .x , id))
            purrr::map(nomi1_f(), ~ make_ui(proc_data1()[[.x]], .x , id, ""))
          )
        )
      })

      # RENDER OF TITLES
      output$titolo11 <- shiny::renderText(titolo11())
      output$titolo12 <- shiny::renderText(titolo12())
      output$warning1 <- shiny::renderText(nomi_w())

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
            scrollX = T,
            scrollCollapse = T,
            dom = '<"row_b" B><"row_i" fl><"row_i" pi>rt<"row_i" pi><"row_e" <"row_e_overlay" >><"row_sc_cont" <"#rowsc1.row_sc"<"row_sc_i" >>>',
            buttons = list('copy', 'excel',#'colvis','colvisRestore',
                           list(extend = "pdf", pageSize = "A3", orientation = "landscape", exportOptions = list(rows = list(page = "all"), columns = ":visible"))
            )
            ,lengthMenu = list(c(10,25,50,100,1000,-1),c(10,25,50,100,1000,"All"))
          )
        ) %>% DT::formatRound(nomi_format())

        #%>% DT::formatRound(c("VAF","Start","End"))
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
        #print(filter_vars1$l)
        #print(input)
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
      #BUTTON TO DISABLE ENTER KEY (OVERVIEW TAB)
      enterDisable_server("inutile")
    })
}
