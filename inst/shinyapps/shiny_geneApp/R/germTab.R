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
            accept = c(".tsv",".csv",".maf",".xlsx",".xls")
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
            style = "white-space: nowrap;overflow: auto;"
          ),

          shiny::tags$div(
            id = "warning_2",
            shiny::textOutput(shiny::NS(id,"warning2")),

          )
        )
      ), # WELL_1 END (FILE2)

      # WELL_2 COL SELECTION CHECKBOX (FILE2)
      shiny::uiOutput(shiny::NS(id,"checkbox2")),

      # WELL_3 ROW FILTERS PANEL (FILE2)
      shiny::uiOutput(shiny::NS(id,"filter2")),

    ), # CONTAINER END (FILE2)
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
      DT::DTOutput(shiny::NS(id,"germ_table"))
    )
  )
}

germTab_server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # REACTIVE TITLES
      titolo21 <- shiny::reactiveVal("CARICA IL FILE CON il germinale")
      titolo22 <- shiny::reactiveVal("")

      # REACTIVE DATA VALUES
      data2 <- shiny::reactiveVal()
      proc_data2 <- shiny::reactiveVal()
      nomi2 <- shiny::reactiveVal()
      nomi2_f <- shiny::reactiveVal()
      nomi2_w <- shiny::reactiveVal()
      nomi2_format <- shiny::reactiveVal()

      # REACTIVE FILTERS
      filter_vars2 <- shiny::reactiveValues(l = list())

      # REACTIVE OBSERVERS FOR INPUTS
      obs2 <- shiny::reactiveVal(list())

      # ------ FILE INPUT 1 -------
      shiny::observeEvent(input$fromfile2,{    #forse ci va un if(!is.null(input$fromfile))
        titolo21(input$fromfile2$name)
        titolo22("")

        d <-  data.table::fread(
          input$fromfile2$datapath,
          data.table = FALSE,
          ### TROVA UN MODO MEGLIO DE FA STA' COSA  ###
          #select = c("Actionable.O","Actionable.M","Actionable.C","Moderate.risk","azionabile","depth","High.risk","actionable","Amino_acids","Protein_position","cancervar_tier","tiering","POS","pos","SYMBOL","Symbol","symbol","hugo_symbol" ,"Hugo_Symbol","HUGO_SYMBOL","Gene.refGene","Gene","gene","CHROM", "Chromosome","Chr","chrom","chromosome","Ref","REF","Tumor_Seq_Allele1","Alt","ALT","Tumor_Seq_Allele2","alt","vaf","VAF","Vaf","Consequence","Variant_Classification","Func.refGene","consequence","VARIANT_CLASS","Variant_Type","ExonicFunc.refGene","variant_class","CLIN_SIG","clinvar","Clinvar","Start_Position","start","Start","End_Position","end","End","Existing_variation","AAChange.refGene","Variation","Var","variation","HGVSp_Short","HGVSp","hgvsp","Exon_Number","EXON","exon","Exon"),  #vedi per il select delle variabili
          na.strings = base::getOption("NA")
        )
        data2(d)
      })

      #CONNECTION BETWEEN SERVER AND CLIENT TO ACCESS FILESYSTEM
      #con fileGetter puoi mettere delle restrizioni ed implementare una sorta di ricerca files
      #altrimenti vedi altre soluzioni
      #fileGetter(roots, restrictions, filetypes, pattern, hidden = FALSE)
      shinyFiles::shinyFileChoose(
        input = input,
        id = "dataset_files2",
        session = session,
        roots = c(wd = "C:/Users/facke/Desktop/datasets"),
        defaultPath="/"
      )

      #INPUT FROM SERVER (SINGLE FILE)
      shiny::observeEvent(input$dataset_files2, {
        if (!is.null(input$dataset_files2)){
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
          }}
      })

      #PROCESS FILE 2
      shiny::observeEvent(data2(),{
        #initialize filter list
        filter_vars2$l <- list()
        #destroy old reactive observers
        if (length(obs2()) > 0){
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

        nomi2_format(unlist(c3))
        nomi2(col_yes)
        nomi2_f(c)
        if (length(c2) > 0){
          nomi2_w(paste("Le colonne mancanti sono:",paste(c2,collapse = ", "), sep=" "))
        } else {nomi2_w("Non ci sono colonne mancanti") }

        proc_data2(d2)
      })

      #DYNAMICALLY CREATE OBSERVERS FOR SOMATIC FILTERS' INPUTS
      shiny::observeEvent(proc_data2(),{
        if(!is.null(proc_data2())){
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

          li <- nomi2_f()
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
                else if (!all(f)){
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
              selected = nomi2(),#c("Gene","Chromosome","VAF","Consequence","Variant_Type","Clinvar","Depth","Ref","Alt","Start","End","Variation","HGVSp","Exon"),
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                actionsBox= TRUE,
                size = 10,
                liveSearch = T
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
          toggle_panel("toggle_filter2", "well_filter_container2","Seleziona i filtri:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_filter_container2",
            class = "collapse in",
            #purrr::map(names(proc_data1()), ~ make_ui(proc_data1()[[.x]], .x , id))
            purrr::map(nomi2_f(), ~ make_ui(proc_data2()[[.x]], .x , id, "2"))
          )
        )
      })

      # RENDER OF TITLES
      output$titolo21 <- shiny::renderText(titolo21())
      output$titolo22 <- shiny::renderText(titolo22())
      output$warning2 <- shiny::renderText(nomi2_w())

      #------ RENDER GERM TABLE ------

      output$germ_table <- DT::renderDT({
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
            scrollX = T,
            scrollCollapse = T,
            dom = '<"row_b" B><"row_i" fl><"row_i" pi>rt<"row_i" pi><"row_e" <"row_e_overlay" >><"row_sc_cont" <"#rowsc2.row_sc"<"row_sc_i" >>>',
            buttons = list('copy', 'excel',#'colvis','colvisRestore',
                           list(extend = "pdf", pageSize = "A3", orientation = "landscape", exportOptions = list(rows = list(page = "all"), columns = ":visible"))
            )
            ,lengthMenu = list(c(10,25,50,100,1000,-1),c(10,25,50,100,1000,"All"))
          )
        ) %>% DT::formatRound(nomi2_format())

        #%>% DT::formatRound(c("VAF","Start","End"))
        #%>% DT::formatStyle(names(proc_data1()),"text-align"= "center", 'min-width' = '250px','width' = '250px','max-width' = '250px')

      },
      server = T
      )

      ### GERM TABLE PROXY SETUP ###
      ds2 <- shiny::reactiveVal()
      rec_val2 <- shiny::reactiveValues(df = NULL)
      proxy2 <- DT::dataTableProxy('germ_table')

      ### GERM TABLE FILTERS PROXY ###
      shiny::observe({
        shiny::req(proc_data2())
        shiny::req(filter_vars2$l)

        fv <- filter_vars2$l
        if (length(filter_vars2$l) != 0 && !identical(length(filter_vars2$l[[1]]),nrow(proc_data2()))){fv <- list()}
        rec_val2$df <<- proc_data2()%>% dplyr::filter(purrr::reduce(fv,`&`,.init = TRUE))
        proxy2 %>% DT::replaceData(rec_val2$df, resetPaging = T,rownames = FALSE)
      })

      ### GERM TABLE CHECKBOX PROXY ###
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
      # END GERM SERVER
    })
}
