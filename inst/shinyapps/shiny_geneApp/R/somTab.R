#' Somatic page sidebar ui module
#' @description
#' the function for creating the sidebar elements in the somatic visualization page
#' @param id the id assigned to the module
#' @return the instance of the sidebar
#' @examples somTab_ui_sidebar("SOM")
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

      # WELL_4 ROW EXPORT PANEL (FILE1)
      shiny::uiOutput(shiny::NS(id,"export1")),

      # DISABLE ENTER KEY PUSH (WORKAROUND)
      enterDisable_ui("inutile"),
      # ENABLE TOOLTIPS (WORKAROUND)
      tooltip_inutile()

    ), # CONTAINER END (FILE1)
  )
}

#' Somatic page table ui module
#' @description
#' the function for creating the main and table elements in the somatic visualization page
#' @param id
#' the id assigned to the element
#' @return
#' the instance of the main somatic page
#' @examples somTab_ui_table("SOM")
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
    shiny::textOutput(shiny::NS(id,"file_error")),
    shiny::fluidRow(
      id = "plotcontainer",
      #shiny::plotOutput(shiny::NS(id,"som_plot2"))
      shiny::uiOutput(shiny::NS(id,"som_plot"))
    ),
    # TABLE MAIN SOMATIC
    shiny::fluidRow(
      id = "tabcontainer",
      style = "display: flex; justify-content: center;",
      DT::DTOutput(shiny::NS(id,"som_table"))
    )
  )
}

#' Somatic page server module
#' @description
#' The module containing the server-side of the somatic page
#' @param id the id assigned to the module
#' @return the server instance
#' @examples somTab_server("SOM")
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
      maf_data1 <- shiny::reactiveVal()
      nomi1 <- shiny::reactiveVal()
      nomi1_f <- shiny::reactiveVal()
      nomi_w <- shiny::reactiveVal()
      nomi_format <- shiny::reactiveVal()
      file_error <- shiny::reactiveVal()

      # REACTIVE FILTERS
      filter_vars1 <- shiny::reactiveValues(l = list())

      # REACTIVE OBSERVERS FOR INPUTS
      obs1 <- shiny::reactiveVal(list())

      # ------ FILE INPUT 1 -------
      shiny::observeEvent(input$fromfile,{
        file_error("")
        t <- try(
          data.table::fread(
            input$fromfile$datapath,
            data.table = FALSE,
            na.strings = base::getOption("NA")
          ),
          silent = T)

        if (inherits(t, "try-error")){
          file_error("Il file non puo essere letto")
          data1(NULL)
        }
        else{
          titolo11(input$fromfile$name)
          titolo12("")
          data1(t)
        }

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
            file_error("")
            t <- try(
              data.table::fread(
                as.character(inFile$datapath),
                data.table = F,
                na.strings = base::getOption("NA")
              ),
            silent = T)

            if (inherits(t, "try-error")){
              file_error("Il file non puo essere letto")
              data1(NULL)
            }
            else{
              titolo11(inFile$name)
              titolo12(inFile$name)
              data1(t)
            }
          }
        }
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
        d_n <- names(data1())
        col_yes_y <- list()
        col_yes <- list()
        col_no <- list()
        d2 <- NULL
        d3 <- NULL
        for (n in d_n){
          y <- check_names(n)
          if (is.null(y)){col_no <- append(col_no,n)}
          else {
            if(y %in% col_yes){col_no <- append(col_no,n)}
            else{
              col_yes <- append(col_yes,y)
              col_yes_y <- append(col_yes_y,n)
            }
          }
        }

        pos <- c("Gene","Hugo_Symbol","Chromosome","VAF","Variant_Classification","Variant_Type","VARIANT_CLASS","CLIN_SIG","t_depth","Reference_Allele","Tumor_Seq_Allele2","Start_Position","End_Position","Existing_Variation","HGVSp","EXON","Tumor_Sample_Barcode")
        col_mancanti <- setdiff(pos,col_yes)
        if (length(col_mancanti)>0){
          for(n in col_mancanti){
            if (n == "Hugo_Symbol"){
              p <- which(d_n %in% c("SYMBOL","Symbol","symbol"))
              if(length(p)>0){
                col_yes <- append(col_yes,"Hugo_Symbol")
                col_yes_y <- append(col_yes_y,d_n[[p]])
              }
            }
            else if (n == "Reference_Allele"){
              p <- which(d_n %in% c("Tumor_Seq_Allele1"))
              if(length(p)>0){
                col_yes <- append(col_yes,"Reference_Allele")
                col_yes_y <- append(col_yes_y,d_n[[p]])
              }
            }

            else if (n == "HGVSp"){
              p <- which(d_n %in% c("HGVSp_Short"))
              if(length(p)>0){
                col_yes <- append(col_yes,"HGVSp")
                col_yes_y <- append(col_yes_y,d_n[[p]])
              }
            }
            else if (n == "EXON"){
              p <- which(d_n %in% c("Exon_Number"))
              if(length(p)>0){
                col_yes <- append(col_yes,"EXON")
                col_yes_y <- append(col_yes_y,d_n[[p]])
              }
            }
            else if (n == "VAF"){
              ##calcola il vaf
            }
            else if (n == "Tumor_Sample_Barcode"){
              col_yes <- append(col_yes,"Tumor_Sample_Barcode")
              col_yes_y <- append(col_yes_y,"Tumor_Sample_Barcode")
              d$Tumor_Sample_Barcode <- substring(titolo11(),1,18)
            }
          }
        }

        if(!("Variant_Type" %in% col_yes) && ("VARIANT_CLASS" %in% col_yes)){
          ref <- NULL
          if ("Reference_Allele" %in% col_yes){
            j <- which(col_yes == "Reference_Allele")
            ref_j <- col_yes_y[[j]]
            ref <- d[[ref_j]]
          }
          alt <- NULL
          if ("Tumor_Seq_Allele2" %in% col_yes){
            k <- which(col_yes == "Tumor_Seq_Allele2")
            alt_k <- col_yes_y[[k]]
            alt <- d[[alt_k]]
          }
          i <- which(col_yes == "VARIANT_CLASS")
          vc_i <- col_yes_y[[i]]
          vc <- d[[vc_i]]
          l <- vc_gen(vc,ref,alt)
          if (length(col_yes)>6){
            col_yes <- append(col_yes,list(x = "Variant_Type"),5)
            col_yes_y <- append(col_yes_y,list(x = "Variant_Type"),5)
          }
          else{
            col_yes <- append(col_yes,"Variant_Type")
            col_yes_y <- append(col_yes_y,"Variant_Type")
          }
          d$Variant_Type <- l
        }

        if (!("Variant_Classification" %in% col_yes) && ("Consequence" %in% col_no || "consequence" %in% col_no)){
          vt <- NULL
          if ("Variant_Type" %in% col_yes){
            j <- which(col_yes == "Variant_Type")
            vt_j <- col_yes_y[[j]]
            vt <- d[[vt_j]]
          }
          cq <- list()
          if ("Consequence" %in% col_no){
            cq <- d[["Consequence"]]
          }
          else {
            cq <- d[["consequence"]]
          }
          l <- consq_gen(cq,vt)
          if (length(col_yes)>5){
            col_yes <- append(col_yes,list(x = "Variant_Classification"),4)
            col_yes_y <- append(col_yes_y,list(x = "Variant_Classification"),4)
          }
          else{
            col_yes <- append(col_yes,"Variant_Classification")
            col_yes_y <- append(col_yes_y,"Variant_Classification")
          }
          d$Variant_Classification <- l
        }

        for (n in pos){
          l <- which(col_yes == n)

          if(length(l)> 0){
            nok <- check_names_ok(n)
            cl <- col_yes_y[[l]]
            if(is.null(d2)){
              d2 <- data.frame(nome = d[[cl]])
              data.table::setnames(d2,nok)
            }
            else{
              d2[[nok]] <- d[[cl]]
            }
            if(is.null(d3)){
              d3 <- data.frame(nome = d[[cl]])
              data.table::setnames(d3,n)
            }
            else{
              d3[[n]] <- d[[cl]]
            }
          }
        }

        for(n in col_no){
          if(is.null(d2)){
            d2 <- data.frame(n = d[[n]])
            data.table::setnames(d2,n)
          }
          else{
            d2[[n]] <- d[[n]]
          }
          if(is.null(d3)){
            d3 <- data.frame(n = d[[n]])
            data.table::setnames(d3,n)
          }
          else{
            d3[[n]] <- d[[n]]
          }
        }
        #qui puoi provare a fare il check su variant_classification
        # if ("Variant_Classification" %in% col_yes){effettua in place il check in d2 e d3}
        pos2 <- c("Chromosome","VAF","Classification","Variant Type","Variant Class","Clinvar","Depth","Start","End")
        pos3 <- c("Hugo_Symbol","Chromosome","Variant_Classification","Variant_Type","VARIANT_CLASS","Reference_Allele","Tumor_Seq_Allele2","Start_Position","End_Position","Tumor_Sample_Barcode")

        pos_ok <- c("Gene","Hugo Symbol","Chromosome","VAF","Classification","Variant Type","Variant Class","Clinvar","Depth","Ref","Alt","Start","End","Variation","HGVSp","Exon")
        form <- c("VAF","Start","End","Depth") #nomi di colonne da formattare nella table
        if (!is.null(d2)){
          nomi_format(unlist(intersect(form,names(d2))))
          nomi1_f(unlist(intersect(pos2,names(d2))))
          nomi1(unlist(intersect(pos_ok,names(d2))))
        }
        else{
          nomi_format(c())
          nomi1_f(c())
          nomi1(c())
        }

        c2 <- setdiff(pos_ok,names(d2))
        if (length(c2) > 0){
          nomi_w(paste("Le colonne mancanti sono:",paste(c2,collapse = ", "), sep=" "))
        }
        else {
          nomi_w("Non ci sono colonne mancanti")
        }

        if (length(setdiff(pos3,names(d3)))==0){
          #print(names(d2))
          t <- try(
            maftools::read.maf(d3),
            silent = T
          )
          if (inherits(t, "try-error")){
            maf_data1(NULL)
          }
          else{
            maf_data1(d3)
          }
        }
        else{maf_data1(NULL)}
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

            purrr::map(nomi1_f(), ~ make_ui(proc_data1()[[.x]], .x , id, "")),
            shiny::tags$div(
              id = "filter1_controls_cont",
              shiny::actionButton(inputId = "GSP-SOM-reset_filter1", label = "Reset Filters")
            )

          )
        )
      })

      error_txt <- shiny::reactiveVal("")
      output$export_error1 <- shiny::renderText({

          error_txt()

      })

      output$pdf_export1 <- shiny::renderUI({
        shiny::req(input$export1_types)
        if (input$export1_types == "pdf"){
          shiny::tags$div(
            id = "pdf_export_cont1",
            shinyWidgets::pickerInput(
              paste("GSP-",shiny::NS(id,"pdf_export_options1"),sep=""),
              "",
              choices = c("Template 1","Template 2","Template 3"),
              selected = c("Template 1"),
              multiple = F,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                size = 10,
              )
            ),
          )
        }
        else{NULL}
      })

      output$every_export1 <- shiny::renderUI({
        shiny::req(input$export1_types)
        if (input$export1_types %in% c("tsv","csv","xlsx")){
          shiny::tags$div(
            id = "every_export_cont1",
            shinyWidgets::materialSwitch(
              inputId = "GSP-SOM-every_export_switch1",
              label = "only selected cols",
              right = TRUE
            )
          )
        }
        else{NULL}
      })

      output$maf_export1 <- shiny::renderUI({
        shiny::req(input$export1_types)
        if (input$export1_types == "maf"){
          shiny::tags$div(
            id = "maf_export_cont1",
            shiny::actionButton(inputId = "GSP-SOM-export_genesum1_mock",label = "Gene summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "GSP-SOM-export_genesum1",style = "display:none;"),
            shiny::actionButton(inputId = "GSP-SOM-export_samplesum1_mock",label = "Sample summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "GSP-SOM-export_samplesum1",style = "display:none;"),
            shiny::actionButton(inputId = "GSP-SOM-export_mafsummary1_mock",label = "MAF summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "GSP-SOM-export_mafsummary1",style = "display:none;")
          )
        }
        else{NULL}
      })

      pre_genesummary_data <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$export_genesum1_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt(e)
        if (is.null(maf_data1())){
          ok<-F
          e <- "il dataset non è compatibile con maf"
        }
        else{
          if (input$export1_modes == "All"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf(maf_data1()))
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Non si può generare il gene summary"
              ok <- F
            }
            else{
              d <- t
            }
          }
          else if (input$export1_modes == "page"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((maf_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_current,]))
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Non si può generare il gene summary"
              ok <- F
            }
            else{
              d <- t
            }
          }
          else if (input$export1_modes == "filtered"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((maf_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_all,]))
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Non si può generare il gene summary"
              ok <- F
            }
            else{
              d <- t
            }
          }
        }
        if(ok){
          if (is.null(d)){error_txt("ERRORE")}
          else{
            pre_genesummary_data(d)
            shinyjs::click("export_genesum1")
          }
        }
        else {
          error_txt(e)
        }
      })

      output$export_genesum1 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo11(),1,20),"_", Sys.Date(),"_GeneSummary_",input$export1_modes,".txt", sep="")
        },
        content = function(file) {
            try(
              write_gene_summary(pre_genesummary_data(),file)
              ,silent = T
            )
        }
      )
      shiny::outputOptions(output, "export_genesum1", suspendWhenHidden = FALSE)

      pre_samplesum_data <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$export_samplesum1_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt(e)
        if (is.null(maf_data1())){
          ok<-F
          e <- "il dataset non è compatibile con maf"
        }
        else{
          if (input$export1_modes == "All"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf(maf_data1()))
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Non si può generare il sample summary"
              ok <- F
            }
            else{
              d <- t
            }
          }
          else if (input$export1_modes == "page"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((maf_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_current,]))
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Non si può generare il sample summary"
              ok <- F
            }
            else{
              d <- t
            }
          }
          else if (input$export1_modes == "filtered"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((maf_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_all,]))
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Non si può generare il sample summary"
              ok <- F
            }
            else{
              d <- t
            }
          }
        }
        if(ok){
          if (is.null(d)){error_txt("ERRORE")}
          else{
            pre_genesummary_data(d)
            shinyjs::click("export_samplesum1")
          }
        }
        else {
          error_txt(e)
        }
      })

      output$export_samplesum1 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo11(),1,20),"_", Sys.Date(),"_SampleSummary_",input$export1_modes,".txt", sep="")
        },
        content = function(file) {
          try(
            write_sample_summary(pre_samplesum_data(),file)
            ,silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_samplesum1", suspendWhenHidden = FALSE)

      pre_mafsummary_data <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$export_mafsummary1_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt(e)
        if (is.null(maf_data1())){
          ok<-F
          e <- "il dataset non è compatibile con maf"
        }
        else{
          if (input$export1_modes == "All"){
            t <- try(
              maftools::read.maf(maf_data1())@summary
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Non si può generare il maf summary"
              ok <- F
            }
            else{
              d <- t
            }
          }
          else if (input$export1_modes == "page"){
            t <- try(
              maftools::read.maf((maf_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_current,])@summary
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Non si può generare il maf summary"
              ok <- F
            }
            else{
              d <- t
            }
          }
          else if (input$export1_modes == "filtered"){
            t <- try(
              maftools::read.maf((maf_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_all,])@summary
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Non si può generare il maf summary"
              ok <- F
            }
            else{
              d <- t
            }
          }
        }
        if(ok){
          if (is.null(d)){error_txt("ERRORE")}
          else{
            pre_genesummary_data(d)
            shinyjs::click("export_mafsummary1")
          }
        }
        else {
          error_txt(e)
        }
      })

      output$export_mafsummary1 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo11(),1,20),"_", Sys.Date(),"_MafSummary_",input$export1_modes,".txt", sep="")
        },
        content = function(file) {
          try(
            write_maf_summary(pre_mafsummary_data(),file)
            ,silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_mafsummary1", suspendWhenHidden = FALSE)

      #RENDER FILTERS FOR SOMATIC SIDEBAR
      output$export1 <- shiny::renderUI({
        shiny::req(proc_data1())
        shiny::wellPanel(
          id = "well_export1",
          toggle_panel("toggle_export1", "well_export_container1","Esporta i dati:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_export_container1",
            class = "collapse in",
            shiny::tags$div(
              id = "export_cont1",
              shinyWidgets::pickerInput(
                paste("GSP-",shiny::NS(id,"export1_types"),sep=""),
                "",
                choices = c("xlsx","pdf","csv","tsv","maf"),
                selected = c("xlsx"),
                multiple = F,
                options = shinyWidgets::pickerOptions(
                  dropdownAlignRight = F,
                  size = 10,
                )
              ),
              shinyWidgets::pickerInput(
                paste("GSP-",shiny::NS(id,"export1_modes"),sep=""),
                "",
                choices = c("All","page","filtered"),
                selected = c("All"),
                multiple = F,
                options = shinyWidgets::pickerOptions(
                  dropdownAlignRight = F,
                  size = 10,
                )
              ),
              shiny::actionButton(inputId = "GSP-SOM-export_button1_mock",label = "Download", icon = shiny::icon("download")),
              shiny::downloadButton(outputId = "GSP-SOM-export_button1",style = "display:none;")
            ),
            shiny::uiOutput("GSP-SOM-every_export1"),
            shiny::uiOutput("GSP-SOM-pdf_export1"),
            shiny::uiOutput("GSP-SOM-maf_export1"),
            shiny::textOutput("GSP-SOM-export_error1")
          )
        )
      })

      pre_print_data <- shiny::reactiveVal()

      shiny::observeEvent(input$export_button1_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt(e)
        if (input$export1_types == "maf"){
          if(is.null(maf_data1())){
            e <- "il dataset non è compatibile con maf"
            ok <- F
          }
          else{
            if (input$export1_modes == "All"){
              t <- try(
                maftools::read.maf(maf_data1())
                ,silent = T
              )
              if (inherits(t, "try-error")){
                e <- "il dataset non è compatibile con maf"
                ok <- F
              }
              else{
                d <- t
              }
            }
            else if (input$export1_modes == "page"){
              t <- try(
                maftools::read.maf((maf_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_current,])
                ,silent = T
              )
              if (inherits(t, "try-error")){
                e <- "il dataset non è compatibile con maf"
                ok <- F
              }
              else{
                d <- t
              }
            }
            else if (input$export1_modes == "filtered"){
              t <- try(
                maftools::read.maf((maf_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_all,])
                ,silent = T
              )
              if (inherits(t, "try-error")){
                e <- "il dataset non è compatibile con maf"
                ok <- F
              }
              else{
                d <- t
              }
            }
          }
        }
        else if (input$export1_types == "pdf" && (input$pdf_export_options1 %in% c("Template 2","Template 3"))){
          if(is.null(maf_data1())){
            e <- "Questo pdf non puo essere generato"
            ok <- F
          }
          else {
            t <- try(
              maftools::read.maf(maf_data1()[input$som_table_rows_all,])
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Questo pdf non puo essere generato"
              ok <- F
            }
            else{
              if (input$export1_modes == "All"){
                d <- proc_data1()
              }
              else if (input$export1_modes == "page"){
                d <- (proc_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_current,]
              }
              else if (input$export1_modes == "filtered"){
                d <- (proc_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_all,]
              }
            }
          }
        }
        else{
          if (input$every_export_switch1 == FALSE){
            if (input$export1_modes == "All"){
              d <- proc_data1()
            }
            else if (input$export1_modes == "page"){
              d <- (proc_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_current,]
            }
            else if (input$export1_modes == "filtered"){
              d <- (proc_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_all,]
            }
            if(nrow(d)==0){
              e <- "Non ci sono record selezionati"
              ok <- F
            }
          }
          else {
            if (input$export1_modes == "All"){
              d <- proc_data1()[,input$checkbox1]
            }
            else if (input$export1_modes == "page"){
              d <- ((proc_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_current,])[,input$checkbox1]
            }
            else if (input$export1_modes == "filtered"){
              d <- ((proc_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE)))[input$som_table_rows_all,])[,input$checkbox1]
            }
            if(nrow(d)==0){
              e <- "Non ci sono record selezionati"
              ok <- F
            }
          }
        }
        if(ok){
          if (is.null(d)){error_txt("ERRORE")}
          else{
            pre_print_data(d)
            shinyjs::click("export_button1")
          }

        }
        else {
          error_txt(e)
        }
      })

      output$export_button1 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo11(),1,20),"_", Sys.Date(),"_",input$export1_modes,".",input$export1_types, sep="")
        },
        content = function(file) {

          if (input$export1_types == "maf"){
            if (input$export1_modes == "All"){
              if (!is.null(maf_data1())){
                try(
                  write_maf_file(pre_print_data(),file)
                  ,silent = T
                )
              }
            }
            else if (input$export1_modes == "page"){
              if (!is.null(maf_data1())){
                try(
                  write_maf_file(pre_print_data(),file)
                  ,silent = T
                )
              }
            }
            else if (input$export1_modes == "filtered"){
              if (!is.null(maf_data1())){
                try(
                  write_maf_file(pre_print_data(),file)
                  ,silent = T
                )
              }
            }
          }
          else {
            if (input$export1_types == "csv"){
              try(
                write.csv(pre_print_data(), file)
                ,silent = T
              )
            }
            else if (input$export1_types == "tsv"){
              try(
                write.table(pre_print_data(),file,row.names = F)
                ,silent = T
              )
            }
            else if (input$export1_types == "xlsx"){
              try(
                writexl::write_xlsx(pre_print_data(),file)
                ,silent = T
              )
            }
            else if (input$export1_types == "pdf"){
              # i pdf si esportano lentamente prova a trovare unmodo piu veloce
              # o metti un cap a quello che puoi printare

              if (input$pdf_export_options1 == "Template 1"){
                n <- substring(titolo11(),1,18)
                out <- try(
                  rmarkdown::render("www/template1.Rmd",output_format = "pdf_document",params = list(name1 =n, table1 = pre_print_data()[,input$checkbox1]),envir = new.env(parent = globalenv()))
                  ,silent = T
                )
                file.rename(out, file)
              }
              if (input$pdf_export_options1 == "Template 2"){
                n <- substring(titolo11(),1,18)
                if (!is.null(maf_data1())){
                  out <- try(
                    rmarkdown::render("www/template2.Rmd",output_format = "pdf_document",params = list(name1 =n, table1 = pre_print_data()[,input$checkbox1], graphd = maftools::read.maf(maf_data1()[input$som_table_rows_all,])),envir = new.env(parent = globalenv()))
                    ,silent = T
                  )
                  file.rename(out, file)
                }
              }
              if (input$pdf_export_options1 == "Template 3"){
                n <- substring(titolo11(),1,18)
                if (!is.null(maf_data1())){
                  out <- try(
                    rmarkdown::render("www/template3.Rmd",output_format = "pdf_document",params = list(name1 =n, table1 = pre_print_data()[,input$checkbox1], graphd = maftools::read.maf(maf_data1()[input$som_table_rows_all,])),envir = new.env(parent = globalenv()))
                    ,silent = T
                  )
                  file.rename(out, file)
                }
              }
            }
          }

        }
      )
      shiny::outputOptions(output, "export_button1", suspendWhenHidden = FALSE)


      # RENDER OF TITLES
      output$titolo11 <- shiny::renderText(titolo11())
      output$titolo12 <- shiny::renderText(titolo12())
      output$warning1 <- shiny::renderText(nomi_w())
      output$file_error <- shiny::renderText(file_error())

      #------ RENDER SOMATIC TABLE ------
      maf_for_plot <- shiny::reactiveVal(NULL)
      output$som_plot2 <- shiny::renderPlot({
        shiny::req(maf_for_plot())
        if (!is.null(maf_for_plot())){
          try(
            maftools::plotmafSummary(maf_for_plot()),
            silent = T
          )
        }
      })

      output$som_plot <- shiny::renderUI({
        shiny::req(maf_data1())
        if (!is.null(maf_data1())){
          m <- maf_data1()%>% dplyr::filter(purrr::reduce(filter_vars1$l,`&`,.init = TRUE))
          md <- try(maftools::read.maf(m[input$som_table_rows_all,]),silent=T)
          if (inherits(md, "try-error")){
            shiny::wellPanel(
              id = "well_plot1",
              toggle_panel("toggle_plot1", "well_plot_container1","Summary:" ),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container1",
                class = "collapse in",
                "Plot non disponibile"
              )
            )
          }
          else{
            maf_for_plot(md)
            shiny::wellPanel(
              id = "well_plot1",
              toggle_panel("toggle_plot1", "well_plot_container1","Summary:" ),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container1",
                class = "collapse in",
                #purrr::map(names(proc_data1()), ~ make_ui(proc_data1()[[.x]], .x , id))
                #shiny::tags$div("Summary plot: "),
                shiny::plotOutput("GSP-SOM-som_plot2")
              )
            )
          }
        }
        else{
          NULL
        }
      })


      output$som_table <- DT::renderDT({
        shiny::req(proc_data1())
        if (is.data.frame(proc_data1())){
        DT::datatable(
          shiny::isolate(proc_data1()),
          extensions = c('Buttons','FixedHeader','Select'),
          rownames = FALSE,
          filter = 'top',
          #fillContainer = T,
          container = htmltools::withTags(table(DT::tableHeader(names(proc_data1())),DT::tableFooter(names(proc_data1())))),
          selection = "none",#list(mode = "multiple"),

          editable = FALSE,
          class = 'display',
          options = list(
            select = T,
            order = list(0,"desc"),#asc
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
            buttons = list(list(
              extend = 'copy',
              exportOptions = list(columns = ":visible")
            )
                           #'colvis','colvisRestore',
                           #list(extend = "pdf", pageSize = "A3", orientation = "landscape", exportOptions = list(rows = list(page = "all"), columns = ":visible"))
            )
            ,lengthMenu = list(c(10,25,50,100,1000,-1),c(10,25,50,100,1000,"All"))
          )
        ) %>% DT::formatRound(nomi_format())
        }

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
        if (is.data.frame(proc_data1())){
          if (length(filter_vars1$l) != 0 && !identical(length(filter_vars1$l[[1]]),nrow(proc_data1()))){fv1 <- list()}
          rec_val$df <<- proc_data1()%>% dplyr::filter(purrr::reduce(fv1,`&`,.init = TRUE))
          proxy1 %>% DT::replaceData(rec_val$df, resetPaging = T,rownames = FALSE)
        }
      })

      ### SOMATIC TABLE CHECKBOX PROXY ###
      shiny::observe({
        #shiny::req(rec_val$df)
        shiny::req(input$checkbox1)
        chk1 <- input$checkbox1
        if (!is.null(chk1) && is.data.frame(proc_data1())){

          if (!identical(union(names(proc_data1()),input$checkbox1), names(proc_data1()))){
            chk1 <- names(proc_data1())
          }

          if (length(ds()) != length(chk1) || (length(ds()) == length(chk1) && length(ds())>1 && ds()[[1]] != chk1[[1]])){#{
            d <- c()
            for (n in chk1){
              d <- append(d, (which(names(proc_data1()) == n))-1)
            }
            ds(d)
          }
          if(!is.null(ds())){
            proxy1 %>% DT::showCols(ds(),reset = T)}
          else {
            proxy1 %>% DT::showCols(names(proc_data1()),reset = T)}
        }
      })
      #BUTTON TO DISABLE ENTER KEY (OVERVIEW TAB)
      enterDisable_server("inutile")
    })
}
