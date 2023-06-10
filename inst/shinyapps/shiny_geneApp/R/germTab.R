#' Germ page sidebar ui module
#' @description
#' the function for creating the sidebar elements in the germ visualization page
#' @param id the id assigned to the module
#' @return the instance of the sidebar
#' @examples germTab_ui_sidebar("GERM")
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
        toggle_panel("toggle_well_input2", "well_input2","Seleziona il campione:"),

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

      # WELL_4 ROW EXPORT PANEL (FILE2)
      shiny::uiOutput(shiny::NS(id,"export2")),


    ), # CONTAINER END (FILE2)
  )
}

#' Germ page table ui module
#' @description
#' the function for creating the main and table elements in the germ visualization page
#' @param id
#' the id assigned to the element
#' @return
#' the instance of the main germ page
#' @examples germTab_ui_table("GERM")
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
    shiny::textOutput(shiny::NS(id,"file_error2")),
    shiny::fluidRow(
      id = "plotcontainer2",
      shiny::uiOutput(shiny::NS(id,"germ_plot"))
    ),
    # TABLE MAIN GERM
    shiny::fluidRow(
      id = "tabcontainer2",
      style = "display: flex; justify-content: center;",
      DT::DTOutput(shiny::NS(id,"germ_table"))
    )
  )
}

#' Germ page server module
#' @description
#' The module containing the server-side of the germ page
#' @param id the id assigned to the module
#' @return the server instance
#' @examples germTab_server("GERM")
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
      maf_data2 <- shiny::reactiveVal()
      nomi2 <- shiny::reactiveVal()
      nomi2_f <- shiny::reactiveVal()
      nomi_w2 <- shiny::reactiveVal()
      nomi_format2 <- shiny::reactiveVal()
      file_error2 <- shiny::reactiveVal()

      # REACTIVE FILTERS
      filter_vars2 <- shiny::reactiveValues(l = list())

      # REACTIVE OBSERVERS FOR INPUTS
      obs2 <- shiny::reactiveVal(list())

      # ------ FILE INPUT 1 -------
      shiny::observeEvent(input$fromfile2,{
        file_error2("")
        t <- try(
          data.table::fread(
            input$fromfile2$datapath,
            data.table = FALSE,
            na.strings = base::getOption("NA")
          ),
          silent = T)

        if (inherits(t, "try-error")){
          file_error2("Il file non puo essere letto")
          data2(NULL)
        }
        else{
          titolo21(input$fromfile2$name)
          titolo22("")
          data2(t)
        }

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
            file_error2("")
            t <- try(
              data.table::fread(
                as.character(inFile$datapath),
                data.table = F,
                na.strings = base::getOption("NA")
              ),
            silent = T)

            if (inherits(t, "try-error")){
              file_error2("Il file non puo essere letto")
              data2(NULL)
            }
            else{
              titolo21(inFile$name)
              titolo22(inFile$name)
              data2(t)
            }
          }
        }
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
        d_n <- names(data2())
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
              d$Tumor_Sample_Barcode <- substring(titolo21(),1,18)
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
          nomi_format2(unlist(intersect(form,names(d2))))
          nomi2_f(unlist(intersect(pos2,names(d2))))
          nomi2(unlist(intersect(pos_ok,names(d2))))
        }
        else{
          nomi_format2(c())
          nomi2_f(c())
          nomi2(c())
        }

        c2 <- setdiff(pos_ok,names(d2))
        if (length(c2) > 0){
          nomi_w2(paste("Le colonne mancanti sono:",paste(c2,collapse = ", "), sep=" "))
        }
        else {
          nomi_w2("Non ci sono colonne mancanti")
        }

        if (length(setdiff(pos3,names(d3)))==0){
          #print(names(d2))
          t <- try(
            maftools::read.maf(d3),
            silent = T
          )
          if (inherits(t, "try-error")){
            maf_data2(NULL)
          }
          else{
            maf_data2(d3)
          }
        }
        else{maf_data2(NULL)}
        proc_data2(d2)
      })

      #DYNAMICALLY CREATE OBSERVERS FOR GERM FILTERS' INPUTS
      shiny::observeEvent(proc_data2(),{
        if(!is.null(proc_data2())){
          li <- nomi2_f()
          res <- lapply(li, function (y) {
            x <- paste(y,"2",sep="")

            shiny::observeEvent(input[[x]], {
              #questionable if, check for redundant initialization of observers
              if(!is.null(input[[x]]) && !is.null(proc_data2())){
                f <- filter_var(proc_data2()[[y]], input[[x]])
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

      #FIRST CHECK BOX
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
              selected = nomi2(),
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


            purrr::map(nomi2_f(), ~ make_ui(proc_data2()[[.x]], .x , id, "2","GSP-")),
            shiny::tags$div(
              id = "filter2_controls_cont",
              class = "filter_controls_cont",
              shiny::actionButton(inputId = "GSP-GERM-reset_filter2", label = "Reset Filters")
            )

          )
        )
      })

      error_txt2 <- shiny::reactiveVal("")
      output$export_error2 <- shiny::renderText({

          error_txt2()

      })

      output$pdf_export2 <- shiny::renderUI({
        shiny::req(input$export2_types)
        if (input$export2_types == "pdf"){
          shiny::tags$div(
            id = "pdf_export_cont2",
            class = "pdf_export_cont",
            shinyWidgets::pickerInput(
              paste("GSP-",shiny::NS(id,"pdf_export_options2"),sep=""),
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

      output$every_export2 <- shiny::renderUI({
        shiny::req(input$export2_types)
        if (input$export2_types %in% c("tsv","csv","xlsx")){
          shiny::tags$div(
            id = "every_export_cont2",
            class = "every_export_cont",
            shinyWidgets::materialSwitch(
              inputId = "GSP-GERM-every_export_switch2",
              label = "only selected cols",
              right = TRUE
            )
          )
        }
        else{NULL}
      })

      output$maf_export2 <- shiny::renderUI({
        shiny::req(input$export2_types)
        if (input$export2_types == "maf"){
          shiny::tags$div(
            id = "maf_export_cont2",
            class = "maf_export_cont",
            shiny::actionButton(inputId = "GSP-GERM-export_genesum2_mock",label = "Gene summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "GSP-GERM-export_genesum2",style = "display:none;"),
            shiny::actionButton(inputId = "GSP-GERM-export_samplesum2_mock",label = "Sample summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "GSP-GERM-export_samplesum2",style = "display:none;"),
            shiny::actionButton(inputId = "GSP-GERM-export_mafsummary2_mock",label = "MAF summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "GSP-GERM-export_mafsummary2",style = "display:none;")
          )
        }
        else{NULL}
      })

      pre_genesummary_data2 <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$export_genesum2_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt2(e)
        if (is.null(maf_data2())){
          ok<-F
          e <- "il dataset non è compatibile con maf"
        }
        else{
          if (input$export2_modes == "All"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf(maf_data2()))
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
          else if (input$export2_modes == "page"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_current,]))
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
          else if (input$export2_modes == "filtered"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,]))
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
          if (is.null(d)){error_txt2("ERRORE")}
          else{
            pre_genesummary_data2(d)
            shinyjs::click("export_genesum2")
          }
        }
        else {
          error_txt2(e)
        }
      })

      output$export_genesum2 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo21(),1,20),"_", Sys.Date(),"_GeneSummary_",input$export2_modes,".txt", sep="")
        },
        content = function(file) {
            try(
              write_gene_summary(pre_genesummary_data2(),file)
              ,silent = T
            )
        }
      )
      shiny::outputOptions(output, "export_genesum2", suspendWhenHidden = FALSE)

      pre_samplesum_data2 <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$export_samplesum2_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt2(e)
        if (is.null(maf_data2())){
          ok<-F
          e <- "il dataset non è compatibile con maf"
        }
        else{
          if (input$export2_modes == "All"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf(maf_data2()))
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
          else if (input$export2_modes == "page"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_current,]))
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
          else if (input$export2_modes == "filtered"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,]))
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
          if (is.null(d)){error_txt2("ERRORE")}
          else{
            pre_samplesum_data2(d)
            shinyjs::click("export_samplesum2")
          }
        }
        else {
          error_txt2(e)
        }
      })

      output$export_samplesum2 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo21(),1,20),"_", Sys.Date(),"_SampleSummary_",input$export2_modes,".txt", sep="")
        },
        content = function(file) {
          try(
            write_sample_summary(pre_samplesum_data2(),file)
            ,silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_samplesum2", suspendWhenHidden = FALSE)

      pre_mafsummary_data2 <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$export_mafsummary2_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt2(e)
        if (is.null(maf_data2())){
          ok<-F
          e <- "il dataset non è compatibile con maf"
        }
        else{
          if (input$export2_modes == "All"){
            t <- try(
              maftools::read.maf(maf_data2())
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
          else if (input$export2_modes == "page"){
            t <- try(
              maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_current,])
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
          else if (input$export2_modes == "filtered"){
            t <- try(
              maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,])
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
          if (is.null(d)){error_txt2("ERRORE")}
          else{
            pre_mafsummary_data2(d@summary)
            shinyjs::click("export_mafsummary2")
          }
        }
        else {
          error_txt2(e)
        }
      })

      output$export_mafsummary2 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo21(),1,20),"_", Sys.Date(),"_MafSummary_",input$export2_modes,".txt", sep="")
        },
        content = function(file) {
          try(
            write_maf_summary(pre_mafsummary_data2(),file)
            ,silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_mafsummary2", suspendWhenHidden = FALSE)

      #RENDER FILTERS FOR GERM SIDEBAR
      output$export2 <- shiny::renderUI({
        shiny::req(proc_data2())
        shiny::wellPanel(
          id = "well_export2",
          class = "well_export",
          toggle_panel("toggle_export2", "well_export_container2","Esporta i dati:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_export_container2",
            class = "collapse in",
            shiny::tags$div(
              id = "export_cont2",
              class = "export_cont",
              shinyWidgets::pickerInput(
                paste("GSP-",shiny::NS(id,"export2_types"),sep=""),
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
                paste("GSP-",shiny::NS(id,"export2_modes"),sep=""),
                "",
                choices = c("All","page","filtered"),
                selected = c("All"),
                multiple = F,
                options = shinyWidgets::pickerOptions(
                  dropdownAlignRight = F,
                  size = 10,
                )
              ),
              shiny::actionButton(inputId = "GSP-GERM-export_button2_mock",label = "Download", icon = shiny::icon("download")),
              shiny::downloadButton(outputId = "GSP-GERM-export_button2",style = "display:none;")
            ),
            shiny::uiOutput("GSP-GERM-every_export2"),
            shiny::uiOutput("GSP-GERM-pdf_export2"),
            shiny::uiOutput("GSP-GERM-maf_export2"),
            shiny::textOutput("GSP-GERM-export_error2")
          )
        )
      })

      pre_print_data2 <- shiny::reactiveVal()

      shiny::observeEvent(input$export_button2_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt2(e)
        if (input$export2_types == "maf"){
          if(is.null(maf_data2())){
            e <- "il dataset non è compatibile con maf"
            ok <- F
          }
          else{
            if (input$export2_modes == "All"){
              t <- try(
                maftools::read.maf(maf_data2())
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
            else if (input$export2_modes == "page"){
              t <- try(
                maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_current,])
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
            else if (input$export2_modes == "filtered"){
              t <- try(
                maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,])
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
        else if (input$export2_types == "pdf" && (input$pdf_export_options2 %in% c("Template 2","Template 3"))){
          if(is.null(maf_data2())){
            e <- "Questo pdf non puo essere generato"
            ok <- F
          }
          else {
            t <- try(
              maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,])
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Questo pdf non puo essere generato"
              ok <- F
            }
            else{
              if (input$export2_modes == "All"){
                d <- proc_data2()
              }
              else if (input$export2_modes == "page"){
                d <- ((proc_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_current,])
              }
              else if (input$export2_modes == "filtered"){
                d <- ((proc_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,])
              }
            }
          }
        }
        else{
          if (input$every_export_switch2 == FALSE){
            if (input$export2_modes == "All"){
              d <- proc_data2()
            }
            else if (input$export2_modes == "page"){
              d <- (proc_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_current,]
            }
            else if (input$export2_modes == "filtered"){
              d <- (proc_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,]
            }
            if(nrow(d)==0){
              e <- "Non ci sono record selezionati"
              ok <- F
            }
          }
          else {
            if (input$export2_modes == "All"){
              d <- proc_data2()[,input$checkbox1]
            }
            else if (input$export2_modes == "page"){
              d <- ((proc_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_current,])[,input$checkbox2]
            }
            else if (input$export2_modes == "filtered"){
              d <- ((proc_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,])[,input$checkbox2]
            }
            if(nrow(d)==0){
              e <- "Non ci sono record selezionati"
              ok <- F
            }
          }
        }
        if(ok){
          if (is.null(d)){error_txt2("ERRORE")}
          else{
            pre_print_data2(d)
            shinyjs::click("export_button2")
          }

        }
        else {
          error_txt2(e)
        }
      })

      output$export_button2 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo21(),1,20),"_", Sys.Date(),"_",input$export2_modes,".",input$export2_types, sep="")
        },
        content = function(file) {

          if (input$export2_types == "maf"){
            if (input$export2_modes == "All"){
              if (!is.null(maf_data2())){
                try(
                  write_maf_file(pre_print_data2(),file)
                  ,silent = T
                )
              }
            }
            else if (input$export2_modes == "page"){
              if (!is.null(maf_data2())){
                try(
                  write_maf_file(pre_print_data2(),file)
                  ,silent = T
                )
              }
            }
            else if (input$export2_modes == "filtered"){
              if (!is.null(maf_data2())){
                try(
                  write_maf_file(pre_print_data2(),file)
                  ,silent = T
                )
              }
            }
          }
          else {
            if (input$export2_types == "csv"){
              try(
                write.csv(pre_print_data2(), file)
                ,silent = T
              )
            }
            else if (input$export2_types == "tsv"){
              try(
                write.table(pre_print_data2(),file,row.names = F)
                ,silent = T
              )
            }
            else if (input$export2_types == "xlsx"){
              try(
                writexl::write_xlsx(pre_print_data2(),file)
                ,silent = T
              )
            }
            else if (input$export2_types == "pdf"){
              # i pdf si esportano lentamente prova a trovare unmodo piu veloce
              # o metti un cap a quello che puoi printare

              if (input$pdf_export_options2 == "Template 1"){
                n <- substring(titolo21(),1,18)
                out <- try(
                  rmarkdown::render("www/templates/template1.Rmd",output_format = "pdf_document",params = list(name1 =n, table1 = pre_print_data2()[,input$checkbox2]),envir = new.env(parent = globalenv()))
                  ,silent = T
                )
                file.rename(out, file)
              }
              if (input$pdf_export_options2 == "Template 2"){
                n <- substring(titolo21(),1,18)
                if (!is.null(maf_data2())){
                  out <- try(
                    rmarkdown::render("www/templates/template2.Rmd",output_format = "pdf_document",params = list(name1 =n, table1 = pre_print_data2()[,input$checkbox2], graphd = maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,])),envir = new.env(parent = globalenv()))
                    ,silent = T
                  )
                  file.rename(out, file)
                }
              }
              if (input$pdf_export_options2 == "Template 3"){
                n <- substring(titolo21(),1,18)
                if (!is.null(maf_data2())){
                  out <- try(
                    rmarkdown::render("www/templates/template3.Rmd",output_format = "pdf_document",params = list(name1 =n, table1 = pre_print_data2()[,input$checkbox2], graphd = maftools::read.maf((maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE)))[input$germ_table_rows_all,])),envir = new.env(parent = globalenv()))
                    ,silent = T
                  )
                  file.rename(out, file)
                }
              }
            }
          }

        }
      )
      shiny::outputOptions(output, "export_button2", suspendWhenHidden = FALSE)


      # RENDER OF TITLES
      output$titolo21 <- shiny::renderText(titolo21())
      output$titolo22 <- shiny::renderText(titolo22())
      output$warning2 <- shiny::renderText(nomi_w2())
      output$file_error2 <- shiny::renderText(file_error2())

      #------ RENDER GERM TABLE ------
      maf_for_plot2 <- shiny::reactiveVal(NULL)
      output$germ_plot2 <- shiny::renderPlot({
        shiny::req(maf_for_plot2())
        if (!is.null(maf_for_plot2())){
          try(
            maftools::plotmafSummary(maf_for_plot2()),
            silent = T
          )
        }
      })

      output$germ_plot <- shiny::renderUI({
        shiny::req(maf_data2())
        if (!is.null(maf_data2())){
          m <- maf_data2()%>% dplyr::filter(purrr::reduce(filter_vars2$l,`&`,.init = TRUE))
          md <- try(maftools::read.maf(m[input$germ_table_rows_all,]),silent=T)
          if (inherits(md, "try-error")){
            shiny::wellPanel(
              id = "well_plot2",
              toggle_panel("toggle_plot2", "well_plot_container2","Summary:" ),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container2",
                class = "collapse in",
                "Plot non disponibile"
              )
            )
          }
          else{
            maf_for_plot2(md)
            shiny::wellPanel(
              id = "well_plot2",
              toggle_panel("toggle_plot2", "well_plot_container2","Summary:" ),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container2",
                class = "collapse in",
                shiny::plotOutput("GSP-GERM-germ_plot2")
              )
            )
          }
        }
        else{
          NULL
        }
      })


      output$germ_table <- DT::renderDT({
        shiny::req(proc_data2())
        if (is.data.frame(proc_data2())){
        DT::datatable(
          shiny::isolate(proc_data2()),
          extensions = c('Buttons','FixedHeader','Select'),
          rownames = FALSE,
          filter = 'top',
          #fillContainer = T,
          container = htmltools::withTags(table(DT::tableHeader(names(proc_data2())),DT::tableFooter(names(proc_data2())))),
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
            dom = '<"row_b" B><"row_i" fl><"row_i" pi>rt<"row_i" pi><"row_e" <"row_e_overlay" >><"row_sc_cont" <"#rowsc2.row_sc"<"row_sc_i" >>>',
            buttons = list(list(
              extend = 'copy',
              exportOptions = list(columns = ":visible")
              )
            )
            ,lengthMenu = list(c(10,25,50,100,1000,-1),c(10,25,50,100,1000,"All"))
          )
        ) %>% DT::formatRound(nomi_format2())
        }


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
        fv1 <- filter_vars2$l
        if (is.data.frame(proc_data2())){
          if (length(filter_vars2$l) != 0 && !identical(length(filter_vars2$l[[1]]),nrow(proc_data2()))){fv1 <- list()}
          rec_val2$df <<- proc_data2()%>% dplyr::filter(purrr::reduce(fv1,`&`,.init = TRUE))
          proxy2 %>% DT::replaceData(rec_val2$df, resetPaging = T,rownames = FALSE)
        }
      })

      ### GERM TABLE CHECKBOX PROXY ###
      shiny::observe({
        #shiny::req(rec_val$df)
        shiny::req(input$checkbox2)
        chk1 <- input$checkbox2
        if (!is.null(chk1) && is.data.frame(proc_data2())){

          if (!identical(union(names(proc_data2()),input$checkbox2), names(proc_data2()))){
            chk1 <- names(proc_data2())
          }

          if (length(ds2()) != length(chk1) || (length(ds2()) == length(chk1) && length(ds2())>1 && ds2()[[1]] != chk1[[1]])){#{
            d <- c()
            for (n in chk1){
              d <- append(d, (which(names(proc_data2()) == n))-1)
            }
            ds2(d)
          }
          if(!is.null(ds2())){
            proxy2 %>% DT::showCols(ds2(),reset = T)}
          else {
            proxy2 %>% DT::showCols(names(proc_data2()),reset = T)}
        }
      })
    })
}
