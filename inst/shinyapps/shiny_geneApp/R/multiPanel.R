#' multiPanel module ui
#' @description
#' ui module for the comparison of multiple files
#' @param id the module id
#'
#' @examples multiPanel_ui("modulename")
#'
multiPanel_ui <- function(id){
  ns <- shiny::NS(id)

  # ------ SECOND TAB -------
  shiny::tabPanel(
    title = "multi",
    id = "multi",
    value =  "multi",

    #LAYOUT CONTAINER
    shiny::tags$div(
      id = "multi_container",

      # ------ SIDEBAR LAYOUT -------
      shiny::sidebarLayout(

        #SIDEBAR CONTAINER
        shiny::tags$div(
          id = "sidebar_multi",
          class ="col-sm-3",

          # ------ SECOND TAB - SIDEBAR -------

          shiny::sidebarPanel(
            width = 3,
            # ------ FILE INPUT - MULTI -------
            # SIDEBAR INTERNAL CONTAINER (FILE3)
            shiny::tags$div(
              id = "file3_sidebar",
              class = "file_side",

              shiny::wellPanel(
                id = "well_3",
                # DIV TOGGLER
                toggle_panel("toggle_well_input3", "well_input3","Seleziona il campione:"),

                shiny::tags$div(
                  # CONTAINER TOGGLER INPUT ID + CLASS
                  id = "well_input3",
                  class = "collapse in",

                  # ------ FILE INPUT - MULTI FROM USER FILESYSTEM -------
                  shiny::fileInput(
                    inputId = shiny::NS(id,"fromfile3"),
                    # LABEL + TOOLTIP (WORKAROUND)
                    label = htmltools::HTML('Carica un file .tsv </label><span data-toggle="tooltip" style="float:right" data-placement="right" title="" data-original-title="A tooltip"><i class="far fa-circle-question" role="presentation" aria-label="circle-question icon"></i></span>'),
                    # LIST HERE THE ALLOWED FILE FORMATS
                    accept = c(".tsv",".csv",".maf",".xlsx",".xls"),
                    multiple = T
                  ),

                  # ------ FILE INPUT - MULTI FROM SERVER FILESYSTEM -------
                  ##### NO BUILT IN SEARCH BAR, LOOK FOR OTHER SOLUTIONS ####
                  shinyFiles::shinyFilesButton(
                    id = shiny::NS(id,"dataset_files3"),
                    label = "Seleziona un campione dai datasets...",
                    title = "Pick a file:" ,
                    viewtype = "detail",
                    multiple = T,
                    style = "overflow: hidden; width: auto; max-width: 100%;"
                  ),

                  # FILE NAME SELECTED FROM SERVER (FILE3)

                    shiny::textOutput(shiny::NS(id,"titolo32")),


                    shiny::textOutput(shiny::NS(id,"titolo33")),

                  shiny::tags$div(
                    id = "warning_3",
                    shiny::textOutput(shiny::NS(id,"warning3")),

                  )
                )
              ), # WELL_1 END (FILE3)

              # WELL_2 COL SELECTION CHECKBOX (FILE3)
              shiny::uiOutput(shiny::NS(id,"checkbox3")),

              # WELL_3 ROW FILTERS PANEL (FILE3)
              shiny::uiOutput(shiny::NS(id,"filter3")),

              # WELL_4 ROW EXPORT PANEL (FILE3)
              shiny::uiOutput(shiny::NS(id,"export3")),


            ), # CONTAINER END (FILE3)

          ) # SIDEBAR END
        ), # SIDEBAR CONTAINER END
        # MAIN CONTAINER
        shiny::tags$div(
          id = "colcol_multi",
          class = "col-sm-9",

          # ------ SECOND TAB - MAIN -------
          shiny::mainPanel(
            width = 9,

            shiny::tabsetPanel(
              id = shiny::NS(id,"main_multi_files"),
              shiny::tabPanel(
                id = "multi_panel",
                title = "pannello1",
                value = "pannello1",

                # TITLE MAIN MULTI
                shiny::tags$h1(
                  shiny::textOutput(shiny::NS(id,"titolo31")),
                  style = "white-space: nowrap; overflow: hidden;"
                ),
                shiny::textOutput(shiny::NS(id,"file_error3")),
                shiny::fluidRow(
                  id = "plotcontainer3",
                  shiny::uiOutput(shiny::NS(id,"multi_plot"))
                ),
                # TABLE MAIN MULTI
                shiny::fluidRow(
                  id = "tabcontainer3",
                  style = "display: flex; justify-content: center;",
                  DT::DTOutput(shiny::NS(id,"multi_table"))
                )
              ),
              # ------ SECOND TAB - MAIN VISUAL-------
              shiny::tabPanel(
                id = "multi_visual_panel",
                title = "pannello2",
                value = "pannello2"
            )
          )
        )
      )
    )
  )
)}


#' multiPanel module server
#' @description
#' server module for the comparison multiple files
#' @param id the module id
#'
#' @examples multiPanel_server("modulename")
#'
multiPanel_server <- function(id){ #oltre id puoi passare altri parametri
  shiny::moduleServer(
    id,

    function(input, output, session) {
      # REACTIVE TITLES
      titolo31 <- shiny::reactiveVal("CARICA FILES MULTIPLI")
      titolo32 <- shiny::reactiveVal("")
      titoli_multi <- shiny::reactiveVal()
      titoli_multi_no <- shiny::reactiveVal()

      # REACTIVE DATA VALUES
      data3 <- shiny::reactiveVal()
      proc_data3 <- shiny::reactiveVal()
      maf_data3 <- shiny::reactiveVal()
      nomi3 <- shiny::reactiveVal()
      nomi3_f <- shiny::reactiveVal()
      nomi_w3 <- shiny::reactiveVal()
      nomi_format3 <- shiny::reactiveVal()
      file_error3 <- shiny::reactiveVal()

      # REACTIVE FILTERS
      filter_vars3 <- shiny::reactiveValues(l = list())

      # REACTIVE OBSERVERS FOR INPUTS
      obs3 <- shiny::reactiveVal(list())

      # ------ FILE INPUT -------
      shiny::observeEvent(input$fromfile3,{
        file_error3("")
        l <- list()
        nl <- list()
        nl_not <- list()
        for (i in 1:length(input$fromfile3$datapath)){
          t <- try(
            data.table::fread(
              input$fromfile3$datapath[[i]],
              data.table = FALSE,
              #na.strings = base::getOption(NA)
            ),
            silent = T)

          if (inherits(t, "try-error")){
            nl_not <- append(nl_not,input$fromfile3$name[[i]])
          }
          else{
            nl <- append(nl,input$fromfile3$name[[i]])
            l <- append(l,list(t))

          }
        }
        titolo31("Files multipli:")
        titoli_multi(nl)
        titoli_multi_no(nl_not)
        titolo32("")
        data3(l)
      })

      #CONNECTION BETWEEN SERVER AND CLIENT TO ACCESS FILESYSTEM
      #con fileGetter puoi mettere delle restrizioni ed implementare una sorta di ricerca files
      #altrimenti vedi altre soluzioni
      #fileGetter(roots, restrictions, filetypes, pattern, hidden = FALSE)
      shinyFiles::shinyFileChoose(
        input = input,
        id = "dataset_files3",
        session = session,
        roots = c(wd = "C:/Users/facke/Desktop/datasets"),
        defaultPath="/"
      )

      #INPUT FROM SERVER (MULTI FILE)
      shiny::observeEvent(input$dataset_files3, {
        if (!is.null(input$dataset_files3)){
          l <- list()
          nl <- list()
          nl_not <- list()
          inFile <- shinyFiles::parseFilePaths(roots = c(wd = "C:/Users/facke/Desktop/datasets"), input$dataset_files3)
          if (length(inFile$datapath) != 0 ){
            for (i in 1:length(inFile$datapath)){
              t <- try(
                data.table::fread(
                  as.character(inFile$datapath[[i]]),
                  data.table = F,
                  #na.strings = base::getOption("NA")
                ),
                silent = T)

              if (inherits(t, "try-error")){
                nl_not <- append(nl_not,inFile$name[[i]])
              }
              else{
                nl <- append(nl,inFile$name[[i]])
                l <- append(l,list(t))
              }
            }
            titolo31("Files multipli:")
            titoli_multi(nl)
            titoli_multi_no(nl_not)
            titolo32("")
            data3(l)
          }
        }
      })


      #PROCESS FILEs
      shiny::observeEvent(data3(),{
        #initialize filter list

        filter_vars3$l <- list()
        #destroy old reactive observers
        if (length(obs3()) > 0){
          for (o in obs3()){
            if (!is.null(o)){
              o$destroy()
              rm(o)
            }
          }
          #reset observers
          obs3(list())
        }
        datasets <- list()
        datasets_maf <- list()
        mancanti_each <- list()
        g = 1
        for (d in data3()){
        #pre process data
          d_n <- names(d)
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
                d$Tumor_Sample_Barcode <- substring(titoli_multi()[[g]],1,18)
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
          if ("Chromosome" %in% col_yes && is.numeric(d2[["Chromosome"]])){
            d2[["Chromosome"]] <- as.character(d2[["Chromosome"]])
            d3[["Chromosome"]] <- as.character(d3[["Chromosome"]])
          }
          if ("seqnames" %in% col_no && is.numeric(d2[["seqnames"]])){
            d2[["seqnames"]] <- as.character(d2[["seqnames"]])
            d3[["seqnames"]] <- as.character(d3[["seqnames"]])
          }
          doubles <- c("gnomAD_AF","gnomAD_SAS_AF","gnomAD_NFE_AF","gnomAD_AFR_AF","gnomAD_FIN_AF","gnomAD_AMR_AF","gnomAD_ASJ_AF","gnomAD_EAS_AF")
          for (n in doubles){
            if (n %in% col_no && is.character(d2[[n]])){
              d2[[n]] <- as.double(d2[[n]])
              d3[[n]] <- as.double(d3[[n]])
            }
          }
          pos_ok_1 <- c("Gene","HugoSymbol","Chromosome","VAF","Classification","VariantType","VariantClass","Clinvar","Depth","Ref","Alt","Start","End","Variation","HGVSp","Exon")
          c_each <- setdiff(pos_ok_1,names(d2))
          datasets <- append(datasets,list(d2))
          datasets_maf <- append(datasets_maf,list(d3))
          mancanti_each[[substring(titoli_multi()[[g]],1,18)]] <- c_each
          g <- g + 1
        }
        total_maf <- NULL
        total_d <- NULL
        t1 <- try(
          dplyr::bind_rows(datasets,.id = "NFILE")
        )
        t2 <- try(
          dplyr::bind_rows(datasets_maf,.id = "NFILE")
        )
        if (!inherits(t1, "try-error")){
          total_d <- t1
        }
        if (!inherits(t2, "try-error")){
          total_maf <- t2
        }
        #qui puoi provare a fare il check su variant_classification
        # if ("Variant_Classification" %in% col_yes){effettua in place il check in d2 e d3}
          pos2 <- c("SampleBarcode","Chromosome","VAF","Classification","VariantType","VariantClass","Clinvar","Depth","Start","End")
          pos3 <- c("Hugo_Symbol","Chromosome","Variant_Classification","Variant_Type","VARIANT_CLASS","Reference_Allele","Tumor_Seq_Allele2","Start_Position","End_Position","Tumor_Sample_Barcode")

          pos_ok <- c("SampleBarcode","Gene","HugoSymbol","Chromosome","VAF","Classification","VariantType","VariantClass","Clinvar","Depth","Ref","Alt","Start","End","Variation","HGVSp","Exon")
          form <- c("VAF","Start","End","Depth") #nomi di colonne da formattare nella table
          if (!is.null(total_d)){
            nomi_format3(unlist(intersect(form,names(total_d))))
            nomi3_f(unlist(intersect(pos2,names(total_d))))
            nomi3(unlist(intersect(pos_ok,names(total_d))))
          }
          else{
            nomi_format3(c())
            nomi3_f(c())
            nomi3(c())
          }

          c2 <- setdiff(pos_ok,names(total_d))
          if (length(c2) > 0 || length(mancanti_each) > 0){
            s <- ""
            for (c in names(mancanti_each)){
              s <- paste(s, "Al file ", c," mancano le colonne: " ,paste(mancanti_each[[c]],"\n",collapse = ", "),sep="")
            }
            if (length(c2) == 0){
              nomi_w3(
                paste(s,"\n","In totale non mancano colonne", sep=" ")
              )
            }
            else{
            nomi_w3(
              paste(s,"\n","Le colonne totali mancanti sono:",paste(c2,collapse = ", "), sep=" ")
            )}
          }
          else {
            nomi_w3("Non ci sono colonne mancanti totali")
          }

          if (length(setdiff(pos3,names(total_maf)))==0){
            #print(names(d2))
            t4 <- try(
              maftools::read.maf(total_maf),
              silent = T
            )
            if (inherits(t4, "try-error")){
              maf_data3(NULL)
            }
            else{
              maf_data3(total_maf)
            }
          }
          else{maf_data3(NULL)}
          proc_data3(total_d)
      })

      #DYNAMICALLY CREATE OBSERVERS FOR MULTI FILTERS' INPUTS
      shiny::observeEvent(proc_data3(),{
        if(!is.null(proc_data3())){
          li <- nomi3_f()
          res <- lapply(li, function (y) {
            x <- paste(y,"3",sep="")

            shiny::observeEvent(input[[x]], {
              #questionable if, check for redundant initialization of observers
              if(!is.null(input[[x]]) && !is.null(proc_data3())){
                f <- filter_var_multi(proc_data3()[[y]], input[[x]])
                if (all(f)){
                  if (x %in% names(filter_vars3$l)){
                    filter_vars3$l[[x]] <- f
                  }
                }
                else if (!all(f)){
                  filter_vars3$l[[x]] <- f
                }
              }
            },ignoreInit = T ) #inner observer end
          }) #lapply end
          obs3(res)

        }
      }, priority =  10) #end outer observer

      #FIRST CHECK BOX
      output$checkbox3 <- shiny::renderUI({
        shiny::req(proc_data3())
        shiny::wellPanel(
          id = "chk3",
          toggle_panel("toggle_chk3", "well_chk_container3","Seleziona le colonne:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_chk_container3",
            class = "collapse in",
            shinyWidgets::pickerInput(
              shiny::NS(id,"checkbox3"),
              "",
              choices = c(names(proc_data3())),
              selected = nomi3(),
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                actionsBox= TRUE,
                size = 10,
                liveSearch = T
              ),
              choicesOpt = list(content = stringr::str_trunc(c(names(proc_data3())), width = 40))
            )
          )
        )
      })

      #RENDER FILTERS FOR MULTI SIDEBAR
      output$filter3 <- shiny::renderUI({
        shiny::req(proc_data3())
        shiny::wellPanel(
          id = "well_filter3",
          toggle_panel("toggle_filter3", "well_filter_container3","Seleziona i filtri:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_filter_container3",
            class = "collapse in",


            purrr::map(nomi3_f(), ~ make_ui(proc_data3()[[.x]], .x , id, "3","")),
            shiny::tags$div(
              id = "filter3_controls_cont",
              class = "filter_controls_cont",
              shiny::actionButton(inputId = "MULTI-reset_filter3", label = "Reset Filters")
            )

          )
        )
      })

      error_txt3 <- shiny::reactiveVal("")
      output$export_error3 <- shiny::renderText({

        error_txt3()

      })

      output$pdf_export3 <- shiny::renderUI({
        shiny::req(input$export3_types)
        if (input$export3_types == "pdf"){
          shiny::tags$div(
            id = "pdf_export_cont3",
            class = "pdf_export_cont",
            shinyWidgets::pickerInput(
              "MULTI-pdf_export_options3",
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

      output$every_export3 <- shiny::renderUI({
        shiny::req(input$export3_types)
        if (input$export3_types %in% c("tsv","csv","xlsx")){
          shiny::tags$div(
            id = "every_export_cont3",
            class = "every_export_cont",
            shinyWidgets::materialSwitch(
              inputId = "MULTI-every_export_switch3",
              label = "only selected cols",
              right = TRUE
            )
          )
        }
        else{NULL}
      })

      output$maf_export3 <- shiny::renderUI({
        shiny::req(input$export3_types)
        if (input$export3_types == "maf"){
          shiny::tags$div(
            id = "maf_export_cont3",
            class = "maf_export_cont",
            shiny::actionButton(inputId = "MULTI-export_genesum3_mock",label = "Gene summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "MULTI-export_genesum3",style = "display:none;"),
            shiny::actionButton(inputId = "MULTI-export_samplesum3_mock",label = "Sample summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "MULTI-export_samplesum3",style = "display:none;"),
            shiny::actionButton(inputId = "MULTI-export_mafsummary3_mock",label = "MAF summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "MULTI-export_mafsummary3",style = "display:none;")
          )
        }
        else{NULL}
      })

      pre_genesummary_data3 <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$export_genesum3_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt3(e)
        if (is.null(maf_data3())){
          ok<-F
          e <- "il dataset non è compatibile con maf"
        }
        else{
          if (input$export3_modes == "All"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf(maf_data3()))
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
          else if (input$export3_modes == "page"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_current,]))
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
          else if (input$export3_modes == "filtered"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,]))
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
          if (is.null(d)){error_txt3("ERRORE")}
          else{
            pre_genesummary_data3(d)
            shinyjs::click("export_genesum3")
          }
        }
        else {
          error_txt3(e)
        }
      })

      output$export_genesum3 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo31(),1,20),"_", Sys.Date(),"_GeneSummary_",input$export3_modes,".txt", sep="")
        },
        content = function(file) {
          try(
            write_gene_summary(pre_genesummary_data3(),file)
            ,silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_genesum3", suspendWhenHidden = FALSE)

      pre_samplesum_data3 <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$export_samplesum3_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt3(e)
        if (is.null(maf_data3())){
          ok<-F
          e <- "il dataset non è compatibile con maf"
        }
        else{
          if (input$export3_modes == "All"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf(maf_data3()))
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
          else if (input$export3_modes == "page"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_current,]))
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
          else if (input$export3_modes == "filtered"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,]))
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
          if (is.null(d)){error_txt3("ERRORE")}
          else{
            pre_samplesum_data3(d)
            shinyjs::click("export_samplesum3")
          }
        }
        else {
          error_txt3(e)
        }
      })

      output$export_samplesum3 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo31(),1,20),"_", Sys.Date(),"_SampleSummary_",input$export3_modes,".txt", sep="")
        },
        content = function(file) {
          try(
            write_sample_summary(pre_samplesum_data3(),file)
            ,silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_samplesum3", suspendWhenHidden = FALSE)

      pre_mafsummary_data3 <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$export_mafsummary3_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt3(e)
        if (is.null(maf_data3())){
          ok<-F
          e <- "il dataset non è compatibile con maf"
        }
        else{
          if (input$export3_modes == "All"){
            t <- try(
              maftools::read.maf(maf_data3())
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
          else if (input$export3_modes == "page"){
            t <- try(
              maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_current,])
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
          else if (input$export3_modes == "filtered"){
            t <- try(
              maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,])
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
          if (is.null(d)){error_txt3("ERRORE")}
          else{
            pre_mafsummary_data3(d@summary)
            shinyjs::click("export_mafsummary3")
          }
        }
        else {
          error_txt3(e)
        }
      })

      output$export_mafsummary3 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo31(),1,20),"_", Sys.Date(),"_MafSummary_",input$export3_modes,".txt", sep="")
        },
        content = function(file) {
          try(
            write_maf_summary(pre_mafsummary_data3(),file)
            ,silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_mafsummary3", suspendWhenHidden = FALSE)

      #RENDER FILTERS FOR MULTI SIDEBAR
      output$export3 <- shiny::renderUI({
        shiny::req(proc_data3())
        shiny::wellPanel(
          id = "well_export3",
          class = "well_export",
          toggle_panel("toggle_export3", "well_export_container3","Esporta i dati:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_export_container3",
            class = "collapse in",
            shiny::tags$div(
              id = "export_cont3",
              class = "export_cont",
              shinyWidgets::pickerInput(
                shiny::NS(id,"export3_types"),
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
                shiny::NS(id,"export3_modes"),
                "",
                choices = c("All","page","filtered"),
                selected = c("All"),
                multiple = F,
                options = shinyWidgets::pickerOptions(
                  dropdownAlignRight = F,
                  size = 10,
                )
              ),
              shiny::actionButton(inputId = "MULTI-export_button3_mock",label = "Download", icon = shiny::icon("download")),
              shiny::downloadButton(outputId = "MULTI-export_button3",style = "display:none;")
            ),
            shiny::uiOutput("MULTI-every_export3"),
            shiny::uiOutput("MULTI-pdf_export3"),
            shiny::uiOutput("MULTI-maf_export3"),
            shiny::textOutput("MULTI-export_error3")
          )
        )
      })

      pre_print_data3 <- shiny::reactiveVal()

      shiny::observeEvent(input$export_button3_mock, {
        ok <- TRUE
        e <- ""
        d <- NULL
        error_txt3(e)
        if (input$export3_types == "maf"){
          if(is.null(maf_data3())){
            e <- "il dataset non è compatibile con maf"
            ok <- F
          }
          else{
            if (input$export3_modes == "All"){
              t <- try(
                maftools::read.maf(maf_data3())
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
            else if (input$export3_modes == "page"){
              t <- try(
                maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_current,])
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
            else if (input$export3_modes == "filtered"){
              t <- try(
                maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,])
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
        else if (input$export3_types == "pdf" && (input$pdf_export_options3 %in% c("Template 2","Template 3"))){
          if(is.null(maf_data3())){
            e <- "Questo pdf non puo essere generato"
            ok <- F
          }
          else {
            t <- try(
              maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,])
              ,silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Questo pdf non puo essere generato"
              ok <- F
            }
            else{
              if (input$export3_modes == "All"){
                d <- proc_data3()
              }
              else if (input$export3_modes == "page"){
                d <- ((proc_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_current,])
              }
              else if (input$export3_modes == "filtered"){
                d <- ((proc_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,])
              }
            }
          }
        }
        else{
          if (input$every_export_switch3 == FALSE){
            if (input$export3_modes == "All"){
              d <- proc_data3()
            }
            else if (input$export3_modes == "page"){
              d <- (proc_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_current,]
            }
            else if (input$export3_modes == "filtered"){
              d <- (proc_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,]
            }
            if(nrow(d)==0){
              e <- "Non ci sono record selezionati"
              ok <- F
            }
          }
          else {
            if (input$export3_modes == "All"){
              d <- proc_data3()[,input$checkbox1]
            }
            else if (input$export3_modes == "page"){
              d <- ((proc_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_current,])[,input$checkbox3]
            }
            else if (input$export3_modes == "filtered"){
              d <- ((proc_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,])[,input$checkbox3]
            }
            if(nrow(d)==0){
              e <- "Non ci sono record selezionati"
              ok <- F
            }
          }
        }
        if(ok){
          if (is.null(d)){error_txt3("ERRORE")}
          else{
            pre_print_data3(d)
            shinyjs::click("export_button3")
          }

        }
        else {
          error_txt3(e)
        }
      })

      output$export_button3 <- shiny::downloadHandler(
        filename = function() {
          paste(substring(titolo31(),1,20),"_", Sys.Date(),"_",input$export3_modes,".",input$export3_types, sep="")
        },
        content = function(file) {

          if (input$export3_types == "maf"){
            if (input$export3_modes == "All"){
              if (!is.null(maf_data3())){
                try(
                  write_maf_file(pre_print_data3(),file)
                  ,silent = T
                )
              }
            }
            else if (input$export3_modes == "page"){
              if (!is.null(maf_data3())){
                try(
                  write_maf_file(pre_print_data3(),file)
                  ,silent = T
                )
              }
            }
            else if (input$export3_modes == "filtered"){
              if (!is.null(maf_data3())){
                try(
                  write_maf_file(pre_print_data3(),file)
                  ,silent = T
                )
              }
            }
          }
          else {
            if (input$export3_types == "csv"){
              try(
                write.csv(pre_print_data3(), file)
                ,silent = T
              )
            }
            else if (input$export3_types == "tsv"){
              try(
                write.table(pre_print_data3(),file,row.names = F)
                ,silent = T
              )
            }
            else if (input$export3_types == "xlsx"){
              try(
                writexl::write_xlsx(pre_print_data3(),file)
                ,silent = T
              )
            }
            else if (input$export3_types == "pdf"){
              # i pdf si esportano lentamente prova a trovare unmodo piu veloce
              # o metti un cap a quello che puoi printare

              if (input$pdf_export_options3 == "Template 1"){
                n <- substring(titolo31(),1,18)
                out <- try(
                  rmarkdown::render("www/templates/template1.Rmd",output_format = "pdf_document",params = list(name1 =n, table1 = pre_print_data3()[,input$checkbox3]),envir = new.env(parent = globalenv()))
                  ,silent = T
                )
                file.rename(out, file)
              }
              if (input$pdf_export_options3 == "Template 2"){
                n <- substring(titolo31(),1,18)
                if (!is.null(maf_data3())){
                  out <- try(
                    rmarkdown::render("www/templates/template2.Rmd",output_format = "pdf_document",params = list(name1 =n, table1 = pre_print_data3()[,input$checkbox3], graphd = maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,])),envir = new.env(parent = globalenv()))
                    ,silent = T
                  )
                  file.rename(out, file)
                }
              }
              if (input$pdf_export_options3 == "Template 3"){
                n <- substring(titolo31(),1,18)
                if (!is.null(maf_data3())){
                  out <- try(
                    rmarkdown::render("www/templates/template3.Rmd",output_format = "pdf_document",params = list(name1 =n, table1 = pre_print_data3()[,input$checkbox3], graphd = maftools::read.maf((maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE)))[input$multi_table_rows_all,])),envir = new.env(parent = globalenv()))
                    ,silent = T
                  )
                  file.rename(out, file)
                }
              }
            }
          }

        }
      )
      shiny::outputOptions(output, "export_button3", suspendWhenHidden = FALSE)


      # RENDER OF TITLES
      output$titolo31 <- shiny::renderText(titolo31())
      output$titolo32 <- shiny::renderText(
        {shiny::req(titoli_multi())
          if(!is.null(titoli_multi())){
          c(
          "Files letti:",
          paste(as.character(titoli_multi()), sep = ", \n "))}
          else{NULL}
        },
        sep = "\n"
      )
      output$titolo33 <- shiny::renderText(
        {shiny::req(titoli_multi_no())
          if(!is.null(titoli_multi_no()) && length(titoli_multi_no())>0)
          {c(
          "Files non letti:",
          paste(as.character(titoli_multi_no()), sep = ", \n "))}
          else{NULL}
        },
        sep = "\n"
      )
      output$warning3 <- shiny::renderText(nomi_w3())
      output$file_error3 <- shiny::renderText(file_error3())

      #------ RENDER MULTI TABLE ------
      maf_for_plot3 <- shiny::reactiveVal(NULL)
      output$multi_plot2 <- shiny::renderPlot({
        shiny::req(maf_for_plot3())
        if (!is.null(maf_for_plot3())){
          try(
            maftools::plotmafSummary(maf_for_plot3()),
            silent = T
          )
        }
      })

      output$multi_plot <- shiny::renderUI({
        shiny::req(maf_data3())
        if (!is.null(maf_data3())){
          m <- maf_data3()%>% dplyr::filter(purrr::reduce(filter_vars3$l,`&`,.init = TRUE))
          md <- try(maftools::read.maf(m[input$multi_table_rows_all,]),silent=T)
          if (inherits(md, "try-error")){
            shiny::wellPanel(
              id = "well_plot3",
              toggle_panel("toggle_plot3", "well_plot_container3","Summary:" ),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container3",
                class = "collapse in",
                "Plot non disponibile"
              )
            )
          }
          else{
            maf_for_plot3(md)
            shiny::wellPanel(
              id = "well_plot3",
              toggle_panel("toggle_plot3", "well_plot_container3","Summary:" ),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container3",
                class = "collapse in",
                shiny::plotOutput("MULTI-multi_plot2")
              )
            )
          }
        }
        else{
          NULL
        }
      })


      output$multi_table <- DT::renderDT({
        shiny::req(proc_data3())
        if (is.data.frame(proc_data3())){
          DT::datatable(
            proc_data3(),
            extensions = c('Buttons','FixedHeader','Select'),
            rownames = FALSE,
            filter = 'top',
            #fillContainer = T,
            container = htmltools::withTags(table(DT::tableHeader(names(proc_data3())),DT::tableFooter(names(proc_data3())))),
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
              dom = '<"row_b" B><"row_i" fl><"row_i" pi>rt<"row_i" pi><"row_e" <"row_e_overlay" >><"row_sc_cont" <"#rowsc3.row_sc"<"row_sc_i" >>>',
              buttons = list(list(
                extend = 'copy',
                exportOptions = list(columns = ":visible")
              )
              )
              ,lengthMenu = list(c(10,25,50,100,1000),c(10,25,50,100,1000))
            )
          ) %>% DT::formatRound(nomi_format3())
        }


      },
      server = T
      )

      ### MULTI TABLE PROXY SETUP ###
      ds3 <- shiny::reactiveVal()
      rec_val3 <- shiny::reactiveValues(df = NULL)
      proxy3 <- DT::dataTableProxy('multi_table')

      ### MULTI TABLE FILTERS PROXY ###
      shiny::observe({
        shiny::req(proc_data3())
        shiny::req(filter_vars3$l)
        fv1 <- filter_vars3$l

        if (is.data.frame(proc_data3())){
          if (length(filter_vars3$l) != 0 && !identical(length(filter_vars3$l[[1]]),nrow(proc_data3()))){fv1 <- list()}
          rec_val3$df <<- proc_data3()%>% dplyr::filter(purrr::reduce(fv1,`&`,.init = TRUE))
          proxy3 %>% DT::replaceData(rec_val3$df, resetPaging = T,rownames = FALSE)
        }
      })

      ### MULTI TABLE CHECKBOX PROXY ###
      shiny::observe({
        #shiny::req(rec_val$df)
        shiny::req(input$checkbox3)
        chk1 <- input$checkbox3
        if (!is.null(chk1) && is.data.frame(proc_data3())){

          if (!identical(union(names(proc_data3()),input$checkbox3), names(proc_data3()))){
            chk1 <- names(proc_data3())
          }

          if (length(ds3()) != length(chk1) || (length(ds3()) == length(chk1) && length(ds3())>1 && ds3()[[1]] != chk1[[1]])){#{
            d <- c()
            for (n in chk1){
              d <- append(d, (which(names(proc_data3()) == n))-1)
            }
            ds3(d)
          }
          if(!is.null(ds3())){
            proxy3 %>% DT::showCols(ds3(),reset = T)}
          else {
            proxy3 %>% DT::showCols(names(proc_data3()),reset = T)}
        }
      })
    })
}

