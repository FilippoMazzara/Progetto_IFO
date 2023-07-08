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
    # SIDEBAR INTERNAL CONTAINER
    shiny::tags$div(
      id = "germ_sidebar",
      class = "file_side",

      shiny::wellPanel(
        id = "germ_side_well",
        # DIV TOGGLER
        toggle_panel("toggle_germ_side_well_input", "germ_side_well_input", "Select a sample:"),

        shiny::tags$div(
          # CONTAINER TOGGLER INPUT ID + CLASS
          id = "germ_side_well_input",
          class = "collapse in",

          # ------ FILE INPUT - GERM FROM USER FILESYSTEM -------
          shiny::fileInput(
            inputId = shiny::NS(id, "germ_file_input_client"),
            # LABEL + TOOLTIP (WORKAROUND)
            label = htmltools::HTML(
              'Upload a file: </label>
              <span data-toggle = "tooltip" style = "float: right" data-placement = "right"
              title = "" data-original-title = "From the sidebar you are able to upload genetic data in various formats or chose it from the datasets stored on the server. \n \n After the data has loaded you will be able to explore it with all the tools that geneApp has to offer. \n \n If you want more details on how to operate the app or if you are experiencing problems, check out the Help page.">
              <i class = "far fa-circle-question" role = "presentation"
              aria-label = "circle-question icon"></i></span>'),
            # LIST HERE THE ALLOWED FILE FORMATS
            accept = c(".tsv",".csv",".maf",".xlsx",".xls")
          ),

          # ------ FILE INPUT - GERM FROM SERVER FILESYSTEM -------
          ##### NO BUILT IN SEARCH BAR, LOOK FOR OTHER SOLUTIONS ####
          shinyFiles::shinyFilesButton(
            id = shiny::NS(id, "germ_file_input_server"),
            label = "Select a sample from the server...",
            title = "Pick a file:",
            viewtype = "detail",
            multiple = FALSE,
            style = "overflow: hidden; width: 100%;"
          ),

          # FILE NAME SELECTED FROM SERVER
          shiny::tags$div(
            shiny::textOutput(shiny::NS(id, "germ_file_title2")),
            style = "white-space: nowrap; overflow: auto;"
          ),

          # FILE WARNINGS
          shiny::tags$div(
            id = "germ_warning",
            shiny::uiOutput(shiny::NS(id, "germ_warning")),

          )
        )
      ), # WELL_1 END

      # WELL_2 COL SELECTION CHECKBOX
      shiny::uiOutput(shiny::NS(id, "germ_column_select")),

      # WELL_3 ROW FILTERS PANEL
      shiny::uiOutput(shiny::NS(id, "germ_table_filters")),

      # WELL_4 ROW EXPORT PANEL
      shiny::uiOutput(shiny::NS(id, "germ_table_export")),

      # HELP PAGE LINK
      shiny::tags$div(
        id = "germ_help_page_link",
        class = "help_page_link",
        "Need any", shiny::actionLink("germ_helplink", shiny::tags$span(("Help"))),"?"
      ),

      #RESET THE CLIENT INPUT WHEN A FILE IS SELECTED FROM THE SERVER
      shiny::actionButton(inputId = shiny::NS(id, "germ_reset_client_input_button"), label = "", style = "display:none;")

    ), # CONTAINER END
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
    title = "Germline",
    value = "Germline",

    # TITLE MAIN GERM
    shiny::uiOutput(shiny::NS(id, "germ_file_title_main")),

    #FILE READ ERROR
    shiny::textOutput(shiny::NS(id, "germ_file_error")),

    #MAIN PLOT CONTAINER
    shiny::fluidRow(
      id = "germ_plot_main_container",
      class = "main_plot_container",
      shiny::uiOutput(shiny::NS(id, "germline_stats")),
    ),

    # TABLE MAIN GERM
    shiny::fluidRow(
      id = "germ_tab_container_main",
      class = "main_tab_container",
      style = "display: flex; justify-content: center;",
      DT::DTOutput(shiny::NS(id, "germ_table"))
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
      germ_file_title1 <- shiny::reactiveVal("")
      germ_file_title_start <- shiny::reactiveVal("\n Upload a Germline sample \n \n  or \n \n Chose it from the available ones \n ")
      germ_file_title2 <- shiny::reactiveVal("")

      # REACTIVE DATA VALUES

      germ_initial_data <- shiny::reactiveVal()
      germ_processed_data <- shiny::reactiveVal()
      germ_maf_data <- shiny::reactiveVal()
      germ_observer_names <- shiny::reactiveVal(c())
      germ_filter_names_initial <- shiny::reactiveVal(c())
      germ_column_names <- shiny::reactiveVal()
      germ_filter_names <- shiny::reactiveVal()
      germ_missing_columns <- shiny::reactiveVal()
      germ_formatted_columns <- shiny::reactiveVal()
      germ_file_error <- shiny::reactiveVal()
      initial_rows <- shiny::reactiveVal()

      # REACTIVE FILTERS RESULTS
      germ_filter_result_list <- shiny::reactiveValues(l = list())

      # REACTIVE OBSERVERS FOR INPUTS
      germ_filter_observer_list <- shiny::reactiveVal(list())

      # ------ FILE INPUT CLIENT -------
      shiny::observeEvent(input$germ_file_input_client, {
        germ_file_error("") #RESET ERROR STATUS

        t <- try(
          #READ THE FILE
          data.table::fread(
            input$germ_file_input_client$datapath,
            data.table = FALSE,
            na.strings = base::getOption("NA")
          ),
          silent = T
        )

        #ERROR HANDLING
        if (inherits(t, "try-error")){
          germ_file_error("There was an error reading the file")
          germ_initial_data(NULL)
        }
        else{
          germ_file_title1(input$germ_file_input_client$name)
          germ_file_title2("")
          germ_file_title_start("")
          germ_initial_data(t)
        }
      })

      #CONNECTION BETWEEN SERVER AND CLIENT TO ACCESS FILESYSTEM
      #fileGetter(roots, restrictions, filetypes, pattern, hidden = FALSE) FOR FILE SEARCH ?
      shinyFiles::shinyFileChoose(
        input = input,
        id = "germ_file_input_server",
        session = session,
        roots = c(wd = "C:/Users/facke/Desktop/datasets"),
        defaultPath = "/"
      )

      # ------ FILE INPUT SERVER -------
      shiny::observeEvent(input$germ_file_input_server, {
        if (!is.null(input$germ_file_input_server)){

          inFile <- shinyFiles::parseFilePaths(roots = c(wd = "C:/Users/facke/Desktop/datasets"), input$germ_file_input_server)

          if (length(inFile$datapath) != 0 ){
            germ_file_error("") #RESET ERROR STATUS

            t <- try(
              #READ THE FILE
              data.table::fread(
                as.character(inFile$datapath),
                data.table = F,
                na.strings = base::getOption("NA")
              ),
              silent = T
            )

            #ERROR HANDLING
            if (inherits(t, "try-error")){
              germ_file_error("There was an error reading the file")
              germ_initial_data(NULL)
            }
            else{
              shinyjs::click("germ_reset_client_input_button")
              germ_file_title1(inFile$name)
              germ_file_title_start("")
              germ_file_title2(inFile$name)
              germ_initial_data(t)
            }
          }
        }
      })

      # ------ FILE PROCESSING -------
      shiny::observeEvent(germ_initial_data(), {
        #initialize filter list
        germ_filter_result_list$l <- list()

        #destroy old reactive observers
        if (length(germ_filter_observer_list()) > 0){
          for (obs in germ_filter_observer_list()){
            if (!is.null(obs)){
              obs$destroy()
              rm(obs)
            }
          }
          #reset observer lists
          germ_observer_names(c())
          germ_filter_names(c())
          germ_filter_observer_list(list())
        }

        #processing variables
        initial_data <- germ_initial_data()
        col_names <- names(germ_initial_data())
        col_names_found_original <- list()
        col_names_found_maf <- list()
        col_names_not_found <- list()
        processing_data <- NULL
        processing_maf_data <- NULL

        #lists of accepted values
        #maf required names by us
        maf_col_list <- c("Gene","Hugo_Symbol","Chromosome","VAF","Variant_Classification","Variant_Type","VARIANT_CLASS","CLIN_SIG","t_depth","Reference_Allele","Tumor_Seq_Allele2","Start_Position","End_Position","Existing_Variation","HGVSp","EXON","Tumor_Sample_Barcode")
        #starting filters
        filter_names <- c("Chromosome","VAF","Classification","VariantType","VariantClass","Clinvar","Depth","Start","End")
        #maf required names in general
        maf_required_cols <- c("Hugo_Symbol","Chromosome","Variant_Classification","Variant_Type","VARIANT_CLASS","Reference_Allele","Tumor_Seq_Allele2","Start_Position","End_Position","Tumor_Sample_Barcode")
        #general names that we agreed on
        final_names_cols <- c("Gene","HugoSymbol","Chromosome","VAF","Classification","VariantType","VariantClass","Clinvar","Depth","Ref","Alt","Start","End","Variation","HGVSp","Exon")
        #column names to post format in DT, (doubles)
        cols_to_post_format <- c("VAF")

        #check which columns are present and sort their names out
        for (name in col_names){
          maf_name <- check_names(name)
          if (is.null(maf_name)){col_names_not_found <- append(col_names_not_found, name)}
          else {
            if(maf_name %in% col_names_found_maf){
              col_names_not_found <- append(col_names_not_found, name)
            }
            else{
              col_names_found_maf <- append(col_names_found_maf, maf_name)
              col_names_found_original <- append(col_names_found_original, name)
            }
          }
        }

        #missing columns
        missing_cols <- setdiff(maf_col_list, col_names_found_maf)

        #try to infer the missing columns values from other columns in the table
        #these can be expanded to accommodate different standards

        #Hugo_Symbol, Reference_Allele, HGVSp, EXON, VAF, Tumor_Sample_Barcode
        if (length(missing_cols) > 0){
          for(n in missing_cols){
            if (n == "Hugo_Symbol"){
              p <- which(tolower(col_names) == "symbol")
              if(length(p) > 0){
                col_names_found_maf <- append(col_names_found_maf, "Hugo_Symbol")
                col_names_found_original <- append(col_names_found_original, col_names[[p]])
              }
            }
            else if (n == "Reference_Allele"){
              p <- which(tolower(col_names) == "tumor_seq_allele1")
              if(length(p) > 0){
                col_names_found_maf <- append(col_names_found_maf, "Reference_Allele")
                col_names_found_original <- append(col_names_found_original, col_names[[p]])
              }
            }
            else if (n == "HGVSp"){
              p <- which(tolower(col_names) == "hgvsp_short")
              if(length(p) > 0){
                col_names_found_maf <- append(col_names_found_maf, "HGVSp")
                col_names_found_original <- append(col_names_found_original, col_names[[p]])
              }
            }
            else if (n == "EXON"){
              p <- which(tolower(col_names) == "exon_number")
              if(length(p) > 0){
                col_names_found_maf <- append(col_names_found_maf, "EXON")
                col_names_found_original <- append(col_names_found_original, col_names[[p]])
              }
            }
            else if (n == "VAF"){
              if ("t_depth" %in% col_names_found_maf && "t_alt_count" %in% col_names_not_found){
                tc <- initial_data[["t_alt_count"]]
                vaf <- c()
                j <- which(col_names_found_maf == "t_depth")
                col_j <- col_names_found_original[[j]]
                dp <- initial_data[[col_j]]
                if (is.numeric(dp) && is.numeric(tc)){
                  for (i in 1:length(dp)){
                    vaf[i] <- round(tc[[i]] / dp[[i]], 3)
                  }
                }
                if(length(vaf) > 0){
                  initial_data$VAF <- vaf
                  col_names_found_maf <- append(col_names_found_maf, "VAF")
                  col_names_found_original <- append(col_names_found_original, "VAF")
                }
              }
            }
            else if (n == "Tumor_Sample_Barcode"){
              col_names_found_maf <- append(col_names_found_maf, "Tumor_Sample_Barcode")
              col_names_found_original <- append(col_names_found_original, "Tumor_Sample_Barcode")
              initial_data$Tumor_Sample_Barcode <- substring(germ_file_title1(), 1, 18)
            }
          }
        }

        #Variant_Type
        if(!("Variant_Type" %in% col_names_found_maf) && ("VARIANT_CLASS" %in% col_names_found_maf)){
          ref <- NULL
          if ("Reference_Allele" %in% col_names_found_maf){
            j <- which(col_names_found_maf == "Reference_Allele")
            ref_j <- col_names_found_original[[j]]
            ref <- initial_data[[ref_j]]
          }

          alt <- NULL
          if ("Tumor_Seq_Allele2" %in% col_names_found_maf){
            k <- which(col_names_found_maf == "Tumor_Seq_Allele2")
            alt_k <- col_names_found_original[[k]]
            alt <- initial_data[[alt_k]]
          }

          i <- which(col_names_found_maf == "VARIANT_CLASS")
          vc_i <- col_names_found_original[[i]]
          vc <- initial_data[[vc_i]]

          vt <- var_type_gen(vc, ref, alt)

          if (length(col_names_found_maf) > 6){
            col_names_found_maf <- append(col_names_found_maf, list(x = "Variant_Type"), 5)
            col_names_found_original <- append(col_names_found_original, list(x = "Variant_Type"), 5)
          }
          else{
            col_names_found_maf <- append(col_names_found_maf, "Variant_Type")
            col_names_found_original <- append(col_names_found_original, "Variant_Type")
          }
          initial_data$Variant_Type <- vt
        }

        #Variant_Classification
        if (!("Variant_Classification" %in% col_names_found_maf) && ("consequence" %in% tolower(col_names_not_found))){

          vt <- NULL
          if ("Variant_Type" %in% col_names_found_maf){
            j <- which(col_names_found_maf == "Variant_Type")
            vt_j <- col_names_found_original[[j]]
            vt <- initial_data[[vt_j]]
          }

          cq <- list()
          k <- which(tolower(col_names) == "consequence")
          col_k <- col_names[[k]]
          cq <- initial_data[[col_k]]

          var_class <- var_class_gen(cq, vt)

          if (length(col_names_found_maf) > 5){
            col_names_found_maf <- append(col_names_found_maf, list(x = "Variant_Classification"), 4)
            col_names_found_original <- append(col_names_found_original, list(x = "Variant_Classification"), 4)
          }
          else{
            col_names_found_maf <- append(col_names_found_maf, "Variant_Classification")
            col_names_found_original <- append(col_names_found_original, "Variant_Classification")
          }
          initial_data$Variant_Classification <- var_class
        }

        #Initialize the new data sets starting from the maf found columns
        for (maf_name in maf_col_list){

          col_i <- which(col_names_found_maf == maf_name)
          if(length(col_i) > 0){
            new_name <- check_custom_names(maf_name)
            original_col_name <- col_names_found_original[[col_i]]

            if(is.null(processing_data)){
              processing_data <- data.frame(nome = initial_data[[original_col_name]])
              data.table::setnames(processing_data, new_name)
            }
            else{
              processing_data[[new_name]] <- initial_data[[original_col_name]]
            }
            if(is.null(processing_maf_data)){
              processing_maf_data <- data.frame(nome = initial_data[[original_col_name]])
              data.table::setnames(processing_maf_data, maf_name)
            }
            else{
              processing_maf_data[[maf_name]] <- initial_data[[original_col_name]]
            }
          }
        }

        #Then add the remaining columns
        for(n in col_names_not_found){
          if(is.null(processing_data)){
            processing_data <- data.frame(n = initial_data[[n]])
            data.table::setnames(processing_data, n)
          }
          else{
            processing_data[[n]] <- initial_data[[n]]
          }
          if(is.null(processing_maf_data)){
            processing_maf_data <- data.frame(n = initial_data[[n]])
            data.table::setnames(processing_maf_data, n)
          }
          else{
            processing_maf_data[[n]] <- initial_data[[n]]
          }
        }

        #Adapt the types of the data to maf standard types
        if ("Chromosome" %in% col_names_found_maf && is.numeric(processing_maf_data[["Chromosome"]])){
          processing_data[["Chromosome"]] <- as.character(processing_data[["Chromosome"]])
          processing_maf_data[["Chromosome"]] <- as.character(processing_maf_data[["Chromosome"]])
        }
        if ("seqnames" %in% col_names_not_found && is.numeric(processing_data[["seqnames"]])){
          processing_data[["seqnames"]] <- as.character(processing_data[["seqnames"]])
          processing_maf_data[["seqnames"]] <- as.character(processing_maf_data[["seqnames"]])
        }
        #list of columns that should be doubles
        doubles <- c("gnomAD_AF","gnomAD_SAS_AF","gnomAD_NFE_AF","gnomAD_AFR_AF","gnomAD_FIN_AF","gnomAD_AMR_AF","gnomAD_ASJ_AF","gnomAD_EAS_AF")
        for (n in doubles){
          if (n %in% col_names_not_found && is.character(processing_data[[n]])){
            processing_data[[n]] <- as.double(processing_data[[n]])
            processing_maf_data[[n]] <- as.double(processing_maf_data[[n]])
          }
        }

        #pass the values found to the reactive values
        if (!is.null(processing_data)){
          germ_formatted_columns(unlist(intersect(cols_to_post_format, names(processing_data))))
          germ_filter_names_initial(unlist(intersect(filter_names, names(processing_data))))
          germ_column_names(unlist(intersect(final_names_cols, names(processing_data))))
        }
        else{
          germ_formatted_columns(c())
          germ_filter_names_initial(c())
          germ_column_names(c())
        }

        #missing required columns warning
        missing_names <- setdiff(final_names_cols, names(processing_data))
        if (length(missing_names) > 0){
          germ_missing_columns(paste("Missing columns:", paste(missing_names, collapse = ", "), sep = " "))
        }
        else {
          germ_missing_columns("There are no missing columns")
        }

        #if the maf data set works then send it to the reactive
        if (length(setdiff(maf_required_cols, names(processing_maf_data))) == 0){
          t <- try(
            maftools::read.maf(processing_maf_data),
            silent = T
          )
          if (inherits(t, "try-error")){
            germ_maf_data(NULL)
          }
          else{
            germ_maf_data(processing_maf_data)
          }
        }
        else{germ_maf_data(NULL)
        }

        initial_rows(nrow(processing_data))

        #send the processed data with the good names to the reactive
        germ_processed_data(processing_data)
      })

      # ------ COLUMNS TOGGLE SELECTION -------
      output$germ_column_select <- shiny::renderUI({
        shiny::req(germ_processed_data())

        shiny::wellPanel(
          id = "germ_col_chk",
          toggle_panel("toggle_germ_col_chk", "well_germ_chk_container", "Show/Hide columns:" ),

          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_germ_chk_container",
            class = "collapse in",

            #columns selection
            shinyWidgets::pickerInput(
              paste("GSP-", shiny::NS(id, "germ_column_select"), sep=""),
              "",
              choices = c(names(germ_processed_data())),
              selected = germ_column_names(),
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                actionsBox = TRUE,
                size = 10,
                liveSearch = T
              ),
              choicesOpt = list(content = stringr::str_trunc(c(names(germ_processed_data())), width = 40))
            )

          )
        )
      })

      # ------ FILTERS TOGGLE SELECTION -------
      output$germ_table_filters <- shiny::renderUI({
        shiny::req(germ_processed_data())

        shiny::wellPanel(
          id = "well_germ_table_filters",
          toggle_panel("toggle_germ_table_filters", "well_germ_filter_container", "Select filters:" ),

          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_germ_filter_container",
            class = "collapse in",

            #filter select
            shinyWidgets::pickerInput(
              paste("GSP-", shiny::NS(id, "checkbox_filters_germ"), sep=""),
              "",
              choices = germ_filter_names_initial(),
              selected = c(),
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                actionsBox = TRUE,
                size = 10,
                liveSearch = T
              )
            ),

            shiny::tags$hr(style = "margin-top: 5px; margin-bottom: 5px;"),

            #filters container
            shiny::tags$div(
              id = "germ_table_filters_cont",
              class = "table_filters_cont"
            ),

            shiny::tags$hr(style = "margin-top: 5px; margin-bottom: 10px;"),

            #reset filters button
            shiny::tags$div(
              id = "germ_table_filters_controls_cont",
              class = "filter_controls_cont",
              shiny::actionButton(inputId = "GSP-GERM-reset_germ_table_filters", label = "Reset Filters")
            )

          )
        )
      })

      # ------ FILTERS TOGGLE SELECTION CONTROLS-------
      shiny::observeEvent(input$checkbox_filters_germ, {

          #determine which filters to keep or to remove
          chk <- input$checkbox_filters_germ
          if (is.null(chk)){
            chk <- c()
          }
          remove_filter <- setdiff(germ_filter_names(), chk)
          add_filter <- setdiff(chk, germ_filter_names())
          if (length(add_filter) > 0){
            li <- c()

            #add filters
            for (n in add_filter){
              if (!(n %in% germ_observer_names())){
                li <- append(li, n)
              }
              shiny::insertUI(
                selector = "#germ_table_filters_cont",
                where = "beforeEnd",
                ui = shiny::tags$div(id = paste("cont_", n, "2", sep = ""), make_ui(germ_processed_data()[[n]], n, id, "2", "GSP-"))
              )
              germ_filter_names(append(germ_filter_names(), n))
            }

            if(length(li) > 0){
              #generate the observers for the inputs, only the not yet generated ones
              res <- lapply(li, function(filter_name) {
                germ_filter_name <- paste(filter_name, "2", sep = "")
                #observer start
                shiny::observeEvent(input[[germ_filter_name]], {
                  if(!is.null(input[[germ_filter_name]]) && !is.null(germ_processed_data())){
                    filter_value <- filter_var(germ_processed_data()[[filter_name]], input[[germ_filter_name]])
                    #check if you can get this to be more efficient
                    if (all(filter_value)){
                      if (germ_filter_name %in% names(germ_filter_result_list$l)){
                        germ_filter_result_list$l[[germ_filter_name]] <- filter_value
                      }
                    }
                    else if (!all(filter_value)){
                      germ_filter_result_list$l[[germ_filter_name]] <- filter_value
                    }
                  }
                },
                ignoreInit = T ) #inner observer end
              }) #lapply end
              germ_filter_observer_list(append(germ_filter_observer_list(), res))
              germ_observer_names(append(germ_observer_names(), li))
            }
          }

          if (length(remove_filter) > 0){
            #remove filters
            for (n in remove_filter){
              shiny::removeUI(
                selector = paste("#cont_", n, "2", sep="")
              )
              nf <- germ_filter_names()
              nf <- nf[ !nf == n ]
              germ_filter_names(nf)
              x <- paste(n, "2", sep="")
              #remove the filter result
              germ_filter_result_list$l[[x]] <- NULL
            }
          }
      },
      priority = 10,
      ignoreNULL = FALSE) #end outer observer

      # ------ DATA EXPORT -------

      #EXPORT ERROR HANDLING
      file_export_error <- shiny::reactiveVal("")
      output$export_error_germ <- shiny::renderText({
          file_export_error()
      })

      #PDF EXPORT
      output$pdf_germ_table_export <- shiny::renderUI({
        shiny::req(input$germ_table_export_types)
        if (input$germ_table_export_types == "pdf"){
          shiny::tags$div(
            id = "pdf_export_cont_germ",
            class = "pdf_export_cont",
            #chose the templates
            shinyWidgets::pickerInput(
              paste("GSP-", shiny::NS(id,"pdf_export_options_germ"), sep=""),
              "",
              choices = c("Template 1", "Template 2", "Template 3"),
              selected = c("Template 1"),
              multiple = F,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                size = 10,
              )
            )
          )
        }
        else{NULL}
      })

      #SWITCH TO TOGGLE SELECTED/ALL COLUMNS TO EXPORT
      output$every_germ_table_export <- shiny::renderUI({
        shiny::req(input$germ_table_export_types)
        if (input$germ_table_export_types %in% c("tsv", "csv", "xlsx")){
          shiny::tags$div(
            id = "every_export_cont_germ",
            class = "every_export_cont",
            shinyWidgets::materialSwitch(
              inputId = "GSP-GERM-every_export_switch_germ",
              label = "only selected cols",
              right = TRUE
            )
          )
        }
        else{NULL}
      })

      #MAF EXPORT
      output$maf_germ_table_export <- shiny::renderUI({
        shiny::req(input$germ_table_export_types)
        if (input$germ_table_export_types == "maf"){
          shiny::tags$div(
            id = "maf_export_cont_germ",
            class = "maf_export_cont",
            #BUTTONS AND MOCK BUTTONS FOR THE MAF EXPORTS
            shiny::actionButton(inputId = "GSP-GERM-export_genesum_mock_germ", label = "Gene summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "GSP-GERM-export_genesum_germ", style = "display:none;"),
            shiny::actionButton(inputId = "GSP-GERM-export_samplesum_germ_mock", label = "Sample summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "GSP-GERM-export_samplesum_germ", style = "display:none;"),
            shiny::actionButton(inputId = "GSP-GERM-export_mafsummary_germ_mock", label = "MAF summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "GSP-GERM-export_mafsummary_germ", style = "display:none;")
          )
        }
        else{NULL}
      })

      #MAF GENE SUMMARY EXPORT
      pre_genesummary_germ_data <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$export_genesum_mock_germ, {
        ok <- TRUE
        e <- ""
        res_data <- NULL
        file_export_error(e)
        if (is.null(germ_maf_data())){
          ok <- F
          e <- "data set is not maf compatible"
        }
        else{
          #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
          if (input$germ_table_export_modes == "All"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf(germ_maf_data())),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't create gene summary"
              ok <- F
            }
            else{
              res_data <- t
            }
          }
          else if (input$germ_table_export_modes == "page"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_current,])),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't create gene summary"
              ok <- F
            }
            else{
              res_data <- t
            }
          }
          else if (input$germ_table_export_modes == "filtered"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,])),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't create gene summary"
              ok <- F
            }
            else{
              res_data <- t
            }
          }
        }

        #CHECK FOR ERRORS
        if(ok){
          if (is.null(res_data)){file_export_error("FILE EXPORT ERROR")}
          else{
            pre_genesummary_germ_data(res_data)
            shinyjs::click("export_genesum_germ")
          }
        }
        else {
          file_export_error(e)
        }
      })

      output$export_genesum_germ <- shiny::downloadHandler(
        filename = function() {
          paste(substring(germ_file_title1(), 1, 20), "_", Sys.Date(), "_GeneSummary_", input$germ_table_export_modes, ".txt", sep="")
        },
        content = function(file) {
            try(
              write_gene_summary(pre_genesummary_germ_data(), file),
              silent = T
            )
        }
      )
      shiny::outputOptions(output, "export_genesum_germ", suspendWhenHidden = FALSE)

      #SAMPLE SUMMARY EXPORT
      pre_samplesum_germ_data <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$export_samplesum_germ_mock, {
        ok <- TRUE
        e <- ""
        res_data <- NULL
        file_export_error(e)
        if (is.null(germ_maf_data())){
          ok <- F
          e <- "data set is not maf compatible"
        }
        else{
          #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
          if (input$germ_table_export_modes == "All"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf(germ_maf_data())),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't create sample summary"
              ok <- F
            }
            else{
              res_data <- t
            }
          }
          else if (input$germ_table_export_modes == "page"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_current,])),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't create sample summary"
              ok <- F
            }
            else{
              res_data <- t
            }
          }
          else if (input$germ_table_export_modes == "filtered"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,])),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't create sample summary"
              ok <- F
            }
            else{
              res_data <- t
            }
          }
        }

        #CHECK FOR ERRORS
        if(ok){
          if (is.null(res_data)){file_export_error("FILE EXPORT ERROR")}
          else{
            pre_samplesum_germ_data(res_data)
            shinyjs::click("export_samplesum_germ")
          }
        }
        else{
          file_export_error(e)
        }
      })

      output$export_samplesum_germ <- shiny::downloadHandler(
        filename = function() {
          paste(substring(germ_file_title1(), 1, 20), "_", Sys.Date(), "_SampleSummary_", input$germ_table_export_modes, ".txt", sep="")
        },
        content = function(file) {
          try(
            write_sample_summary(pre_samplesum_germ_data(), file),
            silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_samplesum_germ", suspendWhenHidden = FALSE)

      #MAF SUMMARY EXPORT
      pre_mafsummary_germ_data <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$export_mafsummary_germ_mock, {
        ok <- TRUE
        e <- ""
        res_data <- NULL
        file_export_error(e)
        if (is.null(germ_maf_data())){
          ok <- F
          e <- "data set is not maf compatible"
        }
        else{
          #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
          if (input$germ_table_export_modes == "All"){
            t <- try(
              maftools::read.maf(germ_maf_data()),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't create maf summary"
              ok <- F
            }
            else{
              res_data <- t
            }
          }
          else if (input$germ_table_export_modes == "page"){
            t <- try(
              maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_current,]),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't create maf summary"
              ok <- F
            }
            else{
              res_data <- t
            }
          }
          else if (input$germ_table_export_modes == "filtered"){
            t <- try(
              maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,]),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't create maf summary"
              ok <- F
            }
            else{
              res_data <- t
            }
          }
        }

        #CHECK FOR ERRORS
        if(ok){
          if (is.null(res_data)){file_export_error("FILE EXPORT ERROR")}
          else{
            pre_mafsummary_germ_data(res_data@summary)
            shinyjs::click("export_mafsummary_germ")
          }
        }
        else {
          file_export_error(e)
        }
      })

      output$export_mafsummary_germ <- shiny::downloadHandler(
        filename = function() {
          paste(substring(germ_file_title1(), 1, 20), "_", Sys.Date(), "_MafSummary_", input$germ_table_export_modes, ".txt", sep="")
        },
        content = function(file) {
          try(
            write_maf_summary(pre_mafsummary_germ_data(), file),
            silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_mafsummary_germ", suspendWhenHidden = FALSE)

      #FILE EXPORT MAIN CONTROLS
      output$germ_table_export <- shiny::renderUI({
        shiny::req(germ_processed_data())
        shiny::wellPanel(
          id = "well_germ_table_export",
          class = "well_export",
          toggle_panel("toggle_germ_table_export", "well_export_container_germ", "Export the data:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_export_container_germ",
            class = "collapse in",
            shiny::tags$div(
              id = "export_cont_germ",
              class = "export_cont",
              #FILE TYPE SELECTION
              shinyWidgets::pickerInput(
                paste("GSP-", shiny::NS(id, "germ_table_export_types"), sep=""),
                "",
                choices = c("xlsx", "pdf", "csv", "tsv", "maf"),
                selected = c("xlsx"),
                multiple = F,
                options = shinyWidgets::pickerOptions(
                  dropdownAlignRight = F,
                  size = 10
                )
              ),
              #RECORD SELECTION
              shinyWidgets::pickerInput(
                paste("GSP-", shiny::NS(id, "germ_table_export_modes"), sep=""),
                "",
                choices = c("All", "page", "filtered"),
                selected = c("All"),
                multiple = F,
                options = shinyWidgets::pickerOptions(
                  dropdownAlignRight = F,
                  size = 10
                )
              ),
              #EXPORT BUTTONS
              shiny::actionButton(inputId = "GSP-GERM-export_button_germ_mock", label = "Download", icon = shiny::icon("download")),
              shiny::downloadButton(outputId = "GSP-GERM-export_button_germ", style = "display:none;")
            ),
            shiny::tags$hr(),
            #DIFFERENT FILE TYPES EXPORT UI
            shiny::uiOutput("GSP-GERM-every_germ_table_export"),
            shiny::uiOutput("GSP-GERM-pdf_germ_table_export"),
            shiny::uiOutput("GSP-GERM-maf_germ_table_export"),
            #ERROR MESSAGE
            shiny::textOutput("GSP-GERM-export_error_germ")
          )
        )
      })

      #TABLE DATA EXPORT
      pre_print_germ_data <- shiny::reactiveVal()

      shiny::observeEvent(input$export_button_germ_mock, {
        ok <- TRUE
        e <- ""
        res_data <- NULL
        file_export_error(e)
        #MAF EXPORT SELECTED
        if (input$germ_table_export_types == "maf"){
          if(is.null(germ_maf_data())){
            e <- "data set is not maf compatible"
            ok <- F
          }
          else{
            #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
            if (input$germ_table_export_modes == "All"){
              t <- try(
                maftools::read.maf(germ_maf_data()),
                silent = T
              )
              if (inherits(t, "try-error")){
                e <- "data set is not maf compatible"
                ok <- F
              }
              else{
                res_data <- t
              }
            }
            else if (input$germ_table_export_modes == "page"){
              t <- try(
                maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_current,]),
                silent = T
              )
              if (inherits(t, "try-error")){
                e <- "data set is not maf compatible"
                ok <- F
              }
              else{
                res_data <- t
              }
            }
            else if (input$germ_table_export_modes == "filtered"){
              t <- try(
                maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,]),
                silent = T
              )
              if (inherits(t, "try-error")){
                e <- "data set is not maf compatible"
                ok <- F
              }
              else{
                res_data <- t
              }
            }
          }
        }
        #PDF EXPORT TEMPLATES 2 AND 3
        else if (input$germ_table_export_types == "pdf" && (input$pdf_export_options_germ %in% c("Template 2", "Template 3"))){
          if(is.null(germ_maf_data())){
            e <- "Can't render PDF"
            ok <- F
          }
          else {
            #CHECK IF THE PLOTS CAN BE RENDERED
            t <- try(
              maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,]),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't render PDF"
              ok <- F
            }
            else{
              #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
              if (input$germ_table_export_modes == "All"){
                res_data <- germ_processed_data()
              }
              else if (input$germ_table_export_modes == "page"){
                res_data <- ((germ_processed_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_current,])
              }
              else if (input$germ_table_export_modes == "filtered"){
                res_data <- ((germ_processed_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,])
              }
            }
          }
        }
        #OTHER FILES EXPORT
        else{
          if (input$every_export_switch_germ == FALSE){
            #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
            if (input$germ_table_export_modes == "All"){
              res_data <- germ_processed_data()
            }
            else if (input$germ_table_export_modes == "page"){
              res_data <- (germ_processed_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_current,]
            }
            else if (input$germ_table_export_modes == "filtered"){
              res_data <- (germ_processed_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,]
            }
            if(nrow(res_data)==0){
              e <- "No records selected"
              ok <- F
            }
          }
          else {
            #ONLY SHOWN COLUMNS
            if (input$germ_table_export_modes == "All"){
              res_data <- germ_processed_data()[,input$germ_column_select]
            }
            else if (input$germ_table_export_modes == "page"){
              res_data <- ((germ_processed_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_current,])[,input$germ_column_select]
            }
            else if (input$germ_table_export_modes == "filtered"){
              res_data <- ((germ_processed_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,])[,input$germ_column_select]
            }
            if(nrow(res_data)==0){
              e <- "No records selected"
              ok <- F
            }
          }
        }
        #CHECK FOR ERRORS
        if(ok){
          if (is.null(res_data)){file_export_error("FILE EXPORT ERROR")}
          else{
            pre_print_germ_data(res_data)
            shinyjs::click("export_button_germ")
          }
        }
        else {
          file_export_error(e)
        }
      })

      output$export_button_germ <- shiny::downloadHandler(
        filename = function() {
          paste(substring(germ_file_title1(), 1, 20), "_", Sys.Date(), "_", input$germ_table_export_modes, ".", input$germ_table_export_types, sep="")
        },
        content = function(file) {
          if (input$germ_table_export_types == "maf"){
            if (!is.null(germ_maf_data())){
              try(
                write_maf_file(pre_print_germ_data(), file),
                silent = T
              )
            }
          }
          else {
            if (input$germ_table_export_types == "csv"){
              try(
                write.csv(pre_print_germ_data(), file),
                silent = T
              )
            }
            else if (input$germ_table_export_types == "tsv"){
              try(
                write.table(pre_print_germ_data(), file, row.names = F),
                silent = T
              )
            }
            else if (input$germ_table_export_types == "xlsx"){
              try(
                writexl::write_xlsx(pre_print_germ_data(), file),
                silent = T
              )
            }
            else if (input$germ_table_export_types == "pdf"){
              if (input$pdf_export_options_germ == "Template 1"){
                n <- substring(germ_file_title1(), 1, 18)
                out <- try(
                  rmarkdown::render("www/templates/template1.Rmd", output_format = "pdf_document", params = list(name1 = n, table1 = pre_print_germ_data()[,input$germ_column_select]), envir = new.env(parent = globalenv())),
                  silent = T
                )
                file.rename(out, file)
              }
              if (input$pdf_export_options_germ == "Template 2"){
                n <- substring(germ_file_title1(),1,18)
                if (!is.null(germ_maf_data())){
                  out <- try(
                    rmarkdown::render("www/templates/template2.Rmd", output_format = "pdf_document", params = list(name1 = n, table1 = pre_print_germ_data()[,input$germ_column_select], graphd = maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,])), envir = new.env(parent = globalenv())),
                    silent = T
                  )
                  file.rename(out, file)
                }
              }
              if (input$pdf_export_options_germ == "Template 3"){
                n <- substring(germ_file_title1(),1,18)
                if (!is.null(germ_maf_data())){
                  out <- try(
                    rmarkdown::render("www/templates/template3.Rmd", output_format = "pdf_document", params = list(name1 = n, table1 = pre_print_germ_data()[,input$germ_column_select], graphd = maftools::read.maf((germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,])), envir = new.env(parent = globalenv())),
                    silent = T
                  )
                  file.rename(out, file)
                }
              }
            }
          }
        }
      )
      shiny::outputOptions(output, "export_button_germ", suspendWhenHidden = FALSE)


      # RENDER OF TITLES
      output$germ_file_title_main <- shiny::renderUI({
        if (nchar(germ_file_title_start()) > 0){
          shiny::tags$div(
            shiny::tags$h1(
              class = "file_title_start",
              germ_file_title_start()
            ),
            shiny::tags$div(
              shiny::icon('upload', style = "font-size: xxx-large;"),
              style = "display: flex; justify-content: center;"
            )
          )
        }
        else{
          shiny::tags$h1(
            class = "file_title1",
            germ_file_title1()
          )
        }
      })

      output$germ_file_title2 <- shiny::renderText(germ_file_title2())

      # RENDER OF ERRORS
      output$germ_warning <- shiny::renderUI({
        shiny::req(germ_missing_columns())
        shiny::tags$div(
          style = "text-align: center;",
          shiny::tags$hr(style = "margin-top: 5px; margin-bottom: 5px;"),
          germ_missing_columns()
        )
      })

      output$germ_file_error <- shiny::renderText(germ_file_error())

      #------ PLOTTING ON GERMLINE PAGE ------
      #PLOT SUMMARY DATA
      maf_for_plot_germ <- shiny::reactiveVal(NULL)
      maf_for_plot_germ_df <- shiny::reactiveVal(NULL)

      #SUMMARY PLOT OUTPUT
      output$germ_plot_maf <- shiny::renderPlot({
        shiny::req(maf_for_plot_germ())
        if (!is.null(maf_for_plot_germ())){
          try(
            maftools::plotmafSummary(maf_for_plot_germ(), fs = 1.10),
            silent = T
          )
        }
      })

      output$germ_plot_clusters <- shiny::renderPlot({
        shiny::req(maf_for_plot_germ())
        if (!is.null(maf_for_plot_germ())){
          try(
            maftools::plotClusters(maftools::inferHeterogeneity(maf_for_plot_germ(), vafCol = "VAF")),
            silent = T
          )
        }
      })

      output$germ_plot_vaf <- shiny::renderPlot({
        shiny::req(maf_for_plot_germ())
        if (!is.null(maf_for_plot_germ())){
          try(
            maftools::plotVaf(maf_for_plot_germ(), vafCol = "VAF", top = 10),
            silent = T
          )
        }
      })

      output$germline_charts <- shiny::renderUI({
        shiny::req(input$germline_well_selection)
        if (!is.null(input$germline_well_selection)){
          if (input$germline_well_selection == "germ_overview_chart"){

            shiny::tags$div(
              id = "germline_charts_container",
              class = "charts_container",

              shiny::tags$div(
                id = "germ_initial_rows",
                class = "panel panel-primary",
                shiny::tags$div(
                  class = "panel-heading",
                  shiny::tags$p(initial_rows()),
                  shiny::icon("list", class = "fa-regular", lib = "font-awesome")
                ),
                shiny::tags$div(
                  class = "panel-body",
                  shiny::tags$p(shiny::tags$b("Initial")),
                  shiny::tags$p("Mutations")
                )
              ),

              shiny::tags$div(
                id = "germ_filtered_rows",
                class = "panel panel-info",
                shiny::tags$div(
                  class = "panel-heading",
                  shiny::tags$p(nrow(maf_for_plot_germ_df())),
                  shiny::icon("pie-chart", class = "fa-regular", lib = "font-awesome")
                ),
                shiny::tags$div(
                  class = "panel-body",
                  shiny::tags$p(shiny::tags$b("Filtered")),
                  shiny::tags$p("Mutations")
                )
              )
            )
          }
          else if (input$germline_well_selection == "germ_vaf_chart"){
            if (!is.null(maf_for_plot_germ()) && "VAF" %in% names(maf_for_plot_germ_df())){
              shiny::tags$div(
                id = "germline_vafs_container",
                class = "charts_container",
                shiny::plotOutput("GSP-GERM-germ_plot_vaf"),
                shiny::plotOutput("GSP-GERM-germ_plot_clusters")
              )
            }
            else{
              "Can't generate the plot"
            }
          }
          else if (input$germline_well_selection == "germ_mafsummary_chart"){
            if (!is.null(maf_for_plot_germ())){
              shiny::plotOutput("GSP-GERM-germ_plot_maf")
            }
            else{
              "Can't generate the plot"
            }
          }
        }
      })

      output$germline_stats <- shiny::renderUI({
        shiny::req(germ_maf_data())
        if (!is.null(germ_maf_data())){
          maf_data <- germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE))
          maf_data <- maf_data[input$germ_table_rows_all,]
          t_germ <- try(maftools::read.maf(maf_data), silent = T)
          if (inherits(t_germ, "try-error")){
            maf_for_plot_germ(NULL)
            #ERROR HANDLING
            shiny::wellPanel(
              id = "well_plot_germline_1",
              class = "well_plot",
              toggle_panel("toggle_plot_germline_1", "well_plot_container_germline_1", "Germline Charts:"),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container_germline_1",
                class = "collapse in",
                shiny::tags$span(
                  "Can't generate the plot"
                )
              )
            )
          }
          else {
            maf_for_plot_germ(t_germ)
            maf_for_plot_germ_df(maf_data)
            shiny::wellPanel(
              id = "well_plot_germline_1",
              class = "well_plot",
              toggle_panel("toggle_plot_germline_1", "well_plot_container_germline_1", "Germline Charts:"),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container_germline_1",
                class = "collapse in",
                shiny::tags$div(
                  class = "chart_controls_cont",
                  shinyWidgets::radioGroupButtons(
                    inputId = "GSP-GERM-germline_well_selection",
                    label = "",
                    choices = c(`<p>Overview<i class='fa fa-pie-chart' style = "margin-left: 6px;"></i></p>` = "germ_overview_chart", `<p>Vaf<i class='fa fa-line-chart' style = "margin-left: 6px;"></i></p>` = "germ_vaf_chart",
                                `<p>Maf Summary<i class='fa fa-bar-chart' style = "margin-left: 6px;"></i></p>` = "germ_mafsummary_chart"),
                    justified = TRUE
                  ),
                ),

                shiny::tags$hr(style = "margin-top: 5px; margin-bottom: 5px;"),

                shiny::uiOutput("GSP-GERM-germline_charts")

              )
            )
          }
        }
        else{
          NULL
        }
      })

      #------ GERM TABLE ------

      #JS FUNCTION TO GET COLREORDER TO WORK
      js_reorder <- c(
        'table.on("column-reorder", function(e, settings, details){
        Shiny.onInputChange("GSP-GERM-order_germ", table.colReorder.order());
         });'
      )

      #TABLE RENDERING
      output$germ_table <- DT::renderDT({
        shiny::req(germ_processed_data())
        if (is.data.frame(germ_processed_data())){
          DT::datatable(
            germ_processed_data(),
            extensions = c('Buttons', 'FixedHeader', 'Select', 'ColReorder'),
            rownames = FALSE,
            filter = 'top',
            #CUSTOM HEADER AND FOOTER
            container = htmltools::withTags(table(DT::tableHeader(names(germ_processed_data())), DT::tableFooter(names(germ_processed_data())))),
            selection = "none",
            callback = DT::JS(js_reorder),
            editable = FALSE,
            class = 'display',
            options = list(
              stateSave = TRUE,
              colReorder = list(realtime = F),
              select = T,
              serverSide = TRUE,
              paging = T,
              processing = TRUE,
              autoWidth = F,
              fixedHeader = list(header = F, footer = F),
              pageLength = 25,
              pagingType = 'full_numbers',
              scrollX = T,
              scrollCollapse = T,
              #TABLE CONTAINER LAYOUT
              dom = '<"row_b" B><"row_i" fl><"row_i" pi>rt<"row_i" pi><"row_e" <"row_e_overlay" >><"row_sc_cont" <"#rowsc_germ.row_sc" <"row_sc_i" >>>',
              #COPY SELECTEDOR PAGE ECORDS TO CLIPBOARD
              buttons = list(
                list(
                  extend = 'copy',
                  exportOptions = list(columns = ":visible")
                )
              ),
              lengthMenu = list(c(10, 25, 50, 100, 1000, -1), c(10, 25, 50, 100, 1000, "All"))
            )
          ) %>% DT::formatRound(germ_formatted_columns())
        }
      },
      #SERVER SIDE PROCESSING
      server = T
    )

      ### PROXY SETUP ###
      germ_filtered_df <- shiny::reactiveValues(df = NULL)
      germ_proxy <- DT::dataTableProxy('germ_table')

      ### FILTERS PROXY ###
      shiny::observe({
        shiny::req(germ_processed_data())
        shiny::req(germ_filter_result_list$l)
        gfrl <- germ_filter_result_list$l
        if (is.data.frame(germ_processed_data())){
          # THE FOLLOWING IS EXECUTED ONLY WHEN TRANSITIONING BETWEEN DATASETS TO PREVENT ERRORS
          if (length(germ_filter_result_list$l) != 0 && !identical(length(germ_filter_result_list$l[[1]]), nrow(germ_processed_data()))){gfrl <- list()}
          #REPLACE THE ORIGINAL TABLE'S DATA WITH THE FILTERED ONE
          germ_filtered_df$df <<- germ_processed_data() %>% dplyr::filter(purrr::reduce(gfrl, `&`, .init = TRUE))
          germ_proxy %>% DT::replaceData(germ_filtered_df$df, resetPaging = T, rownames = FALSE)
        }
      })

      ### REPOSITION THE SCROLLER WHEN THE ORDER OF COLUMNS CHANGES ###
      shiny::observeEvent(input[["order_germ"]], {
        if (!is.null(input[["order_germ"]])){
          shinyjs::runjs('

            var y = document.querySelector("#rowsc_germ");
            y.scrollLeft = 0;

          ')
        }
      })

      ### COLUMNS SELECTION PROXY ###
      shiny::observe({
        shiny::req(input$germ_column_select)
        col_chk_res <- input$germ_column_select
        if (!is.null(col_chk_res) && is.data.frame(germ_processed_data())){
          # THE FOLLOWING IS EXECUTED ONLY WHEN TRANSITIONING BETWEEN DATASETS TO PREVENT ERRORS
          if (!identical(union(names(germ_processed_data()), input$germ_column_select), names(germ_processed_data()))){col_chk_res <- names(germ_processed_data())}
          #HIDE AND SHOW THE SELECTED COLUMNS AND ADDRESS THE CHANGING OF THE INDEXES WHEN REORDERING THE COLUMNS
          col_indexes <- c()
          final_indexes <- c()
          for (n in col_chk_res){
            col_indexes <- append(col_indexes, which(names(germ_processed_data()) == n) - 1)
          }
          if(is.null(input[["order_germ"]])){
            final_indexes <- col_indexes
          }
          else{
            for (n in col_indexes){
              final_indexes <- append(final_indexes, which(input[["order_germ"]] == n) - 1)
            }
          }
          germ_proxy %>% DT::showCols(final_indexes, reset = T)
      }
    })

    ### DATA FOR STATISTICS ###
    germ_statistics_data <- shiny::reactive({
      shiny::req(germ_maf_data())
      (germ_maf_data() %>% dplyr::filter(purrr::reduce(germ_filter_result_list$l, `&`, .init = TRUE)))[input$germ_table_rows_all,]
    })

    # RETURN VALUE FOR STATISTICS
    return(germ_statistics_data)

    #--- END OF SERVER MODULE ---
  })
}
