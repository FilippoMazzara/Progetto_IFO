#' Combine page ui module
#' @description
#' the function for creating the elements in the combine visualization page
#' @param id the id assigned to the module
#' @return the instance of the ui
#' @examples multiTab_page_ui("MULTI")
multiTab_page_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = "Combine",
    id = "multi",
    value =  "multi",

    #LAYOUT CONTAINER
    shiny::tags$div(
      id = "multi_container",

      # ------ SIDEBAR LAYOUT -------
      shiny::sidebarLayout(

        # SIDEBAR CONTAINER
        shiny::tags$div(

          id = "sidebar_multi",
          class = "col-sm-3",

          shiny::sidebarPanel(

            width = 3,

            # SIDEBAR INTERNAL CONTAINER (MULTI)
            shiny::tags$div(
              id = "multi_sidebar_inner",
              class = "file_side",
              shiny::wellPanel(
                id = "multi_side_well",
                # DIV TOGGLER
                toggle_panel("toggle_multi_side_well_input", "multi_side_well_input", "Select a sample:"),

                shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "multi_side_well_input",
                class = "collapse in",

                  # ------ FILE INPUT - MULTI FROM USER FILESYSTEM -------
                  shiny::fileInput(
                    inputId = shiny::NS(id, "multi_file_input_client"),
                    # LABEL + TOOLTIP (WORKAROUND)
                    label = htmltools::HTML(
                      'Upload a file: </label>
                      <span data-toggle = "tooltip" style = "float: right" data-placement = "right"
                      title = "" data-original-title = " From the sidebar you are able to upload genetic data in various formats or chose it from the datasets stored on the server. \n \n After the data has loaded you will be able to explore it with all the tools that geneApp has to offer. \n \n If you want more details on how to operate the app or if you are experiencing problems, check out the Help page.">
                      <i class = "far fa-circle-question" role = "presentation"
                      aria-label = "circle-question icon"></i></span>'
                    ),
                    # LIST HERE THE ALLOWED FILE FORMATS
                    accept = c(".tsv",".csv",".maf",".xlsx",".xls"),
                    multiple = TRUE
                  ),

                  # ------ FILE INPUT - MULTI FROM SERVER FILESYSTEM -------
                  ##### NO BUILT IN SEARCH BAR, LOOK FOR OTHER SOLUTIONS ####
                  shinyFiles::shinyFilesButton(
                    id = shiny::NS(id, "multi_file_input_server"),
                    label = "Select a sample from the server...",
                    title = "Pick a file:",
                    viewtype = "detail",
                    multiple = TRUE,
                    style = "overflow: hidden; width: 100%;"
                  ),

                  shiny::textOutput(shiny::NS(id, "multi_file_error_total")),

                  # FILE WARNINGS
                  shiny::tags$div(
                    id = "multi_warning",
                    shiny::uiOutput(shiny::NS(id, "multi_warning")),
                  )
                )
              ), # WELL_1 END

              # WELL_2 COL SELECTION CHECKBOX
              shiny::uiOutput(shiny::NS(id, "multi_column_select")),

              # WELL_3 ROW FILTERS PANEL
              shiny::uiOutput(shiny::NS(id, "multi_table_filters")),

              # WELL_4 ROW EXPORT PANEL
              shiny::uiOutput(shiny::NS(id, "multi_table_export")),

              # HELP PAGE LINK
              shiny::tags$div(
                id = "multi_help_page_link",
                class = "help_page_link",
                "Need any", shiny::actionLink("multi_helplink", shiny::tags$span(("Help"))),"?"
              ),

              #RESET THE CLIENT INPUT WHEN A FILE IS SELECTED FROM THE SERVER
              shiny::actionButton(inputId = shiny::NS(id, "multi_reset_client_input_button"), label = "", style = "display:none;")
            )
          )
        ), # SIDEBAR CONTAINER END
        shiny::tags$div(
          id = "colcol_multi",
          class = "col-sm-9",

          # ------ MAIN -------
          shiny::mainPanel(
            width = 9,

            shiny::tabsetPanel(
              id = "main_multi_files",
              shiny::tabPanel(
                id = "multi_main_panel",
                title = "Combine Samples",
                value = "multi_main",

                # TITLE MAIN MULTI
                shiny::uiOutput(shiny::NS(id, "multi_file_title_main")),

                #FILE READ ERROR
                shiny::textOutput(shiny::NS(id, "multi_file_error")),

                #MAIN PLOT CONTAINER
                shiny::fluidRow(
                  id = "multi_plot_main_container_main",
                  class = "main_plot_container",
                  shiny::uiOutput(shiny::NS(id, "multi_plot_main"))
                ),

                # TABLE MAIN MULTI
                shiny::fluidRow(
                  id = "multi_tab_container_main",
                  class = "main_tab_container",
                  style = "display: flex; justify-content: center;",
                  DT::DTOutput(shiny::NS(id, "multi_table"))
                )
              ),
              # ------ SECOND TAB - MAIN VISUAL-------
              shiny::tabPanel(
                id = "multi_statistics_panel",
                title = "Statistics",
                value = "multi_statistics"
              )
            )
          )
        )# MAIN CONTAINER END
      )
    )
  )
}


#' Combine page server module
#' @description
#' The module containing the server-side of the multi page
#' @param id the id assigned to the module
#' @return the server instance
#' @examples multiTab_page_server("MULTI")
multiTab_page_server <- function(id) {

  shiny::moduleServer(

    id,

    function(input, output, session) {

      # REACTIVE TITLES
      multi_file_title1 <- shiny::reactiveVal("")
      multi_file_title_start <- shiny::reactiveVal("\n Upload a multiple samples \n \n  or \n \n Chose them from the available ones \n ")
      multi_titles <- shiny::reactiveVal()
      missing_multi_titles <- shiny::reactiveVal()

      # REACTIVE DATA VALUES

      multi_initial_data <- shiny::reactiveVal()
      multi_processed_data <- shiny::reactiveVal()
      multi_maf_data <- shiny::reactiveVal()
      multi_observer_names <- shiny::reactiveVal(c())
      multi_filter_names_initial <- shiny::reactiveVal(c())
      multi_column_names <- shiny::reactiveVal()
      multi_filter_names <- shiny::reactiveVal()
      multi_missing_columns <- shiny::reactiveVal()
      multi_formatted_columns <- shiny::reactiveVal()
      multi_file_error <- shiny::reactiveVal()

      # REACTIVE FILTERS RESULTS
      multi_filter_result_list <- shiny::reactiveValues(l = list())

      # REACTIVE OBSERVERS FOR INPUTS
      multi_filter_observer_list <- shiny::reactiveVal(list())

      # ------ FILE INPUT CLIENT -------
      shiny::observeEvent(input$multi_file_input_client, {
        multi_file_error("") #RESET ERROR STATUS

        read_files <- list()
        read_files_names <- list()
        error_files_names <- list()
        for (i in 1:length(input$multi_file_input_client$datapath)){
          t <- try(
            data.table::fread(
              input$multi_file_input_client$datapath[[i]],
              data.table = FALSE
            ),
            silent = T
          )

          if (inherits(t, "try-error")){
            error_files_names <- append(error_files_names, input$multi_file_input_client$name[[i]])
          }
          else{
            read_files_names <- append(read_files_names, input$multi_file_input_client$name[[i]])
            read_files <- append(read_files, list(t))
          }
        }

        if (length(read_files) == 0){
          multi_initial_data(NULL)
          multi_file_error("There was an error reading the files")
        }
        else{
          multi_file_title1("Files multipli:")
          multi_titles(read_files_names)
          missing_multi_titles(error_files_names)
          multi_file_title_start("")
          multi_initial_data(read_files)
        }
      })

      #CONNECTION BETWEEN SERVER AND CLIENT TO ACCESS FILESYSTEM
      #fileGetter(roots, restrictions, filetypes, pattern, hidden = FALSE) FOR FILE SEARCH ?
      shinyFiles::shinyFileChoose(
        input = input,
        id = "multi_file_input_server",
        session = session,
        roots = c(wd = "C:/Users/facke/Desktop/datasets"),
        defaultPath = "/"
      )

      # ------ FILE INPUT SERVER -------
      shiny::observeEvent(input$multi_file_input_server, {
        if (!is.null(input$multi_file_input_server)){
          inFile <- shinyFiles::parseFilePaths(roots = c(wd = "C:/Users/facke/Desktop/datasets"), input$multi_file_input_server)
          if (length(inFile$datapath) != 0 ){
            multi_file_error("") #RESET ERROR STATUS

            read_files <- list()
            read_files_names <- list()
            error_files_names <- list()
            for (i in 1:length(inFile$datapath)){
              t <- try(
                data.table::fread(
                  inFile$datapath[[i]],
                  data.table = FALSE
                ),
                silent = T
              )

              if (inherits(t, "try-error")){
                error_files_names <- append(error_files_names, inFile$name[[i]])
              }
              else{
                read_files_names <- append(read_files_names, inFile$name[[i]])
                read_files <- append(read_files, list(t))
              }
            }

            if (length(read_files) == 0){
              multi_initial_data(NULL)
              multi_file_error("There was an error reading the files")
            }
            else{
              multi_file_title1("Files multipli:")
              multi_titles(read_files_names)
              missing_multi_titles(error_files_names)
              multi_file_title_start("")
              multi_initial_data(read_files)
            }
          }
        }
      })

      # ------ FILE PROCESSING -------
      shiny::observeEvent(multi_initial_data(), {
        #initialize filter list
        multi_filter_result_list$l <- list()

        #destroy old reactive observers
        if (length(multi_filter_observer_list()) > 0){
          for (obs in multi_filter_observer_list()){
            if (!is.null(obs)){
              obs$destroy()
              rm(obs)
            }
          }
          #reset observer lists
          multi_observer_names(c())
          multi_filter_names(c())
          multi_filter_observer_list(list())
        }

        #lists of accepted values
        #maf required names by us
        maf_col_list <- c("Gene","Hugo_Symbol","Chromosome","VAF","Variant_Classification","Variant_Type","VARIANT_CLASS","CLIN_SIG","t_depth","Reference_Allele","Tumor_Seq_Allele2","Start_Position","End_Position","Existing_Variation","HGVSp","EXON","Tumor_Sample_Barcode")
        #starting filters
        filter_names <- c("SampleBarcode","Chromosome","VAF","Classification","VariantType","VariantClass","Clinvar","Depth","Start","End")
        #maf required names in general
        maf_required_cols <- c("Hugo_Symbol","Chromosome","Variant_Classification","Variant_Type","VARIANT_CLASS","Reference_Allele","Tumor_Seq_Allele2","Start_Position","End_Position","Tumor_Sample_Barcode")
        #general names that we agreed on
        final_names_cols <- c("SampleBarcode","Gene","HugoSymbol","Chromosome","VAF","Classification","VariantType","VariantClass","Clinvar","Depth","Ref","Alt","Start","End","Variation","HGVSp","Exon")
        #column names to post format in DT, (doubles)
        cols_to_post_format <- c("VAF")

        #combine variables
        datasets <- list()
        datasets_maf <- list()
        missing_each <- list()
        nfile = 1

        #for each uploaded file
        for(dataset in multi_initial_data()){
          #processing variables
          initial_data <- dataset
          col_names <- names(dataset)
          col_names_found_original <- list()
          col_names_found_maf <- list()
          col_names_not_found <- list()
          processing_data <- NULL
          processing_maf_data <- NULL



          #check which columns are present and sort their names out
          for (name in col_names){
            maf_name <- check_names(name)
            if (is.null(maf_name)) {
              col_names_not_found <- append(col_names_not_found, name)
            }
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
                initial_data$Tumor_Sample_Barcode <- substring(multi_titles()[[nfile]], 1, 18)
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

          missing_cols_each <- setdiff(final_names_cols, names(processing_data))
          datasets <- append(datasets, list(processing_data))
          datasets_maf <- append(datasets_maf, list(processing_maf_data))
          missing_each[[substring(multi_titles()[[nfile]], 1, 18)]] <- missing_cols_each
          nfile <- nfile + 1

        } #END FOR

        #merge the datasets
        total_maf <- NULL
        total_data <- NULL
        t1 <- try(
          dplyr::bind_rows(datasets, .id = "NFILE")
        )
        t2 <- try(
          dplyr::bind_rows(datasets_maf, .id = "NFILE")
        )
        if (!inherits(t1, "try-error")){
          total_data <- t1
        }
        if (!inherits(t2, "try-error")){
          total_maf <- t2
        }

        #pass the values found to the reactive values
        if (!is.null(total_data)){
          multi_formatted_columns(unlist(intersect(cols_to_post_format, names(total_data))))
          multi_filter_names_initial(unlist(intersect(filter_names, names(total_data))))
          multi_column_names(unlist(intersect(final_names_cols, names(total_data))))
        }
        else{
          multi_formatted_columns(c())
          multi_filter_names_initial(c())
          multi_column_names(c())
        }

        #missing required columns warning
        missing_names <- setdiff(final_names_cols, names(total_data))
        if (length(missing_names) > 0 || length(missing_each) > 0){
          message <- ""
          for (c in names(missing_each)){
            if (length(missing_each[[c]]) > 0){
              message <- paste(message, "File ", c, " missing columns: ", paste(missing_each[[c]], "\n", collapse = ", "), "\n", sep = "")
            }
            else{
              message <- paste(message, "File ", c, " has no missing columns ", "\n", "\n",sep = "")
            }
          }
          if (length(missing_names) == 0){
            multi_missing_columns(
              paste(message, "There are no missing columns in the final dataset", sep = " ")
            )
          }
          else{
            multi_missing_columns(
              paste(message, "\n", "The total missing columns are:", paste(missing_names, collapse = ", "), sep = " ")
            )
          }
        }
        else {
          multi_missing_columns("There are no missing columns")
        }

        #if the maf data set works then send it to the reactive
        if (length(setdiff(maf_required_cols, names(total_maf))) == 0){
          t <- try(
            maftools::read.maf(total_maf),
            silent = T
          )
          if (inherits(t, "try-error")){
            multi_maf_data(NULL)
          }
          else{
            multi_maf_data(total_maf)
          }
        }
        else{multi_maf_data(NULL)
        }

        #send the processed data with the good names to the reactive
        multi_processed_data(total_data)
      })

      # ------ COLUMNS TOGGLE SELECTION -------
      output$multi_column_select <- shiny::renderUI({
        shiny::req(multi_processed_data())

        shiny::wellPanel(
          id = "multi_col_chk",
          toggle_panel("toggle_multi_col_chk", "well_multi_chk_container", "Show/Hide columns:" ),

          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_multi_chk_container",
            class = "collapse in",

            #columns selection
            shinyWidgets::pickerInput(
              paste(shiny::NS(id, "multi_column_select"), sep=""),
              "",
              choices = c(names(multi_processed_data())),
              selected = multi_column_names(),
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                dropdownAlignRight = F,
                actionsBox = TRUE,
                size = 10,
                liveSearch = T
              ),
              choicesOpt = list(content = stringr::str_trunc(c(names(multi_processed_data())), width = 40))
            )
          )
        )
      })

      # ------ FILTERS TOGGLE SELECTION -------
      output$multi_table_filters <- shiny::renderUI({
        shiny::req(multi_processed_data())

        shiny::wellPanel(
          id = "well_multi_table_filters",
          toggle_panel("toggle_multi_table_filters", "well_multi_filter_container", "Select filters:" ),

          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_multi_filter_container",
            class = "collapse in",

            #filter select
            shinyWidgets::pickerInput(
              paste(shiny::NS(id, "checkbox_filters_multi"), sep=""),
              "",
              choices = multi_filter_names_initial(),
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
              id = "multi_table_filters_cont",
              class = "table_filters_cont"
            ),

            shiny::tags$hr(style = "margin-top: 5px; margin-bottom: 10px;"),

            #reset filters button
            shiny::tags$div(
              id = "multi_table_filters_controls_cont",
              class = "filter_controls_cont",
              shiny::actionButton(inputId = "MULTI-reset_multi_table_filters", label = "Reset Filters")
            )
          )
        )
      })

      # ------ FILTERS TOGGLE SELECTION CONTROLS-------
      shiny::observeEvent(input$checkbox_filters_multi, {

          #determine which filters to keep or to remove
          chk <- input$checkbox_filters_multi
          if (is.null(chk)){
            chk <- c()
          }
          remove_filter <- setdiff(multi_filter_names(), chk)
          add_filter <- setdiff(chk, multi_filter_names())
          if (length(add_filter) > 0){
            li <- c()

            #add filters
            for (n in add_filter){
              if (!(n %in% multi_observer_names())){
                li <- append(li, n)
              }
              shiny::insertUI(
                selector = "#multi_table_filters_cont",
                where = "beforeEnd",
                ui = shiny::tags$div(id = paste("cont_", n, "3", sep = ""), make_ui(multi_processed_data()[[n]], n, id, "3", ""))
              )
              multi_filter_names(append(multi_filter_names(), n))
            }

            if(length(li) > 0){
              #generate the observers for the inputs, only the not yet generated ones
              res <- lapply(li, function(filter_name) {
                multi_filter_name <- paste(filter_name, "3", sep = "")
                #observer start
                shiny::observeEvent(input[[multi_filter_name]], {
                  if(!is.null(input[[multi_filter_name]]) && !is.null(multi_processed_data())){
                    filter_value <- filter_var_multi(multi_processed_data()[[filter_name]], input[[multi_filter_name]])
                    #check if you can get this to be more efficient
                    if (all(filter_value)){
                      if (multi_filter_name %in% names(multi_filter_result_list$l)){
                        multi_filter_result_list$l[[multi_filter_name]] <- filter_value
                      }
                    }
                    else if (!all(filter_value)){
                      multi_filter_result_list$l[[multi_filter_name]] <- filter_value
                    }
                  }
                },
                ignoreInit = T ) #inner observer end
              }) #lapply end
              multi_filter_observer_list(append(multi_filter_observer_list(), res))
              multi_observer_names(append(multi_observer_names(), li))
            }
          }

          if (length(remove_filter) > 0){
            #remove filters
            for (n in remove_filter){
              shiny::removeUI(
                selector = paste("#cont_", n, "3", sep="")
              )
              nf <- multi_filter_names()
              nf <- nf[ !nf == n ]
              multi_filter_names(nf)
              x <- paste(n, "3", sep="")
              #remove the filter result
              multi_filter_result_list$l[[x]] <- NULL
            }
          }
      },
      priority = 10,
      ignoreNULL = FALSE) #end outer observer

      # ------ DATA EXPORT -------

      #EXPORT ERROR HANDLING
      file_export_error <- shiny::reactiveVal("")
      output$export_error_multi <- shiny::renderText({
          file_export_error()
      })

      #PDF EXPORT
      output$pdf_multi_table_export <- shiny::renderUI({
        shiny::req(input$multi_table_export_types)
        if (input$multi_table_export_types == "pdf"){
          shiny::tags$div(
            id = "pdf_export_cont_multi",
            class = "pdf_export_cont",
            #chose the templates
            shinyWidgets::pickerInput(
              paste(shiny::NS(id,"pdf_export_options_multi"), sep=""),
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
      output$every_multi_table_export <- shiny::renderUI({
        shiny::req(input$multi_table_export_types)
        if (input$multi_table_export_types %in% c("tsv", "csv", "xlsx")){
          shiny::tags$div(
            id = "every_export_cont_multi",
            class = "every_export_cont",
            shinyWidgets::materialSwitch(
              inputId = "MULTI-every_export_switch_multi",
              label = "only selected cols",
              right = TRUE
            )
          )
        }
        else{NULL}
      })

      #MAF EXPORT
      output$maf_multi_table_export <- shiny::renderUI({
        shiny::req(input$multi_table_export_types)
        if (input$multi_table_export_types == "maf"){
          shiny::tags$div(
            id = "maf_export_cont_multi",
            class = "maf_export_cont",
            #BUTTONS AND MOCK BUTTONS FOR THE MAF EXPORTS
            shiny::actionButton(inputId = "MULTI-export_genesum_mock_multi", label = "Gene summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "MULTI-export_genesum_multi", style = "display:none;"),
            shiny::actionButton(inputId = "MULTI-export_samplesum_multi_mock", label = "Sample summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "MULTI-export_samplesum_multi", style = "display:none;"),
            shiny::actionButton(inputId = "MULTI-export_mafsummary_multi_mock", label = "MAF summary", icon = shiny::icon("download")),
            shiny::downloadButton(outputId = "MULTI-export_mafsummary_multi", style = "display:none;")
          )
        }
        else{NULL}
      })

      #MAF GENE SUMMARY EXPORT
      pre_genesummary_multi_data <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$export_genesum_mock_multi, {
        ok <- TRUE
        e <- ""
        res_data <- NULL
        file_export_error(e)
        if (is.null(multi_maf_data())){
          ok <- F
          e <- "data set is not maf compatible"
        }
        else{
          #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
          if (input$multi_table_export_modes == "All"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf(multi_maf_data())),
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
          else if (input$multi_table_export_modes == "page"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_current,])),
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
          else if (input$multi_table_export_modes == "filtered"){
            t <- try(
              maftools::getGeneSummary(maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,])),
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
            pre_genesummary_multi_data(res_data)
            shinyjs::click("export_genesum_multi")
          }
        }
        else {
          file_export_error(e)
        }
      })

      output$export_genesum_multi <- shiny::downloadHandler(
        filename = function() {
          paste(substring(multi_file_title1(), 1, 20), "_", Sys.Date(), "_GeneSummary_", input$multi_table_export_modes, ".txt", sep="")
        },
        content = function(file) {
            try(
              write_gene_summary(pre_genesummary_multi_data(), file),
              silent = T
            )
        }
      )
      shiny::outputOptions(output, "export_genesum_multi", suspendWhenHidden = FALSE)

      #SAMPLE SUMMARY EXPORT
      pre_samplesum_multi_data <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$export_samplesum_multi_mock, {
        ok <- TRUE
        e <- ""
        res_data <- NULL
        file_export_error(e)
        if (is.null(multi_maf_data())){
          ok <- F
          e <- "data set is not maf compatible"
        }
        else{
          #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
          if (input$multi_table_export_modes == "All"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf(multi_maf_data())),
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
          else if (input$multi_table_export_modes == "page"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_current,])),
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
          else if (input$multi_table_export_modes == "filtered"){
            t <- try(
              maftools::getSampleSummary(maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,])),
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
            pre_samplesum_multi_data(res_data)
            shinyjs::click("export_samplesum_multi")
          }
        }
        else{
          file_export_error(e)
        }
      })

      output$export_samplesum_multi <- shiny::downloadHandler(
        filename = function() {
          paste(substring(multi_file_title1(), 1, 20), "_", Sys.Date(), "_SampleSummary_", input$multi_table_export_modes, ".txt", sep="")
        },
        content = function(file) {
          try(
            write_sample_summary(pre_samplesum_multi_data(), file),
            silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_samplesum_multi", suspendWhenHidden = FALSE)

      #MAF SUMMARY EXPORT
      pre_mafsummary_multi_data <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$export_mafsummary_multi_mock, {
        ok <- TRUE
        e <- ""
        res_data <- NULL
        file_export_error(e)
        if (is.null(multi_maf_data())){
          ok <- F
          e <- "data set is not maf compatible"
        }
        else{
          #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
          if (input$multi_table_export_modes == "All"){
            t <- try(
              maftools::read.maf(multi_maf_data()),
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
          else if (input$multi_table_export_modes == "page"){
            t <- try(
              maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_current,]),
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
          else if (input$multi_table_export_modes == "filtered"){
            t <- try(
              maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,]),
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
            pre_mafsummary_multi_data(res_data@summary)
            shinyjs::click("export_mafsummary_multi")
          }
        }
        else {
          file_export_error(e)
        }
      })

      output$export_mafsummary_multi <- shiny::downloadHandler(
        filename = function() {
          paste(substring(multi_file_title1(), 1, 20), "_", Sys.Date(), "_MafSummary_", input$multi_table_export_modes, ".txt", sep="")
        },
        content = function(file) {
          try(
            write_maf_summary(pre_mafsummary_multi_data(), file),
            silent = T
          )
        }
      )
      shiny::outputOptions(output, "export_mafsummary_multi", suspendWhenHidden = FALSE)

      #FILE EXPORT MAIN CONTROLS
      output$multi_table_export <- shiny::renderUI({
        shiny::req(multi_processed_data())
        shiny::wellPanel(
          id = "well_multi_table_export",
          class = "well_export",
          toggle_panel("toggle_multi_table_export", "well_export_container_multi", "Export the data:" ),
          shiny::tags$div(
            # CONTAINER TOGGLER INPUT ID + CLASS
            id = "well_export_container_multi",
            class = "collapse in",
            shiny::tags$div(
              id = "export_cont_multi",
              class = "export_cont",
              #FILE TYPE SELECTION
              shinyWidgets::pickerInput(
                paste(shiny::NS(id, "multi_table_export_types"), sep=""),
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
                paste(shiny::NS(id, "multi_table_export_modes"), sep=""),
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
              shiny::actionButton(inputId = "MULTI-export_button_multi_mock", label = "Download", icon = shiny::icon("download")),
              shiny::downloadButton(outputId = "MULTI-export_button_multi", style = "display:none;")
            ),
            shiny::tags$hr(),
            #DIFFERENT FILE TYPES EXPORT UI
            shiny::uiOutput("MULTI-every_multi_table_export"),
            shiny::uiOutput("MULTI-pdf_multi_table_export"),
            shiny::uiOutput("MULTI-maf_multi_table_export"),
            #ERROR MESSAGE
            shiny::textOutput("MULTI-export_error_multi")
          )
        )
      })

      #TABLE DATA EXPORT
      pre_print_multi_data <- shiny::reactiveVal()

      shiny::observeEvent(input$export_button_multi_mock, {
        ok <- TRUE
        e <- ""
        res_data <- NULL
        file_export_error(e)
        #MAF EXPORT SELECTED
        if (input$multi_table_export_types == "maf"){
          if(is.null(multi_maf_data())){
            e <- "data set is not maf compatible"
            ok <- F
          }
          else{
            #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
            if (input$multi_table_export_modes == "All"){
              t <- try(
                maftools::read.maf(multi_maf_data()),
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
            else if (input$multi_table_export_modes == "page"){
              t <- try(
                maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_current,]),
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
            else if (input$multi_table_export_modes == "filtered"){
              t <- try(
                maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,]),
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
        else if (input$multi_table_export_types == "pdf" && (input$pdf_export_options_multi %in% c("Template 2", "Template 3"))){
          if(is.null(multi_maf_data())){
            e <- "Can't render PDF"
            ok <- F
          }
          else {
            #CHECK IF THE PLOTS CAN BE RENDERED
            t <- try(
              maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,]),
              silent = T
            )
            if (inherits(t, "try-error")){
              e <- "Can't render PDF"
              ok <- F
            }
            else{
              #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
              if (input$multi_table_export_modes == "All"){
                res_data <- multi_processed_data()
              }
              else if (input$multi_table_export_modes == "page"){
                res_data <- ((multi_processed_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_current,])
              }
              else if (input$multi_table_export_modes == "filtered"){
                res_data <- ((multi_processed_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,])
              }
            }
          }
        }
        #OTHER FILES EXPORT
        else{
          if (input$every_export_switch_multi == FALSE){
            #DIFFERENT SETS OF RECORDS TO GENERATE THE REPORT ON
            if (input$multi_table_export_modes == "All"){
              res_data <- multi_processed_data()
            }
            else if (input$multi_table_export_modes == "page"){
              res_data <- (multi_processed_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_current,]
            }
            else if (input$multi_table_export_modes == "filtered"){
              res_data <- (multi_processed_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,]
            }
            if(nrow(res_data)==0){
              e <- "No records selected"
              ok <- F
            }
          }
          else {
            #ONLY SHOWN COLUMNS
            if (input$multi_table_export_modes == "All"){
              res_data <- multi_processed_data()[,input$multi_column_select]
            }
            else if (input$multi_table_export_modes == "page"){
              res_data <- ((multi_processed_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_current,])[,input$multi_column_select]
            }
            else if (input$multi_table_export_modes == "filtered"){
              res_data <- ((multi_processed_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,])[,input$multi_column_select]
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
            pre_print_multi_data(res_data)
            shinyjs::click("export_button_multi")
          }
        }
        else {
          file_export_error(e)
        }
      })

      output$export_button_multi <- shiny::downloadHandler(
        filename = function() {
          paste(substring(multi_file_title1(), 1, 20), "_", Sys.Date(), "_", input$multi_table_export_modes, ".", input$multi_table_export_types, sep="")
        },
        content = function(file) {
          if (input$multi_table_export_types == "maf"){
            if (!is.null(multi_maf_data())){
              try(
                write_maf_file(pre_print_multi_data(), file),
                silent = T
              )
            }
          }
          else {
            if (input$multi_table_export_types == "csv"){
              try(
                write.csv(pre_print_multi_data(), file),
                silent = T
              )
            }
            else if (input$multi_table_export_types == "tsv"){
              try(
                write.table(pre_print_multi_data(), file, row.names = F),
                silent = T
              )
            }
            else if (input$multi_table_export_types == "xlsx"){
              try(
                writexl::write_xlsx(pre_print_multi_data(), file),
                silent = T
              )
            }
            else if (input$multi_table_export_types == "pdf"){
              if (input$pdf_export_options_multi == "Template 1"){
                n <- substring(multi_file_title1(), 1, 18)
                out <- try(
                  rmarkdown::render("www/templates/template1.Rmd", output_format = "pdf_document", params = list(name1 = n, table1 = pre_print_multi_data()[,input$multi_column_select]), envir = new.env(parent = globalenv())),
                  silent = T
                )
                file.rename(out, file)
              }
              if (input$pdf_export_options_multi == "Template 2"){
                n <- substring(multi_file_title1(),1,18)
                if (!is.null(multi_maf_data())){
                  out <- try(
                    rmarkdown::render("www/templates/template2.Rmd", output_format = "pdf_document", params = list(name1 = n, table1 = pre_print_multi_data()[,input$multi_column_select], graphd = maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,])), envir = new.env(parent = globalenv())),
                    silent = T
                  )
                  file.rename(out, file)
                }
              }
              if (input$pdf_export_options_multi == "Template 3"){
                n <- substring(multi_file_title1(),1,18)
                if (!is.null(multi_maf_data())){
                  out <- try(
                    rmarkdown::render("www/templates/template3.Rmd", output_format = "pdf_document", params = list(name1 = n, table1 = pre_print_multi_data()[,input$multi_column_select], graphd = maftools::read.maf((multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE)))[input$multi_table_rows_all,])), envir = new.env(parent = globalenv())),
                    silent = T
                  )
                  file.rename(out, file)
                }
              }
            }
          }
        }
      )
      shiny::outputOptions(output, "export_button_multi", suspendWhenHidden = FALSE)


      # RENDER OF TITLES
      output$multi_file_title_main <- shiny::renderUI({
        if (nchar(multi_file_title_start()) > 0){
          shiny::tags$div(
            shiny::tags$h1(
              class = "file_title_start",
              multi_file_title_start()
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
            "Multiple Files:"
          )
        }
      })


      # RENDER OF ERRORS
      output$multi_warning <- shiny::renderUI({
        shiny::req(multi_missing_columns())
        shiny::tags$div(
          shiny::tags$hr(style = "margin-top: 5px; margin-bottom: 5px;"),
          shiny::tags$div(
            style = "text-align: center; word-break: break-word; white-space: break-spaces;",
            multi_missing_columns()
          )
        )
      })

      output$multi_file_error_total <- shiny::renderText(
        {shiny::req(missing_multi_titles())
          if(!is.null(missing_multi_titles()) && length(missing_multi_titles())>0)
          {c(
            "Files non letti:",
            paste(as.character(missing_multi_titles()), sep = ", \n "))}
          else{NULL}
        },
        sep = "\n"
      )

      output$multi_file_error <- shiny::renderText(multi_file_error())

      #------ PLOTTING ON MULTILINE PAGE ------
      #PLOT SUMMARY DATA
      maf_for_plot_multi <- shiny::reactiveVal(NULL)

      #SUMMARY PLOT OUTPUT
      output$multi_plot_main2 <- shiny::renderPlot({
        shiny::req(maf_for_plot_multi())
        if (!is.null(maf_for_plot_multi())){
          try(
            maftools::plotmafSummary(maf_for_plot_multi(), fs = 1.10),
            silent = T
          )
        }
      })

      #PLOT CONTROLS AND CONTAINER RENDERING
      output$multi_plot_main <- shiny::renderUI({
        shiny::req(multi_maf_data())
        if (!is.null(multi_maf_data())){
          #CHECK IF THE PLOT CAN BE GENERATED
          maf_data <- multi_maf_data() %>% dplyr::filter(purrr::reduce(multi_filter_result_list$l, `&`, .init = TRUE))
          t <- try(maftools::read.maf(maf_data[input$multi_table_rows_all,]), silent = T)
          if (inherits(t, "try-error")){
            #ERROR HANDLING
            shiny::wellPanel(
              id = "multi_well_plot2",
              toggle_panel("multi_toggle_plot2", "multi_well_plot_container2", "Summary:"),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "multi_well_plot_container2",
                class = "collapse in",
                "Can't generate the plot"
              )
            )
          }
          else{
            maf_for_plot_multi(t)
            shiny::wellPanel(
              id = "well_plot_multi",
              class = "well_plot",
              toggle_panel("toggle_plot_multi", "well_plot_container_multi", "Summary:"),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container_multi",
                class = "collapse in",
                shiny::plotOutput("MULTI-multi_plot_main2")
              )
            )
          }
        }
        else{
          NULL
        }
      })

      #------ MULTI TABLE ------

      #JS FUNCTION TO GET COLREORDER TO WORK
      js_reorder <- c(
        'table.on("column-reorder", function(e, settings, details){
        Shiny.onInputChange("MULTI-order_multi", table.colReorder.order());
         });'
      )

      #TABLE RENDERING
      output$multi_table <- DT::renderDT({
        shiny::req(multi_processed_data())
        if (is.data.frame(multi_processed_data())){
          DT::datatable(
            multi_processed_data(),
            extensions = c('Buttons', 'FixedHeader', 'Select', 'ColReorder'),
            rownames = FALSE,
            filter = 'top',
            #CUSTOM HEADER AND FOOTER
            container = htmltools::withTags(table(DT::tableHeader(names(multi_processed_data())), DT::tableFooter(names(multi_processed_data())))),
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
              dom = '<"row_b" B><"row_i" fl><"row_i" pi>rt<"row_i" pi><"row_e" <"row_e_overlay" >><"row_sc_cont" <"#rowsc_multi.row_sc" <"row_sc_i" >>>',
              #COPY SELECTEDOR PAGE ECORDS TO CLIPBOARD
              buttons = list(
                list(
                  extend = 'copy',
                  exportOptions = list(columns = ":visible")
                )
              ),
              lengthMenu = list(c(10, 25, 50, 100, 1000, -1), c(10, 25, 50, 100, 1000, "All"))
            )
          ) %>% DT::formatRound(multi_formatted_columns())
        }
      },
      #SERVER SIDE PROCESSING
      server = T
    )

      ### PROXY SETUP ###
      multi_filtered_df <- shiny::reactiveValues(df = NULL)
      multi_proxy <- DT::dataTableProxy('multi_table')

      ### FILTERS PROXY ###
      shiny::observe({
        shiny::req(multi_processed_data())
        shiny::req(multi_filter_result_list$l)
        gfrl <- multi_filter_result_list$l
        if (is.data.frame(multi_processed_data())){
          # THE FOLLOWING IS EXECUTED ONLY WHEN TRANSITIONING BETWEEN DATASETS TO PREVENT ERRORS
          if (length(multi_filter_result_list$l) != 0 && !identical(length(multi_filter_result_list$l[[1]]), nrow(multi_processed_data()))){gfrl <- list()}
          #REPLACE THE ORIGINAL TABLE'S DATA WITH THE FILTERED ONE
          multi_filtered_df$df <<- multi_processed_data() %>% dplyr::filter(purrr::reduce(gfrl, `&`, .init = TRUE))
          multi_proxy %>% DT::replaceData(multi_filtered_df$df, resetPaging = T, rownames = FALSE)
        }
      })

      ### REPOSITION THE SCROLLER WHEN THE ORDER OF COLUMNS CHANGES ###
      shiny::observeEvent(input[["order_multi"]], {
        if (!is.null(input[["order_multi"]])){
          shinyjs::runjs('

            var y = document.querySelector("#rowsc_multi");
            y.scrollLeft = 0;

          ')
        }
      })

      ### COLUMNS SELECTION PROXY ###
      shiny::observe({
        shiny::req(input$multi_column_select)
        col_chk_res <- input$multi_column_select
        if (!is.null(col_chk_res) && is.data.frame(multi_processed_data())){
          # THE FOLLOWING IS EXECUTED ONLY WHEN TRANSITIONING BETWEEN DATASETS TO PREVENT ERRORS
          if (!identical(union(names(multi_processed_data()), input$multi_column_select), names(multi_processed_data()))){col_chk_res <- names(multi_processed_data())}
          #HIDE AND SHOW THE SELECTED COLUMNS AND ADDRESS THE CHANGING OF THE INDEXES WHEN REORDERING THE COLUMNS
          col_indexes <- c()
          final_indexes <- c()
          for (n in col_chk_res){
            col_indexes <- append(col_indexes, which(names(multi_processed_data()) == n) - 1)
          }
          if(is.null(input[["order_multi"]])){
            final_indexes <- col_indexes
          }
          else{
            for (n in col_indexes){
              final_indexes <- append(final_indexes, which(input[["order_multi"]] == n) - 1)
            }
          }
          multi_proxy %>% DT::showCols(final_indexes, reset = T)
      }
    })

    #--- END OF SERVER MODULE ---
  })
}
