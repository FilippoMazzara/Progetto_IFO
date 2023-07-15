#' gersomPanel module ui
#' @description
#' ui module for the comparison of somatic and germ files
#' @param id the module id
#' @examples gersomPanel_ui("modulename")
gersomPanel_ui <- function(id){
  ns <- shiny::NS(id)

  # ------ COMPARISON TAB -------
  shiny::tabPanel(
    title = "S/G Comparison",
    id = "somatic_germ_comparison",
    value =  "somatic_germ_comparison",

    #LAYOUT CONTAINER
    shiny::tags$div(
      id = "overview_container",

      # ------ SIDEBAR LAYOUT -------
      shiny::sidebarLayout(

        #SIDEBAR CONTAINER
        shiny::tags$div(
          id = "sidebar",
          class ="col-sm-3",

          # ------ COMPARISON TAB - SIDEBAR -------

          shiny::sidebarPanel(
            width = 3,
            # SIDEBAR SWITCH
            shiny::tags$div(
              style = "display: flex; justify-content: center;",
              shiny::actionButton(inputId = shiny::NS(id, "sidebar_somatic"), label = "SOMATIC", class = "btn btn-default action-button shiny-bound-input active"),
              shiny::actionButton(inputId = shiny::NS(id, "sidebar_germ"), label = "GERMLINE")
            ),

            shiny::tags$br(),

            #SIDEBAR TABS
            shiny::tabsetPanel(
              id = shiny::NS(id, "sidebar_tabset"),
              # ------ COMPARISON TAB - SIDEBAR SOMATIC-------
              somTab_ui_sidebar(shiny::NS(id, "SOM")),

              # ------ COMPARISON TAB - SIDEBAR GERM-------
              germTab_ui_sidebar(shiny::NS(id, "GERM")),
            ),

          )
        ),

        # MAIN CONTAINER
        shiny::tags$div(
          id = "colcol",
          class = "col-sm-9",

          # ------ COMPARISON TAB - MAIN -------
          shiny::mainPanel(
            width = 9,

            shiny::tabsetPanel(
              id = shiny::NS(id, "main_overview_files"),

              # ------ COMPARISON TAB - MAIN SOMATIC-------
              somTab_ui_table(shiny::NS(id, "SOM")),

              # ------ COMPARISON TAB - MAIN GERM-------
              germTab_ui_table(shiny::NS(id, "GERM")),

              # ------ COMPARISON TAB - MAIN VISUAL-------
              shiny::tabPanel(
                id = "comparison_visual_panel",
                title = "Statistics",
                value = "Statistics",
                shiny::fluidRow(
                  id = "comparison_title",
                  shiny::tags$h1("Statistics and Charts"),
                  htmltools::HTML(

                    '<span data-toggle = "tooltip" style = "float: left; margin-left: 15px; margin-top: 15px;" data-placement = "left"
                      title = "" data-original-title = " When you select a dataset all the stats will be shown on this page. \n \n If too much information is displayed you can minimize some of the visualization panels. \n \n If some charts do not show up there is probably a problem with your dataset, check the help section for more information or the error messages displayed on screen.">
                      <i class = "far fa-circle-question" role = "presentation"
                      aria-label = "circle-question icon"></i></span>'

                  )
                ),

                shiny::uiOutput(shiny::NS(id, "comparison_stats")),

                shiny::uiOutput(shiny::NS(id, "comparison_message"))

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
#' @examples gersomPanel_server("modulename")
gersomPanel_server <- function(id){
  shiny::moduleServer(

    id,

    function(input, output, session) {

      # ------ COMPARISON TAB - SERVER SOMATIC -------
      som_statistics_data <- somTab_server("SOM")

      # ------ COMPARISON TAB - SERVER GERM -------
      germ_statistics_data <- germTab_server("GERM")

      shiny::observeEvent(input[["GERM-germ_file_input_client"]], {
        shiny::updateTabsetPanel(session, "main_overview_files", "Germline")
      })

      shiny::observeEvent(input[["GERM-germ_file_input_server"]], {
        shiny::updateTabsetPanel(session, "main_overview_files", "Germline")
      })

      shiny::observeEvent(input[["SOM-som_file_input_client"]], {
        shiny::updateTabsetPanel(session, "main_overview_files", "Somatic")
      })

      shiny::observeEvent(input[["SOM-som_file_input_server"]], {
        shiny::updateTabsetPanel(session, "main_overview_files", "Somatic")
      })

      #JS ON SOMATIC SIDEBAR BUTTON PUSH
      shiny::observeEvent(input$sidebar_somatic, {
        shinyjs::runjs('

          document.querySelector("#GSP-sidebar_somatic").className += " active";
          document.querySelector("#GSP-sidebar_germ").className = "btn btn-default action-button shiny-bound-input";

        ')
        shiny::updateTabsetPanel(session, "main_overview_files", "Somatic")
      })

      #JS ON GERM SIDEBAR BUTTON PUSH
      shiny::observeEvent(input$sidebar_germ, {
        shinyjs::runjs('

          document.querySelector("#GSP-sidebar_germ").className += " active";
          document.querySelector("#GSP-sidebar_somatic").className = "btn btn-default action-button shiny-bound-input";

        ')
        shiny::updateTabsetPanel(session, "main_overview_files", "Germline")
      })


      ### COMPARISON PANEL SWITCH BUTTONS + TABS ###
      shiny::observeEvent(input$main_overview_files, {
        if(input$main_overview_files == "Somatic"){
          shinyjs::runjs('

            document.querySelector("#GSP-sidebar_somatic").className += " active";
            document.querySelector("#GSP-sidebar_germ").className = "btn btn-default action-button shiny-bound-input";

          ')
          shiny::updateTabsetPanel(session, "sidebar_tabset", "som_nav")
        }
        else if (input$main_overview_files == "Germline"){
          shinyjs::runjs('

            document.querySelector("#GSP-sidebar_germ").className += " active";
            document.querySelector("#GSP-sidebar_somatic").className = "btn btn-default action-button shiny-bound-input";

          ')
          shiny::updateTabsetPanel(session, "sidebar_tabset", "germ_nav")
        }
      })


      #---- STATISTICS TAB SERVER ----#
      maf_for_plot_comparison <- shiny::reactiveVal(NULL)
      comp_message <- shiny::reactiveVal("\n Here will be displayed the samples stats once available")
      last_graph_opt <- shiny::reactiveVal("comp_overview_chart")

      #SUMMARY PLOT OUTPUT
      output$combined_maf_plot <- shiny::renderPlot({
        shiny::req(maf_for_plot_comparison())
        if (!is.null(maf_for_plot_comparison()) && input$comparison_well_selection == "comp_maf_chart"){
          last_graph_opt("comp_maf_chart")
          try(
            maftools::plotmafSummary(maf_for_plot_comparison(), addStat = "median", fs = 1.10),
            silent = T
          )
        }
      }, res = 100)

      output$comp_charts <- shiny::renderUI({
        shiny::req(input$comparison_well_selection)
        if (!is.null(input$comparison_well_selection)){
          if (input$comparison_well_selection == "comp_overview_chart"){
            last_graph_opt("comp_overview_chart")
            if (!is.null(som_statistics_data()) && !is.null(germ_statistics_data())){
              som_excl <- try(dplyr::anti_join(som_statistics_data(), germ_statistics_data(), by = c("Chromosome", "Tumor_Seq_Allele2", "Start_Position")), silent =  T)
              germ_excl <- try(dplyr::anti_join(germ_statistics_data(), som_statistics_data(), by = c("Chromosome", "Tumor_Seq_Allele2", "Start_Position")), silent =  T)
              common_rows <- try(dplyr::semi_join(som_statistics_data(), germ_statistics_data(), by = c("Chromosome", "Tumor_Seq_Allele2", "Start_Position")), silent =  T)

              shiny::tags$div(
                id = "comp_charts_container",
                class = "charts_container",
                shiny::tags$div(
                  id = "som_tot_mut",
                  class = "panel panel-primary",
                  shiny::tags$div(
                    class = "panel-heading",
                    shiny::tags$p(nrow(som_statistics_data())),
                    shiny::icon("list", class = "fa-regular", lib = "font-awesome")
                  ),
                  shiny::tags$div(
                    class = "panel-body",
                    shiny::tags$p(shiny::tags$b("Somatic"), " Sample"),
                    shiny::tags$p("Filtered Mutations")
                  )
                ),

                shiny::tags$div(
                  id = "germ_tot_mut",
                  class = "panel panel-primary",
                  shiny::tags$div(
                    class = "panel-heading",
                    shiny::tags$p(nrow(germ_statistics_data())),
                    shiny::icon("list", class = "fa-regular", lib = "font-awesome")
                  ),
                  shiny::tags$div(
                    class = "panel-body",
                    shiny::tags$p(shiny::tags$b("Germline"), " Sample"),
                    shiny::tags$p("Filtered Variants")
                  )
                ),

                shiny::tags$div(
                  id = "som_excl_mut",
                  class = "panel panel-info",
                  shiny::tags$div(
                    class = "panel-heading",
                    shiny::tags$p(nrow(som_excl)),
                    shiny::icon("chart-simple", verify_fa = FALSE, lib = "font-awesome")
                  ),
                  shiny::tags$div(
                    class = "panel-body",
                    shiny::tags$p(shiny::tags$b("Somatic"), " Sample"),
                    shiny::tags$p("Exclusive Mutations")
                  )
                ),

                shiny::tags$div(
                  id = "germ_excl_mut",
                  class = "panel panel-info",
                  shiny::tags$div(
                    class = "panel-heading",
                    shiny::tags$p(nrow(germ_excl)),
                    shiny::icon("chart-simple", lib = "font-awesome")
                  ),
                  shiny::tags$div(
                    class = "panel-body",
                    shiny::tags$p(shiny::tags$b("Germline"), " Sample"),
                    shiny::tags$p("Exclusive Variants")
                  )
                ),

                shiny::tags$div(
                  id = "germ_som_common_mut",
                  class = "panel panel-info",
                  shiny::tags$div(
                    class = "panel-heading",
                    shiny::tags$p(nrow(common_rows)),
                    shiny::icon("pie-chart", lib = "font-awesome")
                  ),
                  shiny::tags$div(
                    class = "panel-body",
                    shiny::tags$p(" Samples "),
                    shiny::tags$p(shiny::tags$b("Shared"), " Mutations")
                  )
                ),
              )
            }
            else{
              shiny::tags$span(
                style = "text-align: center; color: red; font-size: initial;",
                shiny::icon("exclamation-triangle", lib = "font-awesome"),
                "Can't generate the plot"
              )
            }
          }
          else if (input$comparison_well_selection == "comp_maf_chart"){
            if (!is.null(maf_for_plot_comparison())){
              shiny::plotOutput("GSP-combined_maf_plot")
            }
            else{
              shiny::tags$span(
                style = "text-align: center; color: red; font-size: initial;",
                shiny::icon("exclamation-triangle", lib = "font-awesome"),
                "Can't generate the plot"
              )
            }
          }
        }
      })

      output$comparison_stats <- shiny::renderUI({
        #shiny::req(som_statistics_data())
        #shiny::req(germ_statistics_data())
        if (!is.null(som_statistics_data()) && !is.null(germ_statistics_data())){
          comp_message("")
          t_som <- try(maftools::read.maf(som_statistics_data()), silent = T)
          t_germ <- try(maftools::read.maf(germ_statistics_data()), silent = T)
          if (inherits(t_som, "try-error") && inherits(t_germ, "try-error")){
            #ERROR HANDLING
            shiny::wellPanel(
              id = "well_plot_comparison_1",
              class = "well_plot",
              toggle_panel("toggle_plot_comparison_1", "well_plot_container_comparison_1", "Summary:"),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container_comparison_1",
                class = "collapse in",
                shiny::tags$span(
                  style = "text-align: center; color: red; font-size: initial;",
                  shiny::icon("exclamation-triangle", lib = "font-awesome"),
                  "Can't generate the plots"
                )
              )
            )
          }
          else if (!inherits(t_som, "try-error") || !inherits(t_germ, "try-error")){
            datasets <- list()
            datasets <- append(datasets, t_som)
            datasets <- append(datasets, t_germ)
            t <- try(
              maftools:::merge_mafs(datasets)
            )
            if (!inherits(t, "try-error")){
              maf_for_plot_comparison(t)
            }
            else{
              maf_for_plot_comparison(NULL)
            }

            shiny::wellPanel(
              id = "well_plot_comparison_1",
              class = "well_plot",
              toggle_panel("toggle_plot_comparison_1", "well_plot_container_comparison_1", "Summary:"),
              shiny::tags$div(
                # CONTAINER TOGGLER INPUT ID + CLASS
                id = "well_plot_container_comparison_1",
                class = "collapse in",
                shiny::tags$div(
                  class = "chart_controls_cont",
                  shinyWidgets::radioGroupButtons(
                    inputId = "GSP-comparison_well_selection",
                    label = "",
                    selected = last_graph_opt(),
                    choices = c(`<p>Overview<i class='fa fa-pie-chart' style = "margin-left: 6px;"></i></p>` = "comp_overview_chart",
                                `<p>MAF Summary<i class='fa fa-bar-chart' style = "margin-left: 6px;"></i></p>` = "comp_maf_chart"),
                    justified = TRUE
                  ),
                ),

                shiny::tags$hr(style = "margin-top: 5px; margin-bottom: 5px;"),

                shiny::uiOutput("GSP-comp_charts")

              )
            )
          }
        }
        else{
          comp_message("\n Here will be displayed the samples stats once available")
          NULL
        }
      })

      output$comparison_message <- shiny::renderUI({
        if (nchar(comp_message()) > 0){
          shiny::tags$div(
            shiny::tags$h3(
              class = "file_title_start_comp",
              comp_message(),
              shiny::tags$div(
                style = "display: flex; justify-content: center; align-items: center;",
                shiny::icon('line-chart', style = "font-size: xx-large; margin-right: 10px; margin-left: 10px;"),
                shiny::icon('bar-chart', style = "font-size: xx-large; margin-right: 10px; margin-left: 10px;"),
                shiny::icon('area-chart', style = "font-size: xx-large; margin-right: 10px; margin-left: 10px;")
              )
            )
          )
        }
        else{
          shiny::tags$h1(
            class = "file_title1",
            comp_message()
          )
        }
      })

    } #SERVER FUNCTION END
  )
}
