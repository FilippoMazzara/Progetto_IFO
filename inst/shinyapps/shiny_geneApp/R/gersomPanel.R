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
                shiny::tags$div(
                  id = "value_boxes_comparison",
                  #info boxes
                  shinydashboard::valueBoxOutput(shiny::NS(id, "totmut")),
                  shinydashboard::valueBoxOutput(shiny::NS(id, "sommut")),
                  shinydashboard::valueBoxOutput(shiny::NS(id, "germmut"))
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
#' @examples gersomPanel_server("modulename")
gersomPanel_server <- function(id){
  shiny::moduleServer(

    id,

    function(input, output, session) {

      # ------ COMPARISON TAB - SERVER SOMATIC -------
      som_statistics_data <- somTab_server("SOM")

      # ------ COMPARISON TAB - SERVER GERM -------
      germ_statistics_data <- germTab_server("GERM")

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
      output$totmut <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          paste0(25, "%"), "Progress", icon = shiny::icon("list"),
          color = "light-blue"
        )
      })

      output$sommut <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          paste0(25, "%"), "Progress", icon = shiny::icon("list"),
          color = "light-blue"
        )
      })

      output$germmut <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          paste0(25, "%"), "Progress", icon = shiny::icon("list"),
          color = "light-blue"
        )
      })

    } #SERVER FUNCTION END
  )
}
