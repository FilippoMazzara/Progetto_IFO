#' gersomPanel module ui
#' @description
#' ui module for the comparison of somatic and germ files
#' @param id the module id
#'
#' @examples gersomPanel_ui("modulename")
#'
gersomPanel_ui <- function(id){
  ns <- shiny::NS(id)

  # ------ FIRST TAB -------
  shiny::tabPanel(
    title = "Graph",
    id = "graph",
    value =  "graph",

    #LAYOUT CONTAINER
    shiny::tags$div(
      id = "overview_container",

      # ------ SIDEBAR LAYOUT -------
      shiny::sidebarLayout(

        #SIDEBAR CONTAINER
        shiny::tags$div(
          id = "sidebar",
          class ="col-sm-3",

          # ------ FIRST TAB - SIDEBAR -------

          shiny::sidebarPanel(
            width = 3,
            # SIDEBAR SWITCH
            shiny::tags$div(
              style = "display:flex; justify-content: center;",
              shiny::actionButton(inputId = shiny::NS(id,"sidebar_somatic"), label = "SOMATIC", class = "btn btn-default action-button shiny-bound-input active" ),
              shiny::actionButton(inputId = shiny::NS(id,"sidebar_germ"), label = "GERMLINE")
            ),

            shiny::tags$br(),

            shiny::tabsetPanel(
              id = shiny::NS(id,"sidebar_tabset"),
              somTab_ui_sidebar(shiny::NS(id,"SOM")),
              germTab_ui_sidebar(shiny::NS(id,"GERM")),
            ),


          ) # FIRST TAB - SIDEBAR END
        ), # FIRST TAB - SIDEBAR CONTAINER END
        # MAIN CONTAINER
        shiny::tags$div(
          id = "colcol",
          class = "col-sm-9",

          # ------ FIRST TAB - MAIN -------
          shiny::mainPanel(
            width = 9,

            shiny::tabsetPanel(
              id = shiny::NS(id,"main_overview_files"),

              # ------ FIRST TAB - MAIN SOMATIC-------
              somTab_ui_table(shiny::NS(id,"SOM")),

              # ------ FIRST TAB - MAIN GERM-------
              germTab_ui_table(shiny::NS(id,"GERM")),

              # ------ FIRST TAB - MAIN VISUAL-------
              shiny::tabPanel(
                id = "gersom_visual_panel",
                title = "pannello3",
                value = "pannello3",
                shiny::tags$div(
                  id = "value_boxes_gersom",
                  shinydashboard::valueBoxOutput(shiny::NS(id,"totmut")),
                  shinydashboard::valueBoxOutput(shiny::NS(id,"sommut")),
                  shinydashboard::valueBoxOutput(shiny::NS(id,"germmut"))
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
#'
#' @examples gersomPanel_server("modulename")
#'
gersomPanel_server <- function(id){ #oltre id puoi passare altri parametri
  shiny::moduleServer(
    id,

    function(input, output, session) {

      somTab_server("SOM")

      germTab_server("GERM")

      #JS ON SOMATIC BUTTON PUSH
      shiny::observeEvent(input$sidebar_somatic, {
        shinyjs::runjs('

          document.querySelector("#GSP-sidebar_somatic").className += " active";
          document.querySelector("#GSP-sidebar_germ").className= "btn btn-default action-button shiny-bound-input";

        ')
        shiny::updateTabsetPanel(session, "main_overview_files", "pannello1")
      })

      #JS ON GERM BUTTON PUSH
      shiny::observeEvent(input$sidebar_germ, {
        shinyjs::runjs('

          document.querySelector("#GSP-sidebar_germ").className += " active";
          document.querySelector("#GSP-sidebar_somatic").className = "btn btn-default action-button shiny-bound-input";

        ')
        shiny::updateTabsetPanel(session, "main_overview_files", "pannello2")
      })


      ### OVERVIEW PANEL SWITCH BUTTONS + TABS ###
      shiny::observeEvent(input$main_overview_files, {
        if(input$main_overview_files == "pannello1"){
          shinyjs::runjs('

            document.querySelector("#GSP-sidebar_somatic").className += " active";
            document.querySelector("#GSP-sidebar_germ").className= "btn btn-default action-button shiny-bound-input";

          ')
          shiny::updateTabsetPanel(session, "sidebar_tabset", "som_nav")
        }
        else if (input$main_overview_files == "pannello2"){
          shinyjs::runjs('

            document.querySelector("#GSP-sidebar_germ").className += " active";
            document.querySelector("#GSP-sidebar_somatic").className = "btn btn-default action-button shiny-bound-input";

          ')
          shiny::updateTabsetPanel(session, "sidebar_tabset", "germ_nav")
        }
      })


      #---- pagina confronto ----#
      output$totmut <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          paste0(25 + input$count, "%"), "Progress", icon = shiny::icon("list"),
          color = "lightblue"
        )
      })

      output$sommut <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          paste0(25 + input$count, "%"), "Progress", icon = shiny::icon("list"),
          color = "lightblue"
        )
      })

      output$germmut <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          paste0(25 + input$count, "%"), "Progress", icon = shiny::icon("list"),
          color = "lightblue"
        )
      })


    }

  )
}
