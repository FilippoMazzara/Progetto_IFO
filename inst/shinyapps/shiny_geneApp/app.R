ui <- shiny::fluidPage(

  id = "mainpage",

  # ------ Styling -------

  shinyjs::useShinyjs(),

  shiny::includeCSS("www/style.css"),

  shiny::includeScript("www/myscript.js"),

  # ------ hidden header -------

  shiny::tags$div(
    style = "display: none",
    shiny::titlePanel(
      title = "",
      windowTitle = "geneApp"
    )
  ),

  # ------ MAIN NAVBAR -------

  shiny::navbarPage(

    # ------ MAIN NAVBAR OPTIONS -------
    windowTitle = "HOME",
    position = "static-top",
    header = NULL,
    footer = NULL,
    collapsible = TRUE,
    id = "topnavbar",
    fluid = TRUE,

    # ------ MAIN NAVBAR TITLE + LOGO-------
    title = shiny::tags$div(
      shiny::tags$a(
        href = "/",
        shiny::tags$img(
          id = "logo1",
          src = "img/logo.jpg"
        )
      )
    ),

    # ------ HOME PAGE MODULE UI -------

    homePanel_ui("homepage"),

    # ------ OVERVIEW PAGE MODULE UI -------

    shiny::tabPanel(
      title = "GERSOM",
      value = "gersom",
      id = "gersom",
      class = "topchoice",

      # ------ OVERVIEW NAVBAR + CONT -------
      # IT'S POSSIBLE TO SWITCH OUT THE NAVBAR IN FAVOR OF A SINGLE TAB

      shiny::tags$div(
        id = "nav_cont_2",
        shiny::navbarPage(

          # ------ OVERVIEW NAVBAR OPTIONS -------

          windowTitle = "GERSOM",
          position = "static-top",
          collapsible = TRUE,
          id = "topnavbar2",


          # ------ OVERVIEW NAVBAR TITLE/LOGO -------

          title = shiny::tags$a(
              id = "toggleSidebar",
              class = "toggleSidebar",
              shiny::tags$span(class = "toggleLine"),
              shiny::tags$span(class = "toggleLine"),
              shiny::tags$span(class = "toggleLine")
          ),

          # ------ SOMATIC AND GERM UI -------

          gersomPanel_ui("GSP"),

          # second and third set of nav tabs
          #PER ORA NON FANNO NULLA
          shiny::navbarMenu(title = "tab1", menuName = "tab1", "panel 1.1", shiny::tabPanel("1.1"), "panel 1.2", shiny::tabPanel("1.2")),
          shiny::navbarMenu(title = "tab2", menuName = "tab2", shiny::tabPanel("2.1"), shiny::tabPanel("2.2")),

        )
      ) #END FLUID ROW AND NAV PAGE AND CONTAINER
    ),

    # ------ ABOUT MODULE UI -------

    about_ui("about")
  ),

  # ------ FOOTER -------

  shiny::tags$div(
    class = "footer",
    shiny::includeHTML("www/footer.html")
  )
)

#' Main geneApp content
#' @description
#' Contains the server, the ui and the launcher function.
#' The app should not be launched through this file.
#' @importFrom magrittr "%>%"
#' @param input
#' the input vector
#' @param output
#' the output vector
#' @return the running gene app
server <- function(input, output, session){

  #data.table::setDTthreads(4)

  # INCREASE HTTP REQUEST SIZE TO 30mb
  options(shiny.maxRequestSize = 30 * 1024^2)

  # ------ HOME SERVER MODULE -------
  homePanel_server("homepage")

  ### HOMEPAGE LINK ###
  shiny::observeEvent(input$linkapp,{
    shiny::updateNavbarPage(session, "topnavbar", "gersom")
  })

  # ------ SOMATIC AND GERM SERVER MODULE -------

  gersomPanel_server("GSP")


  ## altezza dinamica solo tabella som/ possibile mem leak TROVA UN ALTRO MODO PER FORZARE L'HEIGHT
  shinyjs::onevent( event = "mousemove", id = "nav_cont_2", {
    shinyjs::runjs(
      'var x = document.querySelector("#GSP-som_table > div > div.dataTables_scroll > div.dataTables_scrollBody > table");
       var y = document.querySelector("#GSP-som_table > div > div.dataTables_scroll > div.dataTables_scrollBody");
      if (y != null && x != null){
       if (y.style.maxHeight != x.offsetHeight + 20){y.style.maxHeight = x.offsetHeight + 20 + "px";}}'
    )
  })

  ### OVERVIEW SIDEBAR TOGGLE ###
  shinyjs::onclick( id = "toggleSidebar", {
    shinyjs::runjs(
      'var x = document.querySelector("#nav_cont_2 > nav > div > div");
      var y = document.querySelector("#nav_cont_2 > nav > div > div.navbar-collapse.collapse");
      var b1 = document.querySelector("#GSP-som_table > div > div.row_b > div.dt-buttons");
      var b2 = document.querySelector("#GSP-germ_table > div > div.row_b > div.dt-buttons");
      var p1 = document.querySelector("#GSP-som_table > div > div.row_i > div.dataTables_length");
      var p2= document.querySelector("#GSP-germ_table >  div > div.row_i > div.dataTables_length");
      var i1 = document.querySelectorAll("#GSP-som_table > div > div.row_i > div.dataTables_info");
      var i2= document.querySelectorAll("#GSP-germ_table > div > div.row_i > div.dataTables_info");

      if (x.style.width == "93px") {
        if (b1 != null){
          b1.style.left = "25%";
          p1.style.left = "25%";
          i1[0].style.left = "25%";
          i1[1].style.left = "25%";
        }
        if (b2 != null){
          b2.style.left = "25%";
          p2.style.left = "25%";
          i2[0].style.left = "25%";
          i2[1].style.left = "25%";
        }

          x.style.width="24.5%";
          y.style.left="24.5%";

      }
      else {
      if (b1 != null){
        b1.style.left = "0";
        p1.style.left = "0";
        i1[0].style.left = "0";
          i1[1].style.left = "0";
      }
      if (b2 != null){
        b2.style.left = "0";
        p2.style.left = "0";
        i2[0].style.left = "0";
          i2[1].style.left = "0";
      }

      y.style.left="0";
      x.style.width="93px";

      }'
    )
    shinyjs::toggle(
      id = "sidebar",
      anim = TRUE,
      animType = "fade",
      time = 0.1
    )
  })

  # ------ ABOUT SERVER MODULE -------
  about_server("about")

}

#enabling reactlog testing
#reactlog::reactlog_enable()

# Run the application
shiny::shinyApp(ui = ui, server = server)
