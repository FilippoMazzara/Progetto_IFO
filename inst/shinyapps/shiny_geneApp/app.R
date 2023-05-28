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

  ### SOM TABLE STICKY SCROLLER###
  shinyjs::onevent( event = "scroll", id = "rowsc1", {
    shinyjs::runjs("

      var x = document.querySelector('#GSP-SOM-som_table > div > div > div.dataTables_scrollBody');
      var y = document.querySelector('#rowsc1');
      x.scrollLeft = y.scrollLeft;

    ")
  })

  ### GERM TABLE STICKY SCROLLER###
  shinyjs::onevent( event = "scroll", id = "rowsc2", {
    shinyjs::runjs("

      var x = document.querySelector('#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody');
      var y = document.querySelector('#rowsc2');
      x.scrollLeft = y.scrollLeft;

    ")
  })

  #Questa parte di codice js viene eseguita sin da subito
  #ogni 0.5s attende il rendering della pagina GERSOM
  #poi se nota un cambiamento nella pagina, crea l'observer opportuno
  #a sua volta quell'observer ridimensionerà lo scroller se la tabella cambia di width
  shinyjs::runjs('
    function addObserverIfDesiredNodeAvailable() {
      if(document.getElementById("colcol") == null) {
        window.setTimeout(addObserverIfDesiredNodeAvailable,500);
        return;
      }
      else{

        const config = {childList: true, subtree: true };
        var x = 0;
        var y = 0;

        const callback1 = (mutationList, observer) => {
            var x1 = document.querySelector("#GSP-SOM-som_table > div > div > div.dataTables_scrollBody > table");
            var y1 = document.querySelector("#rowsc1 > div");
            if(y1 !== null){y1.style.width = x1.offsetWidth + "px";}
        };

        const callback2 = (mutationList, observer) => {
            var x2 = document.querySelector("#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody > table");
            var y2 = document.querySelector("#rowsc2 > div");
            if(y2 !== null){y2.style.width = x2.offsetWidth + "px";}
        };

        const callback3 = (mutationList, observer) => {
          if (document.querySelector("#GSP-SOM-som_table > div > div > div.dataTables_scrollBody > table") !== null && x == 0) {
            const observer1 = new MutationObserver(callback1);
            x = 1;
            observer1.observe(document.querySelector("#GSP-SOM-som_table > div > div > div.dataTables_scrollBody > table"), config);
          }
          if (x == 1){
            observer.disconnect();
          }
        };

        const callback4 = (mutationList, observer) => {
          if (document.querySelector("#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody > table") !== null && y == 0) {
            y = 1;
            const observer2 = new MutationObserver(callback2);
            observer2.observe(document.querySelector("#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody > table"), config);
          }
          if (y == 1){
            observer.disconnect();
          }
        };

        const observer3 = new MutationObserver(callback3);
        observer3.observe(document.getElementById("tabcontainer"), config);

        const observer4 = new MutationObserver(callback4);
        observer4.observe(document.getElementById("tabcontainer2"), config);
      }
    }
    addObserverIfDesiredNodeAvailable();
  ')

  ##syncronize the scroll position of the two scrollers##
  shinyjs::runjs('

    $("#GSP-SOM-som_table").on("mouseup",function() {
      var x = document.querySelector("#GSP-SOM-som_table > div > div > div.dataTables_scrollBody");
      var y = document.querySelector("#rowsc1");
      y.scrollLeft = x.scrollLeft;
    });

  ')

  shinyjs::runjs('

    $("#GSP-GERM-som_table").on("mouseup",function() {
      var x = document.querySelector("#GSP-GERM-som_table > div > div > div.dataTables_scrollBody");
      var y = document.querySelector("#rowsc2");
      y.scrollLeft = x.scrollLeft;
    });

  ')

  ### OVERVIEW SIDEBAR TOGGLE ###
  shinyjs::onclick( id = "toggleSidebar", {
    shinyjs::runjs('

      var x = document.querySelector("#nav_cont_2 > nav > div > div");

      if (x.style.width == "93px") {
        x.style.width = "25%";
        x.style.minWidth = "inherit";
      }
      else {
        x.style.setProperty("min-width", "auto", "important");
        x.style.width="93px";
      }

    ')
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
