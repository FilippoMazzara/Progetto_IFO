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
          #collapsible = TRUE,
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

          # ------ MULTIPLE FILES UI -------

          multiPanel_ui("MULTI"),

          # second and third set of nav tabs
          #PER ORA NON FANNO NULLA

          #shiny::navbarMenu(title = "tab1", menuName = "tab1", "panel 1.1", shiny::tabPanel("1.1"), "panel 1.2", shiny::tabPanel("1.2")),
          #shiny::navbarMenu(title = "tab2", menuName = "tab2", shiny::tabPanel("2.1"), shiny::tabPanel("2.2")),

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
  options(shiny.maxRequestSize = 120 * 1024^2)

  # ------ HOME SERVER MODULE -------
  homePanel_server("homepage")

  ### HOMEPAGE LINK ###
  shiny::observeEvent(input$linkapp,{
    shiny::updateNavbarPage(session, "topnavbar", "gersom")
  })

  # ------ SOMATIC AND GERM SERVER MODULE -------

  gersomPanel_server("GSP")

  # ------ MULTIPLE FILES SERVER MODULE -------

  multiPanel_server("MULTI")

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

  ### MULTI TABLE STICKY SCROLLER###
  shinyjs::onevent( event = "scroll", id = "rowsc3", {
    shinyjs::runjs("

      var x = document.querySelector('#MULTI-multi_table > div > div > div.dataTables_scrollBody');
      var y = document.querySelector('#rowsc3');
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
        var z = 0;

        const callback1 = (mutationList, observer) => {
            var x1 = document.querySelector("#GSP-SOM-som_table > div > div > div.dataTables_scrollBody > table");
            var y1 = document.querySelector("#rowsc1 > div");
            if(y1 !== null && x1 !== null){
              y1.style.width = x1.offsetWidth - 1 + "px";
            }
        };

        const callback2 = (mutationList, observer) => {
            var x2 = document.querySelector("#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody > table");
            var y2 = document.querySelector("#rowsc2 > div");
            if(y2 !== null && x2 !== null){
              y2.style.width = x2.offsetWidth - 1 + "px";
            }
        };

        const callback5 = (mutationList, observer) => {
            var x5 = document.querySelector("#MULTI-multi_table > div > div > div.dataTables_scrollBody > table");
            var y5 = document.querySelector("#rowsc3 > div");
            if(y5 !== null && x5 !== null){
              y5.style.width = x5.offsetWidth - 1 + "px";
            }
        };

        const callback3 = (mutationList, observer) => {
          if (document.querySelector("#GSP-SOM-som_table > div > div > div.dataTables_scrollBody > table") !== null && x == 0) {
            const observer1 = new MutationObserver(callback1);
            x = 1;
            observer1.observe(document.getElementById("tabcontainer"), config);
          }
          if (x == 1){

            observer.disconnect();
          }
        };

        const callback4 = (mutationList, observer) => {
          if (document.querySelector("#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody > table") !== null && y == 0) {
            y = 1;
            const observer2 = new MutationObserver(callback2);
            observer2.observe(document.getElementById("tabcontainer2"), config);
          }
          if (y == 1){
            observer.disconnect();
          }
        };

        const callback6 = (mutationList, observer) => {
          if (document.querySelector("#MULTI-multi_table > div > div > div.dataTables_scrollBody > table") !== null && z == 0) {
            z = 1;
            const observer5 = new MutationObserver(callback5);
            observer5.observe(document.getElementById("tabcontainer3"), config);
          }
          if (z == 1){
            observer.disconnect();
          }
        };


        const observer3 = new MutationObserver(callback3);
        observer3.observe(document.getElementById("tabcontainer"), config);

        const observer4 = new MutationObserver(callback4);
        observer4.observe(document.getElementById("tabcontainer2"), config);

        const observer6 = new MutationObserver(callback5);
        observer6.observe(document.getElementById("tabcontainer3"), config);

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

  shinyjs::runjs('

    $("#MULTI-multi_table").on("mouseup",function() {
      var x = document.querySelector("#MULTI-multi_table > div > div > div.dataTables_scrollBody");
      var y = document.querySelector("#rowsc3");
      y.scrollLeft = x.scrollLeft;
    });

  ')
##SISTEMA QUESTO CODICE E COPIALO IN GERM E ASSICURATI CHE FUNZIONI
  ### OVERVIEW SIDEBAR TOGGLE ###
  shinyjs::onclick( id = "toggleSidebar", {
    shinyjs::runjs('

      var x = document.querySelector("#nav_cont_2 > nav > div > div");
      var h1 = document.querySelector("#GSP-SOM-som_table > div > div.dataTables_scroll > div.dataTables_scrollHead > div.dataTables_scrollHeadInner > table");
      var b1 = document.querySelector("#GSP-SOM-som_table > div > div.dataTables_scroll > div.dataTables_scrollBody");
      var b2 = document.querySelector("#GSP-GERM-germ_table > div > div.dataTables_scroll > div.dataTables_scrollBody");
      var h2 = document.querySelector("#GSP-GERM-germ_table > div > div.dataTables_scroll > div.dataTables_scrollHead > div.dataTables_scrollHeadInner > table");
      var b3 = document.querySelector("#MULTI-multi_table > div > div.dataTables_scroll > div.dataTables_scrollBody");
      var h3 = document.querySelector("#MULTI-multi_table > div > div.dataTables_scroll > div.dataTables_scrollHead > div.dataTables_scrollHeadInner > table");
      plot1 = document.querySelector("#GSP-SOM-som_plot2 > img ");
      plot2 = document.querySelector("#GSP-GERM-germ_plot2 > img ");
      plot3 = document.querySelector("#MULTI-multi_plot2 > img ");
      if (x.style.width == "93px") {
        x.style.width = "25%";
        x.style.minWidth = "inherit";
        if(plot1 !== null){
          plot1.style.width = "auto";
        }
        if(plot2 !== null){
          plot2.style.width = "auto";
        }
        if(plot3 !== null){
          plot3.style.width = "auto";
        }
        if(h1 !== null){
          b1.style.maxWidth = "inherit";
        }
        if(h2 !== null){
          b2.style.maxWidth = "inherit";
        }
        if(h3 !== null){
          b3.style.maxWidth = "inherit";
        }

      }
      else {

        x.style.setProperty("min-width", "auto", "important");
        x.style.width = "93px";
        if (plot1 != null){
            plot1.style.width = "100%"
        }
        if (plot2 != null){
            plot2.style.width = "100%"
        }
        if (plot3 != null){
            plot3.style.width = "100%"
        }
        if (h1 !== null && h1.offsetWidth > 0){
          b1.style.maxWidth = h1.offsetWidth + "px";
        }
        if (h1 !== null && h1.offsetWidth == 0){
          b1.style.maxWidth = "fit-content";
        }
        if (h2 !== null && h2.offsetWidth > 0){
          b2.style.maxWidth = h2.offsetWidth + "px";
        }
        if (h2 !== null && h2.offsetWidth == 0){
          b2.style.maxWidth = "fit-content";
        }
        if (h3 !== null && h3.offsetWidth > 0){
          b3.style.maxWidth = h3.offsetWidth + "px";
        }
        if (h3 !== null && h3.offsetWidth == 0){
          b3.style.maxWidth = "fit-content";
        }

      }
    ')

    shinyjs::toggle(
      id = "sidebar",
      anim = TRUE,
      animType = "fade",
      time = 0.1
    )
    shinyjs::toggle(
      id = "sidebar_multi",
      anim = TRUE,
      animType = "fade",
      time = 0.1
    )
  })

  shinyjs::onclick("GSP-SOM-reset_filter1",{
    shinyjs::reset("well_filter_container1")
  })

  shinyjs::onclick("GSP-GERM-reset_filter2",{
    shinyjs::reset("well_filter_container2")
  })

  shinyjs::onclick("MULTI-reset_filter3",{
    shinyjs::reset("well_filter_container3")
  })



  # ------ ABOUT SERVER MODULE -------
  about_server("about")

}

#enabling reactlog testing
#reactlog::reactlog_enable()

# Run the application
shiny::shinyApp(ui = ui, server = server)
