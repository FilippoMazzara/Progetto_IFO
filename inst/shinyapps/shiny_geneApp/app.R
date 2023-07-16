ui <- shiny::fluidPage( #bslib::page_fluid(

  id = "mainpage",

  # ------ Styling -------

  #ENABLE SHINYJS
  shinyjs::useShinyjs(),

  shiny::includeCSS("www/style.css"),

  shiny::includeScript("www/myscript.js"),

  # ------ hidden header -------

  shiny::tags$div(
    style = "display: none;",
    shiny::titlePanel(
      title = "",
      windowTitle = "geneApp" #CAMBIA QUANDO HAI UN ALTRO NOME
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
          src = "img/logo1.jpg"
        )
      )
    ),

    # ------ HOME PAGE MODULE UI -------

    homePanel_ui("homepage"),

    # ------ OVERVIEW PAGE MODULE UI -------

    shiny::tabPanel(
      title = "Overview",
      value = "overview",
      id = "overview_page",
      class = "topchoice",

      # ------ OVERVIEW NAVBAR + CONT -------

      shiny::tags$div(
        id = "nav_cont_2",
        shiny::navbarPage(

          # ------ OVERVIEW NAVBAR OPTIONS -------

          windowTitle = "Overview",
          position = "static-top",
          id = "topnavbar2",

          # ------ OVERVIEW NAVBAR TITLE/LOGO -------

          title = shiny::tags$a(
              id = "toggleSidebar",
              class = "toggleSidebar",
              shiny::icon("bars", lib = "font-awesome")
          ),

          # ------ SOMATIC AND GERM UI -------

          gersomPanel_ui("GSP"),

          # ------ MULTIPLE FILES UI -------

          multiTab_page_ui("MULTI"),

        )
      ) #END FLUID ROW AND NAV PAGE AND CONTAINER
    ),

    # ------ HELP PAGE MODULE UI -------

    help_page_ui("help_page"),

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

  #NUMBER OF DT THREADS (DEFAULT SINGLE THREADED)
  data.table::setDTthreads(4)

  # INCREASE HTTP REQUEST SIZE TO 120mb
  options(shiny.maxRequestSize = 120 * 1024^2)

  # ------ HOME SERVER MODULE -------
  homePanel_server("homepage")

  ### HOMEPAGE LINK ###
  shiny::observeEvent(input$linkapp,{
    shiny::updateNavbarPage(session, "topnavbar", "overview")
  })

  ### HELP LINKS ###
  shiny::observeEvent(input$som_helplink,{
    shiny::updateNavbarPage(session, "topnavbar", "help_page")
  })

  shiny::observeEvent(input$germ_helplink,{
    shiny::updateNavbarPage(session, "topnavbar", "help_page")
  })

  shiny::observeEvent(input$multi_helplink,{
    shiny::updateNavbarPage(session, "topnavbar", "help_page")
  })

  # ------ SOMATIC AND GERM SERVER MODULE -------

  gersomPanel_server("GSP")

  # ------ MULTIPLE FILES SERVER MODULE -------

  multiTab_page_server("MULTI")

  ### SOM TABLE STICKY SCROLLER###
  shinyjs::onevent( event = "scroll", id = "rowsc_som", {
    shinyjs::runjs("

      var x = document.querySelector('#GSP-SOM-som_table > div > div > div.dataTables_scrollBody');
      var y = document.querySelector('#rowsc_som');
      x.scrollLeft = y.scrollLeft;

    ")
  })

  ### GERM TABLE STICKY SCROLLER###
  shinyjs::onevent( event = "scroll", id = "rowsc_germ", {
    shinyjs::runjs("

      var x = document.querySelector('#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody');
      var y = document.querySelector('#rowsc_germ');
      x.scrollLeft = y.scrollLeft;

    ")
  })

  ### MULTI TABLE STICKY SCROLLER###
  shinyjs::onevent( event = "scroll", id = "rowsc_multi", {
    shinyjs::runjs("

      var x = document.querySelector('#MULTI-multi_table > div > div > div.dataTables_scrollBody');
      var y = document.querySelector('#rowsc_multi');
      x.scrollLeft = y.scrollLeft;

    ")
  })

  #This is immediately executed
  #every 0.5s checks if the page has rendered
  #then create the appropriate observer
  #that will resize the scroller of the table if there is a width variation in the table
  shinyjs::runjs('

    function addObserverIfDesiredNodeAvailable() {

      if(document.getElementById("colcol") == null) {
        window.setTimeout(addObserverIfDesiredNodeAvailable, 500);
        return;
      }

      else{

        const config = {childList: true, subtree: true};
        var x = 0;
        var y = 0;
        var z = 0;

        const callback1 = (mutationList, observer) => {
            var x1 = document.querySelector("#GSP-SOM-som_table > div > div > div.dataTables_scrollBody > table");
            var y1 = document.querySelector("#rowsc_som > div");
            if(y1 !== null && x1 !== null){
              y1.style.width = x1.offsetWidth - 1 + "px";
            }
        };

        const callback2 = (mutationList, observer) => {
            var x2 = document.querySelector("#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody > table");
            var y2 = document.querySelector("#rowsc_germ > div");
            if(y2 !== null && x2 !== null){
              y2.style.width = x2.offsetWidth - 1 + "px";
            }
        };

        const callback5 = (mutationList, observer) => {
            var x5 = document.querySelector("#MULTI-multi_table > div > div > div.dataTables_scrollBody > table");
            var y5 = document.querySelector("#rowsc_multi > div");
            if(y5 !== null && x5 !== null){
              y5.style.width = x5.offsetWidth - 1 + "px";
            }
        };

        const callback3 = (mutationList, observer) => {
          if (document.querySelector("#GSP-SOM-som_table > div > div > div.dataTables_scrollBody > table") !== null && x == 0) {
            const observer1 = new MutationObserver(callback1);
            x = 1;
            observer1.observe(document.getElementById("som_tab_container_main"), config);
          }
          if (x == 1){
            observer.disconnect();
          }
        };

        const callback4 = (mutationList, observer) => {
          if (document.querySelector("#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody > table") !== null && y == 0) {
            y = 1;
            const observer2 = new MutationObserver(callback2);
            observer2.observe(document.getElementById("germ_tab_container_main"), config);
          }
          if (y == 1){
            observer.disconnect();
          }
        };

        const callback6 = (mutationList, observer) => {
          if (document.querySelector("#MULTI-multi_table > div > div > div.dataTables_scrollBody > table") !== null && z == 0) {
            z = 1;
            const observer5 = new MutationObserver(callback5);
            observer5.observe(document.getElementById("multi_tab_container_main"), config);
          }
          if (z == 1){
            observer.disconnect();
          }
        };


        const observer3 = new MutationObserver(callback3);
        observer3.observe(document.getElementById("som_tab_container_main"), config);

        const observer4 = new MutationObserver(callback4);
        observer4.observe(document.getElementById("germ_tab_container_main"), config);

        const observer6 = new MutationObserver(callback5);
        observer6.observe(document.getElementById("multi_tab_container_main"), config);

      }
    }
    addObserverIfDesiredNodeAvailable();

  ')

  ##synchronize the scroll position of the two scrollers for all the tables##
  shinyjs::runjs('

    $("#GSP-SOM-som_table").on("mouseup", function() {
      var x = document.querySelector("#GSP-SOM-som_table > div > div > div.dataTables_scrollBody");
      var y = document.querySelector("#rowsc_som");
      y.scrollLeft = x.scrollLeft;
    });

  ')

  shinyjs::runjs('

    $("#GSP-GERM-germ_table").on("mouseup", function() {
      var x = document.querySelector("#GSP-GERM-germ_table > div > div > div.dataTables_scrollBody");
      var y = document.querySelector("#rowsc_germ");
      y.scrollLeft = x.scrollLeft;
    });

  ')

  shinyjs::runjs('

    $("#MULTI-multi_table").on("mouseup", function() {
      var x = document.querySelector("#MULTI-multi_table > div > div > div.dataTables_scrollBody");
      var y = document.querySelector("#rowsc_multi");
      y.scrollLeft = x.scrollLeft;
    });

  ')

  ### OVERVIEW SIDEBAR TOGGLE ###
  shinyjs::onclick( id = "toggleSidebar", {
    shinyjs::runjs('

      var icon = document.querySelector("#toggleSidebar > i");
      var x = document.querySelector("#nav_cont_2 > nav > div > div");
      var b1 = document.querySelector("#GSP-SOM-som_table > div > div.dataTables_scroll > div.dataTables_scrollBody");
      var h1 = document.querySelector("#GSP-SOM-som_table > div > div.dataTables_scroll > div.dataTables_scrollHead > div.dataTables_scrollHeadInner > table");
      var b2 = document.querySelector("#GSP-GERM-germ_table > div > div.dataTables_scroll > div.dataTables_scrollBody");
      var h2 = document.querySelector("#GSP-GERM-germ_table > div > div.dataTables_scroll > div.dataTables_scrollHead > div.dataTables_scrollHeadInner > table");
      var b3 = document.querySelector("#MULTI-multi_table > div > div.dataTables_scroll > div.dataTables_scrollBody");
      var h3 = document.querySelector("#MULTI-multi_table > div > div.dataTables_scroll > div.dataTables_scrollHead > div.dataTables_scrollHeadInner > table");
      plot1 = document.querySelector("#GSP-SOM-som_plot_main2 > img");
      plot2 = document.querySelector("#GSP-GERM-germ_plot_main2 > img");
      plot3 = document.querySelector("#MULTI-multi_plot_main2 > img");

      if (x.style.width == "93px") {
        icon.classList.toggle("sidebarRotate");
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
        icon.classList.toggle("sidebarRotate");
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

  #RESET THE CLIENT INPUT WHEN A FILE IS SELECTED FROM THE SERVER
  shinyjs::onclick("GSP-SOM-som_reset_client_input_button", {
    shinyjs::reset("som_side_well_input")
  })

  shinyjs::onclick("GSP-GERM-germ_reset_client_input_button", {
    shinyjs::reset("germ_side_well_input")
  })

  shinyjs::onclick("MULTI-multi_reset_client_input_button", {
    shinyjs::reset("multi_side_well_input")
  })

  # Reset filter values buttons
  shinyjs::onclick("GSP-SOM-reset_som_table_filters", {
    shinyjs::reset("som_table_filters_cont")
  })

  shinyjs::onclick("GSP-GERM-reset_germ_table_filters", {
    shinyjs::reset("germ_table_filters_cont")
  })

  shinyjs::onclick("MULTI-reset_multi_table_filters", {
    shinyjs::reset("multi_table_filters_cont")
  })

  # ------ HELP SERVER MODULE -------
  help_page_server("help_page")

  # ------ ABOUT SERVER MODULE -------
  about_server("about")

}

#enabling reactlog testing
#reactlog::reactlog_enable()

# Run the application
shiny::shinyApp(ui = ui, server = server)
