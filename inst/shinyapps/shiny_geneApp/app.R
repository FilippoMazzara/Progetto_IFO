ui <- shiny::fluidPage(
  #reactlog::reactlog_enable(),
  id = "mainpage",
  #Styling
  shinyjs::useShinyjs(),
  shiny::includeCSS("www/style.css"),
  shiny::includeScript("www/myscript.js"),
  # title header (hidden)
  shiny::tags$div(
    style = "display: none",
    shiny::titlePanel(title = "", windowTitle = "geneApp")
  ),
  #top header
  shiny::navbarPage(
    windowTitle = "HOME",
    position = "static-top",
    header = NULL,
    footer = NULL,
    collapsible = TRUE,
    id = "topnavbar",
    fluid = TRUE,
    #logo
    title = shiny::tags$div(
      shiny::tags$a(
        href = "/",
        shiny::tags$img(
          id = "logo1",
          src = "img/logo.jpg"
        )
      )
    ),
    #### HOME ####
    homePanel_ui("homepage"),
    #### OVERVIEW ####
    mainGenePanel_ui("overview"),
    #### ABOUT ####
    about_ui("about")
  #END UI
  ),
  shiny::tags$br(),
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
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  #modulo home
  homePanel_server("homepage")
  #modulo overview
  ch <- shiny::reactive({
    if(is.null(input$checkbox1)){return(NULL)}
    else{return(input$checkbox1)}
    })
  ch2 <- shiny::reactive({
    if(is.null(input$checkbox2)){return(NULL)}
    else{return(input$checkbox2)}
  })

  selected <- shiny::reactive({
    if (!is.null(ch())){
      each_input = list()
      for(c in ch()){
      each_input[[c]] <-input[[c]]}
      return(each_input)
      } else {return(NULL)}
  })


  mainGenePanel_server("overview",ch2=ch2,ch = ch, selected = selected)
  #link



  shinyjs::onclick(id= "sidebar_somatic", {
    shinyjs::runjs('document.querySelector("#file1_sidebar").style.display = "initial";
                   document.querySelector("#sidebar_somatic").className += " active";
                       document.querySelector("#file2_sidebar").style.display = "none";

                   document.querySelector("#sidebar_germ").className= "btn btn-default action-button shiny-bound-input";
                   '
    )
  })
  shinyjs::onclick(id = "sidebar_germ", {
    shinyjs::runjs('document.querySelector("#file1_sidebar").style.display = "none";
                       document.querySelector("#file2_sidebar").style.display = "initial";
                   document.querySelector("#sidebar_germ").className += " active";
                   document.querySelector("#sidebar_somatic").className = "btn btn-default action-button shiny-bound-input";
                   '

    )

  })



  shiny::observeEvent(
    input$linkapp,
    {shiny::updateNavbarPage(session, "topnavbar", "overview-gersom")}
  )
  shinyjs::onclick(id= "toggleSidebar", {
    shinyjs::runjs('var x = document.querySelector("#mainpage > div > div.tab-content > div > nav > div > div");
                    if (x.style.width == "93px") {x.style.width="24.5%";} else {x.style.width="93px";}
                    ')
    shinyjs::toggle(
      id = "sidebar",
      anim = TRUE,
      animType = "fade",
      time = 0.1)
  })

  #modulo about
  about_server("about")
  #observe({
 #   req(input$dataset_files)
  #})




  # shinyFiles::shinyFileChoose(input = input, id = "dataset_files", session = session, roots=c(wd = "C:/Users/facke/Desktop/datasets"), defaultPath="/")
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
