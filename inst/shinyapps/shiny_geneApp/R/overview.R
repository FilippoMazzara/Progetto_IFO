
homepanel_ui<-function(id){
  ns <- shiny::NS(id) #id del modulo
  shiny::tabPanel(
    title = "Graph",
    value = shiny::NS(id, "graph"),
    #declare sidebars layout
    shiny::sidebarLayout(
      #side menu start
      #this menu could be floating or shared across navbar options or unique to this
      shiny::sidebarPanel(
        #inputs panel
        #option to hide the panel,hide only one of the inputs ,only allow from server,
        #manage the inputs from file in another location, or by making user choose in advance
        shiny::wellPanel(
          #input from filesystem
          shiny::fileInput(
            ns("fromfile"),
            label = htmltools::HTML(
              'Carica un file .tsv
              </label><span data-toggle="tooltip" style="float:right" data-placement="right" title="" data-original-title="A tooltip">
              <i class="far fa-circle-question" role="presentation" aria-label="circle-question icon"></i></span>'),
            accept = ".tsv"),
            #bottone per disabilitare il tasto enter fuori da i widget
            #una pecionata pero funziona (attento a possibili errori)
            #non lo puoi mettere statico senno non fa il render finche non lo premi
            shiny::uiOutput(ns("inutile")),
            #input from DB/server
            #guarda le opzioni di selectize
            shiny::selectInput(
              ns("fromserver"),
              "Seleziona il campione memorizzato",
              choices = c("paziente1", "paziente2", "paziente3"),
              selectize = TRUE,
              selected = "paziente...")
        ),
            #anche questo panello puo essere reso interattivo e dinamico,compare diverso e compare solo per una certa condizione
            shiny::wellPanel(
            shiny::sliderInput(
              ns("bins"),
              "Number of bins:",
              min = 1,
              max = 50,
              value = 30)
             )
      ),
      # Show a plot of the generated distribution, main panel
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "pannello1",
            shiny::fluidRow(
              shiny::tags$span(
                style = "float:right;margin-right:5%;margin-top:2%;",
                `data-toggle` = "tooltip",
                `data-placement` = "left",
                title = "A tooltip",
                shiny::icon("question-circle")
              ),
              shiny::plotOutput(ns("distPlot")) #posso usare un div con width x
            )
          ),
          shiny::tabPanel("pannello2")
        )
      )
    #END LAYOUT
    )
  #END TAB
  )
}

homepanel_server <- function(id) {
  shiny::moduleServer(id,
    function(input, output, session) {
      #bottone inutile
      output$inutile <- shiny::renderUI(
        htmltools::HTML( '<button type="submit" disabled style="display: none" aria-hidden="true"></button>'))
      #grafico
      output$distPlot <- shiny::renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        hist(
          x,
          breaks = bins,
          col = 'darkgray',
          border = 'white',
          xlab = 'Waiting time to next eruption (in mins)',
          main = 'Histogram of waiting times'
        )
      })
  })
}
