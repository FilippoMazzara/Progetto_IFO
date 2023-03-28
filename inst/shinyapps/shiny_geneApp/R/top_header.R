top_header <- function() {

  shiny::tags$div(
    id = "pageTop",
    class = "row",
    shiny::tags$div(id = "topDiv",
      shiny::tags$header(
        id = "topHeader",
        shiny::tags$div(
          style = "display: flex; align-items: center;",
          #title
          shiny::tags$a(
            class = "linkTitolo",
            href = "/",
            shiny::tags$h2("ISTITUTI FISIOTERAPICI OSPITALIERI",
              class = "titolo"
              )
            ),
          #home
          shiny::tags$a(
            class = "linkNav",
            href = "/",
            shiny::tags$h4("Home",
              class = "titoloNav"
              )
            ),
          #about
          shiny::tags$a(
            class = "linkNav",
            id="b1",
            shiny::tags$h4("About",id="b1",
              class = "titoloNav"
            )
          )
          #end div
        )
      )
    )
  )
}
