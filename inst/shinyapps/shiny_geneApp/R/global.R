
#' tooltip_inutile
#' @description
#' funzione per abilitare i tooltip
#' @examples tooltip_inutile()
tooltip_inutile <-function() {
  shiny::fluidRow(
    shiny::tags$span(
      style="display:none;",
      `data-toggle` = "tooltip",
      `data-placement` = "left",
      title = "A tooltip",
      shiny::icon("question-circle")
    )
  )
}
