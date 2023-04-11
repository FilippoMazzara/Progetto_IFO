
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

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    shiny::sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    shiny::selectInput(var, var, choices = levs, selected = levs, multiple = TRUE )
  }
  else if (is.vector(x) || is.array(x)) {
    levs <- levels(as.factor(x))
    shiny::selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  }
  else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    if (val[1]==val[2]) {TRUE}
    else {
      !is.na(x) & x >= val[1] & x <= val[2]}

  } else if (is.factor(x) || is.vector(x) || is.array(x) || is.character(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}
