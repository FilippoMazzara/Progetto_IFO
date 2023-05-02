
#' useless tooltip
#' @description
#' function that activates tooltips
#' use once!
#' @examples tooltip_inutile()
#'
tooltip_inutile <- function() {
  shiny::fluidRow(
    shiny::tags$span(
      style = "display:none;",
      `data-toggle` = "tooltip",
      `data-placement` = "left",
      title = "A tooltip",
      shiny::icon("question-circle")
    )
  )
}

#' check vector type
#' @description
#' this function checks if it is needed
#' to invoke an observer for the matching input
#'
#' @param x the vector to check
#'
#' @return  TRUE if the condition checks NULL otherwise
#'
#' @examples check_ui(dataframecolumn)
#'
check_ui <- function(x) {
  if (is.numeric(x)) {

    rng <- range(x, na.rm = T)
    if (!(rng[1] == rng[2])) {T}
    else {NULL}
  }
  else if (is.factor(x)){ T
  }
  else if ( is.array(x) || is.character(x)) {

    #levs <- levels(factor(x, exclude = NULL))
    #if (length(levs) < 600 && length(levs) > 1){

    levs <- kit::funique(x)
    #remember that this condition is tied to make_ui/2
    if (length(levs) < (length(x)/8)){T}
    else {NULL}
  }
  else if (is.logical(x)) {
    if(all(is.na(x)) || all(is.null(x))) {NULL}
    else {T}
  }
  else {
    # Not supported
    NULL
  }
}

#' make UI of somatic file
#' @description
#' this function creates the inputs for the supplied vectors
#' @param x the vector
#' @param var the name of the vector
#' @param id the module's id where the inputs will be created
#'
#' @return the filter created according to the conditions in the function
#' @examples make_ui(vector,name,moduleid)
#'
make_ui <- function(x, var, id) {
  #NUMERIC VECTORS
  if (is.numeric(x)) {
    rng <- range(x, na.rm = T)
    if (!(rng[1] == rng[2])){
    shiny::sliderInput(shiny::NS(id,var), var, min = rng[1], max = rng[2], value = rng)
    }
    else {NULL}
  }
  #FACTORS
  else if (is.factor(x)) {
    levs <- levels(factor(x, exclude = NULL))
    shinyWidgets::pickerInput(shiny::NS(id,var), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)) )
  }
  #CARACTER VECTORS AND ARRAYS
  else if ( is.array(x) || is.character(x)) {
    ### --- TEST VARIOUS INPUTS HERE --- ###
    levs <- kit::funique(x)

    #per un select normalepuoi fare il truncate e fare un vettore con c(nomi,valori) per le select
    #FILTERING OF VECTORS
    if (length(levs) < (length(x)/8)){
    #if (length(levs) < 600 && length(levs) > 1){
      shinyWidgets::pickerInput(shiny::NS(id,var), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10, virtualScroll= TRUE ),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)) )
      #shiny::selectizeInput(var, var, choices = levs, selected = levs, multiple = TRUE)
    }
    else {NULL}
    #shiny::selectInput(var, var, choices = levs, selected = levs, multiple = TRUE, selectize = T)
  }
  #LOGICAL VECTORS
  else if (is.logical(x)) {
    if(all(is.na(x)) || all(is.null(x))) {NULL}
    else{
      levs <- levels(factor(x, exclude = NULL))
      shinyWidgets::pickerInput(shiny::NS(id,var), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)))
    }
  }
  else {
    # Not supported
    NULL
  }
}

#' make UI of germ file
#' @description
#' this function creates the inputs for the supplied vectors
#' @param x the vector
#' @param var the name of the vector
#' @param id the module's id where the inputs will be created
#'
#' @return the filter created according to the conditions in the function
#' @examples make_ui2(vector,name,moduleid)
#'
make_ui2 <- function(x, var, id){
  #to distinguish from the first
  var2 <- paste(var,"2",sep="")

  if (is.numeric(x)) {
    rng <- range(x, na.rm = T)
    shiny::sliderInput(shiny::NS(id,var2), var, min = rng[1], max = rng[2], value = rng)
  }
  else if (is.factor(x)) {
    levs <- levels(factor(x, exclude = NULL))
    shinyWidgets::pickerInput(shiny::NS(id,var2), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40))  )
  }
  else if ( is.array(x) || is.character(x)) {

    #levs <- levels(factor(x, exclude = NULL))

    levs <- kit::funique(x)
    if (length(levs) < (length(x)/8)){
      shinyWidgets::pickerInput(shiny::NS(id,var2), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40))  )
    }
    else {NULL}
  }
  else if (is.logical(x)) {
    if(all(is.na(x)) || all(is.null(x))) {NULL}
    else{
      levs <- levels(factor(x, exclude = NULL))
      shinyWidgets::pickerInput(shiny::NS(id,var2), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)))
    }
  }
  else {
    # Not supported
    NULL
  }
}


#' Filter function
#' @description
#' this function decide if the values in the table are to keep or not
#' @param x the vector to filter
#' @param val the values supplied from the inputs
#'
#' @return a logical vector mapping the values to remove from the tableù
#' @examples filter_var(vector,inputvalues)
#'
filter_var <- function(x, val) {
  if (is.numeric(x) ) {
    if (val[1]==val[2]) {TRUE}
    else {
      !is.na(x)& x >= val[1] & x <= val[2]
    }
  }
  else if (is.factor(x)){
    x %in% val
  }
  else if (is.character(x)) {
    #v <- levels(factor(x, exclude = NULL))
    as.factor(x) %in% val
    #x %in% val
  }
  else if (is.logical(x)) {T}
  else {
    # No control, so don't filter
    TRUE
  }
}

#' Create a toggle element
#' @description
#' used to create a toggle element in the ui to collapse well panels
#' @param id the button id
#' @param target the well panel id
#' @param text the text to display
#'
#' @return the toggle row
#' @examples toggle_panel(toggleid,targetid,text)
#'
toggle_panel <- function(id, target, text) {
  shiny::tags$div(
    class = "toggle_panel",
    shiny::tags$span(
      class = "toggle_panel_text",
      text
    ),
    shiny::tags$a(
      id = id,
      class = "toggle_well_link",
      `data-toggle` = "collapse",
      `data-target` = paste("#", target, sep = ""),
      shiny::tags$span(class = "toggle_panel_line")
    )
  )
}

#' Create a tooltip
#' @description
#' used to create a tooltip in the ui
#' @param txt the text to display in the tooltip popup
#' @return the tooltip
#' @examples a_tooltip("text to show")
#'
a_tooltip <- function(txt){
  shiny::fluidRow(
    shiny::tags$span(
      style="float:right; margin-right:5%; margin-top:2%;",
      `data-toggle` = "tooltip",
      `data-placement` = "left",
      title = txt,
      shiny::icon("question-circle")
    )
  )
}

