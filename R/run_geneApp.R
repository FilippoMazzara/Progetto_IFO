#' geneApp launcher
#' @description
#' This is the launcher function to the geneApp package
#' @return The running application
#' @export run_geneApp
#' @importFrom magrittr "%>%"
#' @examples run_geneApp()
#' The only way to first start the app
#'
run_geneApp <- function(){
  appDir <- system.file("shinyapps","shiny_geneApp", package = "geneApp")
  if (appDir == "") {
    stop("Could not find shiny app. Try re-installing `geneApp`.", call. = FALSE)
  }
  #aggiungere port, display.mode, quiet, host
  shiny::runApp(appDir)
}

