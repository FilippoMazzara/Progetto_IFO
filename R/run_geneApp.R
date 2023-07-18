#' geneApp launcher
#' @description
#' This is the launcher function to the geneApp package
#' @return The running application
#' @export run_geneApp
#' @importFrom magrittr "%>%"
#' @examples run_geneApp
#' The only way to first start the app
#'
run_geneApp <- function(){
  appDir <- base::system.file("inst", "shinyapps", "shiny_geneApp", package = "geneApp")
  if (appDir == "") {
    stop("Could not find shiny app. Try re-installing `geneApp`.", call. = FALSE)
  }
  else{
    devtools::load_all(".")
    shiny::runApp(appDir, host = '0.0.0.0', port = 3838)
  }

}

