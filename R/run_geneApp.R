#' geneApp launcher
#' @description
#' This is the launcher function to the geneApp package, The only way to first start the app
#' @return The running application
#' @export run_geneApp
#' @importFrom magrittr "%>%"
run_geneApp <- function(){
  appDir <- base::system.file("shinyapps", "shiny_geneApp", package = "geneApp")
  if (appDir == "") {
    stop("Could not find shiny app. Try re-installing `geneApp`.", call. = FALSE)
  }
  else{
    shiny::runApp(appDir, host = '0.0.0.0', port = 3838)
  }

}


