#' @title Launches the web shiny App "StructureIt"
#'
#' @description Simple function to connect UI and Server of the shiny App and
#'   launch it inside the default browser.
#'
#' @param host Optional parameter where to execute the application, default is 127.0.0.1.
#'
#' @param ui R script containing the code of the user interface of the shiny app.
#'
#' @param server R script containing the code of the server of the shiny app.
#'
#' @import shiny
#'
#' @return The function loads the web application StructureIt in your default browser
#' through the \code{\link{shinyApp}} function, which requires a script with the
#' User Interface of the software and a script of the Server.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' runStructureIt()
#' }

runStructureIt <- function() {

  app_Directory <- system.file("App", package = "StructureIt")

  if (app_Directory == "") {

    stop("Could not find the application directory. Try re-installing `StructureIt`.", call. = FALSE)

  }

  shiny::runApp(app_Directory, display.mode = "normal")

}
