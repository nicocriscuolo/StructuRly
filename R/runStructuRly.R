#' @title Launches the web shiny app StructuRly
#'
#' @description Simple function to connect UI and Server of the shiny app and
#'   launch it inside the default browser.
#'
#' @param host Optional parameter to specify where to execute the application,
#'   default is http://127.0.0.1:4753.
#'
#' @import shiny
#'
#' @return The function loads the web application StructuRly in your default browser
#'   through the \code{\link{shinyApp}} function, which requires a script with the
#'   User Interface of the software and a script of the Server.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   runStructuRly()
#' }

runStructuRly <- function() {

  app_Directory <- system.file("App", package = "StructuRly")

  if (app_Directory == "") {

    stop("Could not find the application directory. Try re-installing `StructuRly`.", call. = FALSE)

  }

  shiny::runApp(app_Directory, display.mode = "normal")

}
