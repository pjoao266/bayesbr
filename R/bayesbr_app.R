#'@title The bayesbr Application
#'@name bayesbr_app
#'@aliases bayesbr_app, bayesbr_shiny
#'@description A function that runs the shiny application designed for using bayesbr package functions through a visual interface.
#'@usage bayesbr_app()
#'@details See the application manual: How to use bayesbr shiny app in vignnetes of bayesbr package.
#'@seealso \code{\link{bayesbr}}, \code{\link{shiny}}
#'@export
bayesbr_app <- function() {
  appDir <- system.file("shiny", "shiny_bayesbr", package = "bayesbr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `bayesbr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
