#' Launch
#' @export
run_app <- function(x, ...)
{
  shiny::runApp(appDir = system.file("app", package = "grunID"),
                ...)
}
