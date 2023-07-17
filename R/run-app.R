#' Launch
#' @export
run_app <- function(x, config_path,...)
{
  Sys.setenv("CONFIG_PATH" = config_path)
  shiny::runApp(appDir = system.file("app", package = "grunID"),
                ...)
}
