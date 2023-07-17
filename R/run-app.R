#' @title Launch App
#' @description `run_app()` launches a shiny application that allows the user to
#' upload data to the run ID database.
#' @details the grunID application can be run when the user has a completed plate run
#' with assay results. This application calls `add_new_plate_results()`.
#' @examples
#' grunID::run_app()
#' @md
#' @export
run_app <- function(x, config_path,...)
{
  Sys.setenv("CONFIG_PATH" = config_path)
  shiny::runApp(appDir = system.file("app", package = "grunID"),
                ...)
}


