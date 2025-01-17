#' @title Launch App
#' @description `run_app()` launches a shiny application that allows the user to
#' upload data to the run ID database.
#' @details the grunID application can be run when the user has a completed plate run
#' with assay results. This application calls `add_new_plate_results()`.
#' @examples
#' \dontrun{
#' grunID::run_app()
#' }
#' @md
#' @export
run_app <- function(config_path = NULL, ...) {
  if (is.null(config_path)) {
    config_exists <- tryCatch({
      config_path <- attr(config::get(), "file")
      TRUE
    }, error = function(x) FALSE)

    if (!config_exists) {
      # Ask user if they want to create a config file
      cli::cli_text("No config file found. Would you like to create one? (yes/no): ")
      create_new <- readline()

      if (tolower(create_new) %in% c("y", "yes")) {
        # Ask for the path
        default_path <- getwd()
        cli::cli_text("Enter path for new config file [{default_path}/config.yml]: ")
        new_path <- readline()

        # If user just hits enter, use default path
        if (new_path == "") new_path <- default_path

        # Create the config file
        new_path <- create_config_file(new_path)
        cli::cat_rule("Config file")
        cli::cli_alert_success("Config file created at {new_path}, please fill out the contents before proceeding")
        cli::cli_alert_info("re-run app once config file has been filled in")
        cli::cat_rule()
        return()


      } else {
        cli::cli_abort("No config file available. Please use `create_config_file()` to create one.")
      }
    }
  }

  Sys.setenv("CONFIG_PATH" = config_path)
  shiny::runApp(appDir = system.file("app", package = "grunID"),
                ...)
}
