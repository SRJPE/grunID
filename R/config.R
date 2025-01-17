#' @title Create new config file
#' @export
create_config_file <- function(path=getwd()) {
  f <- system.file("base-config.yml", package = "grunID")
  content <- readr::read_lines(f)
  filename <- paste0(path, "/config.yml")
  readr::write_lines(content, file = filename)
  return(filename)
}

get_config_file <- function() {
  f <- config::get()
  attributes(f)$file
}

#' @title Set values in config file
#' @param key the key to update
#' @param value the value to update the key to
#' @param file config file, will use the file that config package finds by default
#' @export
add_to_config <- function(key, value, file = get_config_file()) {
  cfg <- yaml::yaml.load_file(file)
  cfg$default[[key]] <- value

  yaml::write_yaml(cfg, file)
}
