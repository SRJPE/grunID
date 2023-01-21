# Functions for connecting to the database

#' Azure Token Refresh
#' @description refresh auth token using az cli
az_refresh_token <- function() {
  res <- tryCatch(
    system2("az",
            args = c("account get-access-token --resource https://ossrdbms-aad.database.windows.net"),
            stdout = TRUE),
    error = function(e) {stop("could not find `az` please make sure you have Azure CLI installed", call. = FALSE)}
  )

  return(jsonlite::parse_json(res))
}


db_get_config <- function() {
  cfg <- tryCatch(config::get(),
                  error = function(e) {return(NULL)})

  cfg$password <- az_refresh_token()$accessToken

  return(cfg)
}

#' Connect to Run ID Database
#' @export
gr_db_connect <- function(config = db_get_config(), username = NULL,
                       host = NULL, port = NULL) {

  if (is.null(config)) {
    if (any(is.null(username), is.null(password), is.null(host), is.null(port))) {
      stop("could not find a config file, and one of username, password, host and port was left blank")
    } else {
      config$username <- username
      config$password <- password
      config$host <- host
      config$port <- port
      config$dbname <- "runiddb-prod"
    }
  }

  # at this point config has the creds

  DBI::dbConnect(RPostgres::Postgres(),
                 dbname = config$dbname,
                 host = config$host,
                 port = config$port,
                 user = config$username,
                 password = az_refresh_token()$accessToken)


}
