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
  return(cfg)
}

#' Connect to Run ID Database
#' @description create a connection object to the Run ID Database. By default function will
#' look for a config file using the `config::get()` function. If no file is found user provided
#' username, host, and port will be used. In all cases the password will be obtained using an Azure
#' token generated using the azure cli tool.
#' @param username (optional) username for login
#' @param host (optional) host for login
#' @param port (optional) port for login
#' @details Azure accesstoken will be used for password authentication, users must have the
#' [Azure CLI](https://learn.microsoft.com/en-us/cli/azure/) tool installed. To verify try running
#' `az --version` and confirm a version number and additional information is printed to the screen.
#' @return a "PqConnection" object to be used in queries to database
#' @md
#' @export
gr_db_connect <- function(username = NULL, host = NULL, port = NULL) {

  config <- db_get_config()

  if (is.null(config)) {
    if (any(is.null(username), is.null(password), is.null(host), is.null(port))) {
      stop("could not find a config file, and one of username, password, host and port was left blank")
    } else {
      config$username <- username
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
