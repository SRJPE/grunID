# Functions for connecting to the database
library(jsonlite)

az_refresh_token <- function() {
  res <- tryCatch({
    cli::cli_process_start("refreshing Azure auth token")
    system2("az",
            args = c("account get-access-token --resource https://ossrdbms-aad.database.windows.net"),
            stdout = TRUE)
  },
  error = function(e) {stop("could not find `az` please make sure you have Azure CLI installed", call. = FALSE)}
  )

  cli::cli_process_done("done")
  return(jsonlite::parse_json(res))
}

#' @title Get access token for container
#' @export
az_container_token <- function(storage_account) {
  res <- tryCatch({
    cli::cli_process_start("refreshing Azure Storage token")
    system2("az",
            args = c("storage", "account", "keys", "list",
                     "--account-name", storage_account,
                     "--query", "[0].value",
                     "--output", "tsv"),
            stdout = TRUE)
  },
  error = function(e) {stop("could not find `az` please make sure you have Azure CLI installed", call. = FALSE)}
  )
  cli::cli_process_done("done")
  return(res)
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
#' @details Azure accesstoken will be used for password authentication, users must have the
#' [Azure CLI](https://learn.microsoft.com/en-us/cli/azure/) tool installed. To verify try running
#' `az --version` and confirm a version number and additional information is printed to the screen.
#' @examples
#' \dontrun{
#'
#' # run with config file, file will be searched up directories starting from the working directory
#' con <- gr_db_connect()
#'
#' # run with username, dbname, and host passed in as arguemnts
#' con <- gr_db_connect(
#'                      username = "myusername",
#'                      dbname = "dbname",
#'                      host = "host.com"
#' )
#' dplyr::tbl(con, "agency")
#' }
#' @return a "PqConnection" object to be used in queries to database
#' @md
#' @export
gr_db_connect <- function(username = NULL, host = NULL, dbname = NULL) {

  # no username, host or dbname passed, try to read config file
  if (all(is.null(username), is.null(host), is.null(dbname))) {

    config <- db_get_config()

    if (is.null(config)) {
      stop("Could not find a config file within your working directory", call. = FALSE)
    }
  } else {
    config <- list()
    config$username <- username
    config$host <- host
    config$dbname <- dbname
  }

  auth_token <- az_refresh_token()

  # at this point config has the creds
  DBI::dbConnect(RPostgres::Postgres(),
                 dbname = config$dbname,
                 host = config$host,
                 port = 5432,
                 user = config$username,
                 password = auth_token$accessToken)


}


#' @title Create Connection to AZ Container
#' @export
az_container_connect <- function(account_name, container_name) {
  store_access_key <- grunID::az_container_token(account_name)
  store <- AzureStor::storage_endpoint("https://geneticsedidata.blob.core.windows.net", store_access_key)
  container <- AzureStor::storage_container(store, container_name)
  return(container)
}

