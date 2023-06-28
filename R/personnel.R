
if (interactive()) {

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = 5432,
                      user = cfg$username,
                      password = cfg$password)
}

#' Get all personnel
#' @param con DBI connection object to database
#' @param collected boolean indidcated whether the returned object should be collected from database
#' or returned as a lazy postgres dataframe.
#' @param ... additional named arguments passed into dplyr::filter,
#' use this add filter to be executed on the database before collecting.
#' @export
get_personnel <- function(con, collected=TRUE, ...) {
  d <- dplyr::tbl(con, "lab_personnel") |>
    dplyr::filter(...)
  if (collected) {
    return(dplyr::collect(d))
  }
  else {
    return(d)
  }
}

