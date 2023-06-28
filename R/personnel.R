
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
#' or returned as a postgres lazy_query.
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

#' Add personnel to table
#' @param con connection to database
#' @param first_name first name for personnel
#' @param last_name last name for personnel
#' @param laboratory_id the id for lab personnel is associated with
#' @export
add_personnel <- function(con, first_name, last_name, laboratory_id) {
  d <- data.frame(first_name = first_name, last_name = last_name, laboratory_id = laboratory_id)
  res <- DBI::dbAppendTable(con, "lab_personnel", d)
  return(res)
}

#' Remove personnel
#' @param con connection to the database
#' @param id id for record to remove
#' @details
#' Records are not actually deleted via this API, instead they are just set to inactive.
#' @export
delete_personnel <- function(con, id) {
  q <- glue::glue_sql(
    "UPDATE lab_personnel SET active = false WHERE id = {id};",
    .con = con
  )

  res <- DBI::dbExecute(con, q)
  return(res)
}


