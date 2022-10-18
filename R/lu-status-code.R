#' Retrieve Status Codes
#' @description `get_status_codes()` returns all status codes within the database
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#' sample_types <- get_status_codes(con)
#' @family status code functions
#' @export
#' @md
get_status_codes <- function(con, is_active=TRUE, all_results=FALSE) {
  is_valid_connection(con)

  if(all_results) {
    status_codes <- dplyr::tbl(con, "status_code") |>
      dplyr::collect()
  } else {
    status_codes <- dplyr::tbl(con, "status_code") |>
      dplyr::filter(active == is_active) |>
      dplyr::collect()
  }

  return(status_codes)
}

#' Add Status Code
#' @description `add_status_code()` adds a new status code to the status code lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param status_code A valid status code dataframe containing the following:
#' * **status_code** Status code full name
#' * **description** Brief description of the status code
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#'
#' new_status_code <- data.frame(status_code = "All done",
#'                               description = "Everything is all done")
#' add_status_code(con, new_status_code)
#' @family status code functions
#' @export
#' @md
add_status_code <- function(con, status_code) {
  is_valid_connection(con)
  is_valid_status_code(status_code)

  DBI::dbAppendTable(con, "status_code", status_code)

}

#' Update Status Code
#' @description `update_status_code()` updates an existing status code in the status code lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param status_code_id A numeric ID for the targeted status code \code{\link{get_status_codes}}
#' @param status_code A valid status code dataframe containing the following:
#' * **status_code** Status code full name
#' * **description** Brief description of the status code
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#'
#' all_status_codes <- get_status_codes(con)
#' View(all_status_codes) # to view the ID of the status code needing updates
#'
#' updated_status_code <- all_status_codes[1, 2:3]
#' updated_status_code$status_code <- "New all done"
#' update_status_code(con, 1, updated_status_code)
#' @family status code functions
#' @export
#' @md
update_status_code <- function(con, status_code_id, status_code) {
  is_valid_connection(con)
  is_valid_status_code(status_code)

  query <- glue::glue_sql("UPDATE status_code
                           SET status_code = {status_code$status_code},
                               description = {status_code$description}
                           WHERE id = {status_code_id}
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Update Status Code Status
#' @description `update_status_code_status()` changes active flag on existing status code in the status code lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param status_code_id A numeric ID for the targeted status code \code{\link{get_status_codes}}
#' @param set_active A boolean, TRUE for activating and FALSE for deactivating.
#' When a record is active, it is returned by default when \code{\link{get_status_codes}}
#' is called. This helps preserve look up values that are valid in historic
#' contexts, but are no longer valid for current data records.
#' @export
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#'
#' all_status_codes <- get_status_codes(con)
#' View(all_status_codes) # to view the ID of the status code needing status change
#'
#' #deactivate
#' update_status_code_status(con, 4, set_active=FALSE)
#' #reactivate
#' update_status_code_status(con, 4)
#' @family status code functions
#' @export
#' @md
update_status_code_status <- function(con, status_code_id, set_active=TRUE) {
  is_valid_connection(con)

  query <- glue::glue_sql("UPDATE status_code
                           SET active = {set_active}
                           WHERE id = {status_code_id}
                           RETURNING id, status_code, active, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Delete Status Code
#' @description `delete_status_code()` deletes an existing status code in the status code lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param status_code_id A numeric ID for the targeted status code \code{\link{get_status_codes}}
#' **Note:** If a status code has been associated with a sample, then
#' the database restricts deleting this status code. You must first update those records
#' with a new status code before reattempting to delete the status code. Consider
#' using the \code{\link{update_status_code_status}} function if you are wanting to
#' retire an status code while retaining its value for historic records.
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#'
#' all_status_codes <- get_status_codes(con)
#' View(all_status_codes) # to view the ID of the status code needing deletion
#'
#' delete_status_code(con, 1)
#' @family status code functions
#' @export
#' @md
delete_status_code <- function(con, status_code_id) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM status_code where id = {status_code_id};",
                          .con = con)

  tryCatch(DBI::dbExecute(con, query),
           error = function(e) {
             if (grepl("violates foreign key constraint", e)) {
               stop("This status name is in use. Before deleting this status name,
                    please update existing samples to a new status name. Also consider
                    deactivating this status name instead with update_status_name_status().", call. = FALSE)
             } else {
               stop(e)
             }
           })

}

is_valid_status_code <- function(status_code) {

  if (!is.data.frame(status_code)) {
    stop("Please provide status code as a dataframe", call. = FALSE)
  }

  column_reference <- c("status_code" = "character","description" = "character")
  if (!identical(sapply(status_code, class), column_reference)) {
    stop('The status code supplied is not valid, see `help("add_sample_type")` for correct format', call. = FALSE)
  }

}

