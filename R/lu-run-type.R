#' Retrieve Run Types
#' @description `get_run_types()` returns all run types within the database
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
#' run_types <- get_run_types(con)
#' @family run type functions
#' @export
#' @md
get_run_types <- function(con, is_active=TRUE, all_results=FALSE) {
  is_valid_connection(con)

  if(all_results) {
    run_types <- dplyr::tbl(con, "run_type") |>
      dplyr::collect()
  } else {
    run_types <- dplyr::tbl(con, "run_type") |>
      dplyr::filter(active == is_active) |>
      dplyr::collect()
  }

  return(run_types)
}

#' Add Run Type
#' @description `add_run_type()` adds a new run type to the run type lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param run type A valid run type dataframe containing the following:
#' * **code** Short code representing run type name
#' * **run_name** Run type full name
#' * **description** Brief description of the run type
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
#' new_run_type <- data.frame(code = "SWR",
#'                            run_name = "Spring Winter Run",
#'                            description = "A pretend run")
#' add_run_type(con, new_run_type)
#' @family run type functions
#' @export
#' @md
add_run_type <- function(con, run_type) {
  is_valid_connection(con)
  is_valid_run_type(run_type)

  DBI::dbAppendTable(con, "run_type", run_type)

}

#' Update Run Type
#' @description `update_run_type()` updates an existing run type in the run type lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param run_type_id A numeric ID for the targeted run type \code{\link{get_run_types}}
#' @param run_type A valid run type dataframe containing the following:
#' * **code** Short code representing run type name
#' * **run_name** Run type full name
#' * **description** Brief description of the run type
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
#' all_run_types <- get_run_types(con)
#' View(all_run_types) # to view the ID of the run type needing updates
#'
#' updated_run_type <- all_run_types[1, 2:4]
#' updated_run_type$run_name <- "New Name Run"
#' update_run_type(con, 1, updated_run_type)
#' @family run type functions
#' @export
#' @md
update_run_type <- function(con, run_type_id, run_type) {
  is_valid_connection(con)
  is_valid_run_type(run_type)

  query <- glue::glue_sql("UPDATE run_type
                           SET code = {run_type$code},
                               run_name = {run_type$run_name},
                               description = {run_type$description}
                           WHERE id = {run_type_id}
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Update Run Type Status
#' @description `update_run_type_status()` changes active flag on existing run type in the run type lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param run_type_id A numeric ID for the targeted run type \code{\link{get_agencies}}
#' @param set_active A boolean, TRUE for activating and FALSE for deactivating.
#' When a record is active, it is returned by default when \code{\link{get_agencies}}
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
#' all_run_types <- get_run_types(con)
#' View(all_run_types) # to view the ID of the run type needing status change
#'
#' #deactivate
#' update_run_type_status(con, 4, set_active=FALSE)
#' #reactivate
#' update_run_type_status(con, 4)
#' @family run type functions
#' @export
#' @md
update_run_type_status <- function(con, run_type_id, set_active=TRUE) {
  is_valid_connection(con)

  query <- glue::glue_sql("UPDATE run_type
                           SET active = {set_active}
                           WHERE id = {run_type_id}
                           RETURNING id, code, active, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Delete Run Type
#' @description `delete_run_type()` deletes an existing run type in the run type lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param run_type_id A numeric ID for the targeted run type \code{\link{get_run_types}}
#' **Note:** If an run type has been associated with a sample, then
#' the database restricts deleting this run type. You must first update those records
#' with a new run type before reattempting to delete the run type. Consider
#' using the \code{\link{update_run_type_status}} function if you are wanting to
#' retire an run type while retaining its value for historic records.
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
#' all_run_types <- get_run_types(con)
#' View(all_run_types) # to view the ID of the run type needing deletion
#'
#' delete_run_type(con, 1)
#' @family run type functions
#' @export
#' @md
delete_run_type <- function(con, run_type_id) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM run_type where id = {run_type_id};",
                          .con = con)

  result <- DBI::dbExecute(con, query)

  return(result)
}

is_valid_run_type <- function(run_type) {

  if (!is.data.frame(run_type)) {
    stop("Please provide run type as a dataframe", call. = FALSE)
  }

  column_reference <- c("code" = "character", "run_name" = "character",
                        "description" = "character")
  if (!identical(sapply(run_type, class), column_reference)) {
    stop('The run type supplied is not valid, see `help("add_run_type")` for correct format', call. = FALSE)
  }

}

