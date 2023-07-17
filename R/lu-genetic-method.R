#' Retrieve Genetic Methods
#' @description `get_genetic_methods()` returns all genetic methods within the database
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
#' protocols <- get_genetic_methods(con)
#' @family genetic method functions
#' @export
#' @md
get_genetic_methods <- function(con, ...) {
  is_valid_connection(con)

  genetic_methods <- dplyr::tbl(con, "genetic_method") |>
    dplyr::filter(...) |>
    dplyr::collect()
  return(genetic_methods)

}

#' Add Genetic Method
#' @description `add_genetic_method()` adds a new genetic method type to the genetic method lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param genetic_method A valid genetic method dataframe with the following:
#'
#' 1. **code** *character* 4 letter code in all caps
#' 2. **method_name** *character* Short name for method
#' 3. **description** *character* Brief description of method
#'
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
#'
#' new_method <- data.frame(code = "FAKE", method_name = "Fake Method",
#'                          description = "A fake method created for this example")
#' add_genetic_method(con, new_method)
#' @family genetic method functions
#' @export
#' @md
add_genetic_method <- function(con, genetic_method) {
  is_valid_connection(con)
  is_valid_genetic_method(genetic_method)

  tryCatch(DBI::dbAppendTable(con, "genetic_method", genetic_method),
           error = function(e) {
             if (grepl("duplicate key value violates unique constraint", e)) {
               stop("This genetic method already exists in the database", call. = FALSE)
             } else {
               stop(e)
             }
           })

}

#' Update Genetic Method
#' @description `update_genetic_method()` updates an existing genetic method in the genetic method lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param genetic_method_id A numeric ID for the targeted genetic method \code{\link{get_genetic_methods}}
#' @param genetic_method A valid genetic method dataframe with the following:
#'
#' 1. **code** *character* 4 letter code in all caps
#' 2. **method_name** *character* Short name for method
#' 3. **description** *character* Brief description of method
#'
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
#' all_methods <- get_genetic_methods(con)
#' View(all_methods) # to view the ID of the genetic method needing updates
#'
#' updated_method <- data.frame(code = "SHLK", method_name = "Sherlock",
#'                          description = "Changed the description")
#' update_genetic_method(con, genetic_method_id = 1, updated_method)
#' @family genetic method functions
#' @export
#' @md
update_genetic_method <- function(con, genetic_method_id, genetic_method) {
  is_valid_connection(con)
  is_valid_genetic_method(genetic_method)

  query <- glue::glue_sql("UPDATE genetic_method
                           SET code = {genetic_method$code},
                               method_name = {genetic_method$method_name},
                               description = {genetic_method$description},
                           WHERE id = {genetic_method_id}
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Change Agency Status
#' @description `update_genetic_method_status()` changes active flag on existing genetic method in the genetic method lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param genetic_method_id A numeric ID for the targeted genetic method \code{\link{get_agencies}}
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
#' all_agencies <- get_agencies(con)
#' View(all_agencies) # to view the ID of the genetic method needing status change
#'
#' #deactivate
#' update_genetic_method_status(con, 4, set_active=FALSE)
#' #reactivate
#' update_genetic_method_status(con, 4)
#' @family genetic method functions
#' @export
#' @md
update_genetic_method_status <- function(con, genetic_method_id, set_active=TRUE) {
  is_valid_connection(con)

  query <- glue::glue_sql("UPDATE genetic_method
                           SET active = {set_active}
                           WHERE id = {genetic_method_id}
                           RETURNING id, code, active, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}


#' Delete Genetic Method
#' @description `delete_genetic_method()` deletes an existing genetic method in the genetic method lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param genetic_method_id A numeric ID for the targeted genetic method \code{\link{get_genetic_methods}}
#' **Note:** If an genetic method type has been associated with an assay result record, then
#' the database restricts deleting this genetic method type. You must first update those records
#' with a new genetic method type before reattempting to delete the genetic method type. Consider
#' using the \code{\link{update_genetic_method_status}} function if you are wanting to
#' retire an genetic method type while retaining its value for historic records.
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
#' all_methods <- get_genetic_methods(con)
#' View(all_methods) # to view the ID of the genetic method needing updates
#'
#' delete_genetic_method(con, 1)
#' @family genetic method functions
#' @export
#' @md
delete_genetic_method <- function(con, genetic_method_id) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM genetic_method where id = {genetic_method_id};",
                          .con = con)

  result <- DBI::dbExecute(con, query)

  return(result)
}

is_valid_genetic_method <- function(genetic_method) {

  if (!is.data.frame(genetic_method)) {
    stop("Please provide genetic method as a dataframe", call. = FALSE)
  }

  column_reference <- c("code" = "character", "method_name" = "character", "description" = "character")
  if (!identical(sapply(genetic_method, class), column_reference)) {
    stop('The genetic method supplied is not valid, see `help("add_genetic_method")` for correct format', call. = FALSE)
  }


}

