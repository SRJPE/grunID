#' Retrieve Sample Types
#' @description `get_sample_types()` returns all sample types within the database
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
#' sample_types <- get_sample_types(con)
#' @family sample type functions
#' @export
#' @md
get_sample_types <- function(con, is_active=TRUE, all_results=FALSE) {
  is_valid_connection(con)

  if(all_results) {
    sample_types <- dplyr::tbl(con, "sample_type") |>
      dplyr::collect()
  } else {
    sample_types <- dplyr::tbl(con, "sample_type") |>
      dplyr::filter(active == is_active) |>
      dplyr::collect()
  }

  return(sample_types)
}

#' Add Sample Type
#' @description `add_sample_type()` adds a new sample type to the sample type lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param sample_type A valid sample type dataframe containing the following:
#' * **code** Short code representing sample type name
#' * **sample_type_name** Sample type full name
#' * **description** Brief description of the sample type
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
#' new_sample_type <- data.frame(code = "SFB",
#'                               sample_type_name = "Some Fish Body",
#'                               description = "Took a bit of a fish")
#' add_sample_type(con, new_sample_type)
#' @family sample type functions
#' @export
#' @md
add_sample_type <- function(con, sample_type) {
  is_valid_connection(con)
  is_valid_sample_type(sample_type)

  DBI::dbAppendTable(con, "sample_type", sample_type)

}

#' Update Sample Type
#' @description `update_sample_type()` updates an existing sample type in the sample type lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param sample_type_id A numeric ID for the targeted sample type \code{\link{get_sample_types}}
#' @param sample_type A valid sample type dataframe containing the following:
#' * **code** Short code representing sample type name
#' * **sample_type_name** Sample type full name
#' * **description** Brief description of the sample type
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
#' all_sample_types <- get_sample_types(con)
#' View(all_sample_types) # to view the ID of the sample type needing updates
#'
#' updated_sample_type <- all_sample_types[1, 2:4]
#' updated_sample_type$sample_type_name <- "New Name Run"
#' update_sample_type(con, 1, updated_sample_type)
#' @family sample type functions
#' @export
#' @md
update_sample_type <- function(con, sample_type_id, sample_type) {
  is_valid_connection(con)
  is_valid_sample_type(sample_type)

  query <- glue::glue_sql("UPDATE sample_type
                           SET code = {sample_type$code},
                               sample_type_name = {sample_type$sample_type_name},
                               description = {sample_type$description}
                           WHERE id = {sample_type_id}
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Update Sample Type Status
#' @description `update_sample_type_status()` changes active flag on existing sample type in the sample type lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param sample_type_id A numeric ID for the targeted sample type \code{\link{get_sample_types}}
#' @param set_active A boolean, TRUE for activating and FALSE for deactivating.
#' When a record is active, it is returned by default when \code{\link{get_sample_types}}
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
#' all_sample_types <- get_sample_types(con)
#' View(all_sample_types) # to view the ID of the sample type needing status change
#'
#' #deactivate
#' update_sample_type_status(con, 4, set_active=FALSE)
#' #reactivate
#' update_sample_type_status(con, 4)
#' @family sample type functions
#' @export
#' @md
update_sample_type_status <- function(con, sample_type_id, set_active=TRUE) {
  is_valid_connection(con)

  query <- glue::glue_sql("UPDATE sample_type
                           SET active = {set_active}
                           WHERE id = {sample_type_id}
                           RETURNING id, code, active, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Delete Sample Type
#' @description `delete_sample_type()` deletes an existing sample type in the sample type lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param sample_type_id A numeric ID for the targeted sample type \code{\link{get_sample_types}}
#' **Note:** If an sample type has been associated with a sample, then
#' the database restricts deleting this sample type. You must first update those records
#' with a new sample type before reattempting to delete the sample type. Consider
#' using the \code{\link{update_sample_type_status}} function if you are wanting to
#' retire an sample type while retaining its value for historic records.
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
#' all_sample_types <- get_sample_types(con)
#' View(all_sample_types) # to view the ID of the sample type needing deletion
#'
#' delete_sample_type(con, 1)
#' @family sample type functions
#' @export
#' @md
delete_sample_type <- function(con, sample_type_id) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM sample_type where id = {sample_type_id};",
                          .con = con)

  result <- DBI::dbExecute(con, query)

  return(result)
}

is_valid_sample_type <- function(sample_type) {

  if (!is.data.frame(sample_type)) {
    stop("Please provide sample type as a dataframe", call. = FALSE)
  }

  column_reference <- c("code" = "character", "sample_type_name" = "character",
                        "description" = "character")
  if (!identical(sapply(sample_type, class), column_reference)) {
    stop('The sample type supplied is not valid, see `help("add_sample_type")` for correct format', call. = FALSE)
  }

}

