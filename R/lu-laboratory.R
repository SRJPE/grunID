#' Retrieve Laboratories
#' @description `get_laboratories()` returns all laboratories within the database
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
#' laboratories <- get_laboratories(con)
#' @family laboratory functions
#' @export
#' @md
get_laboratories <- function(con, is_active=TRUE, all_results=FALSE) {
  is_valid_connection(con)

  if(all_results) {
    laboratories <- dplyr::tbl(con, "laboratory") |>
      dplyr::collect()
  } else {
    agencies <- dplyr::tbl(con, "laboratory") |>
      dplyr::filter(active == is_active) |>
      dplyr::collect()
  }

  return(laboratories)
}

#' Add Laboratory
#' @description `add_laboratory()` adds a new laboratory to the laboratory lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param laboratory A valid laboratory dataframe containing the following:
#' * **code** Short code representing laboratory name
#' * **laboratory_name** Laboratory full name
#' * **description** Brief description of the laboratory
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
#' new_laboratory <- data.frame(code = "CNL",
#'                              laboratory_name = "Cool New Lab",
#'                              description = "The coolest new lab ever")
#' add_laboratory(con, new_laboratory)
#' @family laboratory functions
#' @export
#' @md
add_laboratory <- function(con, laboratory) {
  is_valid_connection(con)
  is_valid_laboratory(laboratory)

  DBI::dbAppendTable(con, "laboratory", laboratory)

}

#' Update Laboratory
#' @description `update_laboratory()` updates an existing laboratory in the laboratory lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param laboratory_id A numeric ID for the targeted laboratory \code{\link{get_laboratories}}
#' @param laboratory A valid laboratory dataframe containing the following:
#' * **code** Short code representing laboratory name
#' * **laboratory_name** Laboratory full name
#' * **description** Brief description of the laboratory
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
#' all_laboratories <- get_laboratories(con)
#' View(all_laboratories) # to view the ID of the laboratory needing updates
#'
#' updated_laboratory <- all_laboratories[1, 2:4]
#' updated_laboratory$laboratory_name <- "The Department of Water Resources Genetic Monitoring Laboratory"
#' update_laboratory(con, 1, updated_laboratory)
#' @family laboratory functions
#' @export
#' @md
update_laboratory <- function(con, laboratory_id, laboratory) {
  is_valid_connection(con)
  is_valid_laboratory(laboratory)

  query <- glue::glue_sql("UPDATE laboratory
                           SET code = {laboratory$code},
                               laboratory_name = {laboratory$laboratory_name},
                               description = {laboratory$description}
                           WHERE id = {laboratory_id}
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Update Laboratory Status
#' @description `update_laboratory_status()` changes active flag on existing laboratory in the laboratory lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param laboratory_id A numeric ID for the targeted laboratory \code{\link{get_agencies}}
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
#' View(all_agencies) # to view the ID of the laboratory needing status change
#'
#' #deactivate
#' update_laboratory_status(con, 4, set_active=FALSE)
#' #reactivate
#' update_laboratory_status(con, 4)
#' @family laboratory functions
#' @export
#' @md
update_laboratory_status <- function(con, laboratory_id, set_active=TRUE) {
  is_valid_connection(con)

  query <- glue::glue_sql("UPDATE laboratory
                           SET active = {set_active}
                           WHERE id = {laboratory_id}
                           RETURNING id, code, active, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Delete Laboratory
#' @description `delete_laboratory()` deletes an existing laboratory in the laboratory lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param laboratory_id A numeric ID for the targeted laboratory \code{\link{get_laboratories}}
#' **Note:** If an laboratory has been associated with a assay result record, then
#' the database restricts deleting this laboratory. You must first update those records
#' with a new laboratory before reattempting to delete the laboratory. Consider
#' using the \code{\link{update_laboratory_status}} function if you are wanting to
#' retire an laboratory while retaining its value for historic records.
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
#' all_laboratories <- get_laboratories(con)
#' View(all_laboratories) # to view the ID of the laboratory needing deletion
#'
#' delete_laboratory(con, 1)
#' @family laboratory functions
#' @export
#' @md
delete_laboratory <- function(con, laboratory_id) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM laboratory where id = {laboratory_id};",
                          .con = con)

  result <- DBI::dbExecute(con, query)

  return(result)
}

is_valid_laboratory <- function(laboratory) {

  if (!is.data.frame(laboratory)) {
    stop("Please provide laboratory as a dataframe", call. = FALSE)
  }

  column_reference <- c("code" = "character", "laboratory_name" = "character",
                        "description" = "character")
  if (!identical(sapply(laboratory, class), column_reference)) {
    stop('The laboratory supplied is not valid, see `help("add_laboratory")` for correct format', call. = FALSE)
  }

}

