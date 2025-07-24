#' Retrieve Assays
#' @description `get_assays()` returns all assays within the database
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
#' assays <- get_assays(con)
#' @family assay functions
#' @export
#' @md
get_assays <- function(con, is_active = TRUE, all_results = FALSE) {
  is_valid_connection(con)

  if (all_results) {
    assays <- dplyr::tbl(con, "assay") |>
      dplyr::collect()
  } else {
    assays <- dplyr::tbl(con, "assay") |>
      dplyr::filter(active == is_active) |>
      dplyr::collect()
  }

  return(assays)
}

#' Add Assay
#' @description `add_assay()` adds a new assay type to the assay lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param assay A valid assay dataframe with the following:
#'
#' 1. **code** *character* A short code for the assay
#' 2. **assay_name** *character* The assay's name
#' 3. **description** *character* Short description of assay
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
#' new_assay <- data.frame(code = "OTS28E1",
#'                         assay_name = "Ots28_Early1",
#'                         description = "Targets GREB1L region; 58 bp away from OTS28L1 and targets a SNP")
#'
#' add_assay(con, new_assay)
#' @family assay functions
#' @export
#' @md
add_assay <- function(con, assay) {
  is_valid_connection(con)
  is_valid_assay(assay)

  tryCatch(DBI::dbAppendTable(con, "assay", assay), error = function(e) {
    if (grepl("duplicate key value violates unique constraint", e)) {
      stop("This assay already exists in the database", call. = FALSE)
    } else {
      stop(e)
    }
  })
}

#' Update Assay
#' @description `update_assay()` updates an existing assay type in the assay lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param assay_id A numeric ID for the targeted assay \code{\link{get_assays}}
#' @param assay A valid assay dataframe with the following:
#'
#' 1. **code** *character* A short code for the assay
#' 2. **assay_name** *character* The assay's name
#' 3. **description** *character* Short description of assay
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
#' all_assays <- get_assays(con)
#' View(all_assays) # to view the ID of the assay needing updates
#' updated_assay <- data.frame(code = "OTS28E1",
#'                             assay_name = "Ots28 Early 1",
#'                             description = "Targets GREB1L region")
#'
#' update_assay(con, 1, updated_assay)
#' @family assay functions
#' @export
#' @md
update_assay <- function(con, assay_id, assay) {
  is_valid_connection(con)
  is_valid_assay(assay)

  query <- glue::glue_sql(
    "UPDATE assay
                           SET code = {assay$code},
                               assay_name = {assay$assay_name},
                               description = {assay$description}
                           WHERE id = {assay_id}
                           RETURNING id, updated_at;",
    .con = con
  )

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}


#' Update Assay Status
#' @description `update_assay_status()` changes active flag on existing assay type in the assay lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param assay_id A numeric ID for the targeted assay \code{\link{get_assays}}
#' @param set_active A boolean, TRUE for activating and FALSE for deactivating.
#' When a record is active, it is returned by default when \code{\link{get_assays}}
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
#' all_assays <- get_assays(con)
#' View(all_assays) # to view the ID of the assay needing status change
#'
#' #deactivate
#' update_assay_status(con, 1, set_active=FALSE)
#' #reactivate
#' update_assay_status(con, 1)
#' @family assay functions
#' @export
#' @md
update_assay_status <- function(con, assay_id, set_active = TRUE) {
  is_valid_connection(con)

  query <- glue::glue_sql(
    "UPDATE assay
                           SET active = {set_active}
                           WHERE id = {assay_id}
                           RETURNING id, code, active, updated_at;",
    .con = con
  )

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Delete Assay
#' @description `delete_assay()` deletes an existing assay type in the assay lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param assay_id A numeric ID for the targeted assay \code{\link{get_assays}}
#' @details
#' **Note:** If an assay type has been associated with an assay result record, then
#' the database restricts deleting this assay type. You must first update those records
#' with a new assay type before reattempting to delete the assay type. Consider
#' using the \code{\link{update_assay_status}} function if you are wanting to
#' retire an assay type while retaining its value for historic records.
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
#' all_assays <- get_assays(con)
#' View(all_assays) # to view the ID of the assay needing deletion
#'
#' delete_assay(con, 1)
#' @family assay functions
#' @export
#' @md
delete_assay <- function(con, assay_id) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM assay where id = {assay_id};", .con = con)

  result <- DBI::dbExecute(con, query)

  return(result)
}

is_valid_assay <- function(assay) {
  if (!is.data.frame(assay)) {
    stop("Please provide assay as a dataframe", call. = FALSE)
  }

  column_reference <- c("code" = "character", "assay_name" = "character", "description" = "character")
  if (!identical(sapply(assay, class), column_reference)) {
    stop('The assay supplied is not valid, see `help("add_assay")` for correct format', call. = FALSE)
  }
}
