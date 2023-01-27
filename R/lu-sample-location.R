#' Retrieve Sample Locations
#' @description `get_sample_locations()` returns all sample locations within the database
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
#' sample_locations <- get_sample_locations(con)
#' @family sample location functions
#' @export
#' @md
get_sample_locations <- function(con, is_active=TRUE, all_results=FALSE) {
  is_valid_connection(con)

  if(all_results) {
    sample_locations <-dplyr::tbl(con, "sample_location") |>
      dplyr::collect()
  } else {
  sample_locations <-dplyr::tbl(con, "sample_location") |>
    dplyr::filter(active == is_active) |>
    dplyr::collect()
  }

  return(sample_locations)
}

#' Add Sample Location
#' @description `add_sample_location()` adds a new sample location to the sample location lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param sample_location A valid sample location dataframe with the following:
#'
#' 1. **code** *character* A short code for the location
#' 2. **location_name** *character* The location name
#' 3. **stream_name** *character* The stream name (e.g., "American River")
#' 4. **description** *character*
#' 5. **managing_agency_id** *integer* see \code{\link{get_agencies}}
#' 6. **latitude** *numeric*
#' 7. **longitude** *numeric*
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
#' new_sample_location <- data.frame(code = "",
#'                                   location_name = "",
#'                                   stream_name = "",
#'                                   description = "",
#'                                   managing_agency_id = 1,
#'                                   latitude = 0,
#'                                   longitude = 0)
#' add_sample_location(con, new_sample_location)
#' @family sample location functions
#' @export
#' @md
add_sample_location <- function(con, sample_location) {
  is_valid_connection(con)
  is_valid_sample_location(sample_location)

  tryCatch(DBI::dbAppendTable(con, "sample_location", sample_location),
           error = function(e) {
             if (grepl("duplicate key value violates unique constraint", e)) {
               stop("This sample location already exists in the database", call. = FALSE)
             } else {
               stop(e)
             }
           })

}

#' Update Sample Location
#' @description `update_sample_location()` updates an existing sample location in the sample location lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param sample_location_id A numeric ID for the targeted sample location \code{\link{get_sample_locations}}
#' @param sample_location A valid sample location dataframe with the following:
#'
#' 1. **code** *character* A short code for the agency
#' 2. **agency_name** *character* The agency's proper name
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
#' all_sample_locations <- get_sample_locations(con)
#' View(all_sample_locations) # to view the ID of the sample location needing updates
#' updated_sample_location <- data.frame(code = "UCD",
#'                          agency_name = "University of California, Davis")
#' update_sample_location(con, 4, updated_sample_location)
#' @family sample location functions
#' @export
#' @md
update_sample_location <- function(con, sample_location_id, sample_location) {
  is_valid_connection(con)
  is_valid_sample_location(sample_location)

  query <- glue::glue_sql("UPDATE sample_location
                           SET code = {sample_location$code},
                               location_name = {sample_location$location_name},
                               stream_name = {sample_location$stream_name},
                               description = {sample_location$description},
                               managing_agency_id = {sample_location$managing_agency_id},
                               latitude = {sample_location$latitude},
                               longitude = {sample_location$longitude}
                           WHERE id = {sample_location_id}
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Update Sample Location Status
#' @description `update_sample_location_status()` changes active flag on existing sample location in the sample location lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param sample_location_id A numeric ID for the targeted sample location \code{\link{get_sample_locations}}
#' @param set_active A boolean, TRUE for activating and FALSE for deactivating.
#' When a record is active, it is returned by default when \code{\link{get_sample_locations}}
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
#' all_sample_locations <- get_sample_locations(con)
#' View(all_sample_locations) # to view the ID of the sample location needing status change
#'
#' #deactivate
#' update_sample_location_status(con, 4, set_active=FALSE)
#' #reactivate
#' update_sample_location_status(con, 4)
#' @family sample location functions
#' @export
#' @md
update_sample_location_status <- function(con, sample_location_id, set_active=TRUE) {
  is_valid_connection(con)

  query <- glue::glue_sql("UPDATE sample_location
                           SET active = {set_active}
                           WHERE id = {sample_location_id}
                           RETURNING id, code, active, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Delete Sample Location
#' @description `delete_sample_location()` deletes an existing sample location in the sample location lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param sample_location_id A numeric ID for the targeted sample location \code{\link{get_sample_locations}}
#' @details
#' **Note:** If an sample location has been associated with a permit record, then
#' the database restricts deleting this sample location. You must first update those records
#' with a new sample location before reattempting to delete the sample location. Consider
#' using the \code{\link{update_sample_location_status}} function if you are wanting to
#' retire an sample location while retaining its value for historic records.
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
#' all_sample_locations <- get_sample_locations(con)
#' View(all_sample_locations) # to view the ID of the sample location needing deletion
#'
#' delete_sample_location(con, 4)
#' @family sample location functions
#' @export
#' @md
delete_sample_location <- function(con, sample_location_id) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM sample location WHERE id = {sample_location_id};",
                          .con = con)

  result <- DBI::dbExecute(con, query)

  return(result)
}

is_valid_sample_location <- function(sample_location) {

  if (!is.data.frame(sample_location)) {
    stop("Please provide sample location as a dataframe", call. = FALSE)
  }

  column_reference <- c("code" = "character", "agency_name" = "character")
  if (!identical(sapply(agency, class), column_reference)) {
    stop('The sample location supplied is not valid, see `help("add_sample_location")` for correct format', call. = FALSE)
  }

}

