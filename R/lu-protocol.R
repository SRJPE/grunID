#' Retrieve Protocols
#' @description `get_protocols()` returns all protocols within the database
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param ... filter to be passed to dplyr::filter
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#' protocols <- get_protocols(con)
#' protocols <- get_protocols(con, name == "default") # filter on db
#' @family protocol functions
#' @export
#' @md
get_protocols <- function(con, ...) {
  is_valid_connection(con)

  protocols <- dplyr::tbl(con, "protocol") |>
    dplyr::filter(...) |>
    dplyr::collect() |>
    dplyr::mutate(dplyr::across(
      c("run_mode", "optics", "light_source", "lamp_energy"),
      as.character))

  return(protocols)
}


#' Add Protocol
#' @description `add_protocol()` adds a new protocol to the protocol lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param protocol A valid protocol dataframe, reference \code{\link{protocol_template}} as an example
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
#' new_protocol <- protocol_template
#' new_protocol$name <- "New Protocol Names"
#' new_protocol$software_version <- "3.11.20"
#' add_protocol(con, new_protocol)
#' @family protocol functions
#' @export
#' @md
add_protocol <- function(con, protocol) {
  is_valid_connection(con)
  is_valid_protocol(protocol)

  tryCatch(DBI::dbAppendTable(con, "protocol", protocol),
           error = function(e) {
             if (grepl("duplicate key value violates unique constraint", e)) {
               stop("This protocol already exists in the database", call. = FALSE)
             } else {
               stop(e)
             }
           })

}

#' Update Protocol
#' @description `update_protocol()` updates an existing protocol in the protocol lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param protocol_id A numeric ID for the targeted protocol \code{\link{get_protocols}}
#' @param protocol A valid protocol dataframe, reference \code{\link{protocol_template}} as an example
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
#' all_protocols <- get_protocols(con)
#' View(all_protocols) # to view the ID of the protocol needing updates
#'
#' updated_protocol <- protocol_template
#' updated_protocol$software_version <- "3.11.20"
#' update_protocol(con, 1, updated_protocol)
#' @family protocol functions
#' @export
#' @md
update_protocol <- function(con, protocol_name, protocol) {
  is_valid_connection(con)
  is_valid_protocol(protocol)


  query <- glue::glue_sql("UPDATE protocol
                           SET software_version = {protocol$software_version},
                               reader_type = {protocol$reader_type},
                               reader_serial_number = {protocol$reader_serial_number},
                               plate_type = {protocol$plate_type},
                               set_point = {protocol$set_point},
                               preheat_before_moving = {protocol$preheat_before_moving},
                               runtime = {protocol$runtime},
                               interval = {protocol$interval},
                               read_count = {protocol$read_count},
                               run_mode = {protocol$run_mode},
                               excitation = {protocol$excitation},
                               emissions = {protocol$emissions},
                               optics = {protocol$optics},
                               gain = {protocol$gain},
                               light_source = {protocol$light_source},
                               lamp_energy = {protocol$lamp_energy},
                               read_height = {protocol$read_height}
                           WHERE name = {protocol_name}
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Update Protocol Status
#' @description `update_protocol_status()` changes active flag on existing protocol in the protocol lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param protocol_id A numeric ID for the targeted protocol \code{\link{get_agencies}}
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
#' View(all_agencies) # to view the ID of the protocol needing status change
#'
#' #deactivate
#' update_protocol_status(con, 4, set_active=FALSE)
#' #reactivate
#' update_protocol_status(con, 4)
#' @family protocol functions
#' @export
#' @md
update_protocol_status <- function(con, protocol_id, set_active=TRUE) {
  is_valid_connection(con)

  query <- glue::glue_sql("UPDATE protocol
                           SET active = {set_active}
                           WHERE id = {protocol_id}
                           RETURNING id, code, active, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Delete Protocol
#' @description `delete_protocol()` deletes an existing protocol in the protocol lookup table
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param protocol_id A numeric ID for the targeted protocol \code{\link{get_protocols}}
#' **Note:** If an protocol has been associated with a assay result record, then
#' the database restricts deleting this protocol. You must first update those records
#' with a new protocol before reattempting to delete the protocol. Consider
#' using the \code{\link{update_protocol_status}} function if you are wanting to
#' retire an protocol while retaining its value for historic records.
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
#' all_protocols <- get_protocols(con)
#' View(all_protocols) # to view the ID of the protocol needing deletion
#'
#' delete_protocol(con, 1)
#' @family protocol functions
#' @export
#' @md
delete_protocol <- function(con, protocol_name) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM protocol where name = {protocol_name};",
                          .con = con)

  result <- DBI::dbExecute(con, query)

  return(result)
}

is_valid_protocol <- function(protocol) {

  if (!is.data.frame(protocol)) {
    stop("Please provide protocol as a dataframe", call. = FALSE)
  }

  if (!identical(sapply(protocol_template, class), sapply(protocol, class))) {
    stop("The protocol supplied is not valid, reference `grunID::protocol_template`", call. = FALSE)
  }

  if (!protocol$run_mode %in% c("Kinetic")) {
    stop("The `run_mode` supplied is not valid", call. = FALSE)
  }

  if (!protocol$optics %in% c("Top")) {
    stop("The `optics` supplied is not valid", call. = FALSE)
  }

  if (!protocol$light_source %in% c("Xenon Flash")) {
    stop("The `light_source` supplied is not valid", call. = FALSE)
  }

  if (!protocol$lamp_energy %in% c("High")) {
    stop("The `lamp_energy` supplied is not valid", call. = FALSE)
  }


}

