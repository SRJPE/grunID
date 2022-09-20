#' Retrieve Protocols
#' @export
get_protocol <- function(con) {
  is_valid_connection(con)

  protocols <- dplyr::tbl(con, "protocol") |>
    dplyr::collect() |>
    dplyr::mutate(dplyr::across(
      c("run_mode", "optics", "light_source", "lamp_energy"),
      as.character))

  return(protocols)
}

#' Add Protocol
#' @export
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
#' @export
update_protocol <- function(con, protocol_id, protocol) {
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
                           WHERE id = {protocol_id}
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Delete Protocol
#' @export
delete_protocol <- function(con, protocol_id) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM protocol where id = {protocol_id};",
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

}

