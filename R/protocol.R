#' Retrieve Protocols
#' @export
get_protocol <- function(con) {
  is_valid_connection(con)
  protocols <- dplyr::tbl(con, "protocol") |> dplyr::collect()
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

update_protocol <- function(con) {

}

delete_protocol <- function(con) {

}

is_valid_protocol <- function(protocol) {

  if (!is.data.frame(protocol)) {
    stop("Please provide protocol as a dataframe", call. = FALSE)
  }

  if (!identical(sapply(protocol_template, class), sapply(protocol, class))) {
    stop("The protocol supplied is not valid, reference `grunID::protocol_template`", call. = FALSE)
  }

}

