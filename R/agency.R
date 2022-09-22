#' Retrieve Agencies
#' @description `get_agencies()` returns all agencies within the database
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
#' agencies <- get_agencies(con)
#' @family agency functions
#' @export
#' @md
get_agencies <- function(con) {
  is_valid_connection(con)

  agencies <- dplyr::tbl(con, "agency") |>
    dplyr::collect()

  return(agencies)
}

#' Add Agency
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param agency A valid agency dataframe with the following:
#'
#' 1. **code** *character* A short code for the agency
#' 2. **agency_name** *character* The agency's proper name
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
#' new_agency <- data.frame(code = "USFWS",
#'                          agency_name = "United States Fish and Wildlife Service"))
#' add_agency(con, new_agency)
#' @family agency functions
#' @export
#' @md
add_agency <- function(con, agency) {
  is_valid_connection(con)
  is_valid_agency(agency)

  tryCatch(DBI::dbAppendTable(con, "agency", agency),
           error = function(e) {
             if (grepl("duplicate key value violates unique constraint", e)) {
               stop("This agency already exists in the database", call. = FALSE)
             } else {
               stop(e)
             }
           })

}

#' Update Agency
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param agency_id A numeric ID for the targeted agency \code{\link{get_agencies}}
#' @param agency A valid agency dataframe with the following:
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
#' all_agencies <- get_agencies(con)
#' View(all_agencies) # to view the ID of the agency needing updates
#' updated_agency <- data.frame(code = "UCD",
#'                          agency_name = "University of California, Davis"))
#' update_agency(con, 4, updated_agency)
#' @family agency functions
#' @export
#' @md
update_agency <- function(con, agency_id, agency) {
  is_valid_connection(con)
  is_valid_agency(agency)

  query <- glue::glue_sql("UPDATE agency
                           SET code = {agency$code},
                               agency_name = {agency$agency_name},
                           WHERE id = {agency_id}
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

#' Delete Agency
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param agency_id A numeric ID for the targeted agency \code{\link{get_agencies}}
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
#' View(all_agencies) # to view the ID of the agency needing deletion
#'
#' delete_agency(con, 4)
#' @family agency functions
#' @export
#' @md
delete_agency <- function(con, agency_id) {
  is_valid_connection(con)

  query <- glue::glue_sql("DELETE FROM agency where id = {agency_id};",
                          .con = con)

  result <- DBI::dbExecute(con, query)

  return(result)
}

is_valid_agency <- function(agency) {

  if (!is.data.frame(agency)) {
    stop("Please provide agency as a dataframe", call. = FALSE)
  }

  column_reference <- c("code" = "character", "agency_name" = "character")
  if (!identical(sapply(agency, class), column_reference)) {
    stop('The agency supplied is not valid, see `help("add_agency")` for correct format', call. = FALSE)
  }

}

