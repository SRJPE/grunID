#' @title Create salvage ids
#' @param n_samples the total number of samples to create
make_salvage_ids <- function(n_samples, year) {
  # C230086SWP
  year_str <- as.character(year)

  if (stringr::str_length(year_str) == 4) {
    year_id <- stringr::str_trunc(year_str, width = 2, side = "left", ellipsis = "")
  } else if (stringr::str_length(year_str) == 2) {
    year_id <- year
  } else {
    stop("invalid year parameter supplied to `make_salvage_ids`", call. = FALSE)
  }

  numeric_id <- stringr::str_pad(string = 1:n_samples, width = 4, pad = "0")
  id_part_1 <- glue::glue("C{year_id}{numeric_id}")

  swp_ids <- glue::glue("{id_part_1}SWP")
  cvp_ids <- glue::glue("{id_part_1}CVP")

  return (c(swp_ids, cvp_ids))
}


#' @title Seed Salvate Samples
#' @description
#' Given a total number of samples, this fucntion creates and inserts them into the "external_sample" table
#' in the database connected via the `con` object.
#' @param con a conection the database
#' @param n_samples total number of samples to create
#' @md
#' @export
add_salvage_samples <- function(con, n_samples = 1000, year) {
  salvage_ids <- make_salvage_ids(n_samples, year)

  sql_statement <- glue::glue_sql(
    "INSERT INTO external_sample (sample_id) VALUES ({salvage_ids}) ON CONFLICT (sample_id) DO NOTHING",
    .con = con)

  walk(1:n_samples, \(x) DBI::dbExecute(con, sql_statement[x]))
}





add_new_salvage_plate_results <- function() {

}
