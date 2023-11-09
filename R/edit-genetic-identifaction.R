#' @title Query Genetic Results
#' @description
#' Get table of results populated in the genetic_run_identification table.
#' @export
get_genetic_run_results <- function(con, run = NULL, sample_id = NULL, year = NULL) {
  base_query <- tbl(con, "genetic_run_identification")

  if (!is.null(run)) {
    run_id <- tbl(con, "run_type") |> dplyr::filter(code == run) |> pull(id)
    base_query <- base_query |> dplyr::filter(run_type_id == run_id)
  }
  if (!is.null(sample_id)) {
    base_query <- base_query |> dplyr::filter(sample_id == !!sample_id)
  }
  if (!is.null(year)) {
    all_locations <- grunID::get_sample_locations(con) |> dplyr::pull(code)
    regex <- paste0(paste0("^", all_locations), year, collapse = "|")
    base_query <- base_query |> filter(grepl(regex, sample_id))
  }

  base_query |> dplyr::collect()
}

#' @title Update Genetic Run ID
#' @description
#' Updates genetic id for a sample in the database
#'
#' @export
update_genetic_run_id <- function(con, sample_id, run_type) {
  run_id <- tbl(con, "run_type") |> dplyr::filter(code == run_type) |> pull(id)
  update_statement <- glue::glue_sql("UPDATE genetic_run_identification SET
                                     run_type_id = {run_id},
                                     updated_at = {lubridate::now()},
                                     updated_by = CURRENT_USER
                                     WHERE sample_id = {sample_id}", .con = con)

  DBI::dbExecute(con, update_statement)

}


#' @title View Results Table Audits
#' @description
#' Display table with data for changes made to the `genetic_run_identification` table
#'
#' @md
#' @export
view_genetic_id_audits <- function(con, ...) {
  tbl(con, "genetic_run_id_table_audit") |>
    dplyr::filter(...) |>
    collect()
}
