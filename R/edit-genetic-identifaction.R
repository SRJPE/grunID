#' @title Query Genetic Results
#' @description
#' Get table of results populated in the genetic_run_identification table.
#' @param con connection to database
#' @param run (optional) the run to filter results to
#' @param sample_id (optional) the sample to filter results to
#' @param year (optional) to digit season to filter results to
#' @export
get_genetic_run_results <- function(con, run = NULL, sample_id = NULL, year = NULL) {
  base_query <- tbl(con, "genetic_run_identification")

  if (!is.null(run)) {
    run_id <- dplyr::tbl(con, "run_type") |> dplyr::filter(code == run) |> dplyr::pull(id)
    base_query <- base_query |> dplyr::filter(run_type_id == run_id)
  }
  if (!is.null(sample_id)) {
    base_query <- base_query |> dplyr::filter(sample_id == !!sample_id)
  }
  if (!is.null(year)) {
    all_locations <- grunID::get_sample_locations(con) |> dplyr::pull(code)
    regex <- paste0(paste0("^", all_locations), year, collapse = "|")
    base_query <- base_query |> dplyr::filter(grepl(regex, sample_id))
  }

  base_query |> dplyr::collect()
}

#' @title Update Genetic Run ID
#' @description
#' Updates genetic id for a sample in the database
#' @param sample_id the sample id to update run identification for
#' @param run_type the run code to update sample to, one of "FAL", "SPR", "WIN", "HET", "UNK"
#'
#' @export
update_genetic_run_id <- function(con, sample_id, run_type) {
  run_id <- dplyr::tbl(con, "run_type") |> dplyr::filter(code == run_type) |> dplyr::pull(id)
  if (length(run_id) == 0) {
    stop('run_type submitted is not a valid run type, please only submit one of "Fall", "Spring", "Winter", "Early/Late Heterozygous", "Spring/Winter Heterozygous", or "Unknown"')
  }
  update_statement <- glue::glue_sql("UPDATE genetic_run_identification SET
                                     run_type_id = {run_id},
                                     updated_at = {lubridate::now()},
                                     updated_by = CURRENT_USER
                                     WHERE sample_id = {sample_id}", .con = con)

  res <- DBI::dbExecute(con, update_statement)

  if (res == 1) {
    cli::cli_alert_success(paste0("Run identification for sample ID ", sample_id, " updated to ", run_type))
  } else {
    cli::cli_alert_warning("there was an issue modifying the sample")
  }

}


#' @title View Results Table Audits
#' @description
#' Display table with data for changes made to the `genetic_run_identification` table
#'
#' @md
#' @export
view_genetic_id_audits <- function(con, ...) {
  dplyr::tbl(con, "genetic_run_id_table_audit") |>
    dplyr::filter(...) |>
    dplyr::collect()
}
