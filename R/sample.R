#' Get Sample Details
#' @description View the current or full sample details
#' @param con connection to the database
#' @param sample_ids vector of sample ids to update
#' @param full_history when set to TRUE, will return the current and each previously assigned status for a sample ID
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
#' sample_details <- get_sample_details(con,
#'                                      sample_ids = c("FTH_RM1722_3_A_1",
#'                                                     "FTH_RM1722_3_A_2"))
#' @family status code functions
#' @export
#' @md
get_sample_details <- function(con, sample_ids, full_history = FALSE) {

  is_valid_connection(con)

  sample_event <- dplyr::tbl(con, "sample_event") |>
    dplyr::filter(id == sample_event_id) |>
    dplyr::select(sample_event_id = id, sample_event_number, sample_location_id, first_sample_date) |>
    dplyr::collect()

  sample_bins <- dplyr::tbl(con, "sample_bin") |>
    dplyr::select(sample_bin_id = id, sample_event_id, sample_bin_code, min_fork_length,
           max_fork_length) |>
    dplyr::filter(sample_event_id == sample_event_id) |>
    dplyr::collect()

  sample_bin_ids <- sample_bins |> dplyr::pull(sample_bin_id)

  samples <- dplyr::tbl(con, "sample") |>
    dplyr::filter(id %in% sample_ids) |>
    dplyr::rename(sample_id = id) |>
    dplyr::collect()

  sample_locations <- dplyr::tbl(con, "sample_location") |>
    dplyr::select(sample_location_id = id, sample_location_code = code, location_name) |>
    dplyr::collect()

  sample_code <- dplyr::tbl(con, "status_code") |>
    dplyr::select(status_code_id = id, status_code_name) |>
    dplyr::collect()

  sample_status <- dplyr::tbl(con, "sample_status") |>
    dplyr::filter(sample_id %in% sample_ids) |>
    dplyr::select(sample_id, status_code_id, status_comment = comment,
           status_set_at = created_at,
           status_set_by = created_by,
           status_updated_at = updated_at,
           status_updated_by = updated_by) |>
    dplyr::collect() |>
    dplyr::left_join(sample_code, by = c("status_code_id" = "status_code_id"))

  samples_raw <- samples |>
    dplyr::left_join(sample_bins, by = c("sample_bin_id" = "sample_bin_id")) |>
    dplyr::left_join(sample_event, by = c("sample_event_id" = "sample_event_id")) |>
    dplyr::left_join(sample_locations, by = c("sample_location_id" = "sample_location_id")) |>
    dplyr::left_join(sample_status, by = c("sample_id" = "sample_id"))

  results <- samples_raw |>
    dplyr::select(sample_id, sample_bin_code, min_fork_length, max_fork_length,
                  sample_event_number, first_sample_date,
                  sample_location_code, location_name, status_code_name, status_comment,
                  status_set_at, status_set_by, status_updated_at, status_updated_by,
                  comment, sample_collector, sample_split,
                  created_at, created_by, updated_at, updated_by) |>
    dplyr::arrange(sample_id, desc(status_set_at))

  if (full_history) {
    return(results)
  } else {
    results <- results |>
      dplyr::group_by(sample_id) |>
      dplyr::filter(max(status_updated_at))
  }

  return(results)

}
