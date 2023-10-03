#' Get Samples
#' @export
get_samples <- function(con, ...) {
  is_valid_connection(con)

  assays <- dplyr::tbl(con, "sample") |>
    dplyr::filter(...) |>
    dplyr::collect()

  return(assays)
}

#' Get Samples by Season
#' @description View sample by season with status and run (if assigned)
#' @param con connection to the database
#' @param season year in format YY
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
#' 2022_2023_samples <- get_samples_by_season(con, season == c(2022, 2023))
#' @export
#' @md
get_samples_by_season <- function(con, season) {

  if(any(sapply(season, function(i) {nchar(i) == 4})) == FALSE) {
    stop("You must provide seasons in the format YYYY (i.e. for the 2022 season,
         please pass the argument as season = 2022; for seasons 2022 and 2023,
         provide in format c(2022, 2023)")
  }

  is_valid_connection(con)

  season <- as.integer(season)

  min_date <- as.Date(paste0(min(season) - 1, "-10-01"))
  max_date <- as.Date(paste0(max(season), "-09-30"))

  sample_event_ids <- dplyr::tbl(con, "sample_event") |>
    dplyr::filter(between(first_sample_date, min_date, max_date)) |>
    dplyr::collect() |>
    dplyr::pull(id)

  sample_bin_ids <- dplyr::tbl(con, "sample_bin") |>
    dplyr::filter(sample_event_id %in% sample_event_ids) |>
    dplyr::collect() |>
    dplyr::pull(id)

  status_codes <- dplyr::tbl(con, "status_code") |>
    dplyr::select(status_code_id = id, status = status_code_name) |>
    dplyr::collect()

  run_codes <- dplyr::tbl(con, "run_type") |>
    dplyr::select(run_type_id = id, assigned_run = run_name) |>
    dplyr::collect()

  samples <- dplyr::tbl(con, "sample") |>
    dplyr::filter(sample_bin_id %in% sample_bin_ids) |>
    dplyr::rename(sample_id = id) |>
    dplyr::collect()

  sample_status <- dplyr::tbl(con, "sample_status") |>
    dplyr::collect() |>
    dplyr::filter(sample_id %in% samples$sample_id) |>
    dplyr::left_join(status_codes, by = "status_code_id") |>
    dplyr::select(sample_id, status)

  run_status <- dplyr::tbl(con, "genetic_run_identification") |>
    dplyr::collect() |>
    dplyr::filter(sample_id %in% samples$sample_id) |>
    dplyr::left_join(run_codes, by = "run_type_id") |>
    dplyr::select(sample_id, assigned_run)

  results <- samples |>
    dplyr::left_join(sample_status, by = "sample_id") |>
    dplyr::left_join(run_status, by = "sample_id") |>
    dplyr::select(sample_id, status, assigned_run, field_run_type_id,
                  datetime_collected, comment, updated_at, updated_by)

  return(results)

}
