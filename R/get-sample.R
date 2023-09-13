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

  if(nchar(season) != 2) {
    stop("You must provide a season in the format YY (i.e. for the 2022 season,
         please pass the argument as season = 22")
  }

  season <- as.character(season)

  is_valid_connection(con)

  status_codes <- dplyr::tbl(con, "status_code") |>
    dplyr::select(status_code_id = id, status = status_code_name) |>
    dplyr::collect()

  run_codes <- dplyr::tbl(con, "run_type") |>
    dplyr::select(run_type_id = id, assigned_run = run_name) |>
    dplyr::collect()

  samples <- dplyr::tbl(con, "sample") |>
    dplyr::mutate(season_id = substr(id, 4, 5)) |>
    dplyr::filter(season_id %in% season) |>
    dplyr::rename(sample_id = id) |>
    dplyr::select(-season_id) |>
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
