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
#' @param season year in format YYYY. You can pass in a min and max season as c(YYYY, YYYY)
#' @examples
#' # example database connection
#' con <- gr_db_connect()
#' 2022_2023_samples <- get_samples_by_season(con, season == c(2022, 2023))
#' @export
#' @md
get_samples_by_season <- function(con, season, dataset = c("raw", "clean")) {

  if(any(sapply(season, function(i) {nchar(i) == 4})) == FALSE) {
    stop("You must provide seasons in the format YYYY (i.e. for the 2022 season,
         please pass the argument as season = 2022; for seasons 2022 and 2023,
         provide in format c(2022, 2023)")
  }

  is_valid_connection(con)
  season <- as.integer(season)

  # get additional data for samples
  location_codes <- dplyr::tbl(con, "sample_location") |>
    dplyr::select(sample_location_id = id, stream_name) |>
    dplyr::collect()

  status_codes <- dplyr::tbl(con, "status_code") |>
    dplyr::select(status_code_id = id, status = status_code_name) |>
    dplyr::collect()

  run_codes <- dplyr::tbl(con, "run_type") |>
    dplyr::select(run_type_id = id, assigned_run = run_name) |>
    dplyr::collect()

  # filter by date

  min_date <- as.Date(paste0(min(season) - 1, "-10-01"))
  max_date <- as.Date(paste0(max(season), "-09-30"))

  sample_event_ids <- dplyr::tbl(con, "sample_event") |>
    dplyr::filter(between(first_sample_date, min_date, max_date)) |>
    dplyr::collect() |>
    dplyr::left_join(location_codes, by = "sample_location_id") |>
    dplyr::select(stream_name, id)

  sample_bin_ids <- dplyr::tbl(con, "sample_bin") |>
    dplyr::collect() |>
    dplyr::filter(sample_event_id %in% sample_event_ids$id) |>
    dplyr::left_join(sample_event_ids, by = c("sample_event_id" = "id")) |>
    dplyr::select(id, stream_name)

  samples <- dplyr::tbl(con, "sample") |>
    dplyr::collect() |>
    dplyr::filter(sample_bin_id %in% sample_bin_ids$id) |>
    dplyr::rename(sample_id = id)

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

  clean_results <- samples |>
    dplyr::left_join(sample_bin_ids, by = c("sample_bin_id" = "id")) |>
    dplyr::left_join(sample_status, by = "sample_id") |>
    dplyr::left_join(run_status, by = "sample_id") |> # TODO do we need to join for field_run_type_id here as well?
    dplyr::select(stream_name, datetime_collected, sample_id,
                  assigned_run, field_run_type_id, fork_length_mm,
                  fin_clip, status, updated_at)

  if(dataset == "raw") {

    # assay result table
    assays <- dplyr::tbl(con, "assay") |>
      dplyr::collect() |>
      dplyr::select(assay_id = id, assay_name)

    assay_results <- dplyr::tbl(con, "assay_result") |>
      dplyr::collect() |>
      dplyr::filter(sample_id %in% clean_results$sample_id) |>
      dplyr::left_join(assays, by = "assay_id") |>
      dplyr::select(sample_id, assay_name, raw_fluorescence,
                    threshold, positive_detection, plate_run_id)

    raw_results <- clean_results |>
      dplyr::left_join(assay_results, by = "sample_id") |>
      dplyr::relocate(c(status, updated_at), .after = plate_run_id)

    return(raw_results)

    # TODO query from raw assay results ?
    # sample_types <- dplyr::tbl(con, "sample_type") |>
    #   dplyr::collect() |>
    #   dplyr::select(sample_type_id = id, sample_type_name)

  } else if(dataset == "clean") {
    return(clean_results)
  }

}
