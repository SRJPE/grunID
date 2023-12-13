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
#' @param season year in format YYYY. You can pass in a min and max season as c(YYYY, YYYY). A
#' season consists of all sampling events from the given year up to September 30th and from the previous year after October 1st.
#' @param dataset either "raw", "clean", or "unprocessed".
#' @param heterozygote_filter defaults to FALSE. If TRUE, only heterozygotes are returned.
#' @param failed_filter defaults to FALSE. If TRUE, only "failed" assays (negative for both OTS28 early and late,
#' or negative for both OTS16 spring and winter)
#' @details the parameter `dataset` can be used to determine what information is
#' included in the tibble. The `raw`, `clean`, and `unprocessed` datasets contain the following
#' variables:
#'
#' * stream_name
#' * datetime_collected
#' * sample_event_number
#' * sample_id
#' * sherlock_run_assignment
#' * field_run_assignment
#' * fork_length_mm
#' * fin_clip
#' * status
#' * updated_at
#'
#' If you select `dataset = "raw"`, the tibble will additionally contain:
#'
#' * assay_name
#' * raw_fluorescence
#' * threshold
#' * positive_detection
#' * plate_run_id
#'
#' If you select `dataset = "unprocessed"`, the tibble will additionally contain:
#'
#' * assay_name
#' * sample_type_name
#' * raw_fluorescence
#' * background_value
#' * time
#' * well_location
#' * plate_run_id
#'
#' @examples
#' # example database connection
#' con <- gr_db_connect()
#' 2022_2023_samples <- get_samples_by_season(con, season = c(2022, 2023), dataset = "clean", heterozygote_filter = FALSE)
#' @export
#' @md
get_samples_by_season <- function(con, season, dataset = c("raw", "clean", "unprocessed"),
                                  heterozygote_filter = c(FALSE, TRUE),
                                  failed_filter = c(FALSE, TRUE)) {

  if(any(sapply(season, function(i) {nchar(i) == 4})) == FALSE) {
    stop("You must provide seasons in the format YYYY (i.e. for the 2022 season,
         please pass the argument as season = 2022; for seasons 2022 and 2023,
         provide in format c(2022, 2023)")
  }

  dataset <- match.arg(dataset)
  if(missing(heterozygote_filter)) {heterozygote = FALSE} else {
    heterozygote <- heterozygote_filter
  }
  if(missing(failed_filter)) {failed = FALSE} else {
    failed <- failed_filter
  }

  is_valid_connection(con)
  season <- as.integer(season)

  samples <- grunID::sample_filter_to_season(con, season)

  clean_dataset <- get_clean_dataset(con, filtered_samples = samples,
                                     heterozygote_filter = heterozygote,
                                     failed_filter = failed)

  if(dataset == "clean") {
    results <- clean_dataset
  }
  if(dataset == "raw") {
    results <- get_raw_dataset(con, clean_dataset)
  }
  else if(dataset == "unprocessed") {
    results <- get_unprocessed_dataset(con, clean_dataset)
  }

  return(results)
}

#' Filter dataset by season
#' @export
sample_filter_to_season <- function(con, season) {
  # filter by date
  min_date <- as.Date(paste0(min(season) - 1, "-10-01"))
  max_date <- as.Date(paste0(max(season), "-09-30"))

  location_codes <- dplyr::tbl(con, "sample_location") |>
    dplyr::select(sample_location_id = id, code, stream_name) |>
    dplyr::collect()

  sample_event_ids <- dplyr::tbl(con, "sample_event") |>
    dplyr::filter(between(first_sample_date, min_date, max_date)) |>
    dplyr::collect() |>
    dplyr::left_join(location_codes, by = "sample_location_id") |>
    dplyr::select(stream_name, location_code = code, id, sample_event_number)

  sample_bin_ids <- dplyr::tbl(con, "sample_bin") |>
    dplyr::collect() |>
    dplyr::filter(sample_event_id %in% sample_event_ids$id) |>
    dplyr::left_join(sample_event_ids, by = c("sample_event_id" = "id")) |>
    dplyr::select(id, stream_name, location_code, sample_bin_code, sample_event_number) |>
    dplyr::mutate(sample_bin_code = as.character(sample_bin_code))

  samples <- dplyr::tbl(con, "sample") |>
    dplyr::collect() |>
    dplyr::left_join(sample_bin_ids, by = c("sample_bin_id" = "id")) |>
    dplyr::filter(sample_bin_id %in% sample_bin_ids$id) |>
    dplyr::rename(sample_id = id) |>
    dplyr::select(-c(created_at, created_by, updated_at, updated_by)) # not necessary and confusing

  return(samples)
}

#' Query database for clean dataset
#' @export
get_clean_dataset <- function(con, filtered_samples,
                              heterozygote_filter = c(FALSE, TRUE),
                              failed_filter = c(FALSE, TRUE)) {
  if(heterozygote_filter & failed_filter) {
    cli::cli_abort("You can either have heterozygote filter or failed filter,
                   but not both.")
  }
  filtered_sample_ids <- unique(filtered_samples$sample_id)

  # get additional data for samples
  status_codes <- dplyr::tbl(con, "status_code") |>
    dplyr::select(status_code_id = id, status = status_code_name) |>
    dplyr::collect()

  run_codes <- dplyr::tbl(con, "run_type") |>
    dplyr::select(run_type_id = id, assigned_run = run_name) |>
    dplyr::collect()

  sample_status <- dplyr::tbl(con, "sample_status") |>
    dplyr::filter(sample_id %in% filtered_sample_ids) |>
    dplyr::collect() |>
    dplyr::left_join(status_codes, by = "status_code_id") |>
    dplyr::select(sample_id, status, updated_at, updated_by)

  assigned_runs <- dplyr::tbl(con, "genetic_run_identification") |>
    dplyr::filter(sample_id %in% filtered_sample_ids) |>
    dplyr::collect() |>
    dplyr::left_join(run_codes, by = "run_type_id") |>
    dplyr::select(sample_id, assigned_run)

  clean_results <- filtered_samples |>
    dplyr::left_join(sample_status, by = "sample_id") |>
    dplyr::left_join(run_codes |>
                       dplyr::rename(field_run_assignment = assigned_run),
                     by = c("field_run_type_id" = "run_type_id")) |>
    dplyr::left_join(assigned_runs, by = "sample_id") |>
    dplyr::select(stream_name, datetime_collected, sample_event_number, sample_id,
                  status, sherlock_run_assignment = assigned_run,
                  field_run_assignment, fork_length_mm, fin_clip, status, updated_at) |>
    dplyr::group_by(sample_id) |>
    dplyr::slice_max(updated_at) |> # only take the most recent version of the sample

  if(heterozygote_filter) {
    heterozygote_results <- clean_results |>
      dplyr::filter(sherlock_run_assignment == "Heterozygous")
    return(heterozygote_results)

  } else if (failed_filter) {
    failed_results <- clean_results |>
      dplyr::filter(sherlock_run_assignment == "Unknown")
    return(failed_results)

  }
  else {
    return(clean_results)
  }
}

#' Query database for raw dataset
#' @export
get_raw_dataset <- function(con, clean_results) {

  sample_ids_filter <- unique(clean_results$sample_id)

  # assay result table
  assays <- dplyr::tbl(con, "assay") |>
    dplyr::collect() |>
    dplyr::select(assay_id = id, assay_name)

  assay_results <- dplyr::tbl(con, "assay_result") |>
    dplyr::filter(sample_id %in% sample_ids_filter) |>
    dplyr::collect() |>
    dplyr::left_join(assays, by = "assay_id") |>
    dplyr::select(sample_id, assay_name, raw_fluorescence,
                  threshold, positive_detection, plate_run_id)

  raw_results <- clean_results |>
    dplyr::left_join(assay_results, by = "sample_id") |>
    dplyr::relocate(c(status, updated_at), .after = plate_run_id)

  return(raw_results)
}

#' Query database for unprocessed dataset
#' @export
get_unprocessed_dataset <- function(con, clean_results) {

  sample_ids_filter <- unique(clean_results$sample_id)
  # tables to join
  assays <- dplyr::tbl(con, "assay") |>
    dplyr::collect() |>
    dplyr::select(assay_id = id, assay_name)

  sample_types <- dplyr::tbl(con, "sample_type") |>
    dplyr::collect() |>
    dplyr::select(sample_type_id = id, sample_type_name)

  unprocessed_assay_results <- dplyr::tbl(con, "raw_assay_result") |>
    dplyr::filter(sample_id %in% sample_ids_filter) |>
    dplyr::collect() |>
    dplyr::left_join(assays, by = "assay_id") |>
    dplyr::left_join(sample_types, by = "sample_type_id") |>
    dplyr::select(sample_id, assay_name, sample_type_name, raw_fluorescence,
                  background_value, time, well_location, plate_run_id)

  unprocessed_results <- clean_results |>
    dplyr::left_join(unprocessed_assay_results, by = "sample_id") |>
    dplyr::relocate(c(status, updated_at), .after = plate_run_id)

  return(unprocessed_results)
}
