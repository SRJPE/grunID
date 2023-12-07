#' Generate Subsample
#' @description Subsamples from sampling events in a season.
#' @details This function subsamples from all samples in a given season according to the following logic:
#'
#' * At least 50% of samples per site per event will be sampled.
#' * If the number of samples is odd, divide that number by 2 and round the resulting number up to the nearest integer.
#' * If the total number of samples for a given site in a given event is less than 20, process all samples for that site/event.
#' * If multiple bins are represented in a set of samples for a given site and event, select 50% of the samples from each bin for processing.
#' * If the total number of samples in a bin is less than or equal to 5, process all of the samples for that bin.
#' * If this rule contradicts the “less than 20” rule (above), this rule should be prioritized. For example, if we receive a sample set from a given site and event where Bins A, B, C, D, and E are each represented by five samples (total sample size = 25), process all of the samples for that site/event.
#' * Subsampling should be random.
#' @param con connection to the database
#' @param season The season for which you want to pull subsamples. A season consists of all sampling events
#' from the given year up to September 30th and from the previous year after October 1st.
#' @examples
#' # connect with database
#' con <- gr_db_connect()
#'
#' # subsample from all sampling events in season 2024
#' subsamples <- generate_subsample(con, 2024)
#' @returns A table containing the following columns:
#'
#' * **sample_id**
#' * **datetime_collected**
#' * **stream_name**
#' * **sample_bin_code**
#' * **sample_event_number**
#' @export
#' @md
generate_subsample <- function(con, season) {
  if(!is.numeric(season)) {
    cli::cli_abort("Season must be a numeric value.")
  }
  if(nchar(season) != 4) {
    cli::cli_abort("Season must be in the format YYYY (i.e. 2024).")
  }
  # get all sample event IDs for a season
  samples <- grunID::filter_dataset(con, season) |>
    dplyr::add_count(location_code, sample_event_number, name = "no_samples") |>
    dplyr::add_count(location_code, sample_event_number, sample_bin_code, name = "no_samples_per_bin")

  # apply rules
  samples_with_counts <- samples |>
    dplyr::mutate(one_bin_per_site_event = ifelse(no_samples == no_samples_per_bin, TRUE, FALSE),
           # flag if total number of samples for that site/event combo is less than 20
           subsample_all = ifelse(no_samples < 20, TRUE, FALSE),
           # flag if total number of samples for that site/event/bin combo is less than 5
           subsample_all_within_bin = ifelse(no_samples_per_bin <= 5, TRUE, FALSE),
           # apply rules:
           # if only 1 bin in that site/event combo and we want to subsample all, subsample n = total number of samples
           # if only 1 bin in that site/event combo and we DON'T want to subsample all, subsample n = total number of samples / 2 (rounded up)
           # if > 1 bin in that site/event combo and we want to subsample all within the bin, subsample n = number of samples in that bin
           # if > 1 bin in that site/event combo and we DON'T want to subsample all, subsample n = number of samples in that bin / 2 (rounded up)
           subsample_number = case_when(one_bin_per_site_event & subsample_all ~ no_samples,
                                        one_bin_per_site_event & !subsample_all ~ ceiling(no_samples / 2),
                                        !one_bin_per_site_event & subsample_all_within_bin ~ no_samples_per_bin,
                                        !one_bin_per_site_event & !subsample_all_within_bin ~ ceiling(no_samples_per_bin / 2),
                                        TRUE ~ NA),
           # now get that percentage (if necessary to use in a subsampling function)
           percentage_to_sample = ifelse(one_bin_per_site_event, subsample_number / no_samples,
                                         subsample_number / no_samples_per_bin))

  # subsample
  subsample_table <- samples_with_counts |>
    dplyr::group_split(location_code, sample_event_number, sample_bin_code) |>
    purrr::map(function(x) {
      subsample_n <- unique(x$subsample_number)
      x |>
        dplyr::slice_sample(n = subsample_n)
    }) |>
    purrr::list_rbind() |>
    dplyr::select(sample_id, datetime_collected, stream_name, sample_bin_code, sample_event_number)

  return(subsample_table)

}



