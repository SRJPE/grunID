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
#' @returns A named list containing a `results` table and a `summary` table.
#' The `results` table contains all samples selected by the process with the following columns:
#'
#' * **sample_id**
#' * **datetime_collected**
#' * **stream_name**
#' * **sample_bin_code**
#' * **sample_event_number**
#'
#' The `summary` table contains a short summary summarizing the number of samples for a given subsampling scenario by stream,
#' event, and bin. It contains the following columns:
#'
#' * **stream**
#' * **event**
#' * **bin**
#' * **scenario**
#' * **subsamples**
#' @export
#' @md
generate_subsample <- function(con, sampling_event, season) {

  if(!is.numeric(season)) {
    cli::cli_abort("Season must be a numeric value.")
  }
  if(nchar(season) != 4) {
    cli::cli_abort("Season must be in the format YYYY (i.e. 2024).")
  }

  # get samples from the right season
  samples <- grunID::sample_filter_to_season(con, season) |>
    # filter to sampling event of interest
    dplyr::filter(sample_event_number == sampling_event) |>
    # now make sure that they have the right status
    dplyr::left_join(dplyr::tbl(con, "sample_status") |>
                       dplyr::collect() |>
                       dplyr::select(sample_id, status_code_id),
                     by = "sample_id") |>
    dplyr::filter(status_code_id %in% c(4, 5)) |>
    # now get totals for applying logic
    dplyr::add_count(location_code, sample_event_number, name = "total_samples_in_event") |>
    dplyr::add_count(location_code, sample_event_number, sample_bin_code, name = "no_samples_per_bin")

  if(nrow(samples) == 0) {
    cli::cli_abort(paste0("There are no samples in the database from sampling event ", sampling_event,
                          " and sampling season ", season, " that are marked as returned from field. Please
                          make sure you have processed the field sheets using grunID::process_field_sheet_samples()
                          and added field data to the database using grunID::update_field_sheet_samples()"))
  }

  # apply rules
  samples_with_counts <- samples |>
    dplyr::mutate(one_bin_per_site_event = ifelse(total_samples_in_event == no_samples_per_bin, TRUE, FALSE),
           # flag if total number of samples for that site/event combo is less than 20
           subsample_all = ifelse(total_samples_in_event < 20, TRUE, FALSE),
           # flag if total number of samples for that site/event/bin combo is less than 5
           subsample_all_within_bin = ifelse(no_samples_per_bin <= 5, TRUE, FALSE),
           # apply rules:
           # if only 1 bin in that site/event combo and we want to subsample all, subsample n = total number of samples
           # if only 1 bin in that site/event combo and we DON'T want to subsample all, subsample n = total number of samples / 2 (rounded up)
           # if > 1 bin in that site/event combo and we want to subsample all within the bin, subsample n = number of samples in that bin
           # if > 1 bin in that site/event combo and we DON'T want to subsample all, subsample n = number of samples in that bin / 2 (rounded up)
           subsample_number = case_when(one_bin_per_site_event & subsample_all ~ total_samples_in_event,
                                        one_bin_per_site_event & !subsample_all ~ ceiling(total_samples_in_event / 2),
                                        !one_bin_per_site_event & subsample_all_within_bin ~ no_samples_per_bin,
                                        !one_bin_per_site_event & !subsample_all_within_bin ~ ceiling(no_samples_per_bin / 2),
                                        TRUE ~ NA),
           # now get that percentage (if necessary to use in a subsampling function)
           percentage_to_sample = ifelse(one_bin_per_site_event, subsample_number / total_samples_in_event,
                                         subsample_number / no_samples_per_bin)) |>
    dplyr::mutate(scenario = ifelse(percentage_to_sample == 1, "sampled at 100%", paste0("randomly sampled at ", round(percentage_to_sample * 100, 0), "%")))

  # subsample
  subsample_table_raw <- samples_with_counts |>
    dplyr::group_split(location_code, sample_event_number, sample_bin_code) |>
    purrr::map(function(x) {
      subsample_n <- unique(x$subsample_number)
      x |>
        dplyr::slice_sample(n = subsample_n)
    }) |>
    purrr::list_rbind()


  subsample_table <- subsample_table_raw |>
    dplyr::select(sample_id, datetime_collected, stream_name, sample_bin_code, sample_event_number, scenario)

  subsample_summary <- subsample_table_raw |>
    dplyr::group_by(stream_name, sample_event_number, scenario, sample_bin_code) |>
    dplyr::tally() |>
    dplyr::rename(stream = stream_name, event = sample_event_number, bin = sample_bin_code, subsamples = n) |>
    dplyr::arrange(stream, event, bin)

  return(list("results" = subsample_table,
              "summary" = subsample_summary))

}

# TODO generate subsample_plate_map
# arg dual assay or single assay

