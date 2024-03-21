#' Generate Subsample
#' @description Subsamples from a given sample event in a season.
#' @details This function takes a subsample from all samples returned from field for a given sampling
#' event and a given season. It uses the following subsampling logic:
#'
#' * At least 50% of samples per site per event will be sampled.
#' * If the number of samples is odd, divide that number by 2 and round the resulting number up to the nearest integer.
#' * If the total number of samples for a given site in a given event is less than 20, process all samples for that site/event.
#' * If multiple bins are represented in a set of samples for a given site and event, select 50% of the samples from each bin for processing.
#' * If the total number of samples in a bin is less than or equal to 5, process all of the samples for that bin.
#' * If this rule contradicts the “less than 20” rule (above), this rule should be prioritized. For example, if we receive a sample set from a given site and event where Bins A, B, C, D, and E are each represented by five samples (total sample size = 25), process all of the samples for that site/event.
#' * Subsampling should be random.
#'
#' This function uses `set.seed()`, so that the subsample will be the same for a given
#' `sampling_event` and `season`.
#' @param con connection to the database
#' @param sampling_event A numeric value for the sampling event you are processing (i.e. 1:10)
#' @param season The season for which you want to pull subsamples. A season consists of all sampling events
#' from the given year up to September 30th and from the previous year after October 1st.
#' @examples
#' # connect with database
#' con <- gr_db_connect()
#'
#' # subsample from all sampling events in season 2024
#' subsamples <- generate_subsample(con, 1, 2024)
#' @returns A named list containing a `subsample_for_sherlock` table, a `subsample_summary` table,
#' and a `remainders_for_gt_seq` table.
#' The `subsample_for_sherlock` table contains all samples selected by the process with the following columns:
#'
#' * **sample_id**
#' * **datetime_collected**
#' * **stream_name**
#' * **sample_bin_code**
#' * **sample_event_number**
#' * **scenario**
#'
#' The `subsample_summary` table contains a short summary summarizing the number of samples for a given subsampling scenario by stream,
#' event, and bin. It contains the following columns:
#'
#' * **stream**
#' * **event**
#' * **bin**
#' * **scenario**
#' * **subsamples**
#'
#' The `remainders_for_gt_seq` table contains all samples **not** selected by the subsampling process with the following columns:
#'
#' * **sample_id**
#' * **datetime_collected**
#' * **stream_name**
#' * **sample_bin_code**
#' * **sample_event_number**
#' * **scenario**
#' @export
#' @md
generate_subsample <- function(con, sampling_event, season) {

  set.seed(5674)

  if(!is.numeric(season)) {
    stop("Season must be a numeric value.", call. = FALSE)
  }
  if(nchar(season) != 4) {
    stop("Season must be in the format YYYY (i.e. 2024).", call. = FALSE)
  }

  # get sample status IDs
  sample_status_codes <- tbl(con, "status_code") |>
    filter(status_code_name %in% c("returned from field", "need ots28")) |>
    pull(id)

  # get samples from the right season
  samples <- grunID::sample_filter_to_season(con, season) |>
    # filter to sampling event of interest
    dplyr::filter(sample_event_number == sampling_event) |>
    # now make sure that they have the right status
    dplyr::left_join(dplyr::tbl(con, "sample_status") |>
                       dplyr::collect() |>
                       dplyr::select(sample_id, status_code_id),
                     by = "sample_id") |>
    dplyr::filter(status_code_id %in% sample_status_codes) |>
    # now get totals for applying logic
    dplyr::add_count(location_code, sample_event_number, name = "total_samples_in_event") |>
    dplyr::add_count(location_code, sample_event_number, sample_bin_code, name = "no_samples_per_bin")

  if(nrow(samples) == 0) {
    return(list("subsample_for_sherlock" = NA,
                "subsample_summary" = NA,
                "remainders_for_gt_seq" =  NA))
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

  remainder_samples_for_gt_seq <- samples_with_counts |>
    dplyr::filter(!sample_id %in% subsample_table$sample_id) |>
    dplyr::mutate(scenario = "Remainder samples - for GT-Seq") |>
    dplyr::select(sample_id, datetime_collected, stream_name, sample_bin_code, sample_event_number, scenario)

  return(list("subsample_for_sherlock" = subsample_table,
              "subsample_summary" = subsample_summary,
              "remainders_for_gt_seq" = remainder_samples_for_gt_seq))

}


#' Generate Plate Map from Subsample
#' @title generate_subsample_plate_map
#' @description Generates a plate map from a list of sample IDs
#' @details Fill this out TODO
#' @param sample_ids list of sample IDs with which to populate a plate map
#' @param plate_assay_structure Either `dual_assay` or `single_assay`
#' @param out_filepath Filename. Do not include ".csv" at the end of the filepath.
#' @returns A list of filepath(s) to .csv file(s) with rows `A:P` and columns `1:24` populated with sample IDs and control blanks
#' @export
#' @md
generate_subsample_plate_map <- function(sample_ids, plate_assay_structure, file_basename, path) {
  filepaths_created <- c()

  if(plate_assay_structure == "dual_assay") {

    # determine number of plate maps to generate
    control_blanks <- c("EBK-1-1", NA, "EBK-1-2", NA, "EBK-1-3", NA, "EBK-1-4",
                        "POS-DNA-1", "POS_DNA-2", "POS-DNA-3", "NEG-DNA-1",
                        "NEG-DNA-2", "NEG-DNA-3", "NTC-1", "NTC-2", "NTC-3")

    total_available_wells <- 12 * 16 # 1/2 of a 384-well plate
    available_wells_for_samples <- total_available_wells- length(control_blanks)

    no_sample_ids <- length(sample_ids)
    no_plate_maps <- ceiling(no_sample_ids / available_wells_for_samples)

    cli::cli_bullets(paste0(no_sample_ids, " sample IDs detected; generating ", no_plate_maps, " plate map(s)"))
    # split sample ids into the number of necessary plate map lengths
    split_sample_ids_by_plate_map <- split(sample_ids, ceiling(seq_along(sample_ids)/available_wells_for_samples))

    plate_maps <- purrr::map(split_sample_ids_by_plate_map, grunID::fill_dual_assay_plate_map)

    for(i in 1:no_plate_maps) {
      new_filepath <- fs::file_temp(pattern = glue::glue("{file_basename}-{i}"), tmp_dir = ".", ext = ".csv")
      write.csv(plate_maps[[i]], new_filepath, row.names = TRUE)
      filepaths_created <- append(filepaths_created, new_filepath)
      cli::cli_alert_success(paste0("Plate map generated - see ", new_filepath))
    }
  } else if(plate_assay_structure == "single_assay") {

    total_available_sample_blocks <- 92 # 4 96 well plates (384 total wells) - 16 control blanks

    no_sample_ids <- length(sample_ids)
    no_plate_maps <- ceiling(no_sample_ids / total_available_sample_blocks)

    cli::cli_bullets(paste0(no_sample_ids, " sample IDs detected; generating ", no_plate_maps, " plate map(s)"))
    # split sample ids into the number of necessary plate map lengths
    split_sample_ids_by_plate_map <- split(sample_ids, ceiling(seq_along(sample_ids)/total_available_sample_blocks))

    plate_maps <- purrr::map(split_sample_ids_by_plate_map, grunID::fill_single_assay_plate_map)

    for(i in 1:no_plate_maps) {
      new_filepath <- fs::file_temp(pattern = glue::glue("{file_basename}-{i}"),temp_dir = ".",  ext = ".csv")
      write.csv(plate_maps[[i]], new_filepath, row.names = TRUE)
      filepaths_created <- append(filepaths_created, new_filepath)
      cli::cli_alert_success(paste0("Plate map generated - see ", new_filepath))
    }
  }
  return(filepaths_created)
}

#' Generate Plate Map for Dual Assay layout
#' @title fill_dual_assay_plate_map
#' @description Generates a dual assay 384-well plate map from a list of sample IDs
#' @details This is called in `generate_subsample_plate_map()` and fills in alternating rows first, and then
#' the rows inbetween on a 384-well plate. Sample IDs are populated in the left half of a 384-well plate
#' and then replicated on the right half of the plate with control blanks in rows 12 and 24.
#' @param sample_ids list of sample IDs with which to populate a plate map
#' @returns A list of plate map tables
#' @export
#' @md
fill_dual_assay_plate_map <- function(sample_ids) {

  control_blanks <- c("EBK-1-1", NA, "EBK-1-2", NA, "EBK-1-3", NA, "EBK-1-4",
                      "POS-DNA-1", "POS_DNA-2", "POS-DNA-3", "NEG-DNA-1",
                      "NEG-DNA-2", "NEG-DNA-3", "NTC-1", "NTC-2", "NTC-3")

  # split into the number of rows you have to fill out
  fill_rows <- split(sample_ids, ceiling(seq_along(sample_ids)/11))
  nrows_to_fill <- length(fill_rows)

  out_table <- matrix(NA, nrow = 16, ncol = 24)
  # order in which to fill in rows
  row_fill_lookup <- c(1, 3, 5, 7, 9, 11, 13, 15, 2, 4, 6, 8, 10, 12, 14, 16, 18)

  if(nrows_to_fill <= 8) {
    # get every other row
    fill_indices <- row_fill_lookup[1:nrows_to_fill]
    for(i in 1:nrows_to_fill) {
      fill_cols <- 1:length(fill_rows[[i]]) # fill all cells in a row
      out_table[fill_indices[i], fill_cols] <- fill_rows[[i]]
    }
  } else {
    # get every other row, and then fill in in-between
    fill_indices <- row_fill_lookup[1:nrows_to_fill]
    for(i in 1:nrows_to_fill) {
      fill_cols <- 1:length(fill_rows[[i]])
      out_table[fill_indices[i], fill_cols] <- fill_rows[[i]]
    }
  }

  out_table[, 13:24] <- out_table[, 1:12]
  out_table[, 12] <- control_blanks
  out_table[, 24] <- control_blanks
  out_table[is.na(out_table)] <- ""
  out_df <- data.frame(out_table)

  names(out_df) <- c(1:24)
  rownames(out_df) <- LETTERS[1:16]

  out_df
}


#' Generate Plate Map for Single Assay v4 layout
#' @title fill_single_assay_plate_map
#' @description Generates a single-assay 384-well plate map from a list of sample IDs
#' @details This is called in `generate_subsample_plate_map()` and fills samples in the 384-well
#' layout in blocks of four (A1, B1, A2 and B2 would all be the same sample but with different assays).
#' for more detail on this mapping structure, see `grunID::plate_v4_mapping`.
#' @param sample_ids list of sample IDs with which to populate a plate map
#' @returns A list of plate map tables
#' @export
#' @md
fill_single_assay_plate_map <- function(sample_ids) {

  samples_to_match_to_blocks <- tibble("sample_id" = sample_ids) |>
    mutate(id = row_number())

  sample_mapping <- grunID::plate_v4_mapping |>
    left_join(samples_to_match_to_blocks, by = c("sample_blocks" = "id")) |>
    mutate(row_number = match(rows, LETTERS),
           sample_id = ifelse(!is.na(control_blocks), control_blocks, sample_id)) |>
    select(row_number, cols, sample_blocks, sample_id) |>
    filter(!is.na(sample_id))

  out_table <- matrix(NA, nrow = 16, ncol = 24)
  no_sample_ids <- length(sample_mapping$sample_id)

  for(i in unique(sample_mapping$sample_id)) {
    fill_row <- sample_mapping |>
      filter(sample_id == i) |>
      pull(row_number)
    fill_col <- sample_mapping |>
      filter(sample_id == i) |>
      pull(cols)

    out_table[fill_row, fill_col] <- i
  }

  out_table[is.na(out_table)] <- ""
  out_df <- data.frame(out_table)

  names(out_df) <- c(1:24)
  rownames(out_df) <- LETTERS[1:16]

  out_df
}


