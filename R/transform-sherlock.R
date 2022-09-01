#' Process Protocol File
#' @export
process_protocol_file <- function(protocol_file) {
  metadata <- process_sherlock_metadata("data-raw/exampleoutput_synergyH1trial_data_092021.xlsx")
  metadata$genetic_method_id <- 1
  metadata$laboratory_id <- 1
  metadata$lab_work_preformed_by <- "dog"

  return(metadata)
}


#' Process Sherlock Output
#' @param filepath path to excel file with Sherlock output
#' @export
process_sherlock <- function(sherlock_results_filepath, sample_layout_mapping,
                             plate_size = c(96, 384)) {



  metadata <- process_sherlock_metadata(sherlock_results_filepath)
  result_ranges <- get_result_ranges(plate_size)
  plate_layout <- process_plate_layout(sherlock_results_filepath,
                                       well_layout_range = result_ranges$layout)

  layout <- left_join(sample_layout_mapping, plate_layout)

  number_of_rows <- metadata$read_count

  # raw fluorescence ----
  column_index <- nrow(layout) + 3 # offset 3 columns
  if (plate_size == 96) {
    start_raw_fluorescence <- 43 # TODO (HARDCODE VALUE)
  } else {
    start_raw_fluorescence <- 43 # TODO (HARDCODE VALUE) what if bigger plate
  }
  end_row_raw_fluorescence <- start_raw_fluorescence + number_of_rows
  end_raw_fluorescence <- paste0(excel_column_index[column_index], end_row_raw_fluorescence)

  raw_fluorescence <- readxl::read_excel(sherlock_results_filepath,
                                         range = paste0("B", start_raw_fluorescence,":", end_raw_fluorescence)) %>%
    dplyr::mutate(Time = hms::as_hms(Time)) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::select(-dplyr::starts_with("T°")) %>%
    tidyr::pivot_longer(names_to = "location", values_to = "fluorescence", !Time) %>%
    dplyr::left_join(layout)

  # background values ---
  start_background_fluorescence <- end_row_raw_fluorescence + 4
  end_row_background_fluorescence <- start_background_fluorescence + number_of_rows
  end_background_fluorescence <- paste0(excel_column_index[column_index - 1], end_row_background_fluorescence)
  background_fluorescence <- readxl::read_excel(sherlock_results_filepath,
                                                range = paste0("B", start_background_fluorescence,":",
                                                               end_background_fluorescence)) %>%
    dplyr::mutate(Time = hms::as_hms(Time)) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(names_to = "location", values_to = "background_fluorescence", !Time)

  # raw results encoded as strings because of OVERFLOW and ????? values
  raw_assay_results <- raw_fluorescence %>%
    dplyr::left_join(background_fluorescence) %>%
    dplyr::select(sample_id, raw_fluorescence = fluorescence, background_value = background_fluorescence,
                  time = Time, plate_run_id, well_location = location)

  # results ---
  start_results <- end_row_background_fluorescence + 4
  end_row_results <- start_results + 4*8
  end_results <- paste0(excel_column_index[15], end_row_results)
  results <- readxl::read_excel(sherlock_results_filepath,
                                range = paste0("B", start_results,":", end_results),
                                col_types = "text") %>%
    tidyr::fill(...1) %>%
    tidyr::pivot_longer(names_to = "number_location", values_to = "RFU", !c(...1, ...14)) %>%
    dplyr::transmute(location = paste0(...1, number_location), metric = ...14, RFU = as.numeric(RFU)) %>%
    dplyr::left_join(layout) %>%
    dplyr::filter(!is.na(sample_id)) %>%
    dplyr::mutate(metric = stringr::str_remove(metric, "\\[.+\\]")) %>%
    dplyr::arrange(location) %>%
    dplyr::select(sample_id, sample_type_id, assay_id, rfu_back_subtracted = RFU,
                  plate_run_id, well_location = location)

  return(list(
    metadata = metadata,
    raw_assay_results = raw_assay_results,
    assay_results = results
  ))

}

#' Process Sherlock Metadata
process_sherlock_metadata <- function(filepath) {
  raw_metadata <- readxl::read_excel(filepath,
                                     range = "A2:B27",
                                     col_names = c("key", "value")) %>%
    tidyr::fill(key)

  # parse metadata elements
  plate_num <- raw_metadata[5, 2, drop = TRUE]
  software_version <- raw_metadata[1, 2, drop = TRUE]
  date <- as.Date(as.numeric(raw_metadata[6, 2, drop = TRUE]), origin = "1899-12-30")
  reader_type <- raw_metadata[8, 2, drop = TRUE]
  reader_serial_number <- raw_metadata[9, 2, drop = TRUE]
  plate_type <- raw_metadata[13, 2, drop = TRUE]
  set_point <- as.numeric(stringr::str_extract(raw_metadata[15, 2, drop = TRUE], "[0-9]+"))
  preheat_before_moving <- raw_metadata[16, 2, drop = TRUE] == "Preheat before moving to next step"
  runtime <- stringr::str_extract(raw_metadata[17, 2, drop = TRUE], "(?<=Runtime\\s)\\d+:\\d+:\\d+")
  interval <- stringr::str_extract(raw_metadata[17, 2, drop = TRUE], "(?<=Interval\\s)\\d+:\\d+:\\d+")
  read_count <- as.integer(stringr::str_extract(raw_metadata[17, 2, drop = TRUE], "\\d+(?=\\sReads)"))
  run_mode <- stringr::str_extract(raw_metadata[17, 1, drop = TRUE], "(?<=Start\\s)\\w+")
  excitation <- as.integer(stringr::str_extract(raw_metadata[21, 2, drop = TRUE], "(?<=Excitation:\\s)\\d+"))
  emissions <- as.integer(stringr::str_extract(raw_metadata[21, 2, drop = TRUE], "(?<=Emission:\\s)\\d+"))
  optics <- stringr::str_extract(raw_metadata[22, 2, drop = TRUE], "(?<=Optics:\\s)\\w+")
  gain <- as.integer(stringr::str_extract(raw_metadata[22, 2, drop = TRUE], "(?<=Gain:\\s)\\d+"))
  light_source <- stringr::str_extract(raw_metadata[23, 2, drop = TRUE], "(?<=Light Source:\\s)\\w+ \\w+")
  lamp_energy <- stringr::str_extract(raw_metadata[23, 2, drop = TRUE], "(?<=Lamp Energy:\\s)\\w+")
  read_height <- as.integer(stringr::str_extract(raw_metadata[25, 2, drop = TRUE], "(?<=Read Height:\\s)\\d+"))

  metadata <- tibble::tibble(
    plate_num,
    software_version,
    date,
    reader_type,
    reader_serial_number,
    plate_type,
    set_point,
    preheat_before_moving,
    runtime,
    interval,
    read_count,
    run_mode,
    excitation,
    emissions,
    optics,
    gain,
    light_source,
    lamp_energy,
    read_height
  )

  return(metadata)

}

#' mapping of plate layout location to sample identifier
process_plate_layout <- function(filepath) {
  plate_layout <- readxl::read_excel(filepath, range = "C32:N39",
                                     col_names = as.character(1:12)) %>%
    dplyr::mutate(letter = letters[1:8]) %>%
    tidyr::pivot_longer(names_to = "number", values_to = "psuedo_sample_id", !letter) %>%
    dplyr::transmute(location = toupper(paste0(letter, number)), psuedo_sample_id)
  return(plate_layout)
}


expected_layout_colnames <- function() {
  c("location", "sample_id", "sample_type_id", "assay_id", "plate_run_id")
}

plate_size = 384
wells_used = 331
time_intervals = 41

#' helper extract
#' @export
extract_previous_end_row <- function(cell_ranges) {
  as.numeric(
    stringr::str_extract(
      stringr::str_split(cell_ranges[length(cell_ranges)],
                         ":", simplify = TRUE)[2],
      "[0-9]+")
  )
}

#' Generate all cell ranges
#' @export
generate_ranges <- function(plate_size, wells_used, time_intervals) {

  if (plate_size == 96) {
    column_header_row <- 43
    result_row_count <- 8 * 4
  } else {
    column_header_row <- 51
    result_row_count <- 16 * 4
  }

  max_cells <- 96

  raw_fl_ranges <- generate_range("raw fluorescence", column_header_row)

  bk_fl_header_row <- extract_previous_end_row(raw_fl_ranges) + 4

  background_fl_ranges <- generate_range("background fluorescence", bk_fl_header_row)

  results_header_row  <- extract_previous_end_row(background_fl_ranges) + 4

  result_ranges <- generate_range("results", results_header_row)

}

#' @title Generate a cell range
#' @param wells_used the number of wells used in the well layout table
#' @export
generate_range <- function(table_type = c("raw fluorescence", "background fluorescence", "results"),
                           column_header_row) {

  if (table_type == "raw fluorescence") {
    start_col_index <- "D"
    end_col_index <- "CU"
    left_offset <- 3
  } else if (table_type == "background value") {
    start_col_index <- "C"
    end_col_index <- "CT"
    left_offset <- 2
  } else {
    start_col_index <- "B"
    end_col_index <- "AA"
    left_offset <- 1
  }

  if (table_type == "results") {
    return(sprintf("%s%d:%s%d", start_col_index, column_header_row,
                   end_col_index,
                   column_header_row + result_row_count))
  }

  if (wells_used <= max_cells) {
    return(sprintf("%s%d:%s%d", start_col_index, column_header_row,
                   grunID::excel_column_index[left_offset + wells_used],
                   column_header_row + time_intervals))
  }

  last_table_cols <- wells_used %% max_cells
  full_tables_count <- floor(wells_used/max_cells)

  letter_col_index <- c(rep(end_col_index, full_tables_count),
                       grunID::excel_column_index[left_offset + last_table_cols])
  cell_ranges <- character(full_tables_count + 1)

  for (i in seq_along(letter_col_index)) {
    end_index <- column_header_row + time_intervals
    cell_ranges[i] <- sprintf("%s%d:%s%d",
                              start_col_index,
                              column_header_row,
                              letter_col_index[i],
                              end_index)
    column_header_row <- end_index + 4
  }

  return(cell_ranges)
}







