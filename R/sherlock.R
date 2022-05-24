#' Create plate layout
#' before running plate, recording mapping of samples to wells
#' \example
#' plate_run_id, sample_id, well_id
#' 1, dofji, A3
#' 1, dkfoi, A4
duh <- function(con, filepath) {
  layout_mapping <- read_csv(filepath)
  res <- write_table(con, layout_mapping)
  return(res)
}

#' Process Sherlock Output
#' @param filepath path to excel file with Sherlock output
process_sherlock <- function(filepath, layout_mapping, plate_size = c(96)) {

  metadata <- process_sherlock_metadata(filepath)
  plate_layout <- process_plate_layout(filepath)

  number_of_rows <- metadata$read_count

  # raw fluorescence ----
  column_index <- sum(!is.na(plate_layout$sample_id)) + 3 # offset 3 columns
  if (plate_size == 96) {
    start_raw_fluorescence <- 43 # TODO (HARDCODE VALUE)
  } else {
    start_raw_fluorescence <- 43 # TODO (HARDCODE VALUE) what if bigger plate
  }
  end_row_raw_fluorescence <- start_raw_fluorescence + number_of_rows
  end_raw_fluorescence <- paste0(excel_column_index[column_index], end_row_raw_fluorescence)

  raw_fluorescence <- readxl::read_excel(filepath,
                                 range = paste0("B", start_raw_fluorescence,":", end_raw_fluorescence)) %>%
    dplyr::mutate(Time = hms::as_hms(Time)) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::select(-dplyr::starts_with("T°")) %>%
    tidyr::pivot_longer(names_to = "location", values_to = "fluorescence", !Time) %>%
    dplyr::left_join(plate_layout)

  # background values ---
  start_background_fluorescence <- end_row_raw_fluorescence + 4
  end_row_background_fluorescence <- start_background_fluorescence + number_of_rows
  end_background_fluorescence <- paste0(excel_column_index[column_index - 1], end_row_background_fluorescence)
  background_fluorescence <- readxl::read_excel(filepath,
                                        range = paste0("B", start_background_fluorescence,":",
                                                       end_background_fluorescence)) %>%
    dplyr::mutate(Time = hms::as_hms(Time)) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(names_to = "location", values_to = "background_fluorescence", !Time)

  raw_fluorescence %>%
    left_join(background_fluorescence)
    select(raw_fluorescence, background_value, time, location)
  # results ---
  start_results <- end_row_background_fluorescence + 4
  end_row_results <- start_results + 4*8
  end_results <- paste0(excel_column_index[15], end_row_results)
  results <- readxl::read_excel(filepath,
                        range = paste0("B", start_results,":", end_results),
                        col_types = "text") %>%
    fill(...1) %>%
    tidyr::pivot_longer(names_to = "number_location", values_to = "RFU", !c(...1, ...14)) %>%
    dplyr::transmute(location = paste0(...1, number_location), metric = ...14, RFU) %>%
    dplyr::left_join(plate_layout) %>%
    dplyr::filter(!is.na(sample_id)) %>%
    dplyr::mutate(metric = stringr::str_remove(metric, "\\[.+\\]")) %>%
    dplyr::arrange(location)


  list(
    metadata = metadata,
    raw_results = NA,
    assay_result = results
  )

}

#' Process Sherlock Metadata
process_sherlock_metadata <- function(filepath) {
  raw_metadata <- readxl::read_excel(filepath,
                                 range = "A2:B27",
                                 col_names = c("key", "value")) %>%
    fill(key)

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
  light_source <- str_extract(raw_metadata[23, 2, drop = TRUE], "(?<=Light Source:\\s)\\w+ \\w+")
  lamp_energy <- str_extract(raw_metadata[23, 2, drop = TRUE], "(?<=Lamp Energy:\\s)\\w+")
  read_height <- as.integer(str_extract(raw_metadata[25, 2, drop = TRUE], "(?<=Read Height:\\s)\\d+"))

  metadata <- tibble(
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
    tidyr::pivot_longer(names_to = "number", values_to = "sample_id", !letter) %>%
    dplyr::transmute(location = toupper(paste0(letter, number)), sample_id)
  return(plate_layout)
}



# id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
# software_version VARCHAR(50) NOT NULL,
# plate_num INTEGER NOT NULL,
# date TIME NOT NULL,
# reader_type VARCHAR(50) NOT NULL,
# reader_serial_number VARCHAR(50) NOT NULL,
# plate_type plate_type_enum NOT NULL,
# set_point VARCHAR(50) NOT NULL,
# preheat_before_moving BOOLEAN NOT NULL,
# runtime VARCHAR(10) NOT NULL,
# interval VARCHAR(10) NOT NULL,
# read_count INTEGER NOT NULL,
# run_mode run_mode_enum NOT NULL,
# excitation INTEGER NOT NULL,
# emissions INTEGER NOT NULL,
# optics optics_enum NOT NULL,
# gain INTEGER NOT NULL,
# light_source light_source_enum NOT NULL,
# lamp_energy lamp_energy_enum NOT NULL,
# read_height INTEGER NOT NULL, --in mm
# created_at TIMESTAMP DEFAULT NOW(),
# created_by VARCHAR(30) DEFAULT CURRENT_USER,
# updated_at TIMESTAMP DEFAULT NOW(),
# updated_by VARCHAR(30) DEFAULT CURRENT_USER
