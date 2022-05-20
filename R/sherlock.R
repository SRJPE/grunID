#' Process Sherlock Output
#' @param filepath path to excel file with Sherlock output
process_sherlock <- function(filepath) {

  # # metadata ----
  # metadata <- readxl::read_excel(filepath,
  #                                range = "A2:B27",
  #                                col_names = c("key", "value")) %>%
  #   fill(key)
  #
  # runtime_metadata <- metadata %>%
  #   filter(key == "Start Kinetic") %>%
  #   pull(value) %>%
  #   str_split(",") %>%
  #   unlist() %>%
  #   str_trim() %>%
  #   set_names("Runtime", "Interval", "Number of Reads")
  #
  # # TODO should we remove the readr:: dep

  runtime_metadata <- process_sherlock_metadata(filepath)

  number_of_rows <- readr::parse_number(runtime_metadata["Number of Reads"])

  # mapping of plate layout location to sample identifier
  plate_layout <- readxl::read_excel(filepath, range = "C32:N39",
                             col_names = as.character(1:12)) %>%
    dplyr::mutate(letter = letters[1:8]) %>%
    tidyr::pivot_longer(names_to = "number", values_to = "sample_id", !letter) %>%
    dplyr::transmute(location = toupper(paste0(letter, number)), sample_id)

  # raw fluorescence ----
  column_index <- sum(!is.na(plate_layout$sample_id)) + 3 # offset 3 columns
  start_raw_fluorescence <- 43 # TODO (HARDCODE VALUE)
  end_row_raw_fluorescence <- start_raw_fluorescence + number_of_rows
  end_raw_fluorescence <- paste0(excel_column_index[column_index], end_row_raw_fluorescence)
  raw_fluorescence <- read_excel(filepath,
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
    dplyr::mutate(Time = as_hms(Time)) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(names_to = "location", values_to = "background_fluorescence", !Time) %>%
    dplyr::left_join(plate_layout)

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
  metadata <- readxl::read_excel(filepath,
                                 range = "A2:B27",
                                 col_names = c("key", "value")) %>%
    fill(key)

  software_version <- metadata[1, 2, drop = TRUE]
  plate_run <- metadata[5, 2, drop = TRUE]
  date <- as.Date(as.numeric(metadata[6, 2, drop = TRUE]), origin = "1899-12-30")
  reader_type <- metadata[8, 2, drop = TRUE]
  reader_serial_number <- metadata[9, 2, drop = TRUE]
  plate_type <- metadata[13, 2, drop = TRUE]
  set_point <- as.numeric(stringr::str_extract(metadata[15, 2, drop = TRUE], "[0-9]+"))
  preheat_before_moving <- metadata[16, 2, drop = TRUE] == "Preheat before moving to next step"
  runtime_elements <- stringr::str_split(metadata[17, 2, drop = TRUE], " ", simplify = TRUE)
  runtime <- runtime_elements[1, 2]
  interval <- stringr::str_replace(runtime_elements[1, 5], ",", "")
  read_count <- as.integer(runtime_elements[1, 6])
  run_mode <- stringr::str_split(metadata[17, 1, drop = TRUE], " ")[[1]][2]
  ee <- as.integer(stringr::str_extract_all(metadata[21, 2, drop = TRUE], "[0-9]+")[[1]])
  excitation <- ee[1]
  emissions <- ee[2]
  og <- stringr::str_split(metadata[22, 2, drop = TRUE], " ")[[1]]
  optics <- stringr::str_replace(og[2], ",", "")
  gain <- as.integer(og[5])
  lamp_energy <- str_extract(metadata[23, 2, drop = TRUE], "(?<=Lamp Energy:\\s)\\w+")
  light_source <- str_extract(metadata[23, 2, drop = TRUE], "(?<=Light Source:\\s)\\w+ \\w+")



  runtime_metadata <- metadata %>%
    filter(key == "Start Kinetic") %>%
    pull(value) %>%
    str_split(",") %>%
    unlist() %>%
    str_trim() %>%
    set_names("Runtime", "Interval", "Number of Reads")

  # TODO should we remove the readr:: dep
  number_of_rows <- readr::parse_number(runtime_metadata["Number of Reads"])
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
