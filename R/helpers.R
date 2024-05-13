#' Extract Sherlock Protocol
#' @description helper function for parsing protocol settings within SynergyH1 output
#' @details TODO
#' @export
extract_sherlock_protocol <- function(filepath) {
  raw_metadata <- readxl::read_excel(filepath,
                                     range = "A2:B27",
                                     col_names = c("key", "value")) |>
    tidyr::fill(key)

  # parse metadata elements
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
    software_version,
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

#' Check Database Connection is Valid
is_valid_connection <- function(con) {
  if (!DBI::dbIsValid(con)) {
    cli::cli_abort("Connection argument does not have a valid connection the run-id database.
                   Please try reconnecting to the database using 'DBI::dbConnect'",
                   call. = FALSE)
  }
}

