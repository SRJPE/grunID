#' Extract Sherlock Protocol
#' @description helper function for parsing protocol settings within SynergyH1 output
#' @details TODO
#' @export
extract_sherlock_protocol <- function(filepath) {
  raw_metadata <- readxl::read_excel(
    filepath,
    range = "A2:B27",
    col_names = c("key", "value")
  ) |>
    tidyr::fill(key)

  # parse metadata elements
  software_version <- raw_metadata[1, 2, drop = TRUE]
  date <- as.Date(
    as.numeric(raw_metadata[6, 2, drop = TRUE]),
    origin = "1899-12-30"
  )
  reader_type <- raw_metadata[8, 2, drop = TRUE]
  reader_serial_number <- raw_metadata[9, 2, drop = TRUE]
  plate_type <- raw_metadata[13, 2, drop = TRUE]
  set_point <- as.numeric(stringr::str_extract(
    raw_metadata[15, 2, drop = TRUE],
    "[0-9]+"
  ))
  preheat_before_moving <- raw_metadata[16, 2, drop = TRUE] ==
    "Preheat before moving to next step"
  runtime <- stringr::str_extract(
    raw_metadata[17, 2, drop = TRUE],
    "(?<=Runtime\\s)\\d+:\\d+:\\d+"
  )
  interval <- stringr::str_extract(
    raw_metadata[17, 2, drop = TRUE],
    "(?<=Interval\\s)\\d+:\\d+:\\d+"
  )
  read_count <- as.integer(stringr::str_extract(
    raw_metadata[17, 2, drop = TRUE],
    "\\d+(?=\\sReads)"
  ))
  run_mode <- stringr::str_extract(
    raw_metadata[17, 1, drop = TRUE],
    "(?<=Start\\s)\\w+"
  )
  excitation <- as.integer(stringr::str_extract(
    raw_metadata[21, 2, drop = TRUE],
    "(?<=Excitation:\\s)\\d+"
  ))
  emissions <- as.integer(stringr::str_extract(
    raw_metadata[21, 2, drop = TRUE],
    "(?<=Emission:\\s)\\d+"
  ))
  optics <- stringr::str_extract(
    raw_metadata[22, 2, drop = TRUE],
    "(?<=Optics:\\s)\\w+"
  )
  gain <- as.integer(stringr::str_extract(
    raw_metadata[22, 2, drop = TRUE],
    "(?<=Gain:\\s)\\d+"
  ))
  light_source <- stringr::str_extract(
    raw_metadata[23, 2, drop = TRUE],
    "(?<=Light Source:\\s)\\w+ \\w+"
  )
  lamp_energy <- stringr::str_extract(
    raw_metadata[23, 2, drop = TRUE],
    "(?<=Lamp Energy:\\s)\\w+"
  )
  read_height <- as.integer(stringr::str_extract(
    raw_metadata[25, 2, drop = TRUE],
    "(?<=Read Height:\\s)\\d+"
  ))

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

#' Extract SherQUANT Protocol
#' @description helper function for parsing protocol settings within SherQuant output
#' @details This assumes that if 41 cycles are performed, the protocol was a 2 hour run time.
#' @export
extract_sherquant_protocol <- function(filepath) {
  raw_metadata <- readxl::read_xlsx(
    filepath,
    sheet = "Multicomponent Data",
    range = "A1:B45",
    col_names = c("key", "value")
  ) |>
    tidyr::pivot_wider(names_from = "key", values_from = "value")

  block_type <- raw_metadata$`Block Type`
  instrument_type <- raw_metadata$`Instrument Type`
  passive_reference <- raw_metadata$`Passive Reference`
  quantification_cycle_method <- raw_metadata$`Quantification Cycle Method`
  signal_smoothing_on <- raw_metadata$`Signal Smoothing On`
  chemistry <- raw_metadata$Chemistry
  instrument_name <- raw_metadata$`Instrument Name`
  instrument_serial_number <- raw_metadata$`Instrument Serial Number`
  stage_analysis_performed <- raw_metadata$`Stage/ Cycle where Analysis is performed`
  date_created <- as.Date(raw_metadata$`Date Created`)
  experiment_filename <- raw_metadata$`Experiment File Name`
  experiment_name <- raw_metadata$`Experiment Name`
  experiment_end_time <- raw_metadata$`Experiment Run End Time`
  experiment_user_name <- raw_metadata$`User Name`

  # parse
  plate_size <- readr::parse_number(block_type)

  # from multicomponent data tab
  read_count <- readxl::read_xlsx(
    filepath,
    sheet = "Multicomponent Data",
    skip = 47,
    col_names = c("well", "well_position", "cycle", "rox", "fam")
  ) |>
    dplyr::pull(cycle) |>
    max()

  runtime <- if (read_count == 41) hms::as_hms(2 * 3600) else NA

  metadata <- tibble::tibble(
    plate_size,
    runtime,
    read_count,
    block_type,
    instrument_type,
    passive_reference,
    quantification_cycle_method,
    signal_smoothing_on,
    chemistry,
    instrument_name,
    instrument_serial_number,
    stage_analysis_performed,
    date_created,
    experiment_filename,
    experiment_name,
    experiment_end_time,
    experiment_user_name
  )

  return(metadata)
}

#' Check Database Connection is Valid
is_valid_connection <- function(con) {
  if (!DBI::dbIsValid(con)) {
    cli::cli_abort(
      "Connection argument does not have a valid connection the run-id database.
                   Please try reconnecting to the database using 'DBI::dbConnect'",
      call. = FALSE
    )
  }
}
