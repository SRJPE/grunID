#' Add Enumerated Type Value
#' @description `add_enum_value()` appends a value to an existing enum
#' @details
#' Enumerated (enum) types are data types that comprise a static, ordered set of values.
#' They are useful for controlling the acceptable values within a column of a database
#' when a lookup table is overkill.
#'
#' Existing Enumerated Types:
#'
#' * **bin_code_enum** Used for generating sampling protocols and sample IDs
#' * **survey_type_enum** Used for describing the survey types in permits
#' * **life_stage_enum** Used for specifying chinook life stage in permits
#' * **origin_enum** Used for specifying the chinook origin in permits
#' * **run_mode_enum** Used for describing the run mode in the Synergy H1 protocol
#' * **optics_enum** Used for describing the optics setting in the Synergy H1 protocol
#' * **light_source_enum** Used for describing the light source in the Synergy H1 protocol
#' * **lamp_energy_enum** Used for describing the lamp energy setting in the Synergy H1 protocol
#' * **well_location_enum** Used for mapping the Synergy H1 result output to sample
#' IDs well location in the assay results
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#'
#' # first confirm the new value is not redundant to existing values
#' get_enum_values(con, "lamp_energy_enum")
#'
#' add_enum_value(con, "lamp_energy_enum", "Low")
#' @family enum method functions
#' @export
#' @md
add_enum_value <- function(con,
                     enum = c("bin_code_enum", "survey_type_enum", "life_stage_enum",
                              "origin_enum", "run_mode_enum", "optics_enum",
                              "light_source_enum", "lamp_energy_enum", "well_location_enum"),
                     new_enum_value) {

  enum <- match.arg(enum)

  is_valid_connection(con)

  is_valid_enum(enum, new_enum_value)

  # TODO fix syntax
  query <- glue::glue_sql("ALTER TYPE {enum} ADD VALUE IF NOT EXISTS {new_enum_value};",
                          .con = con)

  DBI::dbExecute(con, query)

}


is_valid_enum <- function(enum, new_enum_value) {

  if (!is.character(new_enum_value)) {
    stop("New enum value must be a character")
  }

  if (enum %in% c("survey_type_enum", "life_stage_enum",
                  "origin_enum", "run_mode_enum", "optics_enum",
                  "light_source_enum", "lamp_energy_enum") &&
      !grepl("^[A-Z].", new_enum_value)) {
    stop("New enum value must begin with a capital letter")
  }

  if (enum == "bin_code_enum" && !(new_enum_value %in% LETTERS)) {
    stop("Bin code must be a single capital letter")
  }

  if (enum == "well_location_enum" && !grepl("^[A-Z][0-9]*$", new_enum_value) ) {
    stop("Well location value must be an alpha numeric code with the first
         digit being a capital letter followed by a number (e.g., A1)")
  }

}

