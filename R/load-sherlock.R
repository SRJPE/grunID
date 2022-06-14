#' @title Create Plate Run
#' @description blah
#' @export
add_plate_run <- function(con, plate_run_settings) {

  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database",
         call. = FALSE)
  }

  # validate incoming data
  if (length(missing_names <- expected_protocol_colnames()[!tibble::has_name(plate_run_settings, expected_protocol_colnames())])) {
    stop(sprintf("the following columns are missing: %s",
                 paste0(missing_names, collapse = ", ")
                 ), call. = FALSE)
  }


  # write to table using the con object
  list2env(plate_run_settings, env = environment())

  query <- glue::glue_sql("
  INSERT INTO plate_run (software_version, date, reader_type, reader_serial_number, plate_type, set_point, preheat_before_moving, runtime, interval, read_count, run_mode, excitation, emissions, optics, gain, light_source, lamp_energy, read_height, genetic_method_id, laboratory_id, lab_work_preformed_by)
  VALUES ({software_version}, {date}, {reader_type}, {reader_serial_number}, {plate_type}, {set_point}, {preheat_before_moving}, {runtime}, {interval}, {read_count}, {run_mode}, {excitation}, {emissions}, {optics}, {gain}, {light_source}, {lamp_energy}, {read_height}, {genetic_method_id}, {laboratory_id}, {lab_work_preformed_by}) RETURNING id;",
                 .con = con)

  res <- DBI::dbSendQuery(con, query)
  plate_run_id <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(plate_run_id)
}

#' @export
add_assay_results <- function(con, transformed_assay_results) {

  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database",
         call. = FALSE)
  }

  list2env(transformed_assay_results, env = environment())

  query <- glue::glue_sql("
  INSERT INTO assay_results (sample_id, sample_type_id, assay_id, rfu_back_subtracted, plate_run_id, well_location)
  VALUES (
    UNNEST(ARRAY[{assay_results$sample_id*}]),
    UNNEST(ARRAY[{assay_results$sample_type_id*}]),
    UNNEST(ARRAY[{assay_results$assay_id*}]),
    UNNEST(ARRAY[{assay_results$rfu_back_subtracted*}]),
    UNNEST(ARRAY[{assay_results$plate_run_id*}]::int[]),
    UNNEST(ARRAY[{assay_results$well_location*}]::well_location_enum[])
  );", .con = con)



  return(DBI::dbExecute(con, query))

}



validate_plate_run_settings <- function(plate_run_settings) {
  # check names of colnames are correct
  expected_names <- c("plate_num", "software_version", "date", "reader_type", "reader_serial_number",
                      "plate_type", "set_point", "preheat_before_moving", "runtime",
                      "interval", "read_count", "run_mode", "excitation", "emissions",
                      "optics", "gain", "light_source", "lamp_energy", "read_height",
                      "genetic_method_", "laboratory_id", "lab_work_preformed_by"
  )

  if ((error_loc <- which(!(names(plate_run_settings) %in% expected_names)))) {
    return(FALSE)
  }


  return(TRUE)


}



expected_protocol_colnames <- function() {
  c("plate_num", "software_version", "date", "reader_type", "reader_serial_number",
    "plate_type", "set_point", "preheat_before_moving", "runtime",
    "interval", "read_count", "run_mode", "excitation", "emissions",
    "optics", "gain", "light_source", "lamp_energy", "read_height",
    "genetic_method_id", "laboratory_id", "lab_work_preformed_by"
  )
}
