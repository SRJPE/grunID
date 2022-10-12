#' @title Create Plate Run
#' @description blah
#' @export
add_plate_run <- function(con, protocol_id, genetic_method_id,
                          laboratory_id, lab_work_preformed_by, date_run) {

  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  query <- glue::glue_sql("
  INSERT INTO plate_run (protocol_id, genetic_method_id,  laboratory_id, lab_work_preformed_by, date_run)
  VALUES ({protocol_id}, {genetic_method_id}, {laboratory_id}, {lab_work_preformed_by}, {date_run}) RETURNING id;",
                 .con = con)

  res <- DBI::dbSendQuery(con, query)
  plate_run_id <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(plate_run_id$id)
}

#' @title Add assay results to run-id-database
#' @export
add_assay_results <- function(con, transformed_assay_results) {

  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
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


  assay_results_rows <- DBI::dbExecute(con, query)


  query <- glue::glue_sql("
  INSERT INTO raw_assay_results (sample_id, sample_type_id, assay_id, raw_fluorescence,
                                background_value, time, plate_run_id, well_location)
  VALUES (
    UNNEST(ARRAY[{raw_assay_results$sample_id*}]),
    UNNEST(ARRAY[{raw_assay_results$sample_type_id*}]),
    UNNEST(ARRAY[{raw_assay_results$assay_id*}]),
    UNNEST(ARRAY[{raw_assay_results$raw_fluorescence*}]),
    UNNEST(ARRAY[{raw_assay_results$background_value*}]),
    UNNEST(ARRAY[{raw_assay_results$time*}]),
    UNNEST(ARRAY[{raw_assay_results$plate_run_id*}]::int[]),
    UNNEST(ARRAY[{raw_assay_results$well_location*}]::well_location_enum[])
  );", .con = con)

  raw_assay_results_rows <- DBI::dbExecute(con, query)

  return(c("raw_results" = raw_assay_results_rows, "results" = assay_results_rows))
}



