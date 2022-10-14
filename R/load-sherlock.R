#' @title Insert all results from Sherlock Output
#' @description  Adds results from Sherlock output after being processed via `process_sherlock()`
#' to the run-id database, as well as computed thresholds for run assignment.
#' @param con a connection to the database
#' @param transformed_assay_results a list of containing two tables, `raw_assay_results` and `assay_results` result from `process_sherlock`
#' @param sample_details dataframe containing sample information at each well
#' @examples
#' sample_details = readr::read_csv("data-raw/sample_layout_template.csv")
#' processed_results <- process_sherlock(filepath = "data-raw/exampleoutput_synergyH1trial_data_092021.xlsx",
#'                  sample_details = sample_details,
#'                  plate_size = 96)
#'
#' add_sherlock_results(con, processed_results, sample_details)
#' @md
#' @export
add_sherlock_results <- function(con, transformed_assay_results, sample_details) {

  # the final results
  add_assay_results(con = con,
                    transformed_assay_results = transformed_assay_results)
  # add the raw results
  add_raw_assay_results(con = con,
                        transformed_assay_results = transformed_assay_results)

  # get variables needed for trhresholds
  ids_from_layout <- layout$sample_id
  plate_run_id <- layout$plate_run_id[1]
  assay_type_id <- unique(layout$assay_id)

  protocol_id <- tbl(con, "plate_run") |>
    dplyr::filter(id  == plate_run_id) |>
    dplyr::select(protocol_id) |>
    pull()

  protocol_from_db <- get_protocols(con, id == protocol_id)
  last_time_val <- protocol_from_db$runtime

  blanks_for_threshold <- get_raw_results(con, sample_id %in% ids_from_layout,
                  time == last_time_val, layout_sample_number == "BLK")


  threshold <- mean(as.numeric(blanks_for_threshold$raw_fluorescence)) * 2

  # add value to join table that includes plate_run_id, assay_type_id, threshold
  add_run_type_threshold(con, plate_run_id, assay_type_id, threshold)


}

# get_assignment_threshold <- function(con, sample_ids, )


#' @export
get_raw_results <- function(con, ...) {
  dplyr::tbl(con, "raw_assay_results") |>
    dplyr::filter(...) |>
    dplyr::collect()

}

#' @export
get_plate_run <- function(con, ...) {
  dplyr::tbl(con, "plate_run") |>
    dplyr::filter(...) |>
    dplyr::collect()
}

#' @title Insert new Threshold
add_run_type_threshold <- function(con, plate_run_id, assay_type_id, threshold) {
  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  query <- glue::glue_sql("
  INSERT INTO plate_run_thresholds (plate_run_id, assay_id, threshold)
  VALUES ({plate_run_id}, {assay_type_id}, {threshold});",
                          .con = con)

  return(DBI::dbExecute(con, query))
}


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

  query <- glue::glue_sql("
  INSERT INTO assay_results (sample_id, sample_type_id, assay_id, rfu_back_subtracted, plate_run_id, well_location)
  VALUES (
    UNNEST(ARRAY[{transformed_assay_results$assay_results$sample_id*}]),
    UNNEST(ARRAY[{transformed_assay_results$assay_results$sample_type_id*}]),
    UNNEST(ARRAY[{transformed_assay_results$assay_results$assay_id*}]),
    UNNEST(ARRAY[{transformed_assay_results$assay_results$rfu_back_subtracted*}]),
    UNNEST(ARRAY[{transformed_assay_results$assay_results$plate_run_id*}]::int[]),
    UNNEST(ARRAY[{transformed_assay_results$assay_results$well_location*}]::well_location_enum[])
  );", .con = con)


  rows_created <- DBI::dbExecute(con, query)

  return(rows_created)

}

#' @title Add Raw Results
#' @export
add_raw_assay_results <- function(con, transformed_assay_results) {

  query <- glue::glue_sql("
  INSERT INTO raw_assay_results (sample_id, sample_type_id, assay_id, raw_fluorescence,
                                background_value, time, plate_run_id, well_location,
                                layout_sample_number)
  VALUES (
    UNNEST(ARRAY[{transformed_assay_results$raw_assay_results$sample_id*}]),
    UNNEST(ARRAY[{transformed_assay_results$raw_assay_results$sample_type_id*}]),
    UNNEST(ARRAY[{transformed_assay_results$raw_assay_results$assay_id*}]),
    UNNEST(ARRAY[{transformed_assay_results$raw_assay_results$raw_fluorescence*}]),
    UNNEST(ARRAY[{transformed_assay_results$raw_assay_results$background_value*}]),
    UNNEST(ARRAY[{transformed_assay_results$raw_assay_results$time*}]),
    UNNEST(ARRAY[{transformed_assay_results$raw_assay_results$plate_run_id*}]::int[]),
    UNNEST(ARRAY[{transformed_assay_results$raw_assay_results$well_location*}]::well_location_enum[]),
    UNNEST(ARRAY[{transformed_assay_results$raw_assay_results$layout_sample_number*}])
  );", .con = con)

  res <- DBI::dbSendQuery(con, query)

  return(res)
}


