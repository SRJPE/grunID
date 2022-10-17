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
  results_added <- add_assay_results(con = con,
                    transformed_assay_results = transformed_assay_results)
  # add the raw results
  raw_results_added <- add_raw_assay_results(con = con,
                        transformed_assay_results = transformed_assay_results)

  # get variables needed for thresholds
  ids_from_layout <- layout$sample_id
  plate_run_id <- layout$plate_run_id[1]
  assay_type_id <- unique(layout$assay_id)

  protocol_id <- tbl(con, "plate_run") |>
    dplyr::filter(id  == plate_run_id) |>
    dplyr::select(protocol_id) |>
    pull()

  protocol_from_db <- get_protocols(con, id == protocol_id)
  last_time_val <- protocol_from_db$runtime

  blanks_for_threshold <- tbl(con, "raw_assay_results") |>
    filter(sample_id %in% ids_from_layout,
           time == last_time_val,
           layout_sample_number == "BLK") |>
    collect()

  thresholds <- blanks_for_threshold |>
    group_by(assay_id) |>
    summarise(
      threshold = mean(as.numeric(raw_fluorescence)) * 2
    ) |>
    mutate(plate_run_id = plate_run_id)

  # add value to join table that includes plate_run_id, assay_type_id, threshold
  thresholds_added <- add_run_type_threshold(con,
                                             thresholds$plate_run_id,
                                             thresholds$assay_id,
                                             thresholds$threshold)


  # update genetic run id based on the thresholds
  threshold_tbl <- tbl(con, "plate_run_thresholds") |>
    select(plate_run_id, assay_id, threshold)

  sample_id_with_genetic_types <- tbl(con, "raw_assay_results") |>
    filter(sample_id %in% ids_from_layout) |>
    select(sample_id, raw_fluorescence, assay_id, plate_run_id) |>
    left_join(threshold_tbl) |>
    mutate(run_type_id = ifelse(raw_fluorescence > threshold, 2, NA)) |> # TODO this part is WRONG!!
    select(sample_id, assay_id, run_type_id) |>
    collect()


  add_genetic_run_type(con, sample_id_with_genetic_types$sample_id,
                       sample_id_with_genetic_types$run_type_id)

  return(c("results" = results_added, "raw_results" = raw_results_added,
           "thresholds" = thresholds_added))


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
  VALUES (
          UNNEST(ARRAY[{plate_run_id*}]),
          UNNEST(ARRAY[{assay_type_id*}]),
          UNNEST(ARRAY[{threshold*}])
  );",
                          .con = con)

  return(DBI::dbExecute(con, query))
}

#' @title Add run type to genetic table
add_genetic_run_type <- function(con, sample_id, run_type_id) {
  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  query <- glue::glue_sql("
  INSERT INTO genetic_run_identification (sample_id, run_type_id)
  VALUES (
          UNNEST(ARRAY[{sample_id*}]),
          UNNEST(ARRAY[{run_type_id*}]::int[])
  );",
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


  res <- DBI::dbExecute(con, query)

  return(res)

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

  res <- DBI::dbExecute(con, query)

  return(res)
}


