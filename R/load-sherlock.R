#' Generate Thresholds
#' @export
generate_threshold <- function(con, plate_run) {

  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  protocol_id <- dplyr::tbl(con, "plate_run") |>
    dplyr::filter(id == plate_run) |>
    dplyr::pull(protocol_id)

  runtime <- dplyr::tbl(con, "protocol") |>
    dplyr::filter(id == protocol_id) |>
    dplyr::pull(runtime)

  control_blanks <- dplyr::tbl(con, "raw_assay_result") |>
    dplyr::filter(time == runtime,
           sample_id == "CONTROL",
           plate_run_id == plate_run) |>
    dplyr::collect()

  thresholds <- control_blanks |>
    dplyr::group_by(plate_run_id, assay_id) |>
    dplyr::summarise(
      threshold = mean(as.numeric(raw_fluorescence)) * 2
    ) |> ungroup() |>
    dplyr::mutate(runtime = runtime)

  return(thresholds)
}

#' @title Set detection on assay results
#' @export
update_assay_detection <- function(con, thresholds) {

  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  plate_run <- unique(thresholds$plate_run_id)
  runtime <- unique(thresholds$runtime)

  if (length(plate_run_id) > 1) {
    stop("TODO")
  }

  if (length(runtime) > 1) {
    stop("TODO")
  }

  detection_results <- dplyr::tbl(con, "raw_assay_result") |>
    dplyr::filter(plate_run_id == plate_run,
           sample_id != "CONTROL",
           time == runtime) |>
    dplyr::collect() |>
    dplyr::left_join(thresholds, by = c("assay_id" = "assay_id", "plate_run_id" = "plate_run_id")) |>
    dplyr::mutate(positive_detection = raw_fluorescence > threshold) |>
    dplyr::select(sample_id, assay_id, raw_fluorescence,
                  threshold, positive_detection, plate_run_id)

  query <- glue::glue_sql("
  INSERT INTO assay_result (sample_id, assay_id, raw_fluorescence, threshold,
                            positive_detection, plate_run_id)
  VALUES (
          UNNEST(ARRAY[{detection_results$sample_id*}]),
          UNNEST(ARRAY[{detection_results$assay_id*}]),
          UNNEST(ARRAY[{detection_results$raw_fluorescence*}]),
          UNNEST(ARRAY[{detection_results$threshold*}]),
          UNNEST(ARRAY[{detection_results$positive_detection*}]),
          UNNEST(ARRAY[{detection_results$plate_run_id*}]::int[])
  );", .con = con)

  return(DBI::dbExecute(con, query))
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
  );", .con = con)

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


#' @title Add Raw Results
#' @export
add_raw_assay_results <- function(con, assay_results) {

  res <- DBI::dbAppendTable(con, "raw_assay_result", assay_results)

  return(res)
}

#' @export
add_genetic_identification <- function(con, plate_run) {
  assay_results <- dplyr::tbl(con, "assay_result") |>
    dplyr::filter(plate_run_id == plate_run) |>
    dplyr::collect()

  run_types <- assay_results |>
    dplyr::filter(positive_detection) |>
    dplyr::mutate(
      run_type_id = dplyr::case_when(
        assay_id == 1 ~ 6,
        assay_id == 2 ~ 5,
        assay_id == 3 ~ 1,
        assay_id == 4 ~ 4
      )
    ) |>
    dplyr::select(sample_id, run_type_id)

  query <- glue::glue_sql("
  INSERT INTO genetic_run_identification (sample_id, run_type_id)
  VALUES (
    UNNEST(ARRAY[{run_types$sample_id*}]),
    UNNEST(ARRAY[{run_types$run_type_id*}])
  );
  ", .con = con)

  return(DBI::dbExecute(con, query))

}











