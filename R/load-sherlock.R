#' Generate Thresholds
#' @export
generate_threshold <- function(con, plate_run_id) {

  protocol_id <- tbl(con, "plate_run") |>
    dplyr::filter(id == plate_run_id) |>
    dplyr::pull(protocol_id)

  last_time_val <- tbl(con, "protocol") |>
    dplyr::filter(id == protocol_id) |>
    dplyr::pull(runtime)


  control_blanks <- tbl(con, "assay_result") |>
    filter(time == last_time_val,
           sample_id == "CONTROL",
           plate_run_id == plate_run_id) |>
    collect()

  thresholds <- control_blanks |>
    group_by(plate_run_id, assay_id) |>
    summarise(
      threshold = mean(as.numeric(raw_fluorescence)) * 2
    ) |> ungroup()

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

  plate_run_id <- unique(thresholds$plate_run_id)

  if (length(plate_run_id) > 1) {
    stop("TODO")
  }

  detection_results <- dplyr::tbl(con, "assay_result") |>
    dplyr::filter(plate_run_id == plate_run_id,
           sample_id != "CONTROL") |>
    dplyr::collect() |>
    dplyr::left_join(thresholds, by = c("assay_id" = "assay_id")) |>
    dplyr::mutate(positive_detection = raw_fluorescence > threshold) |>
    dplyr::select(sample_id, positive_detection)

  positive_ids <- detection_results |>
    dplyr::filter(positive_detection) |>
    dplyr::pull(sample_id)

  negative_ids <- detection_results |>
    dplyr::filter(!positive_detection) |>
    dplyr::pull(sample_id)

  positive_query <- glue::glue_sql("
  UPDATE assay_result SET positive_detection = true
  WHERE sample_id IN ({positive_ids*});",
                          .con = con)

  negative_query <- glue::glue_sql("
  UPDATE assay_result SET positive_detection = false
  WHERE sample_id IN ({negative_ids*});",
                          .con = con)

  positive_results <- DBI::dbExecute(con, positive_query)
  negative_results <- DBI::dbExecute(con, negative_query)

  return(positive_results + negative_results)

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
add_assay_results <- function(con, assay_results) {

  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  query <- glue::glue_sql("
  INSERT INTO assay_result (sample_id, sample_type_id, assay_id, rfu_back_subtracted, plate_run_id, well_location)
  VALUES (
    UNNEST(ARRAY[{assay_results$sample_id*}]),
    UNNEST(ARRAY[{assay_results$sample_type_id*}]),
    UNNEST(ARRAY[{assay_results$assay_id*}]),
    UNNEST(ARRAY[{assay_results$rfu_back_subtracted*}]),
    UNNEST(ARRAY[{assay_results$plate_run_id*}]::int[]),
    UNNEST(ARRAY[{assay_results$well_location*}]::well_location_enum[])
  );", .con = con)


  res <- DBI::dbExecute(con, query)

  return(res)

}

#' @title Add Raw Results
#' @export
add_assay_results <- function(con, assay_results) {

  query <- glue::glue_sql("
  INSERT INTO assay_result (sample_id, sample_type_id, assay_id, raw_fluorescence,
                                background_value, time, plate_run_id, well_location)
  VALUES (
    UNNEST(ARRAY[{assay_results$sample_id*}]),
    UNNEST(ARRAY[{assay_results$sample_type_id*}]),
    UNNEST(ARRAY[{assay_results$assay_id*}]),
    UNNEST(ARRAY[{assay_results$raw_fluorescence*}]),
    UNNEST(ARRAY[{assay_results$background_value*}]),
    UNNEST(ARRAY[{assay_results$time*}]),
    UNNEST(ARRAY[{assay_results$plate_run_id*}]::int[]),
    UNNEST(ARRAY[{assay_results$well_location*}]::well_location_enum[])
  );", .con = con)

  res <- DBI::dbExecute(con, query)

  return(res)
}


