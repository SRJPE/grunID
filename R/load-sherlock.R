#' Generate Thresholds Online
#' @description `generate_threshold()` calculates the raw fluorescence threshold
#' values for an assay.
#' @param con valid connection to the database
#' @param plate_run plate run object obtained from either `add_plate_run` or `get_plate_run`
#' @param .control_id the identifier within the plate run to use as control for calculating thresholds, defaults to "NTC"
#' @details For each assay on a plate run, the threshold value is calculated as two times
#' the mean value of the last time step from the control blank wells. Each
#' assay on a plate will have its own control blanks and threshold value.
#' @returns a table containing thresholds for an event, to be passed to `update_assay_detections()`
#' @export
generate_threshold <- function(con, plate_run, .control_id="NTC") {

  if (attr(plate_run, "offline")) {
    return (generate_threshold_offline(plate_run, .control_id = .control_id))
  } else {


    if (!DBI::dbIsValid(con)) {
      stop("Connection argument does not have a valid connection the run-id database.
       Please try reconnecting to the database using 'DBI::dbConnect'",
           call. = FALSE)
    }
    plate_run_identifier <- plate_run$plate_run_id

    protocol_id <- dplyr::tbl(con, "plate_run") |>
      dplyr::filter(id == !!plate_run_identifier) |>
      dplyr::pull(protocol_id)

    runtime <- dplyr::tbl(con, "protocol") |>
      dplyr::filter(id == !!protocol_id) |>
      dplyr::pull(runtime)

    control_blanks <- dplyr::tbl(con, "raw_assay_result") |>
      dplyr::filter(time == runtime,
                    sample_id == !!.control_id,
                    plate_run_id == !!plate_run_identifier) |>
      dplyr::collect()

    if (nrow(control_blanks) == 0) {
      stop(paste0("no control variables found in plate run with id: '", plate_run_identifier, "'"), call. = FALSE)
    }

    thresholds <- control_blanks |>
      dplyr::group_by(plate_run_id, assay_id) |>
      dplyr::summarise(
        threshold = mean(as.numeric(raw_fluorescence)) * 2
      ) |> dplyr::ungroup() |>
      dplyr::mutate(runtime = runtime)

    return(thresholds)
  }
}


#' Generate Thresholds Offline
#' @description `generate_threshold_offline()` calculates the raw fluorescence threshold
#' values for an assay offline
#' @param .control_id the identifier within the plate run to use as control for calculating thresholds, defaults to "NTC"
#' @param offline_sherlock_results if running offline, this function requires a table of sherlock results
#' @details For each assay on a plate run, the threshold value is calculated as two times
#' the mean value of the last time step from the control blank wells. Each
#' assay on a plate will have its own control blanks and threshold value.
#' @returns a table containing thresholds for an event, to be passed to `update_assay_detections()`
#' @export
generate_threshold_offline <- function(offline_sherlock_results, .control_id="NTC") {

  control_blanks <- offline_sherlock_results$data |>
    dplyr::filter(sample_id == !!.control_id)

  thresholds <- control_blanks |>
    dplyr::group_by(assay_id) |>
    dplyr::summarise(
      threshold = mean(as.numeric(raw_fluorescence)) * 2
    ) |> dplyr::ungroup()

  return(thresholds)
}


#' @title Set detection for assay results online
#' @description `update_assay_detection()` updates the assay result table with
#' positive detections and, depending on the assay, the genetic run type
#' identification.
#' @param con valid connection to the database
#' @param thresholds threshold values calculated in `generate_threshold`
#' @param .control_id identifier used to find the control variable
#' @details The assay result table is updated to reflect whether the assays
#' in a plate run produced raw fluorescence values that exceed the threshold
#' calculated by `generate_threshold()`, resulting in a positive or negative
#' detection (TRUE or FALSE, where TRUE means the assay is a positive
#' detection). The database is then checked for whether other ots_28 and
#' ots_16 have been run on the samples. If so, samples are added to
#' the genetic_run_identification table in the database with a genetic
#' identification number (see `add_genetic_identification` for details).
#' @returns The number of assay results added to the assay_result table
#' and the number of samples updated in the genetic_run_identification table.
#' @export
update_assay_detection <- function(con, thresholds, .control_id = "NTC") {

  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
       Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  plate_run <- unique(thresholds$plate_run_id)
  runtime <- unique(thresholds$runtime)

  if (length(plate_run) > 1) {
    stop("TODO")
  }

  if (length(runtime) > 1) {
    stop("TODO")
  }

  # check if the plate run already exists in the assay_results
  this_plate_run_exists_in_results <- nrow(
    dplyr::collect(dplyr::tbl(con, "assay_result") |> dplyr::filter(plate_run_id == plate_run))
  )

  if (this_plate_run_exists_in_results) {
    stop("the plate run you are trying to upload already exists in the database", call. = FALSE)
  }

  detection_results <- dplyr::tbl(con, "raw_assay_result") |>
    dplyr::filter(plate_run_id == plate_run,
                  sample_id != .control_id,
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
          {detection_results$sample_id},
          {detection_results$assay_id},
          {detection_results$raw_fluorescence},
          {detection_results$threshold},
          {detection_results$positive_detection},
          {detection_results$plate_run_id}::int
  );", .con = con)

  assay_results_added <- purrr::map_dbl(query, function(q) {
    DBI::dbExecute(con, q)
  },
  .progress = list(
    type = "iterator",
    clear = FALSE,
    name = "inserting threshold result into database"
  ))


  genetic_ids_added <- add_genetic_identification(con, unique(detection_results$sample_id),
                                                  online_mode = !!online_mode)

  return(c("Assay records added" = sum(assay_results_added),
           "Samples assigned run type" = genetic_ids_added))
}


#' @title Generate detection for assay results offline
#' @description `generate_assay_detection()` generates an assay result table with
#' positive detections and, depending on the assay, the genetic run type
#' identification, without interacting with the database
#' @param thresholds threshold values calculated in `generate_threshold_offline`
#' @param offline_sherlock_results sherlock results table provided by `process_sherlock`
#' @details The sherlock results are updated to reflect whether the assays
#' in a plate run produced raw fluorescence values that exceed the threshold
#' calculated by `generate_threshold_offline`, resulting in a positive or negative
#' detection (TRUE or FALSE, where TRUE means the assay is a positive
#' detection). The function currently uses the positive detection value for the
#' sample ID at the end of the run time (i.e. the max run time).
#' @returns A table of samples processed with `sample_id`, `assay_id`, `raw_fluorescence`,
#' `threshold`, and `positive_detection`.
#' @export
generate_assay_detection <- function(thresholds, offline_sherlock_results, .control_id = "NTC") {

  detection_results <- offline_sherlock_results$data |>
    dplyr::group_by(sample_id) |>
    dplyr::top_n(1, time) |>
    dplyr::ungroup() |>
    dplyr::filter(sample_id != .control_id) |>
    dplyr::left_join(thresholds, by = "assay_id") |>
    dplyr::mutate(positive_detection = raw_fluorescence > threshold) |>
    dplyr::select(sample_id, assay_id, raw_fluorescence,
                  threshold, positive_detection)

  # TODO update add_genetic_identification update
  # genetic_ids_added <- add_genetic_identification(unique(detection_results$sample_id),
  #                                                 online_mode = !!online_mode)

  return(detection_results)

}


check_results_complete <- function(con, sample_identifiers) {
  results_complete <- dplyr::tbl(con, "assay_result") |>
    dplyr::filter(sample_id %in% sample_identifiers) |>
    dplyr::select(sample_id, assay_id, positive_detection) |>
    dplyr::collect() |>
    tidyr::pivot_wider(names_from = "assay_id", values_from = "positive_detection")

  all_ots_results <- dplyr::bind_rows(
    tibble::tibble(sample_id = "DELETE_ME", `1` = FALSE, `2` = FALSE, `3` = FALSE, `4` = FALSE),
    results_complete
  ) |>
    dplyr::group_by(sample_id) |>
    dplyr::transmute(sample_id,
                     status_code_id = dplyr::case_when(
                       `1` & !`2` ,
                       run_type_id == 5 ~ 11,
                       run_type_id == 1 ~ 11,
                       run_type_id == 4 ~ 11,
                       ots16 & run_type_id == 8 ~ 10,
                       ots28 & run_type_id == 8 ~ 7,
                       ots16 & run_type_id == 7 ~ 9,
                       ots28 & run_type_id == 7 ~ 6
                     ),
                     ots_28 = all(!is.na(`1`), !is.na(`2`)),
                     ots_16 = all(!is.na(`3`), !is.na(`4`))) |>
    dplyr::filter(sample_id != "DELETE_ME", (ots_28 | ots_16)) |>
    dplyr::ungroup()

  return(all_ots_results)
}


#' @title Add Raw Results
#' @export
add_raw_assay_results <- function(con, assay_results) {

  res <- DBI::dbAppendTable(con, "raw_assay_result", assay_results$data)

  return(c("raw assay results added" = res))
}



#' @title Genetic Identification
#' @description `add_genetic_identification` assigns genetic identifier to a
#' sample based on assay results.
#' @param con valid connection to database
#' @param sample_identifiers identifiers for samples to be added
#' @param online_mode whether the function is to be run in online (interacting with database) or offline mode
#' @param offline_assay_results results produced in offline mode, table with assay IDs and positive/negative detection
#' @details `add_genetic_identification` checks the database for all existing
#' assay results for a sample identifier, then uses those to assign a
#' genetic identification value. The genetic_run_identification table in the
#' database is updated with the genetic identification value. The genetic
#' identification values are dependent on the assay results:
#' 1: high value for
add_genetic_identification <- function(con, sample_identifiers) {

  is_valid_connection(con)

  assay_detections <- dplyr::tbl(con, "assay_result") |>
    dplyr::filter(sample_id %in% sample_identifiers) |>
    dplyr::select(sample_id, assay_id, positive_detection) |>
    dplyr::collect() |>
    dplyr::mutate(assay_id_name = dplyr::case_when(assay_id == 1 ~ "ots_28_e",
                                                   assay_id == 2 ~ "ots_28_l",
                                                   assay_id == 3 ~ "ots_16_s",
                                                   assay_id == 4 ~ "ots_16_w"),
                  assay_id_name = factor(assay_id_name, levels = c("ots_28_e","ots_28_l","ots_16_s","ots_16_w"))) |>
    dplyr::select(-assay_id) |>
    tidyr::pivot_wider(names_from = "assay_id_name", values_from = "positive_detection", names_expand = TRUE)

  if (nrow(assay_detections) == 0) {
    return(0)
  }

  run_types <- assay_detections |>
    assign_status_codes() |>
    assign_run_types() |>
    dplyr::select(sample_id, run_type_id, status_code_id)

  spring_winter <- run_types |>
    dplyr::filter(status_code_id == 8)

  message(paste0("identified ", nrow(spring_winter), " samples needing OTS16 spring/winter"))

  run_type_id_data <- run_types |> dplyr::filter(run_type_id != 0)

  if (nrow(run_type_id_data) > 0) {
    query <- glue::glue_sql("
INSERT INTO genetic_run_identification (sample_id, run_type_id)
VALUES (
  {run_type_id_data$sample_id},
  {run_type_id_data$run_type_id}
);
", .con = con)

    total_inserts <- purrr::map_dbl(query, function(q) {
      DBI::dbExecute(con, q)
    },
    .progress = list(
      type = "iterator",
      name = "adding run-id to samples",
      clear = FALSE
    ))
  } else {
    total_inserts <- 0
  }

  set_sample_status(con, run_types$sample_id, run_types$status_code_id)

  return(sum(total_inserts))
}

#' @title Offline Genetic Identification
#' @description `generate_genetic_detection` assigns genetic identifier to a
#' sample based on assay results.
#' @param offline_detections_1 table of sample IDs, assay IDs, and positive/negative detections to be compared
#' @param offline_detections_1 table of sample IDs, assay IDs, and positive/negative detections to be compared
#' @details `generate_genetic_detection` compares two tables of offline
#' positive detection results, then uses those to assign a genetic identification value.
#' @export
generate_genetic_detection <- function(offline_detections_1, offline_detections_2) {

  assay_detections_1 <- offline_detections_1 |>
    dplyr::mutate(assay_id_name = dplyr::case_when(assay_id == 1 ~ "ots_28_e",
                                                   assay_id == 2 ~ "ots_28_l",
                                                   assay_id == 3 ~ "ots_16_s",
                                                   assay_id == 4 ~ "ots_16_w"),
                  assay_id_name = factor(assay_id_name, levels = c("ots_28_e","ots_28_l","ots_16_s","ots_16_w"))) |>
    dplyr::distinct(sample_id, positive_detection, assay_id_name) |>
    tidyr::pivot_wider(names_from = "assay_id_name", values_from = "positive_detection", names_expand = TRUE)

  assay_detections_2 <- offline_detections_2 |>
    dplyr::distinct(sample_id, assay_id, positive_detection) |>
    dplyr::mutate(assay_id_name = dplyr::case_when(assay_id == 1 ~ "ots_28_e_2",
                                                   assay_id == 2 ~ "ots_28_l_2",
                                                   assay_id == 3 ~ "ots_16_s_2",
                                                   assay_id == 4 ~ "ots_16_w_2"),
                  assay_id_name = factor(assay_id_name, levels = c("ots_28_e_2","ots_28_l_2","ots_16_s_2","ots_16_w_2"))) |>
    dplyr::select(-assay_id) |>
    tidyr::pivot_wider(names_from = "assay_id_name", values_from = "positive_detection", names_expand = TRUE)

  assay_detections <- left_join(assay_detections_1, assay_detections_2,
                                by = "sample_id") |>
    dplyr::mutate(ots_28_e = ifelse(is.na(ots_28_e), ots_28_e_2, ots_28_e),
                  ots_28_l = ifelse(is.na(ots_28_l), ots_28_l_2, ots_28_l),
                  ots_16_s = ifelse(is.na(ots_16_s), ots_16_s_2, ots_16_s),
                  ots_16_w = ifelse(is.na(ots_16_w), ots_16_w_2, ots_16_w)) |>
    dplyr::select(sample_id, ots_28_e, ots_28_l, ots_16_s, ots_16_w)

  if (nrow(assay_detections) == 0) {
    return(0)
  }

  # hard code tables
  status_code_lookup <- tibble(status_code_id = 1:13,
                               status= c("created", "prepped", "out to field", "returned from field",
                                         "need ots28", "ots28 in progress", "ots28 complete", "need ots16",
                                         "ots16 inprogress", "ots16 complete", "analysis complete", "archived",
                                         "other lab"))
  run_code_lookup <- tibble(run_type_id = 1:8,
                            run_assignment = c("Spring", "Fall", "LateFall", "Winter", "Fall/LateFall", "Spring/Winter",
                                               "Unknown", "Heterozygous"))

  run_types <- assay_detections |>
    assign_status_codes() |>
    assign_run_types() |>
    dplyr::left_join(status_code_lookup, by = "status_code_id") |>
    dplyr::left_join(run_code_lookup, by = "run_type_id") |>
    dplyr::select(sample_id, status, run_assignment)

  non_unknowns <- run_types |>
    dplyr::filter(run_assignment != "Unknown") |>
    dplyr::pull() |>
    length()

  unknowns <- length(run_types$run_assignment) - non_unknowns


  cli::cat_bullet(paste0(non_unknowns, " samples assigned run type"), bullet_col = "green")
  cli::cat_bullet(paste0(unknowns, " samples with unknown run type"), bullet_col = "green")

  return(run_types)

}


