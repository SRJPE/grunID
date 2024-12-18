#' @title Add New Plate Results
#' @param con connection to the database
#' @param protocol name pf the protocol used for this run
#' @param genetic_method the genetic method used for this run
#' @param laboratory the lab used for this run
#' @param description a description to ne associated with the plate run
#' @param date_run the date that the assat was run
#' @param filepath the filepath to the Sherlock results
#' @param sample_type the sample type
#' @param layout_type the layout that was used for this assay
#' @param plate_size either 96 or 384
#' @param is_salvage samples are obtained from salvage program
#' @export
add_new_plate_results <- function(con, protocol_name, genetic_method, laboratory,
                                  lab_work_performed_by, description, date_run,
                                  filepath, sample_type, layout_type, plate_size = c(96, 384),
                                  .control_id = "EBK", selection_strategy = "recent priority",
                                  threshold_strategy = "twice average",
                                  run_gen_id = FALSE, samples_type = c("jpe", "salvage"),
                                  custom_layout_filepath = NULL) {


  samples_type <- match.arg(samples_type)

  db_tables <- switch(samples_type,
                      "jpe" = list(
                        "raw_assay" = "raw_assay_result",
                        "assay" = "assay_result",
                        "run_id" = "genetic_run_identification",
                        "samples" = "sample",
                        "sample_status" = "sample_status"
                      ),
                      "salvage" = list(
                        "raw_assay" = "external_raw_assay_result",
                        "assay" = "external_assay_result",
                        "run_id" = "external_genetic_run_identification",
                        "samples" = "external_sample",
                        "sample_status" = "external_sample_status"
                      )
  )


  is_valid_connection(con)

  if(is.null(filepath)) {
    rlang::abort(glue::glue(
      "no results file provided, upload valid plate layout and results file"
      ), call = NULL)
  }
  # TODO add check for if single assay type is selected and no single assay type is presented

  protocol_id <- get_protocols(con, name == !!protocol_name) |> dplyr::pull(id)

  if (length(protocol_id) == 0) {
    rlang::abort(glue::glue(
      "there are not protocols with name = '{protocol_name}'"
    ), call = NULL)
  }

  genetic_method_id <- get_genetic_methods(con, code == !!genetic_method) |> dplyr::pull(id)

  if (length(genetic_method_id) == 0) {
    rlang::abort(glue::glue(
      "there are no genetic methods with name = '{genetic_method}'"
    ), call = NULL)
  }

  lab_id <- get_laboratories(con, is_active = TRUE, all_results = FALSE, code == laboratory) |> dplyr::pull(id)

  if (length(lab_id) == 0) {
    rlang::abort(glue::glue(
      "there are no laboratories with name = '{laboratory}'"
    ), call = NULL)
  }

  logger::log_info("Adding plate run to database")
  # create a new plate run in db for these results
  plate_run <- add_plate_run(con,
                             date_run = date_run,
                             protocol_id = protocol_id,
                             genetic_method_id = genetic_method_id,
                             laboratory_id = lab_id,
                             lab_work_performed_by = lab_work_performed_by,
                             description = description,
                             filename = filepath$name)

  logger::log_info("Plate run added to database with id = {plate_run$plate_run_id}")

  logger::log_info("Processing sherlock data")
  sherlock_results_event <- tryCatch(
    suppressMessages(
      process_sherlock(
        filepath = filepath$datapath,
        sample_type = sample_type,
        layout_type = layout_type,
        plate_run_id = plate_run,
        plate_size = plate_size,
        custom_layout_filepath = custom_layout_filepath)
    ),
    error = function(e) {
      pl_id_to_delete <- plate_run$plate_run_id
      logger::log_info("Deleting plate_run_id = {pl_id_to_delete} due to errors trying to read sherlock file")
      DBI::dbExecute(con, glue::glue("DELETE FROM plate_run where id = {pl_id_to_delete};"))
      stop(glue::glue("Error trying to process sherlock output file, message from processing function\n: {e}"))
    }
  )
  logger::log_info("Sherlock results processing complete")

  logger::log_info("adding results to database")
  add_raw_res <- tryCatch(
    add_raw_assay_results(con, sherlock_results_event, destination_table = db_tables$raw_assay),
    error = function(e) {
      logger::log_error("there was an error attempting to add new raw data, removing plate run associated with this from database, see the error below for more details:")
      sql_query <- glue::glue_sql("DELETE FROM plate_run where id = {plate_run$plate_run_id}", .con = con)
      res <- DBI::dbSendQuery(con, sql_query)
      DBI::dbClearResult(res)
      is_sample_fk_violation <- stringr::str_detect(e$message, "COPY returned error")
      if (is_sample_fk_violation) {
        regex_pattern <- "COPY returned error : ERROR:\\s+insert or update on table \"(\\w+)\" violates foreign key constraint \"\\w+\"\\s+DETAIL:\\s+Key \\(sample_id\\)=\\(([^)]+)\\) is not present in table \"sample\""

        match_result <- stringr::str_match(e$message, regex_pattern)

        if (!is.na(match_result[1, 1])) {
          table_name <- match_result[1, 2]
          sample_needs_to_be_created <- match_result[1, 3]

          error_message <- glue::glue("Error attempting insert data into {table_name} table, the sample {sample_needs_to_be_created} does not exists in the sample table")
          stop(error_message, call. = FALSE)
        } else {
          stop(e$message)
        }
      } else {
        stop(e$message)
      }
    }
  )

  logger::log_info("Added {as.numeric(add_raw_res)} results to the database")

  logger::log_info("Generating thresholds for plate run")

  thresholds_event <- generate_threshold(con, plate_run = plate_run, results_table = db_tables$raw_assay, .control_id = .control_id, strategy = threshold_strategy)

  logger::log_info("Threshold done")


  # adds the assay_results data
  add_plate_thresholds(con, thresholds_event, destination_table = db_tables$assay, results_table = db_tables$raw_assay)
  results_valid <- validate_results(con, plate_run = plate_run, results_table = db_tables$assay)


  if (results_valid) {
    logger::log_info("all validation checks passed!")
  }

  if (run_gen_id) {
    # for now just get the samples based on the plate runs
    samples_not_valid <- c(
      "EBK-1-1",
      "EBK-1-2",
      "EBK-1-3",
      "EBK-1-4",
      "EBK-2-1",
      "EBK-2-2",
      "EBK-2-3",
      "EBK-2-4",
      "EBK-3-1",
      "EBK-3-2",
      "EBK-3-3",
      "EBK-3-4",
      "EBK-4-1",
      "EBK-4-2",
      "EBK-4-3",
      "EBK-4-4",
      "POS-DNA-1",
      "POS-DNA-2",
      "POS-DNA-3",
      "NEG-DNA-1",
      "NEG-DNA-2",
      "NEG-DNA-3",
      "NTC-1",
      "NTC-2",
      "NTC-3",
      "CONTROL", paste0(.control_id, "-", 1:4))

    samples_to_use <- dplyr::tbl(con, db_tables$raw_assay) |>
      dplyr::filter(plate_run_id %in% !!thresholds_event$plate_run_id) |>
      dplyr::filter(!(sample_id %in% samples_not_valid)) |>
      dplyr::collect() |>
      dplyr::pull(sample_id)

    # run_genetic_identification(con, samples_to_use, selection_strategy = selection_strategy,
    #                            plate_comment = unique(thresholds_event$plate_comment),
    #                            plate_run_id = plate_run$plate_run_id,
    #                            destination_table = db_tables$run_id,
    #                            sample_table = db_tables$samples,
    #                            results_table = db_tables$assay,
    #                            sample_status_table = db_tables$sample_status,
    #                            layout_type = layout_type)
    run_genetic_identification_v2(con, samples_to_use, plate_run_id = plate_run$plate_run_id)
  }

  return(thresholds_event)

}


#' @title Validate Results
#' @param con a connection to the database
#' @param plate_run_id plate to run through the validation process
#' @md
validate_results <- function(con, plate_run, results_table = c("assay_result", "external_assay_result")) {
  # Check all NTCs (n = 3), NEG-DNA controls (n = 3), and EBKs ("extraction blanks" - n = 4) for RFU values greater than 12,000 (flag if any one has RFU > 12,000).
  assays_results_for_qaqc <- tbl(con, results_table) |>
    filter(plate_run_id == !!plate_run$plate_run_id)

  all_assays_in_plate <- assays_results_for_qaqc |> collect() |> distinct(assay_id) |> pull()
  # for qa/qc error output
  error_messages <- list()

  # TODO: neg-dna does not get RFU check instead compare it to 2x threshold of EBK

  # Check if NTC/NDNA values are above 12k
  rfu_threshold_check_value <- 18000 # 12k
  ntc_ndna_are_above_12k <- assays_results_for_qaqc %>%
    filter(raw_fluorescence > rfu_threshold_check_value,
           sample_id %in% c("NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3",
                            "NTC-1", "NTC-2", "NTC-3")) %>%
    collect()

  ntc_ndna_are_above_12k_failing_assay_id <-
    ntc_ndna_are_above_12k |> distinct(assay_id) |> pull()

  if (nrow(ntc_ndna_are_above_12k) > 0) {
    error_messages <- c(error_messages, glue::glue("Qa/Qc Test Not Passed: Value above 12k for sample_id(s): {ntc_ndna_are_above_12k$sample_id} on assay: {ntc_ndna_are_above_12k_failing_assay_id}"))
  }

  # Check NTC/NDNA values against thresholds
  ntc_ndna_are_above_thresholds <- assays_results_for_qaqc %>%
    filter(raw_fluorescence > threshold,
           sample_id %in% c("NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3",
                            "NTC-1", "NTC-2", "NTC-3")) %>%
    collect()

  ntc_ndna_are_above_thresholds_faling_assay_id <-
    ntc_ndna_are_above_thresholds |> distinct(assay_id) |> pull()

  if (nrow(ntc_ndna_are_above_thresholds) > 0) {
    # remove the data that was added to db up to this point
    error_messages <- c(error_messages, glue::glue("Qa/Qc Test Not Passed: NTC/NEG-DNA Value above Threshold for sample_id(s): {ntc_ndna_are_above_thresholds$sample_id}"))
  }

  # Check POS-DNA controls
  pos_dna_values_are_below_threshold <- assays_results_for_qaqc %>%
    filter(raw_fluorescence < threshold, sample_id %in% c("POS-DNA-1", "POS-DNA-2", "POS-DNA-3")) %>%
    collect() %>%
    group_by(assay_id) %>%
    tally() %>%
    filter(n >= 2)

  pos_dna_values_are_below_threshold_failing_assay_id <-
    pos_dna_values_are_below_threshold |> distinct(assay_id) |> pull()

  if (nrow(pos_dna_values_are_below_threshold)) {
    purrr::walk(seq_along(pos_dna_values_are_below_threshold$assay_id), function(id) {
      error_messages <<- c(error_messages, glue::glue("Qa/Qc Test Not Passed: 2 of 3 Positive DNA were not above threshold for plate with id: '{plate_run$plate_run_id}' on assay: '{id}'"))
    })
  }

  combined_failed_assays <- c(ntc_ndna_are_above_thresholds_faling_assay_id, ntc_ndna_are_above_12k_failing_assay_id,
                              pos_dna_values_are_below_threshold_failing_assay_id)

  all_assays_in_plate_failed <- all(all_assays_in_plate %in% combined_failed_assays)

  if (all_assays_in_plate_failed) {
    del_raw_assay_sql_statement <- glue::glue_sql("DELETE FROM raw_assay_result where plate_run_id={as.integer(plate_run$plate_run_id)};", .con = con)
    del_assay_sql_statement <- glue::glue_sql("DELETE FROM assay_result where plate_run_id={plate_run$plate_run_id};", .con = con)
    deactivate_plate_statement <- glue::glue_sql("UPDATE plate_run SET active = false where id={plate_run$plate_run_id};", .con = con)
    DBI::dbExecute(con, del_raw_assay_sql_statement)
    DBI::dbExecute(con, del_assay_sql_statement)
    DBI::dbExecute(con, deactivate_plate_statement)
    stop(unlist(c("all assays in plate failed checks", error_messages)), call. = FALSE)
  }

  # Print all error messages together
  if (length(error_messages) > 0) {
    del_raw_assay_sql_statement <- glue::glue_sql("DELETE FROM raw_assay_result where plate_run_id={as.integer(plate_run$plate_run_id)} and assay_id IN ({combined_failed_assays*});", .con = con)
    del_assay_sql_statement <- glue::glue_sql("DELETE FROM assay_result where plate_run_id={plate_run$plate_run_id} and assay_id IN ({combined_failed_assays*});", .con = con)
    # deactivate_plate_statement <- glue::glue_sql("UPDATE plate_run SET active = false where id={plate_run$plate_run_id} and assay_id IN {combined_failed_assays};", .con = con)
    DBI::dbExecute(con, del_raw_assay_sql_statement)
    DBI::dbExecute(con, del_assay_sql_statement)
    # DBI::dbExecute(con, deactivate_plate_statement)
    warning(glue::glue("partial failed check, the following assays failed: {combined_failed_assays}, other assays uploaded"))
  }

  return(TRUE)

}



#' @title Query for plate run
#' @export
get_plate_run <- function(con, ...) {

  q_results <- dplyr::tbl(con, "plate_run") |>
    dplyr::filter(active == TRUE, ...) |>
    collect()

  if (nrow(q_results) > 1 ) {
    cli::cli_alert_warning("query results in more than one plate run, not returning as 'Plate Run Object'")
    cli::cli_alert_warning("To return as a plate run object, query must result in a single row, refine your query and try again")
    return(q_results)
  } else if (nrow(q_results) == 0) {
    return(q_results)
  } else {


    return(as_plate_run(q_results))
  }
}


#' @description
#' turns a dataframe into a plate run object for nice printing
#' @keywords internal
as_plate_run <- function(x, ...) {
  return(
    structure(
      list(plate_run_id = x$id,
           data = x),
      class = "plate_run",
      protocol_id=x$protocol_id,
      genetic_method_id=x$genetic_method_id,
      description=x$description,
      performed_by=x$lab_work_performed_by,
      active=x$active
    ))
}


#' @title Create Plate Run
#' @description `add_plate_run()` adds metadata about a plate run to
#' the plate_run table in the database.
#' @param con valid connection to the database
#' @param protocol_id protocol identifier
#' @param genetic_method_id genetic method identifier
#' @param laboratory_id laboratory identifier
#' @param lab_work_performed_by name of staff who performed the plate run
#' @param description a description for the plate run
#' @param date_run date of plate run
#' @returns the unique plate run identifier assigned by the database. This value needs to be retained
#' in the workflow so it can be passed as an argument to functions `process_well_sample_details()` and
#' `generate_threshold()`.
#' @export
add_plate_run <- function(con, protocol_id, genetic_method_id,
                          laboratory_id, lab_work_performed_by, description, date_run, filename) {
  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  plate_run_already_exists_in_db <- dplyr::tbl(con, "plate_run") |>
    dplyr::filter(protocol_id == !!protocol_id,
                  genetic_method_id == !!genetic_method_id,
                  laboratory_id == !!laboratory_id,
                  lab_work_performed_by == !!lab_work_performed_by,
                  date_run == !!date_run,
                  filename == !!filename) |>
    dplyr::collect()

  proceed_inserting <- TRUE


  query <- glue::glue_sql("
  INSERT INTO plate_run (protocol_id, genetic_method_id,  laboratory_id, lab_work_performed_by, description, date_run, filename)
  VALUES ({protocol_id}, {genetic_method_id}, {laboratory_id}, {lab_work_performed_by}, {description}, {date_run}, {filename}) RETURNING id;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  plate_run_id <- DBI::dbFetch(res)
  DBI::dbClearResult(res)



  return(
    structure(
      list(plate_run_id=plate_run_id$id),
      class = "plate_run",
      protocol_id=protocol_id,
      genetic_method_id=genetic_method_id,
      description=description,
      performed_by=lab_work_performed_by
    ))

}


#' @export
print.plate_run <- function(x, ...) {
  cli::cat_rule(sprintf("A Plate Run Object"))
  cli::cat_bullet(sprintf("Protocol ID: '%s'", attr(x, "protocol_id", exact = TRUE)), bullet_col = "green")
  cli::cat_bullet(sprintf("Genetic Method ID: '%s'", attr(x, "genetic_method_id", exact = TRUE)), bullet_col = "green")
  cli::cat_bullet(sprintf("Description: '%s'", attr(x, "description", exact = TRUE)), bullet_col = "green")
  cli::cat_bullet(sprintf("Plate Run ID Assigned: %s", x$plate_run_id), bullet_col = "green")
  cli::cat_bullet(sprintf("Lab Work Performed by: '%s'", attr(x, "performed_by", exact = TRUE)), bullet_col = "green")
  cli::cat_bullet(sprintf("Data:"), bullet_col = "green")
  print(x$data)

}

#' @title Deactivate Plate Run
#' @description `deactivate_plate_run()` deactivates an existing plate run entry in
#' the plate_run table in the database.
#' @param con valid connection to the database
#' @param plate_run_id plate run identifier
#' @returns no return value
#' @export
#' @md
deactivate_plate_run <- function(con, plate_run_id, sub_plates = NULL) {
  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection to the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  is_plate_run_active <- dplyr::tbl(con, "plate_run") |>
    dplyr::filter(id == plate_run_id) |>
    dplyr::collect() |>
    dplyr::pull(active)

  if(length(is_plate_run_active) == 0) {
    stop(sprintf("plate run ID '%s' does not exist in the database", plate_run_id))
  }

  if(!is_plate_run_active) {
    stop(sprintf("plate run ID '%s' is already deactivated", plate_run_id))
  } else {

    query <- glue::glue_sql("UPDATE plate_run
                           SET active = FALSE
                           WHERE id = {plate_run_id}
                           RETURNING id, updated_at;",
                            .con = con)

    res <- DBI::dbSendQuery(con, query)
    DBI::dbClearResult(res)

    cli::cat_bullet(sprintf("Plate run ID '%s' successfully deactivated", plate_run_id), bullet_col = "green")
  }
}

#' @title Activate Plate Run
#' @description `activate_plate_run()` activates an existing plate run entry in
#' the plate_run table in the database.
#' @param con valid connection to the database
#' @param plate_run_id plate run identifier
#' @returns no return value
#' @export
#' @md
activate_plate_run <- function(con, plate_run_id) {
  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection to the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  is_plate_run_active <- dplyr::tbl(con, "plate_run") |>
    dplyr::filter(id == !!plate_run_id) |>
    dplyr::collect() |>
    dplyr::pull(active)

  if(length(is_plate_run_active) == 0) {
    stop(sprintf("plate run ID '%s' does not exist in the database", plate_run_id), call. = FALSE)
  }

  else if(is_plate_run_active) {
    stop(sprintf("plate run ID '%s' is already activated", plate_run_id), call. = FALSE)
  }

  else {

    query <- glue::glue_sql("UPDATE plate_run
                           SET active = TRUE
                           WHERE id = {plate_run_id}
                           RETURNING id, updated_at;",
                            .con = con)

    res <- DBI::dbSendQuery(con, query)
    DBI::dbClearResult(res)

    cli::cat_bullet(sprintf("Plate run ID '%s' successfully activated", plate_run_id), bullet_col = "green")
  }
}

#' @export
remove_plate_run <- function(con, plate_run_id) {
  # check if this is the latest plate on the stack
  latest_plate_run <- tbl(con, "plate_run") |>
    arrange(desc(created_at)) |>
    head(1) |>
    pull(id)

  if (latest_plate_run != plate_run_id) {
    rlang::abort(glue::glue("only the latest plate run can be removed, the latest plate run is currrently: {latest_plate_run}"),
                 call = NULL)
  }

  DBI::dbBegin(con)

  tryCatch({
    sql_stm <- glue::glue_sql("DELETE FROM genetic_run_identification
                              WHERE
                              early_plate_id={plate_run_id} OR
                              late_plate_id={plate_run_id} OR
                              spring_plate_id={plate_run_id} OR
                              winter_plate_id={plate_run_id};", .con = con)
    gen_id_rows_affected <- DBI::dbExecute(con, sql_stm)
    if (gen_id_rows_affected == 0) {
      stop("No rows were deleted from genetic_run_identification")
    }

    sql_stm <- glue::glue_sql("DELETE FROM sample_status WHERE plate_run_id={plate_run_id}", .con = con)
    sample_status_rows_affected <- DBI::dbExecute(con, sql_stm)
    if (sample_status_rows_affected == 0) {
      stop("No rows were deleted from sample_status")
    }

    sql_stm <- glue::glue_sql("DELETE FROM assay_result WHERE plate_run_id={plate_run_id}", .con = con)
    assay_result_rows_affected <- DBI::dbExecute(con, sql_stm)
    if (assay_result_rows_affected == 0) {
      stop("No rows were deleted from assay_result")
    }

    sql_stm <- glue::glue_sql("DELETE FROM raw_assay_result WHERE plate_run_id={plate_run_id}", .con = con)
    raw_assay_result_rows_affected <- DBI::dbExecute(con, sql_stm)
    if (raw_assay_result_rows_affected == 0) {
      stop("No rows were deleted from raw_assay_result")
    }

    sql_stm <- glue::glue_sql("DELETE FROM plate_run WHERE id={plate_run_id}", .con = con)
    plate_run_rows_affected <- DBI::dbExecute(con, sql_stm)
    if (plate_run_rows_affected == 0) {
      stop("No rows were deleted from plate_run")
    }

    DBI::dbCommit(con)

    return(list(
      gen_id_rows_affected = gen_id_rows_affected,
      sample_status_rows_affected = sample_status_rows_affected,
      assay_result_rows_affected = assay_result_rows_affected,
      raw_assay_result_rows_affected = raw_assay_result_rows_affected,
      plate_run_rows_affected = plate_run_rows_affected
    ))
  }, error = function(e) {
    # roll back the transaction if an error occurred
    DBI::dbRollback(con)
    stop(e)
  })
}








