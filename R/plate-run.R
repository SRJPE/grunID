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
#' @export
add_new_plate_results <- function(con, protocol_name, genetic_method,
                                  laboratory, lab_work_performed_by, description, date_run,
                                  filepath, sample_type, layout_type,
                                  plate_size = c(96, 384), .control_id = "NTC",
                                  selection_strategy = "recent priority",
                                  run_gen_id = FALSE) {

  is_valid_connection(con)

  if(is.null(filepath)) {
    stop(cli::format_error(c("x" = "No results file was provided",
                             "i" = "Please upload a valid plate layout and results excel file")), call. = FALSE)
  }
  # TODO add check for if single assay type is selected and no single assay type is presented

  protocol_id <- get_protocols(con, name == !!protocol_name) |> dplyr::pull(id)

  if (length(protocol_id) == 0) {
    stop(cli::format_error(c("x" = "There are no protocols with name = '{protocol_name}'",
                             "i" = "Use `grunID::get_protocols` to see existing protocol names")), call. = FALSE)
  }

  genetic_method_id <- get_genetic_methods(con, code == !!genetic_method) |> dplyr::pull(id)

  if (length(genetic_method_id) == 0) {
    stop(cli::format_error(c(
      "x" = "There are no genetic methods with name = '{genetic_method}'",
      "i" = "Use `grunID::get_genetic_methods` to see existing method codes")), call. = FALSE)
  }

  lab_id <- get_laboratories(con, is_active = TRUE, all_results = FALSE, code == laboratory) |> dplyr::pull(id)

  if (length(lab_id) == 0) {
    stop(cli::format_error(c(
      "x" = "There are no laboratories with name = '{laboratory}'",
      "i" = "Use `grunID::get_laboratories` to see existing lab codes")), call. = FALSE)
  }

  cli::cli_alert_info("Adding plate run to database")
  # create a new plate run in db for these results
  plate_run <- add_plate_run(con,
                             date_run = date_run,
                             protocol_id = protocol_id,
                             genetic_method_id = genetic_method_id,
                             laboratory_id = lab_id,
                             lab_work_performed_by = lab_work_performed_by,
                             description = description)
  cli::cli_alert_success("Plate run added to database with id = {plate_run$plate_run_id}")


  cli::cli_alert_info("Processing sherlock data")
  sherlock_results_event <- suppressMessages(
    process_sherlock(
      filepath = filepath,
      sample_type = sample_type,
      layout_type = layout_type,
      plate_run_id = plate_run,
      plate_size = plate_size)
  )
  cli::cli_alert_success("Sherlock results processing complete")

  cli::cli_alert_info("adding results to database")
  add_raw_res <- tryCatch(
    add_raw_assay_results(con, sherlock_results_event),
    error = function(e) {
      cli::cli_alert_danger("there was an error attempting to add new raw data, removing plate run associated with this from database, see the error below for more details:")
      sql_query <- glue::glue_sql("DELETE FROM plate_run where id = {plate_run$plate_run_id}", .con = con)
      res <- DBI::dbSendQuery(con, sql_query)
      DBI::dbClearResult(res)
      is_sample_fk_violation <- stringr::str_detect(e$message, "COPY returned error: ERROR:  insert or update on table \"raw_assay_result\" violates foreign key constraint \"raw_assay_result_sample_id_fkey")
      if (is_sample_fk_violation) {
        sample_needs_to_be_created <- str_match(e$message, "Key \\(sample_id\\)=\\(([^)]+)\\)")[1, 2]
        stop(glue::glue("the sample {sample_needs_to_be_created} was not found, please add to database using the `add_sample_plan()` function"), call. = FALSE)
      } else {
        stop(e)
      }
    }
  )

  cli::cli_alert_success("Added {as.numeric(add_raw_res)} results to the database")

  cli::cli_alert_info("Generating thresholds for plate run")

  thresholds_event <- generate_threshold(con, plate_run = plate_run, .control_id = .control_id)
  cli::cli_alert_success("Threshold done")


  add_plate_thresholds(con, thresholds_event, .control_id = .control_id)

  # Check all NTCs (n = 3), NEG-DNA controls (n = 3), and EBKs ("extraction blanks" - n = 4) for RFU values greater than 12,000 (flag if any one has RFU > 12,000).
  assays_results_for_qaqc <- tbl(con, "assay_result") |>
    filter(plate_run_id == !!plate_run$plate_run_id)

  values_are_below_12k <- assays_results_for_qaqc |>
    filter(raw_fluorescence > 12000,
           sample_id %in% c("NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3",
                            "NTC-1", "NTC-2", "NTC-3")) |> collect()

  if (nrow(values_are_below_12k) > 0) {
    stop(
      cli::format_error(c(
        "x" = "Qa/Qc Test Not Passed: Value above 12k",
        "i" = glue::glue("the value for {values_are_below_12k$sample_id} (id = {values_are_below_12k$id}) has a value ({values_are_below_12k$raw_fluorescence }) greater then 12,000 RFU")
      )), call. = FALSE
    )
  }


  # EBK exceed 12k then only flag do not stop execution ,  "EBK-1", "EBK-2", "EBK-3", "EBK-4" - map these so they can find them in the raw data
  # IF an EBK is over 12k then DO NOT USE it for thresholds calculation - continue

  # Check NTCs, NEG-DNA controls, and POS-DNA controls (n = 3) against 2xEBK threshold.
  values_are_above_thresholds <- assays_results_for_qaqc |>
    filter(raw_fluorescence > threshold,
           sample_id %in% c("NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3",
                            "NTC-1", "NTC-2", "NTC-3")) |>
    collect()

  # TODO ebk are their own thing only flag and continue the operation

  if (nrow(values_are_above_thresholds) > 0) {
    stop(
      cli::format_error(c(
        "x" = "Qa/Qc Test Not Passed: NTC/NEG-DNA Value above Threshold",
        "i" = glue::glue("the value for {values_are_above_thresholds$sample_id} (id = {values_are_above_thresholds$id}) has a value ({values_are_above_thresholds$raw_fluorescence }) greater then the plate threshold ({values_are_above_thresholds$threshold})")
      )), call. = FALSE
    )
  }

  # 2 out of the 3 POS-DNA controls must return a positive result (greater than 2xEBK threshold).
  pos_dna_values_are_above_threshold <- assays_results_for_qaqc |>
    filter(raw_fluorescence > threshold, sample_id %in% c("POS-DNA-1", "POS-DNA-2", "POS-DNA-3")) |>
    collect() |>
    group_by(assay_id) |>
    tally() |>
    filter(n < 2)

  if (nrow(pos_dna_values_are_above_threshold) > 0) {
    stop(
      cli::format_error(c(
        "x" = "Qa/Qc Test Not Passed: 2 of 3 Positive DNA were not above threshold",
        "i" = glue::glue("the plate with id: '{plate_run$plate_run_id}' contained at least 2 positive DNA controls that did not exceed the plate threshold.")
      )), call. = FALSE
    )
  }



  if (run_gen_id) {
    # for now just get the samples based on the plate runs
    samples_not_valid <- c(
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

    samples_to_use <- dplyr::tbl(con, "raw_assay_result") |>
      dplyr::filter(plate_run_id %in% !!thresholds_event$plate_run_id) |>
      dplyr::filter(!(sample_id %in% samples_not_valid)) |>
      dplyr::collect() |>
      dplyr::pull(sample_id)

    run_genetic_identification(con, samples_to_use, selection_strategy = selection_strategy, plate_comment = unique(thresholds_event$plate_comment))
  }

  return(thresholds_event)

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
                          laboratory_id, lab_work_performed_by, description, date_run) {
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
                  date_run == !!date_run) |>
    dplyr::collect()

  proceed_inserting <- TRUE


  query <- glue::glue_sql("
  INSERT INTO plate_run (protocol_id, genetic_method_id,  laboratory_id, lab_work_performed_by, description, date_run)
  VALUES ({protocol_id}, {genetic_method_id}, {laboratory_id}, {lab_work_performed_by}, {description}, {date_run}) RETURNING id;",
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
deactivate_plate_run <- function(con, plate_run_id) {
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
    stop(sprintf("plate run ID '%s' does not exist in the database", plate_run_id))
  }

  else if(!is_plate_run_active) {
    stop(sprintf("plate run ID '%s' is already deactivated", plate_run_id))
  }

  else {

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
    stop(sprintf("plate run ID '%s' does not exist in the database", plate_run_id))
  }

  else if(is_plate_run_active) {
    stop(sprintf("plate run ID '%s' is already activated", plate_run_id))
  }

  else {

    query <- glue::glue_sql("UPDATE plate_run
                           SET active = TRUE
                           WHERE id = {plate_run_id}
                           RETURNING id, updated_at;",
                            .con = con)

    res <- DBI::dbSendQuery(con, query)
    DBI::dbClearResult(res)

    cli::cat_bullet(sprintf("Plate run ID '%s' successfully avtivated", plate_run_id), bullet_col = "green")
  }
}

