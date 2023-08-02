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
                              selection_strategy = c("recent priority", "positive priority"),
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
      stop(e)
    }
  )

  cli::cli_alert_success("Added {as.numeric(add_raw_res)} results to the database")

  cli::cli_alert_info("Generating thresholds for plate run")

  thresholds_event <- generate_threshold(con, plate_run = plate_run, .control_id = .control_id)
  cli::cli_alert_success("Threshold done")


  add_plate_thresholds(con, thresholds_event, .control_id = .control_id)

  if (run_gen_id) {
    # for now just get the samples based on the plate runs
    samples_not_valid <- c("POS-DNA", "NEG-DNA", "CONTROL", .control_id)
    samples_to_use <- dplyr::tbl(con, "raw_assay_result") |>
      dplyr::filter(plate_run_id %in% !!thresholds$plate_run_id) |>
      dplyr::filter(!(sample_id %in% samples_not_valid)) |>
      dplyr::collect() |>
      dplyr::pull(sample_id)

    run_genetic_identification(con, samples_to_use, selection_strategy = selection_strategy)
  }

  return(thresholds_event)

}



#' @title Query for plate run
#' @export
get_plate_run <- function(con, ...) {

  q_results <- dplyr::tbl(con, "plate_run") |>
    dplyr::filter(...) |>
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
      performed_by=x$lab_work_performed_by
    ))
}




