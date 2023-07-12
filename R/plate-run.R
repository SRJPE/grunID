#' @title Query for plate run
#' @export
get_plate_run <- function(con, ...) {

  q_results <- dplyr::tbl(con, "plate_run") |>
    dplyr::filter(active = TRUE, ...) |>
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
    stop("Connection argument does not have a valid connection to the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  plate_run_already_exists_in_db <- dplyr::tbl(con, "plate_run") |>
    dplyr::filter(protocol_id == !!protocol_id,
                  genetic_method_id == !!genetic_method_id,
                  laboratory_id == !!laboratory_id,
                  lab_work_performed_by == !!lab_work_performed_by,
                  date_run == !!date_run,
                  active == TRUE) |>
    dplyr::collect()

  proceed_inserting <- TRUE

  if (nrow(plate_run_already_exists_in_db)) {
    proceed_inserting <- usethis::ui_yeah("Plate run with these values exists in database, do you wish to insert anyway?
                                          This will deactivate any previously existing plate runs with these values.",
                                          yes = "Yes", no = "No")
  }


  if (proceed_inserting) {

    existing_plate_run_ids <- plate_run_already_exists_in_db |>
      dplyr::select(id)

    delete_plate_run(con, existing_plate_run_ids)

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
  } else {
    cli::cli_alert_info("use the following to view what exists in database")
    cli::cli_code(lines = glue::glue("grunID::get_plate_run(con, id == {plate_run_already_exists_in_db$id})"), language = "R")
    stop("aborting operation", call. = FALSE)
  }
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

#' @title Delete Plate Run
#' @description `delete_plate_run()` deactivates an existing plate run entry in
#' the plate_run table in the database.
#' @param con valid connection to the database
#' @param plate_run_id plate run identifier
#' @returns no return value
#' @export
delete_plate_run <- function(con, plate_run_id) {
  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection to the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  is_plate_run_active <- dplyr::tbl(con, "plate_run") |>
    dplyr::filter(id == !!plate_run_id) |>
    dplyr::select(active) |>
    dplyr::collect()

  if(nrow(is_plate_run_active) == 0) {
    stop("this plate run ID does not exist in the database")
  }

  else if(!is_plate_run_active) {
    stop("plate run ID is already deactivated")
  }

  else {
    query <- glue::glue_sql("UPDATE plate_run
                           SET active = FALSE
                           WHERE id = {plate_run_id}
                           RETURNING id, updated_at;",
                           .con = con)

    res <- DBI::dbSendQuery(con, query)
    plate_run_id <- DBI::dbFetch(res)
    DBI::dbClearResult(res)
  }
}
