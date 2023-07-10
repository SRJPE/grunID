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
