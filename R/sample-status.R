#' Update the status code for a list of existing sample
#' @param con connection to the database
#' @param sample_ids vector of sample ids to update
#' @param status_code status to code to use for update
#' @export
update_sample_status <- function(con, sample_ids,
                                 status = c("created", "prepped", "out to field", "return from field",
                                                 "in analysis", "stored", "archived", "other lab")) {
  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  status <- match.arg(status)

  status_code <- which(status == c("created", "prepped", "out to field", "return from field",
          "in analysis", "stored", "archived", "other lab"))


  sample_status_query <- glue::glue_sql("UPDATE sample_status SET status_code_id = {status_code}
                                        WHERE sample_id IN ({sample_id$id*});",
                                        .con = con)

  DBI::dbExecute(con, sample_status_query)

}

get_sample_status <- function(con, sample_ids) {

}
