#' Set the status code for a list of existing samples
#' @param con connection to the database
#' @param sample_ids vector of sample ids to update
#' @param status status to code to use for update
#' @export
set_sample_status <- function(con, sample_ids,
                              status = c("created", "prepped", "out to field",
                                         "return from field", "in analysis",
                                         "stored", "archived", "other lab"),
                              comment = NULL) {
  is_valid_connection(con)

  status <- match.arg(status)

  status_code <- rep(which(status == c("created", "prepped", "out to field",
                                       "return from field", "in analysis", "stored",
                                       "archived", "other lab")), length(sample_ids))

  if (!is.null(comment)) {
    comments <- rep(comment, length(sample_ids))

    sample_status_query <- glue::glue_sql("INSERT INTO sample_status (sample_id, status_code_id, comment)
                                        VALUES (
                                          UNNEST(ARRAY[{sample_ids*}]),
                                          UNNEST(ARRAY[{status_code*}]),
                                          UNNEST(ARRAY[{comments*}])
                                        );",
                                        .con = con)
  } else {
    sample_status_query <- glue::glue_sql("INSERT INTO sample_status (sample_id, status_code_id)
                                        VALUES (
                                          UNNEST(ARRAY[{sample_ids*}]),
                                          UNNEST(ARRAY[{status_code*}])
                                        );",
                                        .con = con)
  }

  DBI::dbExecute(con, sample_status_query)

}

#' Get Sample Status
#' @param con connection to the database
#' @param sample_ids vector of sample ids to update
#' @param full_history dk
#' @export
get_sample_status <- function(con, sample_ids, full_history = FALSE) {

  is_valid_connection(con)

  sample_names <- dplyr::tbl(con, "status_code") |>
    dplyr::collect() |>
    dplyr::select(status_code_id = id, status_code_name)

  sample_status <- dplyr::tbl(con, "sample_status") |>
    dplyr::filter(sample_id %in% sample_ids) |>
    dplyr::collect()

  if (!full_history) {
    sample_status <- sample_status |>
      dplyr::group_by(sample_id) |>
      dplyr::filter(created_at == max(created_at))
  }

  results <- sample_status |>
    dplyr::left_join(sample_names, by = "status_code_id") |>
    dplyr::select(id, sample_id, status_code_id, status_code_name,
                  comment, created_at, created_by)

  return(results)

}

# TODO should we create these?
# .update_sample_status <- function() {
#
# }
#
# .delete_sample_status <- function() {
#
# }

