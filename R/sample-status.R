#' Set Status Code
#' @description Set the status code for existing samples
#' @param con connection to the database
#' @param sample_ids vector of sample ids to update
#' @param status status code to use for the update
#' * **created** status assigned automatically when a new sample ID is generated
#' * **prepped** sample tube has been created
#' * **out to field** sample tube is out in the field to be collected
#' * **return from field** sample tube has returned from the field
#' * **in analysis** sample is being analyzed
#' * **stored** sample is in storage
#' * **archived** sample has been sent to a tissue archive
#' * **other lab** sample is out at another lab
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#'
#' sample_locations <- set_sample_locations(con,
#'                                          sample_ids = c("FTH_RM1722_3_A_1",
#'                                                          "FTH_RM1722_3_A_2"),
#'                                          status = "prepped",
#'                                          comment = "ready for pickup")
#' @family status code functions
#' @export
#' @md
set_sample_status <- function(con, sample_ids, status_code_id, comment = NULL) {
  is_valid_connection(con)


  if (length(sample_ids) != length(status_code_id)) {
    status_codes_to_insert <- rep(status_code_id, length(sample_ids))
  } else {
    status_codes_to_insert <- status_code_id
  }

  if (!is.null(comment)) {
    comments <- rep(comment, length(sample_ids))

    sample_status_query <- glue::glue_sql("INSERT INTO sample_status (sample_id, status_code_id, comment)
                                        VALUES (
                                          UNNEST(ARRAY[{sample_ids*}]),
                                          UNNEST(ARRAY[{status_codes_to_insert*}]),
                                          UNNEST(ARRAY[{comments*}])
                                        );",
                                        .con = con)
  } else {
    sample_status_query <- glue::glue_sql("INSERT INTO sample_status (sample_id, status_code_id)
                                        VALUES (
                                          {sample_ids},
                                          {status_codes_to_insert}
                                        );",
                                        .con = con)
  }

  total_inserts <- purrr::map_dbl(sample_status_query, \(q) DBI::dbExecute(con, q),
                                  .progress = list(
                                    type = "iterator",
                                    name = "updating status codes for uploaded samples",
                                    clear = FALSE)
                                  )

  return(sum(total_inserts))
}

#' Get Sample Status
#' @description View the current or full sample status history
#' @param con connection to the database
#' @param sample_ids vector of sample ids to update
#' @param full_history when set to TRUE, will return the current and each previously assigned status for a sample ID
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#'
#' sample_locations <- get_sample_locations(con,
#'                                          sample_ids = c("FTH_RM1722_3_A_1",
#'                                                          "FTH_RM1722_3_A_2"))
#' @family status code functions
#' @export
#' @md
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

#' @title Samples needing further genetic analysis
#' @description `get_samples_needing_action` pulls sample IDs that need further
#' genetic analysis.
#' @param con valid connection to database
#' @details `get_samples_needing_action` checks the database table `sample_status`
#' to identify samples in the following categories:
#'
#' * Have been run through OTS 28 Early/Late and need to be run through OTS 16 Spring/Winter
#' * Have been run through OTS 28 Early/Late and produced a negative result for both, indicating
#' they require to be run again
#' * Have been run through OTS 16 Spring/Winter and produced a negative result for both,
#' indicating they require to be run again
#' * Produced positive results for both assays either within OTS 28 Early/Late OR
#' OTS 16 Spring/Winter, indicating they are potential heterozygotes
#'
#' @returns A list with the following elements, each containing a list of Sample IDs in
#' that category:
#'
#' * needs_ots_16
#' * repeat_ots_28
#' * repeat_ots_16
#' * potential_heterozygotes
#' @md
#' @export
get_samples_needing_action <- function(con) {

  #sample_status_table <- dplyr::tbl(con, "sample_status")

  needs_ots_16 <- dplyr::tbl(con, "sample_status") |>
    dplyr::filter(status_code_id == 8) |>
    dplyr::select(sample_id) |>
    dplyr::collect()

  repeat_ots_28 <- dplyr::tbl(con, "sample_status") |>
    dplyr::collect() |>
    dplyr::left_join(dplyr::tbl(con, "genetic_run_identification") |>
                       dplyr::collect(), by = "sample_id") |>
    dplyr::filter(status_code_id == 7 & run_type_id == 7) |>
    dplyr::select(sample_id)

  repeat_ots_16 <- dplyr::tbl(con, "sample_status") |>
    dplyr::filter(status_code_id == 10) |>
    dplyr::select(sample_id) |>
    dplyr::collect()

  potential_heterozygotes <- dplyr::tbl(con, "sample_status") |>
    dplyr::collect() |>
    dplyr::left_join(dplyr::tbl(con, "genetic_run_identification") |>
                       dplyr::collect(), by = "sample_id") |>
    dplyr::filter(status_code_id == 11 & run_type_id == 8) |>
    dplyr::select(sample_id)

  return(list("needs_ots_16" = needs_ots_16,
              "repeat_ots_28" = repeat_ots_28,
              "repeat_ots_16" = repeat_ots_16,
              "potential_heterozygotes" = potential_heterozygotes))
}
