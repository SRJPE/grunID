#' Create sample_events
#' @examples
#' con <- DBI::dbConnect(
#'                       RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host, # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
#'                       port = cfg$port, # or any other port specified by your DBA
#'                       user = cfg$username,
#'                       password = cfg$password
#'                       )
#'
#' sample_bins <- tibble(
#'                      location_code = c("BTC", "BUT"),
#'                      sample_event_number = 1:2,
#'                      first_sample_date = "2020-01-01",
#'                      sample_bin_code = "A",
#'                      min_fork_length = 10,
#'                      max_fork_length = 95,
#'                      expected_number_of_samples = 10
#'                      )
#' sample_events(con, sample_bins)
#' @export
sample_events <- function(con, sample_bins) {

  if (!DBI::dbIsValid(con)) {
    stop("Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'",
         call. = FALSE)
  }

  sample_locations <- dplyr::collect(dplyr::tbl(con, "sample_location"))

  sample_event_insert <- dplyr::left_join(sample_bins, sample_locations,
                                           by = c("location_code" = "code")) %>%
    dplyr::distinct(sample_bins, sample_event_number, first_sample_date, sample_location_id = id)

  sample_event_query <- glue::glue_sql("INSERT INTO sample_event (sample_event_number, sample_location_id, first_sample_date)
                 VALUES (
                  UNNEST(ARRAY[{sample_event_insert$sample_event_number*}]),
                  UNNEST(ARRAY[{sample_event_insert$sample_location_id*}]),
                  UNNEST(ARRAY[{sample_event_insert$first_sample_date*}]::DATE[])
                 ) RETURNING id, sample_event_number;",
                 .con = con)

  res <- DBI::dbSendQuery(con, sample_event_query)
  sample_event_ids <- DBI::dbFetch(res) %>%
    dplyr::transmute(sample_event_id = as.numeric(id), sample_event_number)
  DBI::dbClearResult(res)

  sample_bin_insert <- dplyr::left_join(sample_bins, sample_event_ids,
                                        by = c("sample_event_number"))

  sample_bin_query <- glue::glue_sql("INSERT INTO sample_bin (sample_event_id, sample_bin_code, min_fork_length, max_fork_length, expected_number_of_samples)
                                     VALUES (
                                      UNNEST(ARRAY[{sample_bin_insert$sample_event_id*}]),
                                      UNNEST(ARRAY[{sample_bin_insert$sample_bin_code*}]::bin_code_enum[]),
                                      UNNEST(ARRAY[{sample_bin_insert$min_fork_length*}]),
                                      UNNEST(ARRAY[{sample_bin_insert$max_fork_length*}]),
                                      UNNEST(ARRAY[{sample_bin_insert$expected_number_of_samples*}])
                                     ) RETURNING id, sample_event_id, sample_bin_code;",
                                     .con = con)

  res <- DBI::dbSendQuery(con, sample_bin_query)
  sample_bin_id <- DBI::dbFetch(res) %>%
    dplyr::transmute(sample_bin_id = as.numeric(id), sample_event_id, sample_bin_code = as.character(sample_bin_code))
  DBI::dbClearResult(res)


  sample_id_insert <- dplyr::left_join(sample_bin_insert, sample_bin_id,
                                       by = c("sample_bin_code", "sample_event_id"))

  sample_id <- sample_id_insert %>%
    tidyr::uncount(expected_number_of_samples, .remove = FALSE) %>%
    dplyr::group_by(sample_event_id) %>%
    dplyr::mutate(sample_number = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = paste0(location_code, format(as.Date(first_sample_date), "%y"),
                              "_", sample_event_number, "_", sample_bin_code, "_", sample_number)) %>%
    dplyr::select(id, sample_bin_id)

  sample_id_query <- glue::glue_sql("INSERT INTO sample (id, sample_bin_id)
                                    VALUES (
                                      UNNEST(ARRAY[{sample_id$id*}]),
                                      UNNEST(ARRAY[{sample_id$sample_bin_id*}])
                                    );",
                                    .con = con)

  number_of_samples_added <- DBI::dbExecute(con, sample_id_query)

  return(number_of_samples_added)
}



