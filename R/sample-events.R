#' Add Sample Plan
#' @description `add_sample_plan` registers new sampling events and generates new sample ids
#' @param con
#' @param sample_plan
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
#' sample_plan <- tibble(location_code = c("BTC", "BUT"),
#'                       sample_event_number = 1:2,
#'                       first_sample_date = "2020-01-01",
#'                       sample_bin_code = "A",
#'                       min_fork_length = 10,
#'                       max_fork_length = 95,
#'                       expected_number_of_samples = 10
#'                       )
#' add_sample_plan(con, sample_plan)
#' @export
#' @md
add_sample_plan <- function(con, sample_plan) {
  is_valid_connection(con)
  is_valid_sample_plan(sample_plan)

  sample_event_ids <- add_sample_events(con, sample_plan)
  sample_id_insert <- add_sample_bins(con, sample_plan, sample_event_ids)
  sample_ids <- add_samples(con, sample_plan, sample_id_insert)
  number_of_samples_added <- add_sample_status(con, sample_ids)

  return(number_of_samples_added)
}

#' Create sample events
add_sample_events <- function(con, sample_plan) {
  sample_locations <- get_sample_locations(con)

  sample_event_insert <- sample_plan |>
    dplyr::left_join(sample_locations, by = c("location_code" = "code")) |>
    dplyr::distinct(sample_event_number, first_sample_date,
                    sample_location_id = id)

  sample_event_query <- glue::glue_sql(
    "INSERT INTO sample_event (sample_event_number, sample_location_id, first_sample_date)
        VALUES (
         UNNEST(ARRAY[{sample_event_insert$sample_event_number*}]),
         UNNEST(ARRAY[{sample_event_insert$sample_location_id*}]),
         UNNEST(ARRAY[{sample_event_insert$first_sample_date*}]::DATE[])
        ) RETURNING id, sample_event_number;",
    .con = con)

  res <- DBI::dbSendQuery(con, sample_event_query)

  sample_event_ids <- DBI::dbFetch(res) |>
    dplyr::transmute(sample_event_id = as.numeric(id), sample_event_number)

  DBI::dbClearResult(res)

  print(sample_event_ids)
  return(sample_event_ids)
}

#' Create sample bins
add_sample_bins <- function(con, sample_plan, sample_event_ids) {

  sample_bin_insert <- dplyr::left_join(sample_plan, sample_event_ids,
                                        by = c("sample_event_number"))

  query <- glue::glue_sql(
    "INSERT INTO sample_bin (sample_event_id, sample_bin_code, min_fork_length,
                             max_fork_length, expected_number_of_samples)
        VALUES (
         UNNEST(ARRAY[{sample_bin_insert$sample_event_id*}]),
         UNNEST(ARRAY[{sample_bin_insert$sample_bin_code*}]::bin_code_enum[]),
         UNNEST(ARRAY[{sample_bin_insert$min_fork_length*}]),
         UNNEST(ARRAY[{sample_bin_insert$max_fork_length*}]),
         UNNEST(ARRAY[{sample_bin_insert$expected_number_of_samples*}])
        ) RETURNING id, sample_event_id, sample_bin_code;",
    .con = con)

  res <- DBI::dbSendQuery(con, query)

  sample_bin_id <- DBI::dbFetch(res) |>
    dplyr::transmute(sample_bin_id = as.numeric(id), sample_event_id,
                     sample_bin_code = as.character(sample_bin_code))

  DBI::dbClearResult(res)

  sample_id_insert <- dplyr::left_join(sample_bin_insert, sample_bin_id,
                                       by = c("sample_bin_code", "sample_event_id"))

  return(sample_id_insert)
}

#' Add samples
add_samples <- function(con, sample_plan, sample_id_insert) {

  sample_id <- sample_id_insert |>
    tidyr::uncount(expected_number_of_samples, .remove = FALSE) |>
    dplyr::group_by(sample_event_id) |>
    dplyr::mutate(sample_number = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(id = paste0(location_code, format(as.Date(first_sample_date), "%y"),
                              "_", sample_event_number, "_", sample_bin_code, "_", sample_number)) |>
    dplyr::select(id, sample_bin_id)

  query <- glue::glue_sql("INSERT INTO sample (id, sample_bin_id)
                                    VALUES (
                                      UNNEST(ARRAY[{sample_id$id*}]),
                                      UNNEST(ARRAY[{sample_id$sample_bin_id*}])
                                    ) RETURNING id;",
                                    .con = con)

  res <- DBI::dbSendQuery(con, query)

  sample_ids <- DBI::dbFetch(res) |>
    dplyr::pull(id)

  DBI::dbClearResult(res)

  return(sample_ids)
}

add_sample_status <- function(con, sample_ids) {
  status_code <- rep(1, length(sample_ids))

  sample_status_query <- glue::glue_sql("INSERT INTO sample_status (sample_id, status_code_id)
                                        VALUES (
                                          UNNEST(ARRAY[{sample_ids*}]),
                                          UNNEST(ARRAY[{status_code*}])
                                        );",
                                        .con = con)

  DBI::dbExecute(con, sample_status_query)

}


is_valid_sample_plan <- function(sample_plan) {

  if (!is.data.frame(sample_plan)) {
    stop("Please provide agency as a dataframe", call. = FALSE)
  }

  if (!identical(sapply(sample_plan, class), sapply(sample_plan_template, class))) {
    stop("The sample plan supplied is not valid, reference `grunID::sample_plan_template`", call. = FALSE)
  }

  # check that the locations in the data match those that exist in the database
  # WARNING this list can get stale
  valid_locations <- c("BTC", "BUT", "CLR", "DER", "FTH_RM17", "MIL", "DEL", "KNL",
                       "TIS", "FTH_RM61")

  if (!all(sample_plan$location_code %in% valid_locations)) {
    stop(sprintf("location_code provided not one of valid codes %s",
                 paste0(valid_locations, collapse = ", ")), call. = FALSE)
  }

  if (!all(is.integer(sample_plan$sample_event_number))) {
    stop("sample_event_number must be an integer", call. = FALSE)
  }

  # validate first_sample_date
  if (any(is.na(as.Date(sample_plan$first_sample_date)))) {
    stop("one or more values in first_sample_date could not be parsed", call. = FALSE)
  }

  # validate sample_bin_code
  if (any(!grepl("^[A-E]$", sample_plan$sample_bin_code))) {
    stop("sample_bin_code should be a single capital letter A-E", call. = FALSE)
  }

  # validate min_fork_length
  if (any(is.na(sample_plan$min_fork_length))) {
    stop("min_fork_length missing in at least one record", call. = FALSE)
  }

  if(any(sample_plan$min_fork_length > sample_plan$max_fork_length)) {
    stop("min_fork_length cannot be larger than the max_fork_length for a given
         sample bin")
  }

  min_fl_range <- range(sample_plan$min_fork_length, na.rm = TRUE)
  if (min_fl_range[1] < 0 | min_fl_range[2] > 300) {
    stop("value in min_fork_length is out of valid ranges (0 - 2000)", call. = TRUE)
  }

  if (any(is.na(sample_plan$max_fork_length))) {
    stop("max_fork_length missing in at least one record", call. = FALSE)
  }

  max_fl_range <- range(sample_plan$max_fork_length, na.rm = TRUE)
  if (max_fl_range[1] < 0 | max_fl_range[2] > 300) {
    stop("value in min_fork_length is out of valid ranges (0 - 2000)", call. = TRUE)
  }


}
