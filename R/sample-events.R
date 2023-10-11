#' Add Sample Plan
#' @description `add_sample_plan` registers new sampling events and generates new sample ids
#' @param con A DBI connection object
#' @param sample_plan A table containing the following columns from a sample plan:
#' `location_code`, `sample_event_number`, `first_sample_date`, `sample_bin_code`, `min_fork_length`,
#' `max_fork_length`, and `expected_number_of_samples`.
#' @return a named value `number_of_samples_added` reflecting the number of
#' samples added to the database.
#' @examples
#' # example database connection
#' con <- gr_db_connect()
#' add_sample_plan(con, sample_plan)
#' @export
#' @md
add_sample_plan <- function(con, sample_plan, verbose = FALSE) {
  is_valid_connection(con)
  is_valid_sample_plan(sample_plan)

  sample_event_ids <- add_sample_events(con, sample_plan)
  sample_id_insert <- add_sample_bins(con, sample_plan, sample_event_ids)
  sample_ids <- add_samples(con, sample_plan, sample_id_insert, verbose = verbose)
  number_of_samples_added <- set_sample_status(con, sample_ids, 1)

  return(c("number_of_samples_added" = number_of_samples_added))
}

#' Create sample events
#' @export
add_sample_events <- function(con, sample_plan) {
  sample_locations <- get_sample_locations(con)

  sample_event_insert <- sample_plan |>
    dplyr::left_join(sample_locations, by = c("location_code" = "code")) |>
    dplyr::distinct(sample_event_number, first_sample_date,
                    sample_location_id = id)



  partitioned_inserts <- partition_df_to_size(sample_event_insert, 50)

  sample_event_ids <- purrr::map_df(partitioned_inserts, function(d) {
    sample_event_query <- glue::glue_sql(
      "INSERT INTO sample_event (sample_event_number, sample_location_id, first_sample_date)
        VALUES (
         UNNEST(ARRAY[{d$sample_event_number*}]),
         UNNEST(ARRAY[{d$sample_location_id*}]),
         UNNEST(ARRAY[{d$first_sample_date*}]::DATE[])
        )
        ON CONFLICT (sample_event_number, sample_location_id, first_sample_date)
        DO UPDATE SET
          sample_event_number = EXCLUDED.sample_event_number,
          sample_location_id = EXCLUDED.sample_location_id,
          first_sample_date = EXCLUDED.first_sample_date
      RETURNING id, sample_event_number;",
      .con = con)

    res <- tryCatch(DBI::dbSendQuery(con, sample_event_query),
                    error = function(e) {
                      if (grepl('duplicate key value violates unique constraint "sample_event_sample_event_number_sample_location_id_first_s_key"', e)) {
                        stop("Combination (sample_event_number, sample_location_id, first_sample_date) already exists in the database, this insert violates the unique contraint", call. = FALSE)
                      } else {
                        stop(e)
                      }
                    })

    on.exit(DBI::dbClearResult(res))

    DBI::dbFetch(res) |>
      dplyr::transmute(sample_event_id = as.numeric(id), sample_event_number)

  }, .progress = list(
    type = "iterator",
    name = "inserting sample events from plan",
    clear = TRUE,
    format_failed = "an error occured and operation was not complete.")
  )



  return(sample_event_ids)
}

#' Create sample bins
#' @export
add_sample_bins <- function(con, sample_plan, sample_event_ids) {

  sample_locations <- dplyr::tbl(con, "sample_location") |> dplyr::collect()
  sample_event_ids_for_insert <- dplyr::tbl(con, "sample_event") |>
    dplyr::filter(id %in% !!sample_event_ids$sample_event_id) |> dplyr::collect()

  unique_sample_bins_in_plan <- dplyr::distinct(.data = sample_plan,
                                                location_code,
                                                sample_event_number,
                                                first_sample_date,
                                                sample_bin_code,
                                                min_fork_length,
                                                max_fork_length,
                                                expected_number_of_samples)

  event_ids_with_locations <- dplyr::left_join(sample_event_ids_for_insert,
                                               sample_locations,
                                               by = c("sample_location_id" = "id")) |>
    dplyr::select(sample_event_id=id, sample_event_number, sample_location_id, code)

  sample_bin_insert <- dplyr::left_join(unique_sample_bins_in_plan, event_ids_with_locations,
                                        by = c("sample_event_number", "location_code" = "code"))


  # partitioned_inserts <- partition_df_to_size(sample_bin_insert, 100)

  # sample_id_insert <- purrr::map_df(partitioned_inserts, function(d) {


  query <- glue::glue_sql(
    "INSERT INTO sample_bin (sample_event_id, sample_bin_code, min_fork_length,
                               max_fork_length, expected_number_of_samples)
          VALUES (
           {sample_bin_insert$sample_event_id},
           {sample_bin_insert$sample_bin_code}::bin_code_enum,
           {sample_bin_insert$min_fork_length},
           {sample_bin_insert$max_fork_length},
           {sample_bin_insert$expected_number_of_samples}
          )
          ON CONFLICT (sample_event_id, sample_bin_code)
          DO UPDATE SET
            sample_event_id=EXCLUDED.sample_event_id,
            sample_bin_code=EXCLUDED.sample_bin_code,
            min_fork_length=EXCLUDED.min_fork_length,
            max_fork_length=EXCLUDED.max_fork_length,
            expected_number_of_samples=EXCLUDED.expected_number_of_samples,
            number_of_samples_received=EXCLUDED.number_of_samples_received
          RETURNING id, sample_event_id, sample_bin_code;",
    .con = con)



  sample_id_insert <- purrr::map_df(query, function(q) {
    res <- DBI::dbSendQuery(con, q)
    on.exit(DBI::dbClearResult(res))

    db_res <- DBI::dbFetch(res) |>
      dplyr::transmute(sample_bin_id = as.numeric(id), sample_event_id,
                       sample_bin_code = as.character(sample_bin_code))

    return(db_res)

  },
  .progress = list(
    type = "iterator",
    name = "inserting sample bins from plan",
    clear = FALSE))


  return(sample_id_insert)
}

#' Add samples
#' @export
add_samples <- function(con, sample_plan, sample_id_insert, verbose = FALSE) {

  event_ids <- unique(sample_id_insert$sample_event_id)

  all_locations <- get_sample_locations(con) |>
    dplyr::select(sample_location_id=id, location_code=code)

  sample_events_for_incoming_samples <- dplyr::tbl(con, "sample_event") |>
    dplyr::filter(id %in% event_ids) |>
    dplyr::select(id, sample_event_number, sample_event_id = id, sample_location_id) |>
    dplyr::collect()

  sample_id_inserts <- sample_plan |>
    dplyr::left_join(all_locations, by="location_code") |>
    dplyr::left_join(sample_events_for_incoming_samples, by = c("sample_event_number", "sample_location_id"="sample_location_id")) |>
    dplyr::left_join(sample_id_insert, by = c("sample_bin_code", "sample_event_id"))

  # set season based on water year
  sample_id <- sample_id_inserts |>
    tidyr::uncount(expected_number_of_samples, .remove = FALSE) |>
    dplyr::group_by(sample_event_id, sample_bin_code) |>
    dplyr::mutate(sample_number = dplyr::row_number(),
                  season = ifelse(as.numeric(format(as.Date(first_sample_date), "%m")) >= 10,
                                  as.numeric(format(as.Date(first_sample_date), "%y")) + 1,
                                  as.numeric(format(as.Date(first_sample_date), "%y"))),
                  season = as.character(season)) |>
    dplyr::ungroup() |>
    dplyr::mutate(id = paste0(location_code, season,
                              "_", sample_event_number, "_", sample_bin_code, "_", sample_number)) |>
    dplyr::select(id, sample_bin_id)

  partitioned_inserts <- partition_df_to_size(sample_id, 200)

  sample_ids <- purrr::map(
    partitioned_inserts,
    function(part) {
      query <- glue::glue_sql("INSERT INTO sample (id, sample_bin_id)
                                      VALUES (
                                        UNNEST(ARRAY[{part$id*}]),
                                        UNNEST(ARRAY[{part$sample_bin_id*}])
                                      ) ON CONFLICT (id) DO NOTHING
                              RETURNING id;",
                              .con = con)

      res <- DBI::dbSendQuery(con, query)
      on.exit(DBI::dbClearResult(res))

      gc(full = TRUE)

      DBI::dbFetch(res) |>
        dplyr::pull(id)

    }, .progress = list(
      type = "iterator",
      name = "inserting sample_id's for plan",
      clear = FALSE)
    )



  return(purrr::flatten_chr(sample_ids))
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
                       "TIS", "FTH_RM61", "F61", "F17", "YUR")

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


partition_df_to_size <- function(df, chunk_size) {
  split(df, (seq(nrow(df))-1) %/% chunk_size)
}

#' Process Raw Sample Plan
#' @description `process_raw_sample_plan`
#' @param filepath the filepath of the raw sample plan to be processed
#' @param season the season for which the sample plan was developed. format YYYY
#' @return a tidy dataframe that can be passed directly to `add_sample_plan()`. Has the following columns:
#' * location_code
#' * sample_event_number
#' * first_sample_date
#' * sample_bin_code
#' * min_fork_length
#' * max_fork_length
#' * expected_number_of_samples
#' @details this function assigns an maximum fork length of 200mm to all fork length bins that
#' are a "plus" (i.e. a fork length bin that is `130+` will be assigned a minimum fork length of 130 and
#' a maximum fork length of `200`). The function will also assign a `first_sample_date` of January 1st
#' of the season (if you pass `season = 2024`, the `first_sample_date` will be `2024-01-01`).
#' @examples
#' # example database connection
#' process_raw_sample_plan(filepath = "data-raw/2024_raw_sample_plan.xlsx", season = 2024)
#' @export
#' @md
process_raw_sample_plan <- function(filepath, season) {

  # read in file and skip first row
  raw_sample_plan <- suppressMessages(readxl::read_xlsx(filepath, skip = 1))
  fork_length_upper_limit <- 200
  location_code_lookup <- c("BTC", "BUT", "CLR", "DER", "MIL", "DEL", "KNL",
                            "TIS", "F61", "F17", "YUR")

  # manipulate into long data frame format
  clean_sample_plan <- raw_sample_plan |>
    dplyr::filter(`Bin FL ranges (mm)` != "Per-Event Total") |>
    tidyr::fill(Site) |>
    tidyr::pivot_longer(cols = E1:E14,
                        names_to = "sample_event_number",
                        values_to = "expected_number_of_samples") |>
    dplyr::filter(!is.na(expected_number_of_samples)) |>
    dplyr::mutate(fork_lengths = ifelse(stringr::str_detect(`Bin FL ranges (mm)`, "\\+"),
                                        stringr::str_replace(`Bin FL ranges (mm)`, "\\+",
                                                             paste0("-", fork_length_upper_limit)),
                                        `Bin FL ranges (mm)`),
                  sample_event_number = stringr::str_remove_all(sample_event_number, "E")) |>
    tidyr::separate_wider_delim(Site, delim = "(",
                                names = c("Name", "location_code")) |>
    tidyr::separate_wider_delim(fork_lengths, delim = "-",
                                names = c("min_fork_length", "max_fork_length")) |>
    dplyr::mutate(location_code = stringr::str_extract(location_code, paste(location_code_lookup, collapse = "|")),
                  first_sample_date = paste0(season, "-01-01")) |>
    dplyr::select(location_code, sample_event_number, first_sample_date,
                  sample_bin_code = Bin, min_fork_length, max_fork_length,
                  expected_number_of_samples)

  # get in correct format
  final_sample_plan <- clean_sample_plan |>
    dplyr::mutate(sample_event_number = as.integer(sample_event_number),
                  first_sample_date = as.Date(first_sample_date),
                  min_fork_length = as.integer(min_fork_length),
                  max_fork_length = as.integer(max_fork_length),
                  expected_number_of_samples = as.numeric(expected_number_of_samples)) |>
    suppressWarnings()

  cli::cli_alert_success("sample plan processed", "green")

  return(final_sample_plan)

}


