sample_event_df <- tibble(
  location_code = c("BTC", "BAT"),
  sample_event_number = 1:2,
  first_sample_date = "2020-01-01",
  sample_bin_code = "A",
  min_fork_length = 10,
  max_fork_length = 95,
  expect_number_of_samples = 10
)


sample_events <- function(con, sample_event_df) {

  sample_event_inserts <-
    dplyr::distinct(sample_event_df, sample_event_number, first_sample_date)

  sample_event_query <- glue::glue_sql("INSERT INTO sample_event (sample_event_number, first_sample_date)
                 VALUES (
                  UNNEST(ARRAY[{sample_event_inserts$sample_event_number*}]),
                  UNNEST(ARRAY[{sample_event_inserts$first_sample_date*}]::DATE[])
                 ) RETURNING id;",
                 .con = con)

  res <- DBI::dbSendQuery(con, sample_event_query)
  sample_event_ids <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  # TODO: we need to unpack the values from the dataframe into the appropriate columns for sample_bins
  sample_bin_query <- glue::glue_sql("INSERT INTO sample_bin (sample_event_id, sample_location_id, sample_bin_code, min_fork_length, max_fork_length, expect_number_of_samples)
                                     VALUES (
                                     UNNEST(ARRAY[{sample_event_ids*}])
                                     , 1, 'A', 15, 25, 5);",
                                     .con = con)

}



