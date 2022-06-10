skip_if_not_installed("RSQLite")

test_that('create sample events works', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  # test data
  sample_bins <- tibble::tibble(
    location_code = "BTC",
    sample_event_number = 1,
    first_sample_date = "2022-01-01",
    sample_bin_code = "A",
    min_fork_length = 10,
    max_fork_length = 15,
    expected_number_of_samples = 5
  )

  # mock data
  sample_locations <- structure(list(id = 1L, code = "BTC", location_name = "Battle",
                                     stream_name = "Battle Creek", description = "Upper Battle Creek (RM 6.2 above CNFH barrier weir)",
                                     managing_agency_id = 3L, latitude = 38.5471828481241, longitude = -121.699675015962,
                                     geom = structure("0101000020E6100000A5D8B779C76C5EC005D06A160A464340", class = "pq_geometry"),
                                     created_at = structure(1654723304.38836, tzone = "UTC", class = c("POSIXct", "POSIXt")),
                                     created_by = "postgres", updated_at = structure(1654723304.38836, tzone = "UTC", class = c("POSIXct", "POSIXt")),
                                     updated_by = "postgres"), row.names = c(NA, -1L), class = "data.frame")

  # mock functions
  mockery::stub(sample_events, 'dplyr::tbl', sample_locations)
  mockery::stub(sample_events, 'DBI::dbSendQuery', T)
  mockery::stub(sample_events, 'DBI::dbFetch',
                tibble::tibble(id = 1, sample_event_id = 2, sample_bin_code = "A",
                               sample_event_number = 1))
  mockery::stub(sample_events, 'DBI::dbClearResult', T)
  mockery::stub(sample_events, 'DBI::dbExecute', 5)

  expect_equal(sample_events(con, sample_bins), 5)

})
