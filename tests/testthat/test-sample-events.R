source("helper-sample-events.R")

# skip_if_not_installed("RSQLite")
#
# test_that("non-valid connection errors correctly", {
#   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#   DBI::dbDisconnect(con)
#
#   expect_error(
#     add_sample_events(con, "A"),
#     "Connection argument does not have a valid connection the run-id database"
#   )
#
# })
#
# test_that('create sample events works', {
#   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#   on.exit(DBI::dbDisconnect(con))
#
#   # test data
#   sample_bins <- tibble::tibble(
#     location_code = "BTC",
#     sample_event_number = 1,
#     first_sample_date = "2022-01-01",
#     sample_bin_code = "A",
#     min_fork_length = 10,
#     max_fork_length = 15,
#     expected_number_of_samples = 5
#   )
#
#   # mock data
#   sample_locations <- structure(list(id = 1L, code = "BTC", location_name = "Battle",
#                                      stream_name = "Battle Creek", description = "Upper Battle Creek (RM 6.2 above CNFH barrier weir)",
#                                      managing_agency_id = 3L, latitude = 38.5471828481241, longitude = -121.699675015962,
#                                      geom = structure("0101000020E6100000A5D8B779C76C5EC005D06A160A464340", class = "pq_geometry"),
#                                      created_at = structure(1654723304.38836, tzone = "UTC", class = c("POSIXct", "POSIXt")),
#                                      created_by = "postgres", updated_at = structure(1654723304.38836, tzone = "UTC", class = c("POSIXct", "POSIXt")),
#                                      updated_by = "postgres"), row.names = c(NA, -1L), class = "data.frame")
#
#   # mock functions
#   mockery::stub(add_sample_events, 'dplyr::tbl', sample_locations)
#   mockery::stub(add_sample_events, 'DBI::dbSendQuery', T)
#   mockery::stub(add_sample_events, 'DBI::dbFetch',
#                 tibble::tibble(id = 1, sample_event_id = 2, sample_bin_code = "A",
#                                sample_event_number = 1))
#   mockery::stub(add_sample_events, 'DBI::dbClearResult', T)
#   mockery::stub(add_sample_events, 'DBI::dbExecute', 5)
#
#   expect_equal(add_sample_events(con, sample_bins), 5)
#
# })


test_that("sample bins data is valid", {
  bad_sample_bin_location <- tibble::tibble(
    location_code = c("BTC", "DUR"),
    sample_event_number = 1:2,
    first_sample_date = "2020-01-01",
    sample_bin_code = "A",
    min_fork_length = 10,
    max_fork_length = 95,
    expected_number_of_samples = 10
  )

  good_sample_bin_location <- tibble::tibble(
    location_code = c("BTC", "BUT"),
    sample_event_number = 1:2,
    first_sample_date = "2020-01-01",
    sample_bin_code = "A",
    min_fork_length = 10,
    max_fork_length = 95,
    expected_number_of_samples = 10
  )

  bad_sample_bin_location_NA <- tibble::tibble(
    location_code = c("BTC", NA),
    sample_event_number = 1:2,
    first_sample_date = "2020-01-01",
    sample_bin_code = "A",
    min_fork_length = 10,
    max_fork_length = 95,
    expected_number_of_samples = 10
  )

  bad_sample_bin_location_missing <- tibble::tibble(
    sample_event_number = 1:2,
    first_sample_date = "2020-01-01",
    sample_bin_code = "A",
    min_fork_length = 10,
    max_fork_length = 95,
    expected_number_of_samples = 10
  )

  bad_sample_bin_event_number <- tibble::tibble(
    location_code = c("BTC", "BUT"),
    sample_event_number = "1",
    first_sample_date = "2020-01-01",
    sample_bin_code = "A",
    min_fork_length = 10,
    max_fork_length = 95,
    expected_number_of_samples = 10
  )


  mockery::stub(validate_sample_bins, 'get_sample_location', sample_locations)
  con <- TRUE


  expect_error(validate_sample_bins(con, bad_sample_bin_location),
               "location_code provided not one of valid codes BTC, BUT, CLR, DER, FTH_RM17, MIL, DEL, KNL, TIS, FTH_RM61$")

  expect_error(validate_sample_bins(con, bad_sample_bin_location_NA),
               "location code missing in at least one record$")

  expect_error(
    validate_sample_bins(con, bad_sample_bin_location_missing),
    "the following column\\(s\\) are missing\\: location_code$"
    )

  expect_error(
    validate_sample_bins(con, bad_sample_bin_event_number),
    "sample_event_number must be numeric"
  )

  #'                      location_code = c("BTC", "BUT"),
  #'                      sample_event_number = 1:2,
  #'                      first_sample_date = "2020-01-01",
  #'                      sample_bin_code = "A",
  #'                      min_fork_length = 10,
  #'                      max_fork_length = 95,
  #'                      expected_number_of_samples = 10

  # sample date
  bad_sample_bins_date <- tibble::tibble(
    location_code = c("BTC"),
    sample_event_number = 1,
    first_sample_date = c("2020-01-01", "2020-01-01 12:00:00", "fdsafdsa", "03/14/1990"),
    sample_bin_code = "A",
    min_fork_length = 10,
    max_fork_length = 95,
    expected_number_of_samples = 10
  )

  expect_error(
    validate_sample_bins(con, bad_sample_bins_date),
    "one or more values in first_sample_date could not be parsed$"
  )

  # sample bin code
  bad_sample_bins_code <- tibble::tibble(
    location_code = c("BTC", "BUT"),
    sample_event_number = 1:2,
    first_sample_date = "2020-01-01",
    sample_bin_code = NA,
    min_fork_length = 10,
    max_fork_length = 95,
    expected_number_of_samples = 10
  )

  expect_error(
    validate_sample_bins(con, bad_sample_bins_code),
    "sample_bin_code missing in at least one record$"
  )


  # min fork length
  bad_sample_bins_min_fl <- tibble::tibble(
    location_code = c("BTC"),
    sample_event_number = 1,
    first_sample_date = "2020-01-01",
    sample_bin_code = "A",
    min_fork_length = c(0, -1, NA, 2000),
    max_fork_length = 95,
    expected_number_of_samples = 10
  )

  # check for missing
  expect_error(
    validate_sample_bins(con, bad_sample_bins_min_fl),
    "min_fork_length missing in at least one record$"
  )

  # check out of range
  expect_error(
    validate_sample_bins(con, subset(bad_sample_bins_min_fl,
                                     !is.na(min_fork_length))),
    "value in min_fork_length is out of valid ranges \\(0 - 2000\\)$"
  )


  #
  # # max fork length
  # good_sample_bin_location <- tibble::tibble(
  #   location_code = c("BTC", "BUT"),
  #   sample_event_number = 1:2,
  #   first_sample_date = "2020-01-01",
  #   sample_bin_code = "A",
  #   min_fork_length = 10,
  #   max_fork_length = 95,
  #   expected_number_of_samples = 10
  # )
  #
  # expect_error(
  #   validate_sample_bins(con, bad_sample_bins_max_fl)
  # )
  #
  # # expected number of samples
  # good_sample_bin_location <- tibble::tibble(
  #   location_code = c("BTC", "BUT"),
  #   sample_event_number = 1:2,
  #   first_sample_date = "2020-01-01",
  #   sample_bin_code = "A",
  #   min_fork_length = 10,
  #   max_fork_length = 95,
  #   expected_number_of_samples = 10
  # )
  #
  # expect_error(
  #   validate_sample_bins(con, bad_sample_bins_num_of_samples)
  # )


})

