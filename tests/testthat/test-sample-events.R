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

  sample_locations <- structure(list(id = 1:10, code = c("BTC", "BUT", "CLR", "DER",
                                                         "FTH_RM17", "MIL", "DEL", "KNL", "TIS", "FTH_RM61"), location_name = c("Battle",
                                                                                                                                "Butte", "Clear", "Deer", "Feather-RM17", "Mill", "Sac-Delta Entry",
                                                                                                                                "Sac-KNL", "Sac-Tisdale", "Feather-RM61"), stream_name = c("Battle Creek",
                                                                                                                                                                                           "Butte Creek", "Clear Creek", "Deer Creek", "Feather River",
                                                                                                                                                                                           "Mill Creek", "Sacramento River", "Sacramento River", "Sacramento River",
                                                                                                                                                                                           "Feather River"), description = c("Upper Battle Creek (RM 6.2 above CNFH barrier weir)",
                                                                                                                                                                                                                             "Centerville Head Dam to Western Canal Water District Siphon",
                                                                                                                                                                                                                             "Lower Clear Creek - RM 1.7 and Upper Clear Creek - RM 8.4",
                                                                                                                                                                                                                             "RM 9.5 (RM 0 to RM 47)", "RM 17 (New \"lower\" RST)\r\n", "RM 5 (RM 0 to RM 51.44)",
                                                                                                                                                                                                                             "Delta Entry RM 75", "Knights Landing RM 88.5", "Tisdale RM 119.5",
                                                                                                                                                                                                                             "RM 17 (New \"lower\" RST)\r\n"), managing_agency_id = c(3L,
                                                                                                                                                                                                                                                                                      2L, 3L, 2L, 1L, 2L, 2L, 2L, 2L, 1L), latitude = c(38.5471828481241,
                                                                                                                                                                                                                                                                                                                                        38.5461087957394, 38.5579224896337, 38.5471828481241, 38.5461087957394,
                                                                                                                                                                                                                                                                                                                                        38.5579224896337, 38.5471828481241, 38.5461087957394, 38.5579224896337,
                                                                                                                                                                                                                                                                                                                                        38.5579224896337), longitude = c(-121.699675015962, -121.712721280568,
                                                                                                                                                                                                                                                                                                                                                                         -121.734693936748, -121.699675015962, -121.712721280568, -121.734693936748,
                                                                                                                                                                                                                                                                                                                                                                         -121.699675015962, -121.712721280568, -121.734693936748, -121.734693936748
                                                                                                                                                                                                                                                                                                                                        ), geom = structure(c("0101000020E6100000A5D8B779C76C5EC005D06A160A464340",
                                                                                                                                                                                                                                                                                                                                                              "0101000020E6100000FBCCB7399D6D5EC01CE19CE4E6454340", "0101000020E6100000BDB9B739056F5EC0FE560F016A474340",
                                                                                                                                                                                                                                                                                                                                                              "0101000020E6100000A5D8B779C76C5EC005D06A160A464340", "0101000020E6100000FBCCB7399D6D5EC01CE19CE4E6454340",
                                                                                                                                                                                                                                                                                                                                                              "0101000020E6100000BDB9B739056F5EC0FE560F016A474340", "0101000020E6100000A5D8B779C76C5EC005D06A160A464340",
                                                                                                                                                                                                                                                                                                                                                              "0101000020E6100000FBCCB7399D6D5EC01CE19CE4E6454340", "0101000020E6100000BDB9B739056F5EC0FE560F016A474340",
                                                                                                                                                                                                                                                                                                                                                              "0101000020E6100000BDB9B739056F5EC0FE560F016A474340"), class = "pq_geometry"),
                                     created_at = structure(c(1655487861.31747, 1655487861.31747,
                                                              1655487861.31747, 1655487861.31747, 1655487861.31747, 1655487861.31747,
                                                              1655487861.31747, 1655487861.31747, 1655487861.31747, 1655487861.31747
                                     ), class = c("POSIXct", "POSIXt"), tzone = "UTC"), created_by = c("postgres",
                                                                                                       "postgres", "postgres", "postgres", "postgres", "postgres",
                                                                                                       "postgres", "postgres", "postgres", "postgres"), updated_at = structure(c(1655487861.31747,
                                                                                                                                                                                 1655487861.31747, 1655487861.31747, 1655487861.31747, 1655487861.31747,
                                                                                                                                                                                 1655487861.31747, 1655487861.31747, 1655487861.31747, 1655487861.31747,
                                                                                                                                                                                 1655487861.31747), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                     updated_by = c("postgres", "postgres", "postgres", "postgres",
                                                    "postgres", "postgres", "postgres", "postgres", "postgres",
                                                    "postgres")), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA,
                                                                                                                          -10L))

  mockery::stub(validate_sample_bins, 'get_sample_location', sample_locations)
  con <- TRUE


  expect_error(validate_sample_bins(con, bad_sample_bin_location),
               "location_code provided not one of valid codes BTC, BUT, CLR, DER, FTH_RM17, MIL, DEL, KNL, TIS, FTH_RM61")
  expect_error(validate_sample_bins(con, bad_sample_bin_location_NA),
               "one of the records is missing a location code")
  expect_error(
    validate_sample_bins(con, bad_sample_bin_location_missing),
    "the following column\\(s\\) are missing\\: location_code$"
    )


})

