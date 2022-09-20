skip_if_not_installed("RSQLite")
source("helper-protocol.R")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    get_protocol(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    add_protocol(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

})

test_that('duplicate protocols are not added', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  error_message_from_db <- 'Error: COPY returned error: ERROR: duplicate key value violates unique constraint "protocol_software_version_reader_type_reader_serial_number__key"
DETAIL: Key (software_version, reader_type, reader_serial_number, plate_type, set_point, preheat_before_moving, runtime, "interval", read_count, run_mode, excitation, emissions, optics, gain, light_source, lamp_energy, read_height)=(3.11.20, Synergy H1, 21092224, Greiner 384 F bottom, 37, t, 02:00:00, 00:03:00, 41, Kinetic, 484, 530, Top, 100, Xenon Flash, High, 9) already exists.
CONTEXT: COPY protocol, line 1'

  mockery::stub(add_protocol, 'DBI::dbAppendTable', function(con, table, protocol) {stop(error_message_from_db)})
  expect_error(add_protocol(con, protocols), "This protocol already exists in the database")
  DBI::dbDisconnect(con)
})

test_that('add protocol works', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_error(add_protocol(con, protocol = "dog"),
               "^Please provide protocol as a dataframe$")

  expect_error(add_protocol(con, protocol = protocols[, 2:3]),
               "^The protocol supplied is not valid, reference `grunID::protocol_template`$")

  DBI::dbDisconnect(con)

})

