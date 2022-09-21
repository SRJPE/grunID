skip_if_not_installed("RSQLite")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    add_enum_value(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )


})

test_that('duplicate enum values are not added', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  error_message_from_db <- 'Error: COPY returned error: ERROR: duplicate key value violates unique constraint'

  mockery::stub(add_enum_value, 'DBI::dbExecute', function(con, query) {stop(error_message_from_db)})
  expect_error(add_enum_value(con, "bin_code_enum", "A"))

  DBI::dbDisconnect(con)
})

test_that('add enum value identifies invalid enum values', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_error(add_enum_value(con, "bin_code_enum", " a"),
               "^Bin code must be a single capital letter$")

  expect_error(add_enum_value(con, "life_stage_enum", "toddler"),
               "^New enum value must begin with a capital letter$")

  expect_error(add_enum_value(con, "well_location_enum", "a1"),
               "Well location value must be an alpha numeric code")

  expect_error(add_enum_value(con, "well_location_enum", "Dog1"),
               "Well location value must be an alpha numeric code")

  expect_error(add_enum_value(con, "well_location_enum", "1D"),
               "Well location value must be an alpha numeric code")

  expect_error(add_enum_value(con, "well_location_enum", "DD1"),
               "Well location value must be an alpha numeric code")

  expect_error(add_enum_value(con, "well_location_enum", "D1D"),
               "Well location value must be an alpha numeric code")

  DBI::dbDisconnect(con)

})
