skip_if_not_installed("RSQLite")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    get_agencies(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    add_agency(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    update_agency(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    delete_agency(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

})

test_that('duplicate agencies are not added', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  error_message_from_db <- 'Error: COPY returned error: ERROR: duplicate key value violates unique constraint'

  mockery::stub(add_agency, 'DBI::dbExecute', function(con, query) {stop(error_message_from_db)})
  expect_error(add_agency(con, data.frame(code="DWR", agency_name="Deparment of Water Resources")))

  DBI::dbDisconnect(con)
})

test_that('add agency identifies invalid agencies', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  expect_error(add_agency(con, agency = "dog"),
               "^Please provide agency as a dataframe$")

  expect_error(add_agency(con, agency = data.frame(dog = 1)))

  DBI::dbDisconnect(con)

})
