skip_if_not_installed("RSQLite")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    get_status_codes(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    add_status_code(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    update_status_code(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    delete_status_code(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

})


test_that('add status code identifies invalid agencies', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  expect_error(add_status_code(con, status_code = "dog"),
               "^Please provide status code as a dataframe$")

  expect_error(add_status_code(con, status_code = data.frame(dog = 1)))

  DBI::dbDisconnect(con)

})
