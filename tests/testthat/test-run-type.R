skip_if_not_installed("RSQLite")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    get_run_types(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    add_run_type(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    update_run_type(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    delete_run_type(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

})


test_that('add laboratory identifies invalid agencies', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  expect_error(add_run_type(con, run_type = "dog"),
               "^Please provide run type as a dataframe$")

  expect_error(add_run_type(con, run_type = data.frame(dog = 1)))

  DBI::dbDisconnect(con)

})
