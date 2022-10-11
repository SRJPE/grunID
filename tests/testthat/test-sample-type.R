skip_if_not_installed("RSQLite")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    get_sample_types(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    add_sample_type(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    update_sample_type(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    delete_sample_type(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

})


test_that('add laboratory identifies invalid agencies', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  expect_error(add_sample_type(con, sample_type = "dog"),
               "^Please provide sample type as a dataframe$")

  expect_error(add_sample_type(con, sample_type = data.frame(dog = 1)))

  DBI::dbDisconnect(con)

})
