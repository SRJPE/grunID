skip_if_not_installed("RSQLite")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    get_laboratories(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    add_laboratory(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    update_laboratory(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    delete_laboratory(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

})


test_that('add laboratory identifies invalid agencies', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  expect_error(add_laboratory(con, laboratory = "dog"),
               "^Please provide laboratory as a dataframe$")

  expect_error(add_laboratory(con, laboratory = data.frame(dog = 1)))

  DBI::dbDisconnect(con)

})
