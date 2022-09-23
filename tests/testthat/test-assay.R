skip_if_not_installed("RSQLite")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    get_assays(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    add_assay(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    update_assay(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    delete_assay(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

})

test_that('duplicate agencies are not added', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  error_message_from_db <- 'Error: COPY returned error: ERROR: duplicate key value violates unique constraint'

  mockery::stub(add_assay, 'DBI::dbExecute', function(con, query) {stop(error_message_from_db)})
  expect_error(add_assay(con, data.frame(code="RE121", assay_name="Re 121", description = "WHAAAAtttt")))

  DBI::dbDisconnect(con)
})

test_that('add assay identifies invalid agencies', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  expect_error(add_assay(con, assay = "dog"),
               "^Please provide assay as a dataframe$")

  expect_error(add_assay(con, assay = data.frame(dog = 1)))

  DBI::dbDisconnect(con)

})
