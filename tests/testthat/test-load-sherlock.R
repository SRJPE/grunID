skip_if_not_installed("RSQLite")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    add_plate_run(con, "A"),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    add_assay_results(con, "A"),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

})
