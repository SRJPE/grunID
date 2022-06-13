test_that("non-valid connection errors correctly", {
  con <- dbConnect(RSQLite::SQLite())
  DBI::dbDisconnect(con)

  expect_error(
    add_plate_run(con, "A"),
    "Connection argument does not have a valid connection the run-id database"
  )

  closeAllConnections()
})
