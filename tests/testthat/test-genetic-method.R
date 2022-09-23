skip_if_not_installed("RSQLite")

test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(con)

  expect_error(
    get_genetic_methods(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    add_genetic_method(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    update_genetic_method(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

  expect_error(
    delete_genetic_method(con),
    "Connection argument does not have a valid connection the run-id database.
         Please try reconnecting to the database using 'DBI::dbConnect'"
  )

})

test_that('duplicate genetic methods are not added', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  error_message_from_db <- 'Error: COPY returned error: ERROR: duplicate key value violates unique constraint'
  genetic_method <- data.frame(
    code = "SHLK",
    method_name = "SHERLOCK",
    description = "SHERLOCK (Specific High-sensitivity Enzymatic Reporter unLOCKing)
    uses the CRISPR-Cas13a platform  to accurately, sensitively and rapidly distinguish
    three fish species of management interest co-occurring in the San Francisco
    Estuary that are easily misidentified in the field."
  )
  mockery::stub(add_genetic_method, 'DBI::dbAppendTable', function(con, table, genetic_method) {stop(error_message_from_db)})
  expect_error(add_genetic_method(con, genetic_method))
  # TODO what is with this error message
               # "This genetic_method already exists in the database")
  DBI::dbDisconnect(con)
})

test_that('add genetic method identifies invalid method formats', {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_error(add_genetic_method(con, genetic_method = "dog"),
               "^Please provide genetic method as a dataframe$")

  expect_error(add_genetic_method(con, genetic_method = data.frame(dog = 1)))
  # TODO what is with this error message
# 'The genetic method supplied is not valid, see `help("add_genetic_method")` for correct format'
  DBI::dbDisconnect(con)

})
