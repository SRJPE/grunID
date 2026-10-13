base_args <- list(
  con = "fake_connection",
  protocol_name = "a protocol",
  genetic_method = "a method",
  laboratory = "a lab",
  lab_work_performed_by = "user",
  description = "a test run",
  date_run = "2024-01-01",
  filepath = list(name = "f", datapath = "f"),
  sample_type = "mucus"
)

test_that("add_new_plate_results rejects an unknown instrument", {
  mockery::stub(add_new_plate_results, "is_valid_connection", TRUE)

  expect_error(
    do.call(add_new_plate_results, c(base_args, list(
      instrument = "some_other_machine",
      layout_type = "split_plate_early_late"
    ))),
    "instrument must be one of 'sherlock' or 'sherquant'"
  )
})

test_that("add_new_plate_results requires layout_type when instrument is sherlock", {
  mockery::stub(add_new_plate_results, "is_valid_connection", TRUE)

  expect_error(
    do.call(add_new_plate_results, c(base_args, list(
      instrument = "sherlock",
      layout_type = NULL
    ))),
    "you must provide a valid layout type"
  )
})

test_that("add_new_plate_results does not require layout_type when instrument is sherquant", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE protocol (id INTEGER, name TEXT, run_mode TEXT, optics TEXT, light_source TEXT, lamp_energy TEXT)")

  sherquant_args <- base_args
  sherquant_args$con <- con

  # with layout_type left NULL, the next thing hit should be the protocol
  # lookup (a real DB call returning zero rows) rather than the layout_type
  # guard - confirms sherquant is exempt from that check
  expect_error(
    do.call(add_new_plate_results, c(sherquant_args, list(
      instrument = "sherquant",
      layout_type = NULL
    ))),
    "there are not protocols with name"
  )
})
