test_that("non-valid connection errors correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite())
  DBI::dbDisconnect(con)

  expect_error(
    add_plate_run(con, "A"),
    "Connection argument does not have a valid connection the run-id database"
  )

  closeAllConnections()
})


test_that("non-valid plate_run_settings errors correctly for wrong names", {
  con <- DBI::dbConnect(RSQLite::SQLite())

  pr_settings <- structure(list(plate_num = "Plate 1", software_version = "3.11.19",
                                date = structure(18890, class = "Date"), reader_type = "Synergy H1",
                                reader_serial_number = "2104013D", plate_type = "BRAND 96/transparent/black/white F-bottom",
                                set_point = 37, preheat_before_moving = TRUE, runtime = "0:30:00",
                                interval = "0:01:00", read_count = 31L, run_mode = "Kinetic",
                                excitation = 485L, emissions = 528L, optics = "Top", gain = 100L,
                                light_source = "Xenon Flash", lamp_energy = "High", read_height = 7L,
                                genetic_method = 1, laboratory_id = 1, lab_work_preformed_by = "dog"), row.names = c(NA,
                                                                                                                        -1L), class = c("tbl_df", "tbl", "data.frame"))

  expect_error(
    add_plate_run(con, pr_settings),
    "'plate_run_settings' file is not formatted correctly")
  DBI::dbDisconnect(con)
})