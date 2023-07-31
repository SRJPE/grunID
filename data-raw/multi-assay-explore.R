
run_genetic_identification <- function(con, sample_id = NULL, location = NULL, year = NULL, selection_strategy = "positive priority") {

  if (is.null(year)) {
    year <- as.integer(format(Sys.Date(), "%Y"))
  }

  if (!is.null(sample_id)) {

    sample_to_run_on <- dplyr::tbl(con, "sample") |>
      dplyr::filter(id == sample_id) |>
      dplyr::collect()

    if (nrow(sample_to_run_on) == 0) {
      cli::cli_abort(c("x" = "location specified did not return any data"))
    }
  } else if (!is.null(location)) {

    query <- glue::glue_sql("select distinct(sample.id), sample_location.code from sample
    join sample_bin on sample.sample_bin_id = sample_bin.id
    join sample_event on sample_bin.sample_event_id = sample_event.id
    join sample_location on sample_event.sample_location_id = sample_location.id
where date_part('year', sample_event.first_sample_date) = {year} and sample_location.code = {location}", .con = con)

    res <- DBI::dbSendQuery(con, query)
    sample_to_run_on <- DBI::dbFetch(res)
    DBI::dbClearResult(res)

    if (nrow(sample_to_run_on) == 0) {
      cli::cli_abort(c("x" = "location specified did not return any data"))
    }



  }

  early_late_resp <- purrr::map(
    sample_to_run_on$id, ~ots_early_late_detection(con, ., selection_strategy = selection_strategy),
    .progress = list(
      type = "iterator",
      name = "running early late detections",
      clear = FALSE
    ))

  early_late_resp_data <- parse_detection_results(early_late_resp)

  analysis_complete_status_insert <- early_late_resp_data |>
    dplyr::filter(status_code == "analysis complete")

  # updates status code to complete
  all_status_codes <- grunID::get_status_codes(con)
  status_code_name_to_id <- all_status_codes$id
  names(status_code_name_to_id) <- all_status_codes$status_code_name



  analysis_complete_status_insert$comment <- "auto-generated comment added when running this sample through run_genetic_identification"
  analysis_complete_status_insert$status_code_id <- status_code_name_to_id["analysis complete"]
  analysis_complete_status_insert <- dplyr::select(analysis_complete_status_insert, sample_id, status_code_id, comment)
  DBI::dbAppendTable(con, "sample_status", analysis_complete_status_insert)

  # updates run type
  all_run_type_id <- grunID::get_run_types(con)
  run_type_name_to_id <- all_run_type_id$id
  names(run_type_name_to_id) <- all_run_type_id$code

  analysis_complete_gen_insert <- early_late_resp_data |>
    dplyr::filter(status_code == "analysis complete")


  analysis_complete_gen_insert$run_type_id = as.numeric(run_type_name_to_id[analysis_complete_gen_insert$run_type])
  analysis_complete_gen_insert <- dplyr::select(analysis_complete_gen_insert, sample_id, run_type_id, early_plate, late_plate)
  analysis_complete_gen_insert$updated_at <- lubridate::now(tzone = "UTC")

  purrr::walk(1:nrow(analysis_complete_gen_insert), function(row) {
    this_sample_id <- analysis_complete_gen_insert$sample_id[row]
    this_run_type_id <- analysis_complete_gen_insert$run_type_id[row]
    this_early_plate <- analysis_complete_gen_insert$early_plate[row]
    this_late_plate <- analysis_complete_gen_insert$late_plate[row]

    insert_safely_Q <- glue::glue_sql(
      "INSERT INTO genetic_run_identification (sample_id, run_type_id, early_plate_id, late_plate_id, updated_at)
    VALUES
      ({this_sample_id}, {this_run_type_id}, {this_early_plate}, {this_late_plate}, CURRENT_TIMESTAMP AT TIME ZONE 'UTC')
    ON CONFLICT (sample_id) DO UPDATE
    SET
      run_type_id = EXCLUDED.run_type_id,
      early_plate_id = EXCLUDED.early_plate_id,
      late_plate_id = EXCLUDED.late_plate_id,
      updated_at = EXCLUDED.updated_at;
    ",
      .con = con
    )

    DBI::dbExecute(con, insert_safely_Q)
  })

  # at this point we have inserted all of the samples that were complete, we update created status
  created_status_to_insert <-
    early_late_resp_data |> dplyr::filter(status_code == "created")

  created_status_to_insert$comment <- "auto-generated comment added when running this sample through run_genetic_identification"
  created_status_to_insert$status_code_id <- status_code_name_to_id["created"]
  created_status_to_insert <- dplyr::select(created_status_to_insert, sample_id, status_code_id, comment)
  DBI::dbAppendTable(con, "sample_status", created_status_to_insert)

  # created cannot do any gen id updates

  # OTS28 in progress / eihter assay 1 or 2 is not done

  ots28_inprogress_to_insert <- early_late_resp_data |>
    dplyr::filter(status_code == "ots28 in progress")

  ots28_inprogress_to_insert$comment <- "auto-generated comment added when running this sample through run_genetic_identification"
  ots28_inprogress_to_insert$status_code_id <- status_code_name_to_id["ots28 in progress"]
  ots28_inprogress_to_insert <- dplyr::select(ots28_inprogress_to_insert, sample_id, status_code_id, comment)
  DBI::dbAppendTable(con, "sample_status", ots28_inprogress_to_insert)

  # needs ots 16 will be passed into function that tries to assign run based on assay 3 and 4

  ots_28_ids <- early_late_resp_data |> dplyr::filter(status_code == "need ots16")

  if (nrow(ots_28_ids) == 0) {
    cli::cli_warn(c("x" = "no samples are ready for ots 16"))
  }



  return(early_late_resp)
}


detection_results <- run_genetic_identification(con, location = "F17")
detection_data <- parse_detection_results(detection_results)
detection_data |> dplyr::filter(status_code != "created")

# next update the database with this information




cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = 5432,
                      user = cfg$username,
                      password = cfg$password)


samples_and_locations_q <- glue::glue_sql("select sample.id, sample_location.code from sample
    join sample_bin on sample.sample_bin_id = sample_bin.id
    join sample_event on sample_bin.sample_event_id = sample_event.id
    join sample_location on sample_event.sample_location_id = sample_location.id
where date_part('year', sample_event.first_sample_date) = 2022 and sample_location.code = 'F17';", .con = con)

res <- DBI::dbSendQuery(con, samples_and_locations_q)
db_data <- DBI::dbFetch(res)
DBI::dbClearResult(res)


grunID::ots_early_late_detection(con, "F1722_3_C_29", selection_strategy = "recent")



early_late_resp <- purrr::map(
  db_data$id, ~grunID::ots_early_late_detection(con, ., selection_strategy = "recent"),
  .progress = list(
    type = "iterator",
    name = "running early late detections",
    clear = FALSE
  ))

detection_results <- early_late_resp


detection_data <- purrr::map_df(detection_results, function(x) {
  tibble::tibble(
    sample_id = x$sample_id,
    status_code = x$status_code,
    run_type = x$run_type,
    early_plate = x$early_plate,
    late_plate = x$late_plate
  )
})


parse_detection_results <- function(detection_results) {
  purrr::map_df(detection_results, function(x) {
    data.frame(sample_id = x$sample_id,
               status_code = x$status_code,
               run_type = x$run_type,
               early_plate = x$early_plate,
               late_plate = x$late_plate)
  })

}

detection_data <- parse_detection_results(detection_results)














