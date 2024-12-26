#' @title Check-in Samples from field
#' @param con a connection to the database
#' @param filepath filepath to check-in excel file
#' @param season year for season
#' @export
check_in_jpe_field_samples <- function(con, filepath, season = get_current_season()) {
  raw_data <- readxl::read_excel(filepath,
                                 col_names = c("sample_id", "received_sample", "event", "entered_by", "verified_by", "alias", "comments"),
                                 skip = 1)

  samples_received <- raw_data |>
    filter(!is.na(sample_id), received_sample == "Y")

  samples_requested_to_be_removed <- raw_data |>
    filter(!is.na(sample_id), received_sample == "N") |> pull(sample_id)

  # for samples that need to be removed, they must be in a returned for field status,
  # otherwise we will not remove them
  if (length(samples_requested_to_be_removed) > 0) {
    logger::log_info("Sample deletion requested for {length(samples_requested_to_be_removed)} samples")
    rff_id <- get_status_codes(con, is_active=T, all_results=F, status_code_name == "returned from field") |>
      pull(id)
    sample_to_be_removed <- tbl(con, "sample_status") |>
      filter(sample_id %in% samples_requested_to_be_removed,
             status_code_id == rff_id) |>
      pull(sample_id)
    if (length(sample_to_be_removed) > 0) {
      sql_delete_arc_plate <- glue::glue_sql(
        "DELETE FROM sample_archive_plates where sample_id IN ({paste(sample_to_be_removed, collapse=',')})",
        .con = con
      )
      DBI::dbExecute(con, sql_delete_arc_plate)

      sql_delete_sample_status <- glue::glue_sql(
        "DELETE FROM sample_status where sample_id IN ({paste(sample_to_be_removed, collapse=',')})",
        .con = con
      )
      logger::log_info(sql_delete_sample_status)
      DBI::dbExecute(con, sql_delete_sample_status)

      sql_delete_sample <- glue::glue_sql(
        "DELETE FROM sample where id IN ({paste(sample_to_be_removed, collapse=',')})",
        .con = con)
      DBI::dbExecute(con, sql_delete_sample)
      logger::log_info("samples deleted")
    } else {
      logger::log_info("no samples found on database that needed deletion")
    }
  }

  season_code <- stringr::str_sub(as.character(season$year), 3, 4)
  samples_in_db_for_season <- tbl(con, "sample") |> filter(season == season_code)
  samples_in_db_for_season_ids <- samples_in_db_for_season |> collect() |> pull(id)


  samples_to_be_created <- samples_received$sample_id[which(!(toupper(samples_received$sample_id) %in% samples_in_db_for_season_ids))]

  if (length(samples_to_be_created) > 0) {
    temp_df <- tibble(sample_id = toupper(samples_to_be_created)) |>
      mutate(
        matches = str_match(sample_id, "^([A-Z0-9]+)(\\d{2})_(\\d{1,2})_([A-Z])_(\\d{1,3})$"),
        location = matches[,2],
        year = as.numeric(matches[,3]),
        event = as.numeric(matches[,4]),
        bin = matches[,5],
        sample_no = as.numeric(matches[,6])
      ) |>
      select(location, year, event, bin, sample_no) |>
      group_by(location, year, event, bin) |>
      summarise(
        max_sample_no = max(sample_no)
      ) |>
      ungroup() |>
      transmute(
        location_code = location,
        sample_event_number = event,
        first_sample_date = lubridate::as_date(paste0(year, "-01-01")),
        sample_bin_code = bin,
        min_fork_length = 1,
        max_fork_length = 200,
        expected_number_of_samples = as.numeric(max_sample_no)
      ) |>
      mutate(sample_event_number = as.integer(sample_event_number),
             min_fork_length = as.integer(min_fork_length),
             max_fork_length = as.integer(max_fork_length)) |>
      filter(!is.na(location_code))



    grunID::add_sample_plan(con, temp_df, sample_status_code = "created", verbose = TRUE)

    cli::cli_alert(glue::glue("created the following new samples in database:\n {paste0(samples_to_be_created, collapse = ', ')}"))
  }


  created_status_id <- DBI::dbGetQuery(con, "select id from status_code where status_code_name = 'created'")[[1]]

  # set sample status to returned from field to all of these that currently have status of "created"
  samples_current_status_is_created <- DBI::dbGetQuery(con,
                                                       "
SELECT sample_id, status_code_id
FROM (
    SELECT sample_id, status_code_id,
           ROW_NUMBER() OVER (PARTITION BY sample_id ORDER BY updated_at DESC) AS row_num
    FROM sample_status
) sub
WHERE row_num = 1;") |>
    filter(status_code_id == created_status_id)

  samples_needs_status_update <-
    samples_current_status_is_created$sample_id[which(samples_current_status_is_created$sample_id %in% samples_received$sample_id)]

  if (length(samples_needs_status_update) > 0) {
    set_sample_status(con, samples_needs_status_update, sample_status_code = "returned from field")
  }

  # add comments to samples that have them in the file
  samples_received_with_comments <- samples_received |>
    filter(!is.na(comments))

  purrr::walk(seq_len(nrow(samples_received_with_comments)), function(idx) {
    this_sample_id <- samples_received_with_comments$sample_id[idx]
    this_comment <- samples_received_with_comments$comments[idx]

    update_statement <- glue::glue_sql("update sample set comment = {this_comment} where id = {this_sample_id};", .con = con)
    DBI::dbExecute(con, update_statement)
  })


  return(samples_to_be_created)

}
