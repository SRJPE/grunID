#' @title Check-in Samples from field
#' @export
check_in_jpe_field_samples <- function(con, filepath, season = year(today())) {
  raw_data <- readxl::read_excel(filepath,
                                 col_names = c("sample_id", "received_sample", "event", "entered_by", "verified_by", "alias", "comments"),
                                 skip = 1)

  samples_received <- raw_data |>
    filter(!is.na(sample_id))

  season_regex <- paste0(".", season - 2000, ".")
  samples_in_db_for_season <- tbl(con, "sample") |> filter(str_detect(id, season_regex))
  samples_in_db_for_season_ids <- samples_in_db_for_season |> collect() |> pull(id)


  samples_to_be_created <- samples_received$sample_id[which(!(samples_received$sample_id %in% samples_in_db_for_season_ids))]

  if (length(samples_to_be_created) > 0) {
    temp_df <- tibble(sample_id = samples_to_be_created) %>%
      mutate(
        matches = str_match(sample_id, "^([A-Z0-9]+)(\\d{2})_(\\d{1,2})_([A-Z])_(\\d{1,2})$"),
        location = matches[,2],
        year = as.numeric(matches[,3]),
        event = as.numeric(matches[,4]),
        bin = matches[,5],
        sample_no = as.numeric(matches[,6])
      ) %>%
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
             max_fork_length = as.integer(max_fork_length))



    grunID::add_sample_plan(con, temp_df, sample_status_code = "returned from field", verbose = TRUE)

    cli::cli_alert(glue::glue("created the following new samples in database:\n {paste0(samples_to_be_created, collapse = ', ')}"))
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
