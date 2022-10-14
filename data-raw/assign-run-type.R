results$raw_assay_results


# get the last time step for blank values
plate_run_uid

# to get the last time stamp just get the value of run time
# you can filter the results to this time value
final_time_step <- get_protocols(con) |>
  filter(id == protocol_id) |>
  select(runtime) |>
  pull()

blank_values <- results$raw_assay_results |>
  filter(time == final_time_step,
         sample_id == "BLK") |>
  pull(raw_fluorescence) |>
  as.numeric()

mean(blank_values) * 2

results$raw_assay_results |>
  distinct(time)
