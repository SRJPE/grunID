filepath = "data-raw/exampleoutput_synergyH1trial_data_092021.xlsx"
sample_details = layout
plate_size = 96
range <- "B113:AA145"
stat_col_id <- "...14"

readxl::read_excel(filepath,
                   range = range, col_types = "text") |>
  tidyr::fill(...1) |>
  tidyr::pivot_longer(names_to = "number_location", values_to = "result", !c(...1, stat_col_id)) |>
  dplyr::rename(metric = stat_col_id) |>
  dplyr::arrange(...1, as.numeric(number_location)) |>
  dplyr::transmute(location = paste0(...1, number_location), result, metric) |>
  dplyr::filter(!is.na(result)) |>
  dplyr::left_join(layout) |>
  dplyr::mutate(metric = stringr::str_remove(metric, "\\s\\[.+\\]")) |>
  dplyr::select(sample_id, sample_type_id, assay_id, result, metric,
                plate_run_id, well_location = location) |>
  tidyr::pivot_wider(names_from = "metric", values_from = "result") |>
  dplyr::mutate(
    t_at_max_v = as.POSIXct.numeric(`t at Max V`, origin = "1899-12-30")
  ) |> glimpse()


as.POSIXct(0.0111111111111111, origin = "1899-12-30")
as.POSIXct(0.00983796296296296, origin = "1899-12-30")
as.POSIXct(0.000379629629, origin = "1899-12-30")

lubridate::as_datetime(0.0111111111111111, origin = "1999-12-31 00:00:00")
