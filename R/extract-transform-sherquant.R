#' Process Sherquant results
#' @description `process_sherquant()` processes output from the Sherquant machine
#' @export
#' @md
process_sherquant() <- function(filepath) {
  raw_results <- read_xlsx(
    filepath,
    sheet = "Multicomponent Data",
    skip = 45
  ) |>
    janitor::clean_names()

  max_cycle <- max(data$cycle)

  data <- raw_results |>
    filter(cycle == max_cycle) |>
    select(well_position, raw_fluorescence = fam) |>
    filter(!is.na(raw_fluorescence))

  # needs to return list format where $data contains
  # sample_id, sample_type_id, assay_id, plate_run_id, raw_fluorescence, background_value, time, well_location, sub_plate
  return(data)
}
