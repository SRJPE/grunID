#' Process SherQuant results
#' @description `process_sherquant()` processes output from the Sherquant machine
#' @export
#' @md
process_sherquant <- function(filepath, sample_type, plate_run_id) {
  if (!class(plate_run_id) == "plate_run") {
    stop(sprintf("the plate_run_id must be created by calling 'add_plate_run'"))
  }

  metadata <- extract_sherquant_protocol(filepath)

  sample_details <- process_well_sample_details_sherquant(
    filepath,
    sample_type,
    plate_run_id$plate_run_id
  )

  raw_results <- readxl::read_xlsx(
    filepath,
    sheet = "Multicomponent Data",
    skip = 46
  ) |>
    janitor::clean_names()

  max_cycle <- max(raw_results$cycle)

  raw_assay_results <- raw_results |>
    dplyr::filter(cycle == max_cycle) |>
    dplyr::select(location = well_position, raw_fluorescence = fam) |>
    dplyr::filter(!is.na(raw_fluorescence)) |>
    dplyr::left_join(sample_details$data, by = "location") |>
    dplyr::mutate(
      background_value = NA_real_,
      time = metadata$runtime,
      # TODO confirm we won't have sub-plates?
      sub_plate = 1
    ) |>
    dplyr::select(
      sample_id,
      sample_type_id,
      assay_id,
      raw_fluorescence,
      plate_run_id,
      well_location = location,
      background_value,
      time,
      sub_plate
    ) |>
    dplyr::filter(!is.na(sample_id))

  return(
    structure(
      list(
        data = raw_assay_results,
        sample_details = sample_details$data
      ),
      class = "sherquant_output",
      filepath = filepath,
      plate_type = NA_character_,
      sample_type = sample_type,
      plate_run_id = plate_run_id,
      plate_size = metadata$plate_size
    )
  )
}

#' Process well sample details for SherQuant
#' @description `process_well_sample_details_sherquant()` processes well sample details from the Sherquant machine
#' @export
#' @md
process_well_sample_details_sherquant <- function(
  filepath,
  sample_type,
  plate_run_id
) {
  # read Sample Setup, take location straight from Well Position,
  # sample_id from Sample Name, and derive assay_id from Target Name
  # via case_when("Early" ~ 1, "Late" ~ 2, "Spring" ~ 3, "Winter" ~ 4).
  # No layout_type argument. Output columns must match
  # expected_layout_colnames() (location, sample_id, sample_type_id,
  # assay_id, plate_run_id)
  raw_well_data <- readxl::read_xlsx(
    filepath,
    sheet = "Sample Setup",
    skip = 46
  ) |>
    janitor::clean_names()

  plate_layout <- raw_well_data |>
    dplyr::select(
      location = well_position,
      sample_id = sample_name,
      assay_name = target_name
    ) |>
    dplyr::mutate(
      assay_id = case_when(
        assay_name == "Early" ~ 1,
        assay_name == "Late" ~ 2,
        assay_name == "Spring" ~ 3,
        assay_name == "Winter" ~ 4
      ),
      plate_run_id = plate_run_id,
      sample_type_id = ifelse(sample_type == "mucus", 1, 2)
    ) |>
    dplyr::select(-assay_name) |>
    dplyr::filter(!is.na(sample_id))

  return(
    structure(
      list(data = plate_layout),
      class = "plate_layout",
      plate_type = NA_character_,
      filepath = filepath
    )
  )
}

#' @export
print.sherquant_output <- function(x, ...) {
  cli::cat_rule(sprintf("A SherQuant Output Object"))
  cli::cat_bullet(
    sprintf("Filepath: '%s'", attr(x, "filepath", exact = TRUE)),
    bullet_col = "green"
  )
  cli::cat_bullet(
    sprintf("Layout Type: '%s'", attr(x, "plate_type", exact = TRUE)),
    bullet_col = "green"
  )
  cli::cat_bullet(
    sprintf("Sample Type: '%s'", attr(x, "sample_type", exact = TRUE)),
    bullet_col = "green"
  )
  cli::cat_bullet(
    sprintf("Plate Run ID: '%s'", attr(x, "plate_run_id", exact = TRUE)),
    bullet_col = "green"
  )
  cli::cat_bullet(
    sprintf("Plate Size: '%s'", attr(x, "plate_size", exact = TRUE)),
    bullet_col = "green"
  )
  cli::cat_bullet(sprintf("Data:"), bullet_col = "green")
  cli::cat_print(x$data)
}
