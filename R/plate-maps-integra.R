#' @title Check Integra Source Wells
#' @description
#' Validate the archive plate wells found for a set of candidate samples before
#' they are assigned to Integra target plates.
#'
#' @param source_wells dataframe with columns `sample_id`, `arc_plate_id` and
#'   `arc_well_id`, as read from the `sample_archive_plates` table
#' @param candidate_samples character vector of candidate sample ids
#' @returns `source_wells`, invisibly. Errors if no candidate has an archive
#'   plate record or if any sample is on more than one archive plate, and warns
#'   about candidates without an archive plate record.
#' @family integra functions
#' @export
#' @md
check_integra_source_wells <- function(source_wells, candidate_samples) {
  if (nrow(source_wells) == 0) {
    cli::cli_abort(c(
      "No archive plates were found for the candidate samples.",
      "i" = "Candidate samples: {.val {candidate_samples}}"
    ))
  }

  duplicated_samples <- unique(source_wells$sample_id[duplicated(source_wells$sample_id)])
  if (length(duplicated_samples) > 0) {
    cli::cli_abort(c(
      "Samples found on more than one archive plate, resolve these before cherry picking.",
      "i" = "Samples: {.val {duplicated_samples}}"
    ))
  }

  missing_samples <- setdiff(candidate_samples, source_wells$sample_id)
  if (length(missing_samples) > 0) {
    cli::cli_warn(c(
      "Candidate samples without an archive plate were left out of the cherry pick.",
      "i" = "Samples: {.val {missing_samples}}"
    ))
  }

  invisible(source_wells)
}

#' @title Assign Integra Target Plates
#' @description
#' Assign archive plate (source) wells to Integra target plates and wells.
#' Source plates are ordered by season, first event and plate number parsed
#' from `arc_plate_id`, and wells within a source plate are picked row-wise
#' (A1, A2, ..., B1). A new target plate is started when the current one holds
#' `max_samples` samples or when the next sample would add more than
#' `max_source_plates` source plates. A source plate can be split across
#' target plates. Target wells are filled column-wise (A1, B1, ..., H1, A2).
#'
#' @param source_wells dataframe with columns `sample_id`, `arc_plate_id` and
#'   `arc_well_id`
#' @param max_samples maximum samples per target plate, 90 leaves C12-H12 open
#'   for the lab's controls
#' @param max_source_plates maximum source plates per target plate
#' @returns `source_wells` in pick order with added columns `target_plate`
#'   (integer) and `target_well`
#' @family integra functions
#' @export
#' @md
assign_integra_target_plates <- function(source_wells, max_samples = 90, max_source_plates = 3) {
  if (max_samples > 96) {
    cli::cli_abort("{.arg max_samples} cannot be more than 96, the size of a plate.")
  }

  sorted <- source_wells |>
    mutate(
      .season = as.numeric(stringr::str_extract(arc_plate_id, "(?<=^JPE)\\d+")),
      .first_event = as.numeric(stringr::str_extract(arc_plate_id, "(?<=_E)\\d+")),
      .plate_num = as.numeric(stringr::str_extract(arc_plate_id, "(?<=_P)\\d+")),
      .well_row = stringr::str_sub(arc_well_id, 1, 1),
      .well_col = as.numeric(stringr::str_sub(arc_well_id, 2))
    ) |>
    arrange(.season, .first_event, .plate_num, arc_plate_id, .well_row, .well_col) |>
    select(-starts_with("."))

  target_plate <- integer(nrow(sorted))
  current_plate <- 1L
  current_count <- 0
  current_sources <- character(0)
  for (i in seq_len(nrow(sorted))) {
    source_plate <- sorted$arc_plate_id[i]
    is_new_source <- !(source_plate %in% current_sources)
    if (current_count == max_samples ||
        (is_new_source && length(current_sources) == max_source_plates)) {
      current_plate <- current_plate + 1L
      current_count <- 0
      current_sources <- character(0)
    }
    if (!(source_plate %in% current_sources)) {
      current_sources <- c(current_sources, source_plate)
    }
    current_count <- current_count + 1
    target_plate[i] <- current_plate
  }

  column_wise_wells <- paste0(LETTERS[1:8], rep(1:12, each = 8))

  sorted |>
    mutate(target_plate = target_plate) |>
    group_by(target_plate) |>
    mutate(target_well = column_wise_wells[row_number()]) |>
    ungroup()
}

#' @title Format Integra Input
#' @description
#' Build the Integra cherry pick input table from target plate assignments.
#'
#' @param assignments dataframe returned by [assign_integra_target_plates()]
#' @param target_plate_ids character vector of target plate ids, one per target
#'   plate in `assignments`, in target plate order
#' @param transfer_volume volume to transfer per sample
#' @returns dataframe with columns `SampleID`, `SourcePlateID`, `SourceWell`,
#'   `TargetPlateID`, `TargetWell` and `TransferVolume`
#' @family integra functions
#' @export
#' @md
format_integra_input <- function(assignments, target_plate_ids, transfer_volume = 16) {
  if (!is.numeric(transfer_volume) || length(transfer_volume) != 1 || transfer_volume <= 0) {
    cli::cli_abort("{.arg transfer_volume} must be a single positive number.")
  }

  n_target_plates <- n_distinct(assignments$target_plate)
  if (length(target_plate_ids) != n_target_plates) {
    cli::cli_abort(
      "{.arg target_plate_ids} must have one id per target plate ({n_target_plates}), not {length(target_plate_ids)}."
    )
  }

  assignments |>
    transmute(
      SampleID = sample_id,
      SourcePlateID = arc_plate_id,
      SourceWell = arc_well_id,
      TargetPlateID = target_plate_ids[target_plate],
      TargetWell = target_well,
      TransferVolume = transfer_volume
    )
}

#' @title Write Integra Input
#' @description Write an Integra cherry pick input table as a semicolon delimited file.
#'
#' @param integra_input dataframe returned by [format_integra_input()]
#' @param file_name the file name to write to
#' @family integra functions
#' @export
#' @md
write_integra_input <- function(integra_input, file_name) {
  readr::write_delim(integra_input, file_name, delim = ";")
}

#' @title Summarise Integra Session
#' @description
#' Summarise a cherry picking session, one row per Integra input file.
#'
#' @param integra_input dataframe returned by [format_integra_input()]
#' @param file_names input file names, one per target plate, in target plate order
#' @returns dataframe with columns `Round`, `FileName`, `TargetPlateID`,
#'   `SourcePlateIDs` and `SampleCount`
#' @family integra functions
#' @export
#' @md
summarise_integra_session <- function(integra_input, file_names) {
  n_target_plates <- n_distinct(integra_input$TargetPlateID)
  if (length(file_names) != n_target_plates) {
    cli::cli_abort(
      "{.arg file_names} must have one file per target plate ({n_target_plates}), not {length(file_names)}."
    )
  }

  integra_input |>
    mutate(TargetPlateID = factor(TargetPlateID, levels = unique(TargetPlateID))) |>
    group_by(TargetPlateID) |>
    summarise(
      SourcePlateIDs = paste(unique(SourcePlateID), collapse = ", "),
      SampleCount = n()
    ) |>
    transmute(
      Round = row_number(),
      FileName = basename(file_names),
      TargetPlateID = as.character(TargetPlateID),
      SourcePlateIDs,
      SampleCount
    )
}

#' @title Create Plate Maps for Integra Cherry Picking
#' @description
#' Generate Integra cherry pick input files for the selected events. Each file
#' fills one target plate with at most 90 samples from at most 3 archive
#' (source) plates. Also writes a plate layout for each target plate, a dual
#' assay OTS-16 layout for each target plate when the destination is Sherlock,
#' and a summary of the session.
#'
#' @param con a database connection
#' @param events event numbers to generate files for
#' @param destination either "sherlock" or "gtseq"
#' @param season season obtained from [get_current_season()]
#' @param output_dir directory to write files to
#' @param transfer_volume volume to transfer per sample
#' @returns list with `success`, `message`, `summary` (the session summary) and
#'   `files` (paths written)
#' @family integra functions
#' @export
#' @md
make_integra_plate_maps <- function(
  con,
  events,
  destination = c("sherlock", "gtseq"),
  season = get_current_season(),
  output_dir = tempdir(),
  transfer_volume = 16
) {
  is_valid_connection(con)
  destination <- match.arg(destination)
  if (output_dir == ".") {
    output_dir <- getwd()
  }

  logger::log_info("getting data for events {paste(events, collapse = ', ')}")
  candidate_samples <- get_sw_plates_candidates(
    con,
    destination = destination,
    events = as.numeric(events),
    season = season$year
  )
  logger::log_info("found {length(candidate_samples)} candidate samples to process")

  source_wells <- tbl(con, "sample_archive_plates") |>
    filter(sample_id %in% candidate_samples) |>
    select(sample_id, arc_plate_id, arc_well_id) |>
    collect()

  check_integra_source_wells(source_wells, candidate_samples)
  assignments <- assign_integra_target_plates(source_wells)

  session <- write_integra_session(
    assignments,
    events = events,
    destination = destination,
    season = season,
    output_dir = output_dir,
    transfer_volume = transfer_volume
  )

  return(list(
    success = TRUE,
    message = "process complete",
    summary = session$summary,
    files = session$files
  ))
}

#' @title Write Integra Session
#' @description
#' Write the files for an Integra cherry picking session: one semicolon
#' delimited input file and one plate layout per target plate, a dual assay
#' OTS-16 layout per target plate when the destination is Sherlock, and a
#' session summary.
#'
#' @param assignments dataframe returned by [assign_integra_target_plates()]
#' @inheritParams make_integra_plate_maps
#' @returns list with `summary` (the session summary) and `files` (paths written)
#' @family integra functions
#' @export
#' @md
write_integra_session <- function(
  assignments,
  events,
  destination = c("sherlock", "gtseq"),
  season = get_current_season(),
  output_dir = tempdir(),
  transfer_volume = 16
) {
  destination <- match.arg(destination)
  destination_label <- if (destination == "sherlock") "SW" else "GT"
  events_code <- glue::glue("E{min(as.numeric(events))}-{max(as.numeric(events))}")
  session_prefix <- glue::glue("JPE{season$season_code}_{events_code}_{destination_label}")
  target_plate_ids <- glue::glue("{session_prefix}_P{sort(unique(assignments$target_plate))}")

  integra_input <- format_integra_input(assignments, target_plate_ids, transfer_volume)

  input_files <- glue::glue("{output_dir}/{target_plate_ids}_CP_inputfile.csv")
  layout_files <- glue::glue("{output_dir}/{target_plate_ids}_DNA.xlsx")
  sherlock_files <- glue::glue("{output_dir}/{target_plate_ids}_SH.xlsx")

  purrr::walk(seq_along(target_plate_ids), function(i) {
    plate_input <- integra_input |> filter(TargetPlateID == target_plate_ids[i])

    write_integra_input(plate_input, input_files[i])
    write_layout_to_file(
      suppressWarnings(make_plate_layout(plate_input$SampleID, ebks = NULL, type = "single")),
      layout_files[i]
    )
    if (destination == "sherlock") {
      plate_input |>
        transmute(SampleID, WellIDDestination = TargetWell) |>
        make_dual_ots_16_from_cherry_pick() |>
        write_layout_to_file(sherlock_files[i])
    }
  })

  summary <- summarise_integra_session(integra_input, input_files)
  summary_file <- glue::glue("{output_dir}/{session_prefix}_CP_summary.csv")
  readr::write_csv(summary, summary_file)

  files <- as.character(c(input_files, layout_files, if (destination == "sherlock") sherlock_files, summary_file))
  logger::log_info(
    "{nrow(integra_input)} samples arranged into {length(target_plate_ids)} Integra files in {output_dir}"
  )
  purrr::walk(files, \(f) logger::log_info("created {f}"))

  list(summary = summary, files = files)
}
