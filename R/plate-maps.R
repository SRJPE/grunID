#' @title Parttion DF every N
#' @description partition a dataframe by every nth row
#' @param df a dataframe
#' @param n an integer
#' @export
partition_df_every_n <- function(df, n) {
  df |>
    mutate(part = ceiling(row_number() / n)) |>
    group_split(part)
}

#' @title Create Plate Layout
#' @description creates a plate layout dataframe by a given layout size
#' @param samples vector of samples to create map for
#' @param layout_size the size of the layout to create
#' @param ebks character vector of EBK values to insert
#' @export
make_plate_layout <- function(samples, layout_size = 96, ebks = NULL, type = c("dual", "single")) {
  type <- match.arg(type)
  pad_amount <- layout_size - length(samples)
  raw <- matrix(c(samples, rep(NA, pad_amount)), nrow = 8, ncol = 12, byrow = FALSE)
  dat <- as.data.frame(raw)
  colnames(dat) <- 1:12
  rownames(dat) <- LETTERS[1:8]

  if (!is.null(ebks)) {
    if (length(ebks) < 1 || length(ebks) > 4) {
      stop("can only allocate between 0 and 4 extraction blanks", call. = FALSE)
    }

    if (type == "dual") {
      for (i in seq_along(ebks)) {
        dat[i, 12] <- ebks[i]
      }
    } else {
      for (i in seq_along(ebks)) {
        dat[4 + i, 12] <- ebks[i]
      }
    }
  }
  return(dat)
}


make_plate_map <- function(df) {
  purrr::map(df, \(x) make_plate_layout(x$id))
}

distribute_ebks_in_plate <- function() {
  ebk_idx <- list()
  ebks_to_insert <- c("EBK-1-1", "EBK-1-2", "EBK-1-3", "EBK-1-4")
  if (length(samples_parted) == 1) {
    ebk_idx[[1]] <- ebks_to_insert
  } else if (length(samples_parted) == 2) {
    ebk_idx[[1]] <- ebks_to_insert[1:2]
    ebk_idx[[2]] <- ebks_to_insert[3:4]
  } else if (length(samples_parted) == 3) {
    ebk_idx[[1]] <- ebks_to_insert[1]
    ebk_idx[[2]] <- ebks_to_insert[2]
    ebk_idx[[3]] <- ebks_to_insert[3:4]
  } else if (length(samples_parted) == 4) {
    ebk_idx[[1]] <- ebks_to_insert[1]
    ebk_idx[[2]] <- ebks_to_insert[2]
    ebk_idx[[3]] <- ebks_to_insert[3]
    ebk_idx[[4]] <- ebks_to_insert[4]
  }

  return(ebk_idx)
}


#' @title Make Single Assay Layout
#' @export
make_single_assay_layout <- function(data) {
  return(list())
}

#' @title Make Dual Assay Layout
#' @export
make_dual_assay_layout <- function(data, layout_size = 96, output_dir, season_filter, events_name) {

  samples_parted <- partition_df_every_n(data, 88)

  # TODO: HACK!!!! lets not hard-code this, but for now this is fine
  ebk_idx <- list()
  ebks_to_insert <- c("EBK-1-1", "EBK-1-2", "EBK-1-3", "EBK-1-4")
  if (length(samples_parted) == 1) {
    ebk_idx[[1]] <- ebks_to_insert
  } else if (length(samples_parted) == 2) {
    ebk_idx[[1]] <- ebks_to_insert[1:2]
    ebk_idx[[2]] <- ebks_to_insert[3:4]
  } else if (length(samples_parted) == 3) {
    ebk_idx[[1]] <- ebks_to_insert[1]
    ebk_idx[[2]] <- ebks_to_insert[2]
    ebk_idx[[3]] <- ebks_to_insert[3:4]
  } else if (length(samples_parted) == 4) {
    ebk_idx[[1]] <- ebks_to_insert[1]
    ebk_idx[[2]] <- ebks_to_insert[2]
    ebk_idx[[3]] <- ebks_to_insert[3]
    ebk_idx[[4]] <- ebks_to_insert[4]
  } else {
    new_samples_parted <- samples_parted[-c(1:4)]
    if (length(new_samples_parted) == 1) {
      ebk_idx[[5]] <- ebks_to_insert
    } else if (length(new_samples_parted) == 2) {
      ebk_idx[[5]] <- ebks_to_insert[1:2]
      ebk_idx[[6]] <- ebks_to_insert[3:4]
    } else if (length(new_samples_parted) == 3) {
      ebk_idx[[5]] <- ebks_to_insert[1]
      ebk_idx[[6]] <- ebks_to_insert[2]
      ebk_idx[[7]] <- ebks_to_insert[3:4]
    } else if (length(new_samples_parted) == 4) {
      ebk_idx[[5]] <- ebks_to_insert[1]
      ebk_idx[[6]] <- ebks_to_insert[2]
      ebk_idx[[7]] <- ebks_to_insert[3]
      ebk_idx[[8]] <- ebks_to_insert[4]
    }
  }



  layouts_list <- imap(samples_parted, \(x, i) suppressWarnings(make_plate_layout(x$id, ebks = ebk_idx[[i]], type="dual")))
  n_layout_groups <- ceiling(length(layouts_list) / 4) # 4 subplates per "packet"
  group_ids <- rep(1:n_layout_groups, each = 4)

  message(glue::glue("A total of {nrow(data)} samples were arranged into {length(layouts_list)} plates"))


  filenames <- glue::glue("{output_dir}/JPE{season_filter}_E{events_name}_P{seq_along(layouts_list)}_ARC.xlsx")
  purrr::walk(seq_along(layouts_list), function(i) {
    write_layout_to_file(layouts_list[[i]], filenames[i])
    message(paste(filenames[i], "file created"))

  })

  names(layouts_list) <- tools::file_path_sans_ext(filenames)

  out <- layouts_list |>
    enframe(name = "archive_plate_id") |>
    unnest(cols = value) |>
    mutate(letter_val = rep(LETTERS[1:8], length(filenames))) |>
    pivot_longer(cols = `1`:`12`,
                 names_to = NULL,
                 values_to = "sample_id") |>
    transmute(
      archive_plate_id,
      sample_id,
      num_val = rep(1:12, 8 * length(filenames)),
      well_id = paste0(letter_val, num_val),
      plate_type = "dual") |>
    filter(!is.na(sample_id)) |> # remove intentional blanks and EBK
    select(-num_val)

  # JPE25_E1-3-4_EL_P1-2_SH
  # Where P1-2 indicates which EL DNA plates are represented on the SHERLOCK run?

  sherlock_plates <- make_dual_ots28_plates_from_arc(arc_df = out)
  filename_arc_plate_reference <- paste(seq_along(layouts_list), collapse = "-")
  filenames_sherlock <- glue::glue("{output_dir}/JPE{season_filter}_E{events_name}_EL_P{filename_arc_plate_reference}_SH.xlsx")
  purrr::walk(seq_along(sherlock_plates), function(i) {
    write_layout_to_file(sherlock_plates[[i]], filenames_sherlock[i])
    message(paste(filenames_sherlock[i], "file created"))
  })

  return(
    list(
      type = "dual",
      data = out,
      sherlock_plate_names = paste0(filenames_sherlock),
      arc_plate_names = paste0(filenames)
    )
  )

}


#' @title Create Plate Maps for Archive Plates
#' @param con a database connection
#' @param events a vector of events to generate plate maps for
#' @param season a season to produce plates for, uses current year be default
#' @export
make_archive_plate_maps_by_event <- function(con, events, season = get_current_season()$year, output_dir=NULL) {

  events <- sort(events)
  output_dir <- if (is.null(output_dir)) "." else output_dir
  season_filter <- stringr::str_sub(season, -2)
  # TODO make the mutate happen at the function level
  samples <- get_archive_plates_candidates(con, events = events, season = season) |>
    mutate(
      sample_num = str_extract(id, "_[0-9]$"),
      sample_bin = str_extract(id, "_[A-Z]_")) |>
    arrange(sample_location, event_number, sample_bin, sample_num) |>
    select(-sample_bin, -sample_num)

  if (nrow(samples) == 0) {
    return(list(
      success = FALSE,
      message = "No samples 'returned' from field were found",
      files = character(0),
      archive_ids = tibble::tibble(archive_plate_id = character(0), sample_id = character(0))
    ))
  }

  # the logic is, if the number of samples can fill a single assay layout (368) do
  # that as many times as possible. The remaining samples that cannot fill up the single
  # assay layout will be used in dual assays
  sample_cap_for_single_assay <- 368
  sample_cap_for_dual_assay <- 176

  single_assay_fits <- floor(nrow(samples) / sample_cap_for_single_assay)
  dual_assay_fits <- nrow(samples) / sample_cap_for_dual_assay

  events_name <- paste(events, collapse="-")

  samples_parted <- partition_df_every_n(samples, n = 92)

  single_assay_idx <- seq_len(sample_cap_for_single_assay * single_assay_fits)
  single_assay_idx <- ifelse(length(single_assay_idx) == 1, single_assay_idx, 0)
  dual_assay_idx <- (single_assay_idx + 1):nrow(samples)

  single_assay_samples <- samples[single_assay_idx, ]
  dual_assay_samples <- samples[dual_assay_idx, ]

  single_assay_layouts <- make_single_assay_layout(single_assay_samples)

  dual_assay_layouts <- make_dual_assay_layout(dual_assay_samples,
                                               output_dir = output_dir,
                                               season_filter = season_filter,
                                               events_name = events_name)


  return(list(
    dual = dual_assay_layouts,
    single = single_assay_layouts,
    success = TRUE,
    messages = "created files"
  ))
}


#' @export
insert_archive_plate_ids <- function(con, archive) {
  sql_statement <- glue::glue_sql("
  INSERT INTO sample_archive_plates (sample_id, arc_plate_id, arc_well_id) values(
    UNNEST(ARRAY[{archive$sample_id*}]),
    UNNEST(ARRAY[{archive$archive_plate_id*}]),
    UNNEST(ARRAY[{archive$well_id*}])
  ) ON CONFLICT (sample_id, arc_plate_id)
  DO UPDATE SET
    arc_plate_id = EXCLUDED.arc_plate_id,
    arc_well_id = EXCLUDED.arc_well_id,
    updated_at = now(),
    updated_by = current_user;", .con = con)

  DBI::dbExecute(con, sql_statement)
}

#' @export
insert_hamilton_plate_ids <- function(con, archive) {
  sql_statement <- glue::glue_sql("
  INSERT INTO sample_hamilton_plates (sample_id, ham_plate_id) values(
    UNNEST(ARRAY[{archive$sample_id*}]),
    UNNEST(ARRAY[{archive$hamilton_plate_id*}])
  ) ON CONFLICT (sample_id, ham_plate_id)
  DO UPDATE SET ham_plate_id = EXCLUDED.ham_plate_id;", .con = con)

  DBI::dbExecute(con, sql_statement)
}

#' @param df dataframe to write to file
#' @param file_name the file name to use
write_layout_to_file <- function(df, file_name) {
  # Create a new workbook
  sheet_name <- "map"
  wb <- openxlsx::createWorkbook()

  # Add a worksheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Initialize the starting row
  start_row <- 1

  # Write each dataframe to the worksheet
  openxlsx::writeData(wb, sheet_name, df, rowNames = TRUE)

  # Save the workbook
  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
}

#' @title Get Current Season
#' @export
get_current_season <- function() {
  current_date <- lubridate::today()
  lubridate::day(current_date) <- 1
  start_date <- current_date
  end_date <- current_date
  if (lubridate::month(current_date) >= 10) {
    lubridate::month(start_date) <- 10
    lubridate::month(end_date) <- 8
    lubridate::year(end_date) <- lubridate::year(end_date) + 1
  } else if (lubridate::month(current_date) %in% 1:8) {
    lubridate::month(start_date) <- 10
    lubridate::year(start_date) <- lubridate::year(current_date) - 1
    lubridate::month(end_date) <- 8
  }

  year_val <- lubridate::year(end_date)

  return(
    list(
      "start_date" = start_date,
      "end_date" = end_date,
      "year" = year_val,
      "season_code" = as.numeric(stringr::str_sub(as.character(year_val), 3, 4)))
  )
}

#' @title Archinve Plates Candidates
#' @description
#' Obtain dataframe of samples ready to be plated onto Archinve Plates
#'
#' @param con a database connection
#' @param season season obtained from get_current_season()
#'
#' @export
get_archive_plates_candidates <- function(con, events, season = get_current_season()$year) {
  candidate_samples <- get_sample_status(con, season = season) |>
    filter(status_code_name == "returned from field") |> pull(sample_id)

  tbl(con, "sample") |>
    filter(id %in% candidate_samples) |>
    collect()

}

#' @title Hamilton Plate Candidates
#' @export
get_sw_plates_candidates <- function(con, destination, events, season = get_current_season()$year) {
  candidate_samples <- get_sample_status(con, season = season) |>
    dplyr::filter(status_code_name %in% c("need ots16", "need gtseq"),
                  event_number %in% events)

  if (destination == "gtseq") {
    # first filter by location
    candidate_samples |>
      filter(status_code_name == "need gtseq" | sample_location %in% c("KNL", "TIS", "DEL")) |>
      pull(sample_id)
  } else if (destination == "sherlock") {
    candidate_samples |>
      filter(status_code_name == "need ots16", !(sample_location %in% c("KNL", "TIS", "DEL"))) |>
      pull(sample_id)
  }
}

#' @title Create Plate Maps for Hamilton Machine
#' @description
#' Plate maps will be generated for input into the Hamilton cherry picking machine.
#' Only samples that are in the "need ots 16" will be used for creating these plate maps
#'
#' @export
make_sw_plate_maps <- function(con, events,
                               destination = c("sherlock", "gtseq"),
                               season = get_current_season(), output_dir = tempdir()) {

  if (output_dir == ".") {
    output_dir <- getwd()
  }

  destination <- match.arg(destination)
  # need to get the candiate samples
  logger::log_info("getting data for events {events}")
  candidate_samples <- get_sw_plates_candidates(con, destination = destination,
                                                events = as.numeric(events), season = season$year)

  archive_plate_for_candidates <- tbl(con, "sample_archive_plates") |>
    filter(sample_id %in% candidate_samples) |> collect()

  plate_to_arc_plate_lookup <-
    archive_plate_for_candidates |>
    group_by(arc_plate_id) |>
    summarise(min_event = min(event_number)) |>
    ungroup() |>
    mutate(plate_num = str_extract(arc_plate_id, "_P[0-9]_")) |>
    arrange(min_event, plate_num) |>
    transmute(
      PlateID=1:n(),
      arc_plate_id
    )

  events_candiate_code <- glue::glue("E-{min(archive_plate_for_candidates$event_number)}-{max(archive_plate_for_candidates$event_number)}")

  hamilton_letters <- paste0(LETTERS[1:8], rep(1:12, each = 8))

  # get only the latest result per sample from the results table
  hamilton_cherry_pick <- archive_plate_for_candidates |>
    select(sample_id, arc_plate_id, arc_well_id) |>
    mutate(grp = ceiling(row_number() / 88)) |>
    group_by(grp) |>
    mutate(
      destination_well_id = hamilton_letters[1:n()]
    ) |>
    transmute(
      SampleID = sample_id,
      WellIDSource = arc_well_id,
      WellIDDestination = destination_well_id,
      arc_plate_id,
      grp
    ) |> left_join(plate_to_arc_plate_lookup, by=c("arc_plate_id"="arc_plate_id")) |>
    transmute(
      SampleID,
      PlateID=paste0("Plate", PlateID),
      WellIDSource,
      WellIDDestination,
      grp
    ) |>
    group_split(grp) |>
    map(function(d) {
      d |> select(-grp)
    })


  message(glue::glue("A total of {nrow(hamilton_cherry_pick)} will be processed in this file"))

  destination_label <- if (destination == "sherlock") "SW" else "GT"

  input_files_created <- c()
  iwalk(hamilton_cherry_pick, function(d, i) {
    cp_input_filename <- glue::glue("{output_dir}/JPE{season$season_code}_{events_candiate_code}_{destination_label}_CP{i}_inputfile.txt")
    write_csv(d, cp_input_filename)
    input_files_created <<- append(input_files_created, cp_input_filename)
  })

  platekey_filename <- glue::glue("{output_dir}/JPE{season$season_code}_{events_candiate_code}_{destination_label}_CP_platekey.txt")
  write_csv(plate_to_arc_plate_lookup, platekey_filename)


  message(glue::glue("Saving map file to: {platekey_filename}"))
  message(glue::glue("Saving lookup file to: {input_files_created}"))

  return(list(
    success = TRUE,
    message = "process complete"
  ))
}

pull_as_numeric <- function(x) {
  as.numeric(pull(x))
}

#' @export
make_dual_ots28_plates_from_arc <- function(arc_df) {
  d <- arc_df |> mutate(sub_plate = stringr::str_extract(archive_plate_id, "(?<=P)\\d+")) |>
    filter(!str_detect(sample_id, "EBK")) |>
    mutate(
      sample_location = str_sub(sample_id, 1, 3),
      event_number = str_extract(sample_id, "_[0-9]+_"),
      sample_num = readr::parse_number(str_extract(sample_id, "_[0-9]+$")),
      sample_bin = str_extract(sample_id, "_[A-Z]_")) |>
    arrange(sample_location, event_number, sample_bin, sample_num) |>
    select(-sample_bin, -sample_num, -event_number, -sample_location)

  max_cells_for_dual <- 88 * 2
  subplates_in_batch <- d |> distinct(sub_plate) |> pull_as_numeric()
  total_subplates_in_batch <- length(subplates_in_batch)

  if (total_subplates_in_batch == 1) {
    p1 <- d |> filter(sub_plate == 1)

    p1_ids <- p1$sample_id
    p1_dual_matrix_left <- matrix(NA, nrow = 16, ncol = 11)

    p1_dual_matrix_left[seq(1, 16, 2), ] <- matrix(p1_ids[1:88], ncol = 11, byrow = FALSE)
    p1_dual_matrix_left[seq(2, 16, 2), ] <- matrix(p1_ids[89:176], ncol = 11, byrow = FALSE)
    p1_dual_matrix_left <- cbind(p1_dual_matrix_left, c("EBK-1-1",
                                                        NA_character_,
                                                        "EBK-1-2",
                                                        NA_character_,
                                                        "EBK-1-3",
                                                        NA_character_,
                                                        "EBK-1-4",
                                                        "POS-DNA-1", "POS-DNA-2", "POS-DNA-3",
                                                        "NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3",
                                                        "NTC-1", "NTC-2", "NTC-3"))

    p1_full <- cbind(p1_dual_matrix_left, p1_dual_matrix_left)
    rownames(p1_full) <- LETTERS[1:16]
    colnames(p1_full) <- 1:24
    return(list(p1_full))

  } else if (total_subplates_in_batch == 2) {
    p12 <- d |> filter(sub_plate %in% 1:2)

    p12_ids <- p12$sample_id
    p12_dual_matrix_left <- matrix(NA, nrow = 16, ncol = 11)

    p12_dual_matrix_left[seq(1, 16, 2), ] <- matrix(p12_ids[1:88], ncol = 11, byrow = FALSE)
    p12_dual_matrix_left[seq(2, 16, 2), ] <- matrix(p12_ids[89:176], ncol = 11, byrow = FALSE)
    p12_dual_matrix_left <- cbind(p12_dual_matrix_left, c("EBK-1-1",
                                                          NA_character_,
                                                          "EBK-1-2",
                                                          NA_character_,
                                                          "EBK-2-1",
                                                          NA_character_,
                                                          "EBK-2-2",
                                                          "POS-DNA-1", "POS-DNA-2", "POS-DNA-3",
                                                          "NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3",
                                                          "NTC-1", "NTC-2", "NTC-3"))

    p12_full <- cbind(p12_dual_matrix_left, p12_dual_matrix_left)
    rownames(p12_full) <- LETTERS[1:16]
    colnames(p12_full) <- 1:24

    return(list(p12_full))
  } else if (total_subplates_in_batch == 3) {
    p12 <- d |> filter(sub_plate %in% 1:2)
    p3 <- d |> filter(sub_plate == 3)

    p12_ids <- p12$sample_id
    p12_dual_matrix_left <- matrix(NA, nrow = 16, ncol = 11)
    p12_dual_matrix_right <- matrix(NA, nrow = 16, ncol = 11)

    p12_dual_matrix_left[seq(1, 16, 2), ] <- matrix(p12_ids[1:88], ncol = 11, byrow = FALSE)
    p12_dual_matrix_left[seq(2, 16, 2), ] <- matrix(p12_ids[89:176], ncol = 11, byrow = FALSE)
    p12_dual_matrix_left <- cbind(p12_dual_matrix_left, c("EBK-1-1",
                                                          NA_character_,
                                                          "EBK-1-2",
                                                          NA_character_,
                                                          "EBK-2-1",
                                                          NA_character_,
                                                          "EBK-2-2",
                                                          "POS-DNA-1", "POS-DNA-2", "POS-DNA-3",
                                                          "NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3",
                                                          "NTC-1", "NTC-2", "NTC-3"))

    p12_full <- cbind(p12_dual_matrix_left, p12_dual_matrix_left)
    rownames(p12_full) <- LETTERS[1:16]
    colnames(p12_full) <- 1:24

    p3_ids <- p3$sample_id
    p3_dual_matrix_left <- matrix(NA, nrow = 16, ncol = 11)
    p3_dual_matrix_left[seq(1, 16, 2), ] <- matrix(p3_ids[1:88], ncol = 11, byrow = TRUE)
    p3_dual_matrix_left <- cbind(p3_dual_matrix_left, c("EBK-1-1",
                                                        NA_character_,
                                                        "EBK-1-2",
                                                        NA_character_,
                                                        "EBK-1-3",
                                                        NA_character_,
                                                        "EBK-1-4",
                                                        "POS-DNA-1", "POS-DNA-2", "POS-DNA-3",
                                                        "NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3",
                                                        "NTC-1", "NTC-2", "NTC-3"))

    p3_full <- cbind(p3_dual_matrix_left, p3_dual_matrix_left)
    rownames(p3_full) <- LETTERS[1:16]
    colnames(p3_full) <- 1:24
    return(list(p12_full, p3_full))
  } else if (total_subplates_in_batch == 4) {
    return(list())
  }
}


make_dual_ots_16_from_cherry_pick <- function(d) {
  d |> separate(WellIDDestination, into = c("letter_row", "num_col"), sep = 1) |>
    select(SampleID, letter_row, num_col) |>
    pivot_wider(num_col) |>
    arrange(letter_row)
}

#' @title Register Archive Plate
#' @export
register_arc_plate <- function(con, datapath, filename) {
  d <- readxl::read_excel(datapath) |>
    pivot_longer(-...1, names_to = "row", values_to = "sample_id") |>
    transmute(well_id = paste0(...1, row),
              sample_id,
              archive_plate_id = tools::file_path_sans_ext(basename(filename))) |>
    filter(!is.na(sample_id))

  insert_archive_plate_ids(con, d)
}


#' @title Upload Blob to Azure
#' @export
upload_to_azure_storage <- function(container, datapath, filename) {
  if (!is.null(container)) {
    AzureStor::storage_upload(container, datapath, filename)
  }
}



