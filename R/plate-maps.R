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
make_plate_layout <- function(samples, layout_size = 96, ebks = NULL) {
  pad_amount <- layout_size - length(samples)
  raw <- matrix(c(samples, rep(NA, pad_amount)), nrow = 8, ncol = 12, byrow = FALSE)
  dat <- as.data.frame(raw)
  colnames(dat) <- 1:12
  rownames(dat) <- LETTERS[1:8]

  if (!is.null(ebks)) {
    if (length(ebks) < 1 || length(ebks) > 4) {
      stop("can only allocate between 0 and 4 extraction blanks", call. = FALSE)
    }

    for (i in seq_along(ebks)) {
      dat[4 + i, 12] <- ebks[i]
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

#' @title Create Plate Maps for Archive Plates
#' @param con a database connection
#' @param events a vector of events to generate plate maps for
#' @param season a season to produce plates for, uses current year be default
#' @export
make_archive_plate_maps_by_event <- function(con, events, season = get_current_season()$year, output_dir=NULL) {

  events <- sort(events)
  output_dir <- if (is.null(output_dir)) "." else output_dir
  season_filter <- stringr::str_sub(season, -2)
  samples <- get_archive_plates_candidates(con, events = events, season = season)

  if (nrow(samples) == 0) {
    return(list(
      success = FALSE,
      message = "No samples 'returned' from field were found",
      files = character(0),
      archive_ids = tibble::tibble(archive_plate_id = character(0), sample_id = character(0))
    ))
  }

  events_name <- paste(events, collapse="-")

  samples_parted <- partition_df_every_n(samples, n = 92)

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

  layouts_list <- imap(samples_parted, \(x, i) suppressWarnings(make_plate_layout(x$id, ebks = ebk_idx[[i]])))
  n_layout_groups <- ceiling(length(layouts_list) / 4) # 4 subplates per "packet"
  group_ids <- rep(1:n_layout_groups, each = 4)




  message(glue::glue("A total of {nrow(samples)} samples were arranged into {length(layouts_list)} plates"))

  filenames <- glue::glue("JPE{season_filter}_E{events_name}_P{seq_along(layouts_list)}_ARC.xlsx")
  purrr::walk(seq_along(layouts_list), function(i) {
    write_layout_to_file(layouts_list[[i]], paste0(output_dir, "/", filenames[i]))
    message(paste(filenames[i], "file created"))

  })

  names(layouts_list) <- tools::file_path_sans_ext(filenames)

  out <- layouts_list|>
    enframe(name = "archive_plate_id") |>
    unnest(cols = value) |>
    pivot_longer(cols = -archive_plate_id,
                 names_to = NULL,
                 values_to = "sample_id") |>
    filter(!is.na(sample_id),
           !stringr::str_detect(sample_id, "EBK")) # remove intentional blanks and EBK

  return(list(
    success = TRUE,
    message = glue::glue("Created {length(layouts_list)} plate files for {nrow(samples)} samples"),
    files = paste0(output_dir, "/", filenames),
    archive_ids = out
  ))
}


#' @export
insert_archive_plate_ids <- function(con, archive) {
  sql_statement <- glue::glue_sql("
  INSERT INTO sample_archive_plates (sample_id, arc_plate_id) values(
    UNNEST(ARRAY[{archive$sample_id*}]),
    UNNEST(ARRAY[{archive$archive_plate_id*}])
  ) ON CONFLICT (sample_id, arc_plate_id)
  DO UPDATE SET arc_plate_id = EXCLUDED.arc_plate_id;", .con = con)

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
get_hamilton_plates_candidates <- function(con, events, season = get_current_season()$year) {
  candidate_samples <- get_sample_status(con, season = season) |>
    dplyr::filter(status_code_name == "need ots16") |>
    dplyr::pull(sample_id)

  return(candidate_samples)
}

#' @title Create Plate Maps for Hamilton Machine
#' @description
#' Plate maps will be generated for input into the Hamilton cherry picking machine.
#' Only samples that are in the "need ots 16" will be used for creating these plate maps
#'
#' @export
make_hamilton_plate_maps <- function(con, events,
                                     destination = c("sherlock", "gtseq"),
                                     season = get_current_season(), output_dir = tempdir()) {

  destination <- match.arg(destination)
  # need to get the candiate samples
  candidate_samples <- get_hamilton_plates_candidates(con, events = events, season = season)

  if (length(candidate_samples) == 0) {
    stop("no samples were found that need ots 16", call. = FALSE)
  }

  # get only the latest result per sample from the results table
  hamilton_cherry_pick <- tbl(con, "assay_result") |>
    filter(sample_id %in% candidate_samples) |>
    group_by(sample_id, assay_id) |>
    slice_max(order_by = updated_at, n = 1) |>
    ungroup() |>
    filter(assay_id == 2) |>
    transmute(id = sample_id, well_id_source = as.character(well_location)) |>
    collect()

  samples_parted <- partition_df_every_n(hamilton_cherry_pick, 92)


  events_name <- paste(events, collapse="-")


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

  layouts_list <- purrr::imap(samples_parted, \(x, i) suppressWarnings(make_plate_layout(x$id, ebks = ebk_idx[[i]])))
  n_layout_groups <- ceiling(length(layouts_list) / 4) # 4 subplates per "packet"
  group_ids <- rep(1:n_layout_groups, each = 4)


  # the names for the sw plates should be:
  # JPE25_E1-2_SW_P1
  # JPE25_E1-2_SW_P2

  # the names for the gt seq should be
  # PE25_E1-2_GT_P1
  # JPE25_E1-2_GT_P2

  message(glue::glue("A total of {nrow(hamilton_cherry_pick)} samples were arranged into {length(layouts_list)} plates"))

  destination_file_code <- if_else(destination == "sherlock", "SW", "GT")
  filenames <- glue::glue("JPE{season}_E{events_name}_{destination_file_code}_P{seq_along(layouts_list)}.xlsx")
  purrr::walk(seq_along(layouts_list), function(i) {
    write_layout_to_file(layouts_list[[i]], paste0(output_dir, "/", filenames[i]))
    message(paste(filenames[i], "file created"))

  })

  names(layouts_list) <- tools::file_path_sans_ext(filenames)

  out <- layouts_list |>
    enframe(name = "hamilton_plate_id") |>
    unnest(cols = value) |>
    mutate(letters = LETTERS[1:8]) |>
    pivot_longer(cols = `1`:`12`,
                 names_to = NULL,
                 values_to = "sample_id") |>
    mutate(num = rep(1:12, times = 8), destination_well = paste0(letters, num)) |>
    filter(!is.na(sample_id),
           !stringr::str_detect(sample_id, "EBK"))  |> # remove intentional blanks and EBK
    left_join(hamilton_cherry_pick, by=c("sample_id" = "id")) |>
    select(hamilton_plate_id, sample_id, source_well = well_id_source, destination_well)

  return(list(
    success = TRUE,
    message = glue::glue("Created {length(layouts_list)} plate files for {nrow(hamilton_cherry_pick)} samples"),
    files = paste0(output_dir, "/", filenames),
    hamilton_ids = out
  ))
}

