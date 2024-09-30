#' @title Parttion DF every N
#' @description partition a dataframe by every nth row
#' @param df a dataframe
#' @param n an integer
#' @keywords internal
partition_df_every_n <- function(df, n) {
  df |>
    mutate(part = ceiling(row_number() / n)) |>
    group_split(part)

}

#' @title Create Plate Layout
#' @description creates a plate layout dataframe by a given layout size
#' @param samples vector of samples to create map for
#' @param layout_size the size of the layout to create
#' @keywords internal
make_plate_layout <- function(samples, layout_size = 96) {
  pad_amount <- layout_size - length(samples)
  raw <- matrix(c(samples, rep(NA, pad_amount)), nrow = 8, ncol = 12, byrow = FALSE)
  dat <- tibble::as_tibble(raw)
  colnames(dat) <- 1:12
  rownames(dat) <- LETTERS[1:8]
  return(dat)
}


make_plate_map <- function(df) {
  purrr::map(df, \(x) make_plate_layout(x$id))
}

#' @title Create Plate Maps for Archive Plates
#' @param con a database connection
#' @param events a vector of events to generate plate maps for
#' @param season a season to produce plates for, uses current year be default
#' @export
make_archive_plate_maps_by_event <- function(con, events, season = lubridate::year(lubridate::today()), output_dir=NULL) {

  events <- sort(events)
  output_dir <- if (is.null(output_dir)) "." else output_dir
  season_filter <- stringr::str_sub(season, -2)
  samples <- con |> tbl("sample") |>
    filter(event_number %in% events, season == season_filter) |>
    collect()

  if (nrow(samples) == 0) {
    stop("no samples found that match events and season combination", call. = FALSE)
  }

  events_name <- paste(events, collapse="-")

  samples_parted <- partition_df_every_n(samples, n = 92)
  layouts_list <- map(samples_parted, \(x) suppressWarnings(make_plate_layout(x$id)))
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
    filter(!is.na(sample_id)) # remove intentional blanks

  return(list(
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

write_layout_to_file <- function(df, file_name) {
  # Create a new workbook
  sheet_name <- "map"
  wb <- openxlsx::createWorkbook()

  # Add a worksheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Initialize the starting row
  start_row <- 1

  # Write each dataframe to the worksheet
  openxlsx::writeData(wb, sheet_name, df)

  # Save the workbook
  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
}
