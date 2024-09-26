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
#' @descriotion creates a plate layout dataframe by a given layout size
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

#' @export
make_plate_maps_by_event <- function(con, events, season = lubridate::year(lubridate::today())) {
  season_filter <- stringr::str_sub(season, -2)
  samples <- con |> tbl("sample") |>
    filter(event_number %in% events, season == season_filter) |>
    collect()

  events_name <- paste(events, collapse="-")

  samples_parted <- partition_df_every_n(samples, n = 92)
  layouts_list <- map(samples_parted, \(x) suppressWarnings(make_plate_layout(x$id)))
  n_layout_groups <- ceiling(length(layouts_list) / 4) # 4 subplates per "packet"
  group_ids <- rep(1:n_layout_groups, each = 4)

  message(glue::glue("A total of {nrow(samples)} samples were arranged into {length(layouts_list)} plates"))

  for (i in seq_along(layouts_list)) {
    subplate_num <- if ((x <- i %% 4) == 0) 4 else x
    if (i > 4) {
      subplate_num <- paste0(subplate_num, "-1")
    }
    filename <- glue::glue("sherlock_{events_name}_p{subplate_num}.xlsx")
    write_layout_to_file(layouts_list[[i]], filename)
    message(paste(filename, "file created"))
  }


  invisible(layouts_list)
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
