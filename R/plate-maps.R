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
  d <- matrix(c(samples, rep(NA, pad_amount)), nrow = 8, ncol = 12, byrow = FALSE) |>
    as_tibble()
  colnames(d) <- 1:12
  rownames(d) <- LETTERS[1:8]
  return(d)
}


make_plate_map <- function(df) {
  purrr::map(df, \(x) make_plate_layout(x$id))
}

make_plate_maps_by_event <- function(con, events) {
  samples <- con |> tbl("sample") |>
    filter(event_number %in% events) |>
    collect()

  samples_parted <- partition_df_every_n(samples, n = 92)
  layouts_list <- map(samples_parted, \(x) make_plate_layout(x$id))
}

write_layouts_to_file <- function(plate_maps, file_name) {
  # Create a new workbook
  sheet_name <- "maps"
  wb <- openxlsx::createWorkbook()

  # Add a worksheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Initialize the starting row
  start_row <- 1

  # Write each dataframe to the worksheet
  for (df in plate_maps) {
    openxlsx::writeData(wb, sheet_name, df, startRow = start_row, startCol = 1)
    start_row <- start_row + nrow(df) + 10
  }

  # Save the workbook
  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
}
