#' Create Field Sheets
#' @description `create_field_sheet()` appends a worksheet to an existing excel workbook
#' containing a formatted field sheet to be used by crew collecting genetic samples.
#' @param wb A Workbook object from \code{\link[openxlsx]{createWorkbook}}
#' @param field_sheet_sample_plan A dataframe containing the content of the field sheet,
#' use the \code{field_sheet_sample_plan} output from \code{\link{get_field_sheet_event_plan}}.
#' *Note \code{\link{get_field_sheet_event_plan}} also returns the other arguments required by this function.*
#'
#' The following columns must contain data:
#' * **Bin** The sampling bin identifier A-E
#' * **Bin FL Range (mm)** The min to max fork length range of the sampling bin
#' * **Sample #** A number denoting the order the sample was taken, 1 to the total
#' number of planned samples with in the sampling bin
#' * **Sample ID** \code{{sample_location_code}{YY}_{sample_event_number}_{sample_bin}_{sample_number}} (e.g., "BTC22_3_A_1")
#'
#' The remaining columns ("Date", "Time", "FL (mm)", "Field Run ID", "Fin Clip (Y/N)", "Comments")
#' will be left empty and filled out by hand in the field
#' @param sample_event_number The non-unique sampling event number, enumerated from 1 at
#' the start of each monitoring season for each sampling location
#' @param first_sample_date A date object YYYY-MM-DD representing the first day of
#' sampling of a 2 day sampling event
#' @param sample_location The sampling location name (e.g., "Battle Creek")
#' @param sample_location_code The sampling location short code (e.g., "BTC")
#' @returns A Workbook object from \code{\link[openxlsx]{createWorkbook}} with the new worksheet
#' @examples
#' wb <- openxlsx::createWorkbook()
#' # Each sample_event will be a tab in a workbook
#' plan <- get_field_sheet_event_plan(con, sample_event_id = 1)
#' wb <- create_field_sheet(wb = wb,
#'                          field_sheet_sample_plan = plan$sample_plan,
#'                          sample_event_number = plan$sample_event_number,
#'                          first_sample_date = plan$first_sample_date,
#'                          sample_location = plan$location_name,
#'                          sample_location_code = plan$location_code)
#'
#' plan <- get_field_sheet_event_plan(con, sample_event_id = 2)
#' wb <- create_field_sheet(wb = wb,
#'                          field_sheet_sample_plan = plan$field_sheet_sample_plan,
#'                          sample_event_number = plan$sample_event_number,
#'                          first_sample_date = plan$first_sample_date,
#'                          sample_location = plan$location_name,
#'                          sample_location_code = plan$location_code)
#'
#' saveWorkbook(wb, "test.xlsx", overwrite = TRUE)
#' @export
#' @family field sheet helpers
#' @md
create_field_sheet <- function(wb, field_sheet_sample_plan, sample_event_number,
                               first_sample_date, sample_location,
                               sample_location_code) {

  sheet_name <- paste(sample_location_code, sample_event_number, sep = "-")

  center_header_text <- glue::glue("{format(first_sample_date, '%Y')} SR JPE Genetic Sampling
                                   {sample_location} ({sample_location_code})")
  right_header_text <- glue::glue("Sampling event {sample_event_number}
             Date range: {format(first_sample_date, '%b %d')} - {format(first_sample_date + 1, '%b %d, %Y')}")
  row_range <- 2:nrow(field_sheet_sample_plan)

  col_header <- openxlsx::createStyle(border = "TopBottomLeftRight", borderColour = "#000000",
                            wrapText = TRUE, halign = "center", valign = "top",
                            textDecoration = "bold")
  col_style <- openxlsx::createStyle(halign = "center")
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::pageSetup(wb, sheet = sheet_name, orientation = "landscape", left = 0.5, right = 0.25,
            top = 0.75, bottom = 0.75, printTitleRows = 1, fitToWidth = TRUE)
  openxlsx::setColWidths(wb, sheet_name, cols = 1:10,
                         widths = c(3, 10, 8, 20, 10, 10, 8, 8, 8, 35))
  openxlsx::addStyle(wb, sheet = sheet_name, style = col_style, rows = row_range, cols = row_range)
  openxlsx::writeData(wb, sheet = sheet_name, field_sheet_sample_plan, borders = "all", borderColour = "#000000",
            headerStyle = col_header)
  openxlsx::setHeaderFooter(wb, sheet = sheet_name, header = c(NA, center_header_text, right_header_text))

  return(wb)
}

#' Get Sample Event Information for Creating a Field Sheet
#' @description `get_field_sheet_event_plan()` retrieves the sampling event information
#' from the database that is needed to prepare field sheets.
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param sample_event_id The numeric unique identifier of the targeted sampling event for a location
#' Use \code{\link{get_sample_event}} to query and retrieve the IDs from the database.
#' @returns
#' A list object containing the required arguments for \code{\link{create_field_sheet}}
#' * **field_sheet_sample_plan** A dataframe containing the content of the field sheet
#' * **sample_event_number** The non-unique sample event ID
#' * **first_sample_date** A date object YYYY-MM-DD representing the first day of
#' sampling of a 2 day sampling event
#' * **location_name** The sampling location name (e.g., "Battle Creek")
#' * **location_code** The sampling location short code (e.g., "BTC")
#' @export
#' @family field sheet helpers
#' @md
get_field_sheet_event_plan <- function(con, sample_event_id) {

  sample_event <- tbl(con, "sample_event") |>
    filter(id == sample_event_id) |>
    select(sample_event_id = id, sample_event_number, sample_location_id, first_sample_date) |>
    collect()

  sample_bins <- tbl(con, "sample_bin") |>
    select(sample_bin_id = id, sample_event_id, sample_bin_code, min_fork_length,
           max_fork_length) |>
    filter(sample_event_id == sample_event_id) |>
    collect()

  sample_bin_ids <- sample_bins |> pull(sample_bin_id)

  samples <- tbl(con, "sample") |>
    select(sample_id = id, sample_bin_id) |>
    filter(sample_bin_id %in% sample_bin_ids) |>
    collect()

  sample_locations <- tbl(con, "sample_location") |>
    select(sample_location_id = id, code, location_name) |>
    collect()

  sample_plan_raw <- samples |>
    left_join(sample_bins, by = c("sample_bin_id" = "sample_bin_id")) |>
    left_join(sample_event, by = c("sample_event_id" = "sample_event_id"))

  sample_event_details <- sample_event |>
    left_join(sample_locations, by = c("sample_location_id" = "sample_location_id"))

  field_sheet_sample_plan <- sample_plan_raw |>
    transmute(
      Bin = sample_bin_code,
      `Bin FL Range (mm)` = glue::glue("{min_fork_length}-{max_fork_length}"),
      `Sample #` = as.numeric(stringr::str_extract(sample_id, "([0-9]+)$")),
      `Sample ID` = sample_id,
      Date = "",
      Time = "",
      `FL (mm)` = "",
      `Field Run ID` = "",
      `Fin Clip (Y/N)` = "",
      Comments = "",
    )

  return(list(field_sheet_sample_plan = field_sheet_sample_plan,
              sample_event_number = sample_event_details$sample_event_number,
              first_sample_date = sample_event_details$first_sample_date,
              location_name = sample_event_details$location_name,
              location_code = sample_event_details$code))

}




