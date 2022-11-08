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
#' cfg <- config::get()
#'
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#' wb <- openxlsx::createWorkbook()
#' # Each sample_event will be a tab in a workbook
#' plan <- get_field_sheet_event_plan(con, sample_event_id = 1)
#' wb <- create_field_sheet(wb = wb,
#'                          field_sheet_sample_plan = plan$field_sheet_sample_plan,
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
#' openxlsx::saveWorkbook(wb, "test.xlsx", overwrite = TRUE)
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
#' @examples
#' cfg <- config::get()
#'
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#' plan <- get_field_sheet_event_plan(con, sample_event_id = 1)
#' @export
#' @family field sheet helpers
#' @md
get_field_sheet_event_plan <- function(con, sample_event_id) {

  sample_event <- dplyr::tbl(con, "sample_event") |>
    dplyr::filter(id == sample_event_id) |>
    dplyr::select(sample_event_id = id, sample_event_number, sample_location_id, first_sample_date) |>
    dplyr::collect()

  sample_bins <- dplyr::tbl(con, "sample_bin") |>
    dplyr::select(sample_bin_id = id, sample_event_id, sample_bin_code, min_fork_length,
           max_fork_length) |>
    dplyr::filter(sample_event_id == sample_event_id) |>
    dplyr::collect()

  sample_bin_ids <- sample_bins |> dplyr::pull(sample_bin_id)

  samples <- dplyr::tbl(con, "sample") |>
    dplyr::select(sample_id = id, sample_bin_id) |>
    dplyr::filter(sample_bin_id %in% sample_bin_ids) |>
    dplyr::collect()

  sample_locations <- get_sample_locations(con) |>
    dplyr::select(sample_location_id = id, code, location_name)

  sample_plan_raw <- samples |>
    dplyr::left_join(sample_bins, by = c("sample_bin_id" = "sample_bin_id")) |>
    dplyr::left_join(sample_event, by = c("sample_event_id" = "sample_event_id"))

  sample_event_details <- sample_event |>
    dplyr::left_join(sample_locations, by = c("sample_location_id" = "sample_location_id"))

  field_sheet_sample_plan <- sample_plan_raw |>
    dplyr::transmute(
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


#' Process Sample Field Sheet Data
process_field_sheet_samples <- function(filepath){

  field_data <- purrr::map_dfr(excel_sheets(filepath), function(sheet) {
    readxl::read_excel(path = filepath, sheet = sheet,
                       col_types = c("text", "text", "numeric",
                                     "text", "date", "date",
                                     "numeric", "numeric",
                                     "text", "text"))
  })

  formatted_data <- field_data |>
    dplyr::filter(!is.na(`Field Run ID`)) |> # don't read in any empty rows
    dplyr::mutate(Time= hms::as_hms(Time),
                  datetime_collected = lubridate::ymd_hms(paste0(Date, Time)),
                  fin_clip_y_n = tolower(`Fin Clip\r\n(Y/N)`),
                  fin_clip_y_n = ifelse(fin_clip_y_n == "y", TRUE, FALSE)) |>
    dplyr::rename(fork_length_mm = `FL (mm)`,
                  field_run_type_id = `Field Run ID`,
                  field_comment = `Comments`,
                  sample_id = `Sample ID`)  |>
    dplyr::select(datetime_collected,
                  fork_length_mm,
                  field_run_type_id,
                  fin_clip_y_n,
                  field_comment,
                  sample_id)

  return(formatted_data)
}


#' Update Sample Field Sheet Data
update_field_sheet_samples <- function(con, field_data) {

  #is_valid_connection(con)
  #is_valid_sample_field_data(field_data)

  query <- glue::glue_sql("UPDATE sample
                           SET datetime_collected = (SELECT(UNNEST(ARRAY[{field_data$datetime_collected*}]::timestamp[]))),
                               fork_length_mm = (SELECT(UNNEST(ARRAY[{field_data$fork_length_mm*}]))),
                               field_run_type_id = (SELECT(UNNEST(ARRAY[{field_data$field_run_type_id*}]))),
                               fin_clip = (SELECT(UNNEST(ARRAY[{field_data$fin_clip*}]))),
                               field_comment = (SELECT(UNNEST(ARRAY[{field_data$field_comment*}])))
                           WHERE id IN (SELECT(UNNEST(ARRAY[{field_data$sample_id*}])))
                           RETURNING id, updated_at;",
                          .con = con)

  res <- DBI::dbSendQuery(con, query)
  results <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  return(results)
}

# TODO: this isn't working if you have rows with samples that don't exist in the database
# TODO: check if this still works if you have two rows with sample IDs that exist in the database


#' Helper function - checks field sheet data format
is_valid_sample_field_data <- function(data) {

  if (!is.data.frame(data)) {
    stop("Please provide sample field data as a dataframe", call. = FALSE)
  }

  column_reference <- c("datetime_collected" = "Date", # TODO: this will return false until process_field_sheet_samples returns class = Date
                        "fork_length_mm" = "numeric",
                        "field_run_type_id" = "numeric",
                        "fin_clip" = "logical",
                        "field_comment" = "character")

  if (!identical(sapply(data, class), column_reference)) {
    stop('The sample field data supplied is not valid, see `help("process_field_sheet_samples")` for correct format', call. = FALSE)
  }

}


