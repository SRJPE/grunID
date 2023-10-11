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
#' @param fl_summary a summary of the fork length bins used for the sampling location with min and max fork lengths.
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
                               sample_location_code, fl_summary) {
  # format fork length bin summary
  fl_summary <- fl_summary |>
    dplyr::arrange(sample_bin_code) |>
    dplyr::transmute(Bin = sample_bin_code,
                     `Range (mm)` = paste0(min_fork_length, " - ", max_fork_length))

  # add 5 extra rows to the bottom of the table
  field_sheet_sample_plan_extra_rows <- field_sheet_sample_plan |>
    dplyr::mutate(Bin = as.character(Bin)) |>
    tibble::add_row(Bin = rep(NA_character_, 5))

  # set last sample date to the friday of that week
  last_sample_date <- lubridate::ceiling_date(first_sample_date, "week") - 2

  sheet_name <- paste(sample_location_code, sample_event_number, sep = "-")

  center_header_text <- glue::glue("{format(first_sample_date, '%Y')} SR JPE Genetic Sampling
                                   {sample_location} ({sample_location_code})")
  right_header_text <- glue::glue("Sampling event {sample_event_number}
             Date range: {format(first_sample_date, '%b %d')} - {format(last_sample_date, '%b %d, %Y')}")
  row_range <- 2:nrow(field_sheet_sample_plan)

  col_header <- openxlsx::createStyle(border = "TopBottomLeftRight", borderColour = "#000000",
                            wrapText = TRUE, halign = "center", valign = "top",
                            textDecoration = "bold")
  col_style <- openxlsx::createStyle(halign = "center")
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::pageSetup(wb, sheet = sheet_name, orientation = "landscape", left = 0.5, right = 0.25,
            top = 0.75, bottom = 0.75, printTitleRows = 1, fitToWidth = TRUE)
  openxlsx::setColWidths(wb, sheet_name, cols = 1:9,
                         widths = c(3, 10, 8, 20, 10, 10, 15, 15, 35))
  openxlsx::addStyle(wb, sheet = sheet_name, style = col_style, rows = row_range, cols = row_range)
  openxlsx::writeData(wb, sheet = sheet_name, field_sheet_sample_plan_extra_rows,
                      borders = "all", borderColour = "#000000", headerStyle = col_header,
                      )
  openxlsx::writeData(wb, sheet = sheet_name, fl_summary, borders = "all", borderColour = "#000000",
                      headerStyle = col_header, startRow = nrow(field_sheet_sample_plan_extra_rows) + 5,
                      startCol = 5)
  openxlsx::setHeaderFooter(wb, sheet = sheet_name, header = c(NA, center_header_text, right_header_text))

  # add thick borders
  top_borders <- openxlsx::createStyle(border = "Top", borderColour = "#000000", borderStyle = "medium")
  bottom_borders <- openxlsx::createStyle(border = "Bottom", borderColour = "#000000", borderStyle = "medium")
  left_borders <- openxlsx::createStyle(border = "Left", borderColour = "#000000", borderStyle = "medium")
  right_borders <- openxlsx::createStyle(border = "Right", borderColour = "#000000", borderStyle = "medium")

  thick_border_reference <- field_sheet_sample_plan_extra_rows |>
    dplyr::group_by(Bin) |>
    dplyr::summarise(n = n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(max_range = cumsum(n) + 1,
                  min_range = max_range - n + 1) # add 2 so that we skip the first row

  purrr::walk(thick_border_reference$Bin, function(i) {
    border_cols_range <- ncol(field_sheet_sample_plan_extra_rows)

    if(is.na(i)) {
      border_ref <- thick_border_reference |>
        dplyr::filter(is.na(Bin))
    } else {
      border_ref <- thick_border_reference |>
        dplyr::filter(Bin == i)
    }

    # top border
    openxlsx::addStyle(wb, sheet = sheet_name, style = top_borders,
                       rows = border_ref$min_range,
                       cols = 1:ncol(field_sheet_sample_plan_extra_rows),
                       stack = TRUE)
    # bottom border
    openxlsx::addStyle(wb, sheet = sheet_name, style = bottom_borders,
                       rows = border_ref$max_range,
                       cols = 1:border_cols_range,
                       stack = TRUE)

    # left border
    openxlsx::addStyle(wb, sheet = sheet_name, style = left_borders,
                       rows = border_ref$min_range:border_ref$max_range,
                       cols = 1,
                       stack = TRUE)
    # right border
    openxlsx::addStyle(wb, sheet = sheet_name, style = right_borders,
                       rows = border_ref$min_range:border_ref$max_range,
                       cols = border_cols_range,
                       stack = TRUE)
  })

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
get_field_sheet_event_plan <- function(con, sample_event_id_arg) {

  sample_event <- dplyr::tbl(con, "sample_event") |>
    dplyr::filter(id == sample_event_id_arg) |>
    dplyr::select(sample_event_id = id, sample_event_number, sample_location_id, first_sample_date) |>
    dplyr::collect()

  sample_bins <- dplyr::tbl(con, "sample_bin") |>
    dplyr::select(sample_bin_id = id, sample_event_id, sample_bin_code, min_fork_length,
           max_fork_length) |>
    dplyr::filter(sample_event_id == sample_event_id_arg) |>
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
    dplyr::left_join(sample_event, by = c("sample_event_id" = "sample_event_id")) |>
    dplyr::mutate(max_fork_length = max_fork_length - 0.1)

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
      # `Fin Clip (Y/N)` = "", # not necessary for 2024 season per e-mail from Sean/Melinda
      Comments = "",
    )

  return(list(field_sheet_sample_plan = field_sheet_sample_plan,
              sample_event_number = sample_event_details$sample_event_number,
              first_sample_date = sample_event_details$first_sample_date,
              location_name = sample_event_details$location_name,
              location_code = sample_event_details$code))

}


#' Create Multiple Field Sheets
#' @description `create_multiple_field_sheets()` creates an excel workbook and appends
#' multiple formatted field worksheets for sample event IDs in a given sample plan.
#' @details `create_multiple_field_sheets()` combines `get_field_sheet_sample_plan()` and `create_field_sheet()` into
#' one function. Field sheets are created for all unique sample event IDs in a given sample plan.
#' `get_field_sheet_sample_plan()` and `create_field_sheet()` can still be run independently to create one field
#' sheet at a time.
#' For a given season, the function will gather all sampling events for that year up to September 30th and all sampling events
#' from the previous year after October 1st.
#' @param season format YYYY
#' @param field_sheet_filepath The filepath and name desired for the workbook containing field sheets.
#' @param con A valid connection to the database
#' @returns A Workbook object from \code{\link[openxlsx]{createWorkbook}} with a worksheet for each sampling event in the
#' sample plan.
#' @examples
#' con <- gr_db_connect()
#' # add sample plan
#' sample_plan_2022_final <- read_csv("data-raw/2022_sample_plan.csv") |> distinct_all()
#' 2022_sample_plan <- add_sample_plan(con, sample_plan_2022_final, verbose = TRUE)
#'
#' # create workbook with field sheets for all sample event IDs in the 2022 sample plan
#' create_multiple_field_sheets(con, season = 2022, "data-raw/2022_field_sheets.xlsx")
#' @export
#' @family field sheet helpers
#' @md
create_multiple_field_sheets <- function(con, season, field_sheet_filepath) {
  # create workbook to append each sampling event tab
  wb <- openxlsx::createWorkbook()

  # season is based on water year and includes months from previous year
  min_date <- as.Date(paste0(season - 1, "-10-01"))
  max_date <- as.Date(paste0(season, "-09-30"))

  # get season sample_events
  sample_event_info <- dplyr::tbl(con, "sample_event") |>
    dplyr::filter(dplyr::between(first_sample_date, min_date, max_date)) |>
    dplyr::collect() |>
    dplyr::left_join(dplyr::tbl(con, "sample_location") |>
                       dplyr::select(sample_location_id = id, location_code = code) |>
                       dplyr::collect(),
                     by = "sample_location_id") |>
    dplyr::arrange(location_code) |>
    dplyr::select(id, location_code)

  sample_event_ids <- sample_event_info |>
    dplyr::pull(id)

  # get fork length bin summary
  fork_length_bins <- dplyr::tbl(con, "sample_bin") |>
    dplyr::filter(sample_event_id %in% sample_event_ids) |>
    dplyr::collect() |>
    dplyr::left_join(sample_event_info, by = c("sample_event_id" = "id")) |>
    dplyr::mutate(max_fork_length = max_fork_length - 0.1) |>
    dplyr::distinct(location_code, sample_bin_code, min_fork_length, max_fork_length)

  # loop through unique sample event IDs (input to get_field_sheet_event_plan) to append
  # workbooks
  unique_sample_event_ids <- unique(sample_event_ids)

  purrr::walk(unique_sample_event_ids, function(i) {

    # use get_field_sheet_event_plan() to create a data frame containing content for the
    # field sheets for sampling events
    plan <- get_field_sheet_event_plan(con, sample_event_id = i)

    # get fork length summary table for that location
    fl_summary_table <- fork_length_bins |>
      dplyr::filter(location_code == plan$location_code) |>
      dplyr::mutate(sample_bin_code = as.character(sample_bin_code))

    # append a field sheet to the workbook for that sample event
    # create field sheet for sampling crews to use.
    # Takes in get_field_sheet_sample_plan$field_sheet_sample_plan
    # leaves columns "Date", "Time", "FL(mm)", "Field run ID", "Fin clip (Y/N)", and "Comments"
    # blank intentionally so that they can be filled out in the field
    wb <- create_field_sheet(wb = wb,
                             field_sheet_sample_plan = plan$field_sheet_sample_plan,
                             sample_event_number = plan$sample_event_number,
                             first_sample_date = plan$first_sample_date,
                             sample_location = plan$location_name,
                             sample_location_code = plan$location_code,
                             fl_summary = fl_summary_table)
  },
  .progress = list(
    type = "iterator",
    clear = TRUE,
    name = "writing field sheets to excel file"
  ))

  # then save
  openxlsx::saveWorkbook(wb, paste0(field_sheet_filepath), overwrite = TRUE)

  cli::cli_alert_success(paste0("field sheets created at ", field_sheet_filepath),
                         "green")
}



#' Process Sample Field Sheet Data
#' @description `process_field_sheet_examples()` takes sample field sheets and converts
#' them to database-ready formatting.
#' @details See \code{\link{create_field_sheet}} and \code{\link{get_field_sheet_event_plan}} for
#' more information on creating field sheet workbooks.
#' @param filepath the filepath of the field sheet you want to prepare for upload.
#' @returns
#' A tibble object containing the data from all sheets in the @param filepath workbook
#' in required format for \code{\link{update_field_sheet_samples}}. This contains
#' the following variables:
#' * **sample_id** The unique identifier for the sample recorded.
#' * **datetime_collected** The date and time (YYYY-MM-DD H:M:S) the sample was processed.
#' * **fork_length_mm** The recorded fork length corresponding to the sample.
#' * **field_run_type_id** The unique identifier for the field run type.
#' * **fin_clip** Logical variable indicating whether a fin clip sample was taken.
#' * **field_comment** Any recorded comments from the field regarding the sample.
#' @examples
#' filepath <- "data-raw/test.xlsx"
#' field_data_clean <- process_field_sheet_samples(filepath)
#' @export
#' @family field sheet helpers
#' @md
process_field_sheet_samples <- function(filepath){

  field_data <- purrr::map_dfr(readxl::excel_sheets(filepath), function(sheet) {
    readxl::read_excel(path = filepath, sheet = sheet,
                       col_types = c("text", "text", "numeric",
                                     "text", "date", "date",
                                     "numeric", "numeric",
                                     "text", "text"))
  })

  formatted_data <- field_data |>
    dplyr::filter(!is.na(Date)) |> # don't read in any empty rows
    dplyr::mutate(Time = format(Time, "%H:%M:%S"),
                  datetime_collected = lubridate::ymd_hms(paste0(Date, Time)),
                  fin_clip = tolower(`Fin Clip (Y/N)`),
                  fin_clip = ifelse(fin_clip == "n" | is.na(fin_clip), FALSE, TRUE)) |>
    dplyr::select(sample_id = `Sample ID`,
                  datetime_collected,
                  fork_length_mm = `FL (mm)`,
                  field_run_type_id = `Field Run ID`,
                  fin_clip,
                  field_comment = `Comments`)

  return(formatted_data)
}


#' Update Sample Field Sheet Data
#' @description `update_field_sheet_samples()` takes a formatted tibble of field sample
#' data and updates those samples in the database.
#' @param con A DBI connection object obtained from DBI::dbConnect()
#' @param field_data the field data processed using \code{\link{process_field_sheet_samples}}
#' See \code{\link{create_field_sheet}} and \code{\link{get_field_sheet_event_plan}} for
#' more information on creating field sheet workbooks, and \code{\link{process_field_sheet_samples}}
#' for more information on processing raw data before updating in the database.
#' @details This function requires a valid connection to the database and a processed dataset
#' with all variables in the correct format. Errors may be due to invalid data structure or
#' connection; see \code{\link{is_valid_con}} and \code{\link{is_valid_sample_field_data}} for more
#' information. Variables should be as follows:
#' * **sample_id** The unique identifier for the sample recorded, of class "character".
#' * **datetime_collected** The date and time (YYYY-MM-DD H:M:S) the sample was processed,
#' of class "datetime" or ("POSIXct" "POSIXt")
#' * **fork_length_mm** The recorded fork length corresponding to the sample, of class "numeric".
#' * **field_run_type_id** The unique identifier for the field run type, of class "numeric".
#' * **fin_clip** Logical variable indicating whether a fin clip sample was taken, of class "logical".
#' * **field_comment** Any recorded comments from the field regarding the sample, of class "character".
#' @returns Does not return any objects.
#' @examples
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#'
#' filepath <- "data-raw/test.xlsx"
#' field_data_clean <- process_field_sheet_samples(filepath)
#' update_field_sheet_samples(con, field_data_clean)
#' @export
#' @family field sheet helpers
#' @md
update_field_sheet_samples <- function(con, field_data) {

  is_valid_connection(con)
  is_valid_sample_field_data(field_data)

  query <- glue::glue_sql("UPDATE sample
                           SET datetime_collected = {field_data$datetime_collected},
                               fork_length_mm = {field_data$fork_length_mm},
                               field_run_type_id = {field_data$field_run_type_id},
                               fin_clip = {field_data$fin_clip},
                               field_comment = {field_data$field_comment}
                           WHERE id = {field_data$sample_id};",
                          .con = con)

  for(i in 1:length(query)) {
    res <- DBI::dbSendQuery(con, query[i])
    DBI::dbClearResult(res)
  }

  # res <- DBI::dbSendQuery(con, query)
  # results <- DBI::dbFetch(res)
  # DBI::dbClearResult(res)

  #return(results)
}


#' Helper function - checks field sheet data format
#' @description Helper function for parsing input before updating field sample data in
#' database. Called in \code{\link{update_field_sheet_samples}}.
#' @details Checks whether the connection is valid using \code{\link{is_valid_con}} and
#' checks class of input variables.
#' @export
#' @md
is_valid_sample_field_data <- function(data) {

  if (!is.data.frame(data)) {
    stop("Please provide sample field data as a dataframe", call. = FALSE)
  }

  column_reference <- list("sample_id" = "character",
                           "datetime_collected" = c("POSIXct", "POSIXt"),
                           "fork_length_mm" = "numeric",
                           "field_run_type_id" = "numeric",
                           "fin_clip" = "logical",
                           "field_comment" = "character")

  if (!identical(lapply(data, class), column_reference)) {
    stop('The sample field data supplied is not valid, see `help("process_field_sheet_samples")` for correct format', call. = FALSE)
  }
}


