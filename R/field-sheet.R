# library(openxlsx)
# library(tidyverse)



create_field_sheet <- function(wb, field_sheet_sample_plan,
                               sample_event_number, first_sample_date,
                               sample_location, sample_location_code) {

  sheet_name <- paste(sample_location_code, sample_event_number, sep = "-")

  center_header_text <- glue::glue("{format(first_sample_date, '%Y')} SR JPE Genetic Sampling
                                   {sample_location} ({sample_location_code})")
  right_header_text <- glue::glue("Sampling event {sample_event_number}
             Date range: {format(first_sample_date, '%b %d')} - {format(first_sample_date + 1, '%b %d, %Y')}")
  row_range <- 2:nrow(field_sheet_sample_plan)

  col_header <- createStyle(border = "TopBottomLeftRight", borderColour = "#000000",
                            wrapText = TRUE, halign = "center", valign = "top",
                            textDecoration = "bold")
  col_style <- createStyle(halign = "center")
  addWorksheet(wb, sheetName = sheet_name)
  pageSetup(wb, sheet = sheet_name, orientation = "landscape", left = 0.5, right = 0.25,
            top = 0.75, bottom = 0.75, printTitleRows = 1, fitToWidth = TRUE)
  setColWidths(wb, sheet_name, cols = 1:10, widths = c(3, 11, 8, 12, 10, 10, 8, 8, 8, 37))
  addStyle(wb, sheet = sheet_name, style = col_style, rows = row_range, cols = 1:4)
  writeData(wb, sheet = sheet_name, field_sheet_sample_plan, borders = "all", borderColour = "#000000",
            headerStyle = col_header)
  setHeaderFooter(wb, sheet = sheet_name, header = c(NA, center_header_text, right_header_text))

  return(wb)
}

data <- tibble(
  Bin = "A",
  `Bin FL Range (mm)` = "25-53",
  `Sample #` = 1:30,
  `Sample ID` = paste0("BTC22_3_A_", 1:30),
  Date = "",
  Time = "",
  `FL (mm)` = "",
  `Field Run ID` = "",
  `Fin Clip (Y/N)` = "",
  Comments = ""
)

first_sample_date <- lubridate::as_date("2022-02-08")
sample_event_number <- 3

wb <- createWorkbook()
wb <- create_field_sheet(wb, "BTC-3", data, 3, sample_location = "Battle Creek", "BTC")
saveWorkbook(wb, "test.xlsx", overwrite = TRUE)

