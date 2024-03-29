% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/field-sheet.R
\name{create_season_field_sheets}
\alias{create_season_field_sheets}
\title{Create Season Field Sheets}
\usage{
create_season_field_sheets(con, season, field_sheet_filepath)
}
\arguments{
\item{con}{A valid connection to the database}

\item{season}{format YYYY}

\item{field_sheet_filepath}{The filepath and name desired for the workbook containing field sheets.}
}
\value{
A Workbook object from \code{\link[openxlsx]{createWorkbook}} with a worksheet for each sampling event in the
sample plan.
}
\description{
\code{create_season_field_sheets()} creates an excel workbook and appends
multiple formatted field worksheets for sample event IDs in a given sample plan.
}
\details{
\code{create_season_field_sheets()} creates field sheets for all unique sample event IDs in a given season.
For a given season, the function will gather all sampling events for that year up to September 30th and all sampling events
from the previous year after October 1st.
}
\examples{
con <- gr_db_connect()
# add sample plan
sample_plan_2022_final <- read_csv("data-raw/2022_sample_plan.csv") |> distinct_all()
2022_sample_plan <- add_sample_plan(con, sample_plan_2022_final, verbose = TRUE)

# create workbook with field sheets for all sample event IDs in the 2022 sample plan
create_season_field_sheets(con, season = 2022, "data-raw/2022_field_sheets.xlsx")
}
\seealso{
Other field sheet helpers: 
\code{\link{create_field_sheet}()},
\code{\link{get_field_sheet_event_plan}()},
\code{\link{process_field_sheet_samples}()},
\code{\link{update_field_sheet_samples}()}
}
\concept{field sheet helpers}
