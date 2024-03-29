% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample-events.R
\name{process_raw_sample_plan}
\alias{process_raw_sample_plan}
\title{Process Raw Sample Plan}
\usage{
process_raw_sample_plan(filepath, season)
}
\arguments{
\item{filepath}{the filepath of the raw sample plan to be processed}

\item{season}{the season for which the sample plan was developed. format YYYY}
}
\value{
a tidy dataframe that can be passed directly to \code{add_sample_plan()}. Has the following columns:
\itemize{
\item location_code
\item sample_event_number
\item first_sample_date
\item sample_bin_code
\item min_fork_length
\item max_fork_length
\item expected_number_of_samples
}
}
\description{
\code{process_raw_sample_plan}
}
\details{
this function assigns an maximum fork length of 200mm to all fork length bins that
are a "plus" (i.e. a fork length bin that is \verb{130+} will be assigned a minimum fork length of 130 and
a maximum fork length of \code{200}). The function will also assign a \code{first_sample_date} of January 1st
of the season (if you pass \code{season = 2024}, the \code{first_sample_date} will be \code{2024-01-01}).
}
\examples{
# example database connection
process_raw_sample_plan(filepath = "data-raw/2024_raw_sample_plan.xlsx", season = 2024)
}
