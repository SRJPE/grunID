% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_codes.R
\name{assign_status_codes}
\alias{assign_status_codes}
\title{assign status codes}
\usage{
assign_status_codes(assay_detections)
}
\arguments{
\item{assay_detections}{table of assay detections}
}
\value{
a table with `status_code_id` column updated
}
\description{
helper function called within `add_genetic_identification`
}
\details{
`assign_status_codes` takes in assay detections and assigns a status code
based on assay results logic.
}
\keyword{internal}
