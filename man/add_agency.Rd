% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lu-agency.R
\name{add_agency}
\alias{add_agency}
\title{Add Agency}
\usage{
add_agency(con, agency)
}
\arguments{
\item{con}{A DBI connection object obtained from DBI::dbConnect()}

\item{agency}{A valid agency dataframe with the following:
\enumerate{
\item \strong{code} \emph{character} A short code for the agency
\item \strong{agency_name} \emph{character} The agency's proper name
}}
}
\description{
\code{add_agency()} adds a new agency to the agency lookup table
}
\examples{
# example database connection
cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = cfg$port,
                      user = cfg$username,
                      password = cfg$password)

new_agency <- data.frame(code = "USFWS",
                         agency_name = "United States Fish and Wildlife Service")
add_agency(con, new_agency)
}
\seealso{
Other agency functions: 
\code{\link{delete_agency}()},
\code{\link{get_agencies}()},
\code{\link{update_agency}()},
\code{\link{update_agency_status}()}
}
\concept{agency functions}
