% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lu-agency.R
\name{update_agency_status}
\alias{update_agency_status}
\title{Update Agency Status}
\usage{
update_agency_status(con, agency_id, set_active = TRUE)
}
\arguments{
\item{con}{A DBI connection object obtained from DBI::dbConnect()}

\item{agency_id}{A numeric ID for the targeted agency \code{\link{get_agencies}}}

\item{set_active}{A boolean, TRUE for activating and FALSE for deactivating.
When a record is active, it is returned by default when \code{\link{get_agencies}}
is called. This helps preserve look up values that are valid in historic
contexts, but are no longer valid for current data records.}
}
\description{
\code{update_agency_status()} changes active flag on existing agency in the agency lookup table
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

all_agencies <- get_agencies(con)
View(all_agencies) # to view the ID of the agency needing status change

#deactivate
update_agency_status(con, 4, set_active=FALSE)
#reactivate
update_agency_status(con, 4)
}
\seealso{
Other agency functions: 
\code{\link{add_agency}()},
\code{\link{delete_agency}()},
\code{\link{get_agencies}()},
\code{\link{update_agency}()}
}
\concept{agency functions}
