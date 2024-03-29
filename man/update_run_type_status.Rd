% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lu-run-type.R
\name{update_run_type_status}
\alias{update_run_type_status}
\title{Update Run Type Status}
\usage{
update_run_type_status(con, run_type_id, set_active = TRUE)
}
\arguments{
\item{con}{A DBI connection object obtained from DBI::dbConnect()}

\item{run_type_id}{A numeric ID for the targeted run type \code{\link{get_agencies}}}

\item{set_active}{A boolean, TRUE for activating and FALSE for deactivating.
When a record is active, it is returned by default when \code{\link{get_agencies}}
is called. This helps preserve look up values that are valid in historic
contexts, but are no longer valid for current data records.}
}
\description{
\code{update_run_type_status()} changes active flag on existing run type in the run type lookup table
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

all_run_types <- get_run_types(con)
View(all_run_types) # to view the ID of the run type needing status change

#deactivate
update_run_type_status(con, 4, set_active=FALSE)
#reactivate
update_run_type_status(con, 4)
}
\seealso{
Other run type functions: 
\code{\link{add_run_type}()},
\code{\link{delete_run_type}()},
\code{\link{get_run_types}()},
\code{\link{update_run_type}()}
}
\concept{run type functions}
