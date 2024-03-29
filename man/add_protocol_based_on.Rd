% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lu-protocol.R
\name{add_protocol_based_on}
\alias{add_protocol_based_on}
\title{Add Protocol Based on Existing Protocol}
\usage{
add_protocol_based_on(
  con,
  based_on,
  new_name,
  commit = FALSE,
  software_version = NULL,
  reader_type = NULL,
  reader_serial_number = NULL,
  plate_type = NULL,
  set_point = NULL,
  preheat_before_moving = NULL,
  runtime = NULL,
  interval = NULL,
  read_count = NULL,
  run_mode = NULL,
  excitation = NULL,
  emissions = NULL,
  optics = NULL,
  gain = NULL,
  light_source = NULL,
  lamp_energy = NULL,
  read_height = NULL
)
}
\arguments{
\item{con}{A DBI connection object obtained from DBI::dbConnect()}

\item{based_on}{A valid protocol name, this protocol must exist.}
}
\description{
\code{add_protocol_based_on()} adds a new protocol to the protocol lookup table based on an existing protocol.
Use this instead of \code{add_protocol} in case you have a new protocol that varies just slightly from existing one.
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

# update the software version
add_protocol_based_on(con, based_on = "existing protocol", software_version = "2")
}
\seealso{
Other protocol functions: 
\code{\link{add_protocol}()},
\code{\link{delete_protocol}()},
\code{\link{get_protocols}()},
\code{\link{update_protocol}()},
\code{\link{update_protocol_status}()}
}
\concept{protocol functions}
