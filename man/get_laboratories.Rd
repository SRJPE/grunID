% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lu-laboratory.R
\name{get_laboratories}
\alias{get_laboratories}
\title{Retrieve Laboratories}
\usage{
get_laboratories(con, is_active = TRUE, all_results = FALSE, ...)
}
\arguments{
\item{con}{A DBI connection object obtained from DBI::dbConnect()}
}
\description{
\code{get_laboratories()} returns all laboratories within the database
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
laboratories <- get_laboratories(con)
}
\seealso{
Other laboratory functions: 
\code{\link{add_laboratory}()},
\code{\link{delete_laboratory}()},
\code{\link{update_laboratory}()},
\code{\link{update_laboratory_status}()}
}
\concept{laboratory functions}
