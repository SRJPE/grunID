% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lu-genetic-method.R
\name{update_genetic_method}
\alias{update_genetic_method}
\title{Update Genetic Method}
\usage{
update_genetic_method(con, genetic_method_id, genetic_method)
}
\arguments{
\item{con}{A DBI connection object obtained from DBI::dbConnect()}

\item{genetic_method_id}{A numeric ID for the targeted genetic method \code{\link{get_genetic_methods}}}

\item{genetic_method}{A valid genetic method dataframe with the following:
\enumerate{
\item \strong{code} \emph{character} 4 letter code in all caps
\item \strong{method_name} \emph{character} Short name for method
\item \strong{description} \emph{character} Brief description of method
}}
}
\description{
\code{update_genetic_method()} updates an existing genetic method in the genetic method lookup table
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

all_methods <- get_genetic_methods(con)
View(all_methods) # to view the ID of the genetic method needing updates

updated_method <- data.frame(code = "SHLK", method_name = "Sherlock",
                         description = "Changed the description")
update_genetic_method(con, genetic_method_id = 1, updated_method)
}
\seealso{
Other genetic method functions: 
\code{\link{add_genetic_method}()},
\code{\link{delete_genetic_method}()},
\code{\link{get_genetic_methods}()},
\code{\link{update_genetic_method_status}()}
}
\concept{genetic method functions}
