#' Connec to Run ID Database
library(DBI)

cfg <- config::get()

con <- dbConnect(RPostgres::Postgres(),
                 dbname = cfg$dbname,
                 host = cfg$host, # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = cfg$port, # or any other port specified by your DBA
                 user = cfg$username,
                 password = cfg$password)


# process protocol file
protocol_settings <- process_protocol_file(protocol_file = "")

#
plate_run_uid <- create_plate_run(con, protocol_settings)

DBI::dbDisconnect(con)
