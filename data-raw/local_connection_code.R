# this connects to the local database
# keeping here because I'm not sure where else to store it

# connect to runID database
cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = cfg$port,
                      user = cfg$username,
                      password = cfg$password)
