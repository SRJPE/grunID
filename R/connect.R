#' Connec to Run ID Database

connect <- function() {

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'run_id_db',
                 host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'postgres',
                 password = 'postgres')
}


#' User Login
user_login <- function(username, password) {

}
