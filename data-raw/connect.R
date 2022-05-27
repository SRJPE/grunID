#' Connec to Run ID Database
library(DBI)
library(grunID)
library(dplyr)
library(readr)
cfg <- config::get()

con <- dbConnect(RPostgres::Postgres(),
                 dbname = cfg$dbname,
                 host = cfg$host, # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = cfg$port, # or any other port specified by your DBA
                 user = cfg$username,
                 password = cfg$password)

# process protocol file
protocol_settings <- process_protocol_file(protocol_file = "")

plate_run_uid <- add_plate_run(con, protocol_settings)

# sample layout
layout <- read_csv("data-raw/sample_layout_template.csv")
layout$plate_run_id <- plate_run_uid

# run sherlock

# process
results <- process_sherlock(sherlock_results_filepath = "data-raw/exampleoutput_synergyH1trial_data_092021.xlsx",
                            sample_layout_mapping = layout,
                            plate_size = 96)


grunID::add_assay_results(con, results)

DBI::dbDisconnect(con)
