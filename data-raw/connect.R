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

tbl(con, "agency")

# TODO helpers for protocol id, gen method id, lab id
plate_run_uid <- add_plate_run(con, protocol, genetic_method, laboratory, lab_work_preformed_by)

# sample layout
layout <- prepare_layout("data-raw/sample_layout_template.csv", plate_run_uid)

# run sherlock

# process
results <- process_sherlock(sherlock_results_filepath = "data-raw/exampleoutput_synergyH1trial_data_092021.xlsx",
                            sample_layout_mapping = layout,
                            plate_size = 96)


grunID::add_assay_results(con, results)

DBI::dbDisconnect(con)
