#' Connect to Run ID Database
library(DBI)
library(grunID)
cfg <- config::get()

con <- DBI::dbConnect(RPostgres::Postgres(),
                 dbname = cfg$dbname,
                 host = cfg$host, # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = cfg$port, # or any other port specified by your DBA
                 user = cfg$username,
                 password = cfg$password)

# select protocol
all_protocols <- get_protocol(con)
View(all_protocols) # review available protocols and select appropriate protocol
protocol_id <- all_protocols[1, "id", drop = TRUE]

#

plate_run_uid <- grunID::add_plate_run(con, protocol_id, genetic_method_id,
                                       laboratory_id, lab_work_preformed_by)

# sample layout
layout <- grunID::process_well_sample_details("data-raw/well_sample_template.csv")

# run sherlock

# process
results <- grunID::process_sherlock(sherlock_results_filepath = "data-raw/exampleoutput_synergyH1trial_data_092021.xlsx",
                            sample_layout_mapping = layout,
                            plate_size = 96)

grunID::add_assay_results(con, results)

DBI::dbDisconnect(con)


