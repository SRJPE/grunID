#' Connect to Run ID Database
library(DBI)
library(grunID)
cfg <- config::get()

con <- DBI::dbConnect(RPostgres::Postgres(),
                 dbname = cfg$dbname,
                 host = cfg$host,
                 port = cfg$port,
                 user = cfg$username,
                 password = cfg$password)

test <- add_sample_plan(con, grunID::sample_plan_template)

a <- get_sample_status(con, sample_ids)
View(a)
sample_ids_to_set <- sample_ids[1:8]

set_sample_status(con, sample_ids = sample_ids_to_set, comment = "sent to feather river",
                  status = "out to field")

a <- get_sample_status(con, sample_ids_to_set)
get_sample_status(con, sample_ids_to_set, TRUE) |> View()
View(b)

# select protocol
all_protocols <- get_protocols(con)

View(all_protocols) # review available protocols and select appropriate protocol
protocol_id <- all_protocols[1, "id", drop = TRUE]

plate_run_uid <- grunID::add_plate_run(con, protocol_id, genetic_method_id = 1,
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
