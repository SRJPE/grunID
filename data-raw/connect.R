#' Connect to Run ID Database
library(DBI)
library(grunID)
library(dplyr)

cfg <- config::get()

con <- DBI::dbConnect(RPostgres::Postgres(),
               dbname = cfg$dbname,
               host = cfg$host,
               port = cfg$port,
               user = cfg$username,
               password = cfg$password)

# running this does the following:
# adds sample events to table SAMPLE_EVENT
# adds sample bins for each event SAMPLE_BIN
# adds samples with ids
# updates sample status for each to "created"
plan_1 <- add_sample_plan(con, grunID::sample_plan_template)

# select protocol
all_protocols <- get_protocols(con)

# View(all_protocols) # review available protocols and select appropriate protocol
protocol_id <- all_protocols[2, "id", drop = TRUE]

# get lab id
laboratory_id <- get_laboratories(con) |>
  filter(stringr::str_detect(code, "DWR")) |> pull(id)

# adding a plate run is someone choosing one of the protocols
# and adding a few more parameters to create a "plate run"
plate_run_uid <- add_plate_run(con,
                               date_run = "2022-01-01",
                               protocol_id = protocol_id,
                               genetic_method_id = 1,
                               laboratory_id = laboratory_id,
                               lab_work_preformed_by = "user") # TODO determine the user

# sample layout, is then created by the user and read in here
# the plate run must be identified to correspond to
layout <- process_well_sample_details("data-raw/well_sample_template.csv",
                                      plate_run_id = plate_run_uid)

# process
results <- process_sherlock(
  filepath = "data-raw/exampleoutput_synergyH1trial_data_092021.xlsx",
  sample_details = layout,
  plate_size = 96)

res <- add_sherlock_results(con, results, layout)



DBI::dbDisconnect(con)
