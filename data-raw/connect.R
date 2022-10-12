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

# running this does the following:
# add_sample_events(con, sample_plan)
# add_sample_bins(con, sample_plan, sample_event_ids)
# add_samples(con, sample_plan, sample_id_insert)
# set_sample_status(con, sample_ids, "created")
test <- add_sample_plan(con, grunID::sample_plan_template)

# select protocol
all_protocols <- get_protocols(con)

View(all_protocols) # review available protocols and select appropriate protocol
protocol_id <- all_protocols[1, "id", drop = TRUE]

# get lab id
laboratory_id <- get_laboratories(con) |>
  filter(stringr::str_detect(code, "DWR")) |> pull(id)

plate_run_uid <- add_plate_run(con,
                               date_run = "2022-01-01",
                               protocol_id = protocol_id,
                               genetic_method_id = 1,
                               laboratory_id = laboratory_id,
                               lab_work_preformed_by = "user")

# sample layout
layout <- process_well_sample_details("data-raw/well_sample_template.csv",
                                      plate_run_id = plate_run_uid)

# process
results <- process_sherlock(
  filepath = "data-raw/exampleoutput_synergyH1trial_data_092021.xlsx",
  sample_details = layout,
  plate_size = 96)

grunID::add_assay_results(con, results)

DBI::dbDisconnect(con)
