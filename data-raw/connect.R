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
protocol_id <- all_protocols[1, "id", drop = TRUE]

# get lab id
laboratory_id <- get_laboratories(con) |>
  filter(stringr::str_detect(code, "DWR")) |> pull(id)


# ots 28 early run
plate_run_id_ots_28_e <- add_plate_run(con,
                               date_run = "2022-01-01",
                               protocol_id = protocol_id,
                               genetic_method_id = 1,
                               laboratory_id = laboratory_id,
                               lab_work_preformed_by = "user") # TODO determine the user

# sample layout, is then created by the user and read in here
# the plate run must be identified to correspond to
layout_ots_28_e <- process_well_sample_details("data-raw/ots_28_early_plate_details.csv",
                                      plate_run_id = plate_run_id_ots_28_e)

# ots 28 late run
plate_run_id_ots_28_l <- add_plate_run(con,
                               date_run = "2022-01-01",
                               protocol_id = protocol_id,
                               genetic_method_id = 1,
                               laboratory_id = laboratory_id,
                               lab_work_preformed_by = "user") # TODO determine the user

# sample layout, is then created by the user and read in here
# the plate run must be identified to correspond to
layout_ots_28_l <- process_well_sample_details("data-raw/ots_28_late_plate_details.csv",
                                      plate_run_id = plate_run_id_ots_28_l)


# process
results_ots_28_e <- process_sherlock(
  filepath = "data-raw/081022_Chnk_JPE_Early_Plates7-10_results.xlsx",
  sample_details = layout_ots_28_e,
  plate_size = 384)


results_ots_28_l <- process_sherlock(
  filepath = "data-raw/081122_Chnk_JPE_Late_Plates7-10_results.xlsx",
  sample_details = layout_ots_28_l,
  plate_size = 384)


res_ots_28_e <- add_raw_assay_results(con, results_ots_28_e)
res_ots_28_l <- add_raw_assay_results(con, results_ots_28_l)

thresholds_ots_28_e <- generate_threshold(con, plate_run = plate_run_id_ots_28_e)
thresholds_ots_28_l <- generate_threshold(con, plate_run = plate_run_id_ots_28_l)

update_assay_detection(con, thresholds_ots_28_e)
update_assay_detection(con, thresholds_ots_28_l)


DBI::dbDisconnect(con)





