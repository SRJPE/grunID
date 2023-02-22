#' Connect to Run ID Database
library(DBI)
library(grunID)
library(dplyr)
library(tidyverse)

cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
               dbname = cfg$dbname,
               host = cfg$host,
               port = cfg$port,
               user = cfg$username,
               password = cfg$password)

tbl(con, "agency")
tbl(con, "status_code")
# running this does the following:

`# running this does the following:
# adds sample events to table SAMPLE_EVENT
# adds sample bins for each event SAMPLE_BIN
# adds samples with ids
# updates sample status for each to "created"
# plan_2022 <- add_sample_plan(con, sample_plan_2022_final)

sample_plan_2022_final <- read_csv("data-raw/2022_sample_plan.csv") |> distinct_all()

feather_61_sample_plan <- sample_plan_2022_final |>
  filter(location_code == "F61") |>
  mutate(sample_event_number = as.integer(sample_event_number),
         min_fork_length = as.integer(min_fork_length),
         max_fork_length = as.integer(max_fork_length))

feather_61_ids <- add_sample_plan(con, feather_61_sample_plan, verbose = TRUE)



btc_sample_plan <- sample_plan_2022_final |>
  filter(location_code == "BTC")

btc_plan_2022 <- add_sample_plan(con, btc_sample_plan, verbose = TRUE)


# butte creek
but_sample_plan <- sample_plan_2022_final |>
  filter(location_code == "BUT")

but_plan_2022 <- add_sample_plan(con, but_sample_plan, verbose = TRUE)


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
                               lab_work_performed_by = "user") # TODO determine the user

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
                               lab_work_performed_by = "user") # TODO determine the user

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


# Do the OTS 16

samples_require_analysis <- function(con, assay_name=c("ots28", "ots16")) {

  samples_that_need_ots16 <- tbl(con, "genetic_run_identification") |>
    filter(run_type_id == 6)


  return(samples_that_need_ots16)

}


samples_require_ots16(con)

readr::write_csv(samples_that_need_ots16, "data-raw/ots_16_s1_plate_details.csv")

plate_run_id_ots_16_S <- add_plate_run(con,
                                       date_run = "2022-10-27",
                                       protocol_id = protocol_id,
                                       genetic_method_id = 1,
                                       laboratory_id = laboratory_id,
                                       lab_work_performed_by = "user")

layout_ots_16_S <- process_well_sample_details("data-raw/ots_16_s1_plate_details.csv",
                                               plate_run_id = plate_run_id_ots_16_S)

results_ots_16_S <- process_sherlock(
  filepath = "data-raw/FAKE_OTS16_S1.xlsx",
  sample_details = layout_ots_28_e,
  plate_size = 384)

DBI::dbDisconnect(con)





