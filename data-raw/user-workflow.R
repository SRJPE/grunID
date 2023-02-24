#' Connect to Run ID Database
library(DBI)
library(grunID)
library(dplyr)
library(tidyverse)
library(readxl)

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

# running this does the following:
# adds sample events to table SAMPLE_EVENT
# adds sample bins for each event SAMPLE_BIN
# adds samples with ids
# updates sample status for each to "created"

sample_plan_2022_final <- read_csv("data-raw/2022_sample_plan.csv") |> distinct_all()

feather_61_sample_plan <- sample_plan_2022_final |>
  filter(location_code == "F61") |>
  mutate(sample_event_number = as.integer(sample_event_number),
         min_fork_length = as.integer(min_fork_length),
         max_fork_length = as.integer(max_fork_length))

feather_17_sample_plan <- sample_plan_2022_final |>
  filter(location_code == "F17") |>
  mutate(sample_event_number = as.integer(sample_event_number),
         min_fork_length = as.integer(min_fork_length),
         max_fork_length = as.integer(max_fork_length))

feather_61_ids <- add_sample_plan(con, feather_61_sample_plan, verbose = TRUE)
feather_17_total <- add_sample_plan(con, feather_17_sample_plan, verbose = TRUE)


# check to see if protocol for plate run exists or not
all_protocols <- get_protocols(con)


# View(all_protocols) # review available protocols and select appropriate protocol
protocol_id <- all_protocols[1, "id", drop = TRUE]

# get lab id
laboratory_id <- get_laboratories(con) |>
  filter(stringr::str_detect(code, "DWR")) |> pull(id)


# ots 28 early run
plate_run_4_7_early_id <- add_plate_run(con,
                                        date_run = "2022-01-01",
                                        protocol_id = protocol_id,
                                        genetic_method_id = 1,
                                        laboratory_id = laboratory_id,
                                        lab_work_performed_by = "user",
                                        description = "early run for plates 4-7") # TODO determine the user

# ots 28 late run
plate_run_4_7_late_id <- add_plate_run(con,
                                       date_run = "2022-01-01",
                                       protocol_id = protocol_id,
                                       genetic_method_id = 1,
                                       laboratory_id = laboratory_id,
                                       lab_work_performed_by = "user",
                                       description = "late run for plates 4-7") # TODO determine the user



# sample layout, is then created by the user and read in here
# the plate run must be identified to correspond to
# layout_ots_28_e <- process_well_sample_details("data-raw/ots_28_early_plate_details.csv",
#                                       plate_run_id = plate_run_id_ots_28_e)


#
# layout_ots_28_l <- process_well_sample_details("data-raw/ots_28_late_plate_details.csv",
#                                       plate_run_id = plate_run_id_ots_28_l)

layout_raw <- read_excel("data-raw/sherlock-example-outputs/JPE_Chnk_Early+Late_Plates4-7_results.xlsx",
                         sheet = "Plate Map")

plate_4_to_7_layout_early <- layout_raw |>
  pivot_longer(names_to="col_num", values_to = "sample_id", -...1) |>
  transmute(
    location = paste0(...1, col_num),
    sample_id,
    sample_type_id = 1,
    assay_id = 1,
    plate_run_id = plate_run_4_7_early_id
  ) |>
  mutate(sample_id = ifelse(sample_id == "NTC", "CONTROL", sample_id))

plate_4_to_7_layout_late <- layout_raw |>
  pivot_longer(names_to="col_num", values_to = "sample_id", -...1) |>
  transmute(
    location = paste0(...1, col_num),
    sample_id,
    sample_type_id = 1,
    assay_id = 2,
    plate_run_id = plate_run_4_7_late_id
  ) |>
  mutate(sample_id = ifelse(sample_id == "NTC", "CONTROL", sample_id))

# process
results_plates_4_7_early <- process_sherlock(
  filepath = "data-raw/sherlock-example-outputs/JPE_Chnk_early_plates_4_7.xlsx",
  sample_details = plate_4_to_7_layout_early,
  plate_size = 384)


results_plates_4_7_late <- process_sherlock(
  filepath = "data-raw/sherlock-example-outputs/JPE_Chnk_late_plates_4_7.xlsx",
  sample_details = plate_4_to_7_layout_late,
  plate_size = 384)


plate_4_7_early <- add_raw_assay_results(con, results_plates_4_7_early)
plate_4_7_late <- add_raw_assay_results(con, results_plates_4_7_late)

thresholds_4_7_early <- generate_threshold(con, plate_run_identifier = plate_run_4_7_early_id)
thresholds_4_7_late <- generate_threshold(con, plate_run_identifier = plate_run_4_7_late_id)

update_assay_detection(con, thresholds_4_7_early)
update_assay_detection(con, thresholds_4_7_late)


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





