# load libraries
library(DBI)
library(grunID)
library(dplyr)
library(tidyverse)
library(readxl)

# connect to runID database
cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
               dbname = cfg$dbname,
               host = cfg$host,
               port = cfg$port,
               user = cfg$username,
               password = cfg$password)

# check connection is working - should see head of these tables
tbl(con, "agency")
tbl(con, "status_code")

# read in sample plan created for the season. this contains all samples initialized
# in the database and contains location codes, sample events, sample bins,
# min/max fork lengths, sample dates, and sample IDs
# running this does the following:

sample_plan_2022_final <- read_csv("data-raw/2022_sample_plan.csv") |> distinct_all()

# filter sample plan to locations (this isn't necessary but helpful for
# partitioning workflow)
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

# add sample plans to database. this code:
# adds sample events to table SAMPLE_EVENT
# adds sample bins for each event SAMPLE_BIN
# adds samples with ids
# updates sample status for each to "created"
# returns the number of IDs created
feather_61_ids <- add_sample_plan(con, feather_61_sample_plan, verbose = TRUE)
feather_17_total <- add_sample_plan(con, feather_17_sample_plan, verbose = TRUE)

# now, prepare to run plates

# get metadata for plate run.
# first, check all protocols to select the correct one:
all_protocols <- get_protocols(con) |> print(n=Inf)

# review available protocols and select appropriate protocol id
protocol_id <- all_protocols |>
  filter(id == 1) |>
  pull(id)

# select laboratory ID that corresponds to "DWR"
laboratory_id <- get_laboratories(con) |>
  filter(stringr::str_detect(code, "DWR")) |> pull(id)

# get genetic method id that corresponds to "SHERLOCK"
genetic_method_id <- get_genetic_methods(con) |>
  filter(method_name == "SHERLOCK") |>
  pull(id)

# add plate run to the database for each assay you plan to run.
# be clear with names here. this returns an ID for the plate run
# that will determine where your results are stored.

# ots 28 early run
plate_run_4_7_early_id <- add_plate_run(con,
                                        date_run = "2022-01-01",
                                        protocol_id = protocol_id,
                                        genetic_method_id = genetic_method_id,
                                        laboratory_id = laboratory_id,
                                        lab_work_performed_by = "user",
                                        description = "early run for plates 4-7") # TODO determine the user

# ots 28 late run
plate_run_4_7_late_id <- add_plate_run(con,
                                       date_run = "2022-01-01",
                                       protocol_id = protocol_id,
                                       genetic_method_id = genetic_method_id,
                                       laboratory_id = laboratory_id,
                                       lab_work_performed_by = "user",
                                       description = "late run for plates 4-7") # TODO determine the user


# read in the plate run map that is created before the plate is run.
# this plate map layout should contain generic sherlock-created sample IDs
# which will then be aligned with the corresponding JPE sample IDs.

# get_sample_details process the layout. User must specify assay type
# either "mucus" or "fin clip" as well as assay type - see ?process_well_sample_details()
# for acceptable arguments.

# TODO rename to process_well_sample_details
# TODO check what colors mean in plate map "sheet"
plate_4_to_7_layout_early <- process_well_sample_details(filepath = "data-raw/sherlock-example-outputs/JPE_Chnk_Early+Late_Plates4-7_results.xlsx",
                                                         sample_type = "mucus",
                                                         assay_type = "Ots28_Early1",
                                                         plate_run_id = plate_run_4_7_early_id)

plate_4_to_7_layout_late <- process_well_sample_details(filepath = "data-raw/sherlock-example-outputs/JPE_Chnk_Early+Late_Plates4-7_results.xlsx",
                                                        sample_type = "mucus",
                                                        assay_type = "Ots28_Late1",
                                                        plate_run_id = plate_run_4_7_late_id)


# pass sample details (well layout) to process_sherlock, which reads in
# a file with the output of a sherlock machine. This function also maps
# the generic IDs with the JPE sample IDs.
# process
results_plates_4_7_early <- process_sherlock(
  filepath = "data-raw/sherlock-example-outputs/JPE_Chnk_early_plates_4_7.xlsx",
  sample_details = plate_4_to_7_layout_early,
  plate_size = 384)


results_plates_4_7_late <- process_sherlock(
  filepath = "data-raw/sherlock-example-outputs/JPE_Chnk_late_plates_4_7.xlsx",
  sample_details = plate_4_to_7_layout_late,
  plate_size = 384)

# add raw assay results to database
plate_4_7_early <- add_raw_assay_results(con, results_plates_4_7_early)
plate_4_7_late <- add_raw_assay_results(con, results_plates_4_7_late)

# generate thresholds from raw assay results
thresholds_4_7_early <- generate_threshold(con, plate_run_identifier = plate_run_4_7_early_id)
thresholds_4_7_late <- generate_threshold(con, plate_run_identifier = plate_run_4_7_late_id)

# update assay detection results (TRUE or FALSE for a sample and assay type)
# in the database
update_assay_detection(con, thresholds_4_7_early)
update_assay_detection(con, thresholds_4_7_late)

# view assay results.
# this table contains the sample IDs run, the assay type, and positive detection
tbl(con, "assay_result")

# see genetic run identification results
tbl(con, "genetic_run_identification")
# this is run type IDs and their associated run
tbl(con, "run_type")

# disconnect!
DBI::dbDisconnect(con)





