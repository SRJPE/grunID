# libraries
library(tidyverse)
library(readxl)
library(DBI)
library(grunID)

# establish connection
cfg <- config::get()



con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = 5432,
                      user = cfg$username,
                      password = cfg$password)

tbl(con, "sample")

sample_plan_2023_final <- read_csv("data-raw/sample_plan_2023.csv") |> distinct_all()

# filter sample plan to locations (this isn't necessary but helpful for
# partitioning workflow)
sample_plan_subset <- sample_plan_2023_final |>
  filter(location_code %in% c("BUT", "MIL", "DER")) |>
  mutate(sample_event_number = as.integer(sample_event_number),
         min_fork_length = as.integer(min_fork_length),
         max_fork_length = as.integer(max_fork_length))

# add sample plans to database. this code:
# adds sample events to table SAMPLE_EVENT
# adds sample bins for each event SAMPLE_BIN
# adds samples with ids to SAMPLE
# updates sample status for each to "created" (1)
# returns the number of IDs created and the unique sample IDs created
total_inserted <- add_sample_plan(con, sample_plan_subset, verbose = TRUE)

all_protocols <- get_protocols(con)
all_protocols |> View()

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

# this is if your plate has two assays
split_plate_run_4_7_id <- add_plate_run(con,
                                        date_run = "2022-01-01",
                                        protocol_id = protocol_id,
                                        genetic_method_id = genetic_method_id,
                                        laboratory_id = laboratory_id,
                                        lab_work_performed_by = "user",
                                        description = "early and late assays for plates 4-7")
tbl(con, "plate_run")


# read in the plate run map that is created before the plate is run.
# this plate map layout should contain generic sherlock-created sample IDs
# which will then be aligned with the corresponding JPE sample IDs.

# get_sample_details process the layout. User must specify layout type and
# sample_type. See `?process_well_sample_details()` for acceptable arguments.

# this is for the split plate version
plate_4_to_7_layout_split <- process_well_sample_details(filepath = "C:/Users/emanuel/projects/jpe/grunID/data-raw/2023-use-case/layout-empty-well-values/230518_AF_EL_1&2.xlsx",
                                                         sample_type = "mucus",
                                                         layout_type = "split_plate_early_late",
                                                         plate_run_id = split_plate_run_4_7_id)


# pass sample details (well layout) to process_sherlock, which reads in
# a file with the output of a sherlock machine. This function also maps
# the generic IDs with the JPE sample IDs.
# process
results_plates_4_7_split <- process_sherlock(
  filepath = "data-raw/sherlock-example-outputs/JPE_Chnk_early_plates_4_7.xlsx",
  sample_details = plate_4_to_7_layout_split,
  plate_size = 384)


# add raw assay results to database
tbl(con, "raw_assay_result")
plate_4_7_split <- add_raw_assay_results(con, results_plates_4_7_split)
tbl(con, "raw_assay_result")

thresholds_4_7_split <- generate_threshold(con, plate_run_identifier = split_plate_run_4_7_id)

# update assay detection results (TRUE or FALSE for a sample and assay type)
# in the database
update_assay_detection(con, thresholds_4_7_split)

# view assay results.
# this table contains the sample IDs run, the assay type, and positive detection
tbl(con, "assay_result")

# see genetic run identification results
tbl(con, "genetic_run_identification")
# this is run type IDs and their associated run
tbl(con, "run_type")

# disconnect!
DBI::dbDisconnect(con)
