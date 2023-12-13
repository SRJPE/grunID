# load libraries
library(DBI)
library(grunID)
library(dplyr)
library(tidyverse)
library(readxl)

# connect to runID database
con <- gr_db_connect() # this searches for a config file, starting at the working directory.

cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
               dbname = cfg$dbname,
               host = cfg$host,
               port = 5432,
               user = cfg$username,
               password = cfg$password)




# check connection is working - should see head of these tables
dbListTables(con)
dplyr::tbl(con, "agency")
dplyr::tbl(con, "status_code")

# assume you have added your sample plan that initialized all sample IDs you will
# be processing - see "data-raw/user-workflow-preseason.R"

# prepare to run plates

# get metadata for plate run.
# first, check all protocols to select the correct one:
all_protocols <- get_protocols(con)

# review available protocols and select appropriate protocol id
# protocol_id is a variable you will need to pass to a later function
protocol_id <- all_protocols |>
  filter(id == 1) |>
  pull(id)

# select laboratory ID that corresponds to "DWR"
# laboratory_id is a variable you will need to pass to a later function
laboratory_id <- get_laboratories(con) |>
  dplyr::filter(stringr::str_detect(code, "DWR")) |> dplyr::pull(id)

# get genetic method id that corresponds to "SHERLOCK"
# genetic_method_id is a variable you will need to pass to a later function
genetic_method_id <- get_genetic_methods(con) |>
  dplyr::filter(method_name == "SHERLOCK") |>
  dplyr::pull(id)

# add plate run to the database for each assay you plan to run.
# be clear with names here. this returns an ID for the plate run
# that will determine where your results are stored.

# this is if your plate has two assays
# plate_run_id is a variable you will need to pass to a later function
plate_run_event <- add_plate_run(con,
                                 date_run = "2023-07-10",
                                 protocol_id = protocol_id,
                                 genetic_method_id = genetic_method_id,
                                 laboratory_id = laboratory_id,
                                 lab_work_performed_by = "user",
                                 description = "error testing on 7/10")

# query table in database to see what you've added
dplyr::tbl(con, "plate_run")


# read in the plate run map that is created before the plate is run.
# this plate map layout should contain generic sherlock-created sample IDs
# which will then be aligned with the corresponding JPE sample IDs.

# get_sample_details process the layout. User must specify layout type and
# sample_type. See `?process_well_sample_details()` for acceptable arguments.

# this is for the split plate version
# plate_map_details is a variable you will need to pass to a later function

# pass plate map details (well layout) to process_sherlock, which reads in
# a file with the output of a sherlock machine. This function also maps
# the generic IDs with the JPE sample IDs.
# sherlock_results is a variable you will need to pass to a later function
sherlock_results_event <- process_sherlock(
  filepath = "data-raw/sherlock-example-outputs/2024/061223_JPE24_E+L_E1-2_P1_SH_RH.xlsx",
  sample_type = "mucus",
  layout_type = "split_plate_early_late",
  plate_run_id = plate_run_event,
  plate_size = 384)

# add raw assay results to database
dplyr::tbl(con, "raw_assay_result")
add_raw_assay_results(con, sherlock_results_event)
dplyr::tbl(con, "raw_assay_result")

# generate thresholds from raw assay results
# thresholds is a variable you will need to pass to a later function
thresholds_event <- generate_threshold(con, plate_run = plate_run_event, .control_id = "EBK")

# update assay detection results (TRUE or FALSE for a sample and assay type)
# in the database
update_assay_detection(con, thresholds_event)



# view assay results.
# this table contains the sample IDs run, the assay type, and positive detection
dplyr::tbl(con, "assay_result")

# see genetic run identification results
dplyr::tbl(con, "genetic_run_identification")
# this is run type IDs and their associated run
dplyr::tbl(con, "run_type") |> dplyr::collect() |> print(n=Inf)

# see samples that need further analysis
get_samples_needing_action(con)

# see status of a selected sample ID
get_sample_status(con, "JPE_Sample_ID", full_history = FALSE)

get_samples_by_season(con, 2022, "clean")

# disconnect!
DBI::dbDisconnect(con)

