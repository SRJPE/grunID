# load libraries
library(DBI)
library(grunID)
library(dplyr)
library(tidyverse)
library(readxl)

# connect to runID database
con <- gr_db_connect() # this searches for a config file, starting at the working directory.

# check connection is working - should see head of these tables
dbListTables(con)
tbl(con, "agency")
tbl(con, "status_code")

# assume you have added your sample plan that initialized all sample IDs you will
# be processing - see "data-raw/user-workflow-preseason.R"

# prepare to run plates

# get metadata for plate run.
# first, check all protocols to select the correct one:
all_protocols <- get_protocols(con)
all_protocols |> View()

# review available protocols and select appropriate protocol id
# protocol_id is a variable you will need to pass to a later function
protocol_id <- all_protocols |>
  filter(id == 1) |>
  pull(id)

# select laboratory ID that corresponds to "DWR"
# laboratory_id is a variable you will need to pass to a later function
laboratory_id <- get_laboratories(con) |>
  filter(stringr::str_detect(code, "DWR")) |> pull(id)

# get genetic method id that corresponds to "SHERLOCK"
# genetic_method_id is a variable you will need to pass to a later function
genetic_method_id <- get_genetic_methods(con) |>
  filter(method_name == "SHERLOCK") |>
  pull(id)

# add plate run to the database for each assay you plan to run.
# be clear with names here. this returns an ID for the plate run
# that will determine where your results are stored.

# this is if your plate has two assays
# plate_run_id is a variable you will need to pass to a later function
plate_run_id_event <- add_plate_run(con,
                              date_run = "2022-01-01",
                              protocol_id = protocol_id,
                              genetic_method_id = genetic_method_id,
                              laboratory_id = laboratory_id,
                              lab_work_performed_by = "user",
                              description = "early and late assays for plates 4-7")
# query table in database to see what you've added
tbl(con, "plate_run")


# this function reads in an excel file that contains both sherlock output and the
# plate map associated with that sherlock run. The plate map tab needs to be titled
# "plate_map". See templates/ for an example file.
# the output of this function needs to be stored so it can be passed to add_raw_assay_results()
sherlock_results_event <- process_sherlock(filepath = "data-raw/sherlock-example-outputs/JPE_Chnk_Early+Late_Plates4-7_results.xlsx",
                                           sample_type = "mucus",
                                           layout_type = "split_plate_early_late",
                                           plate_run_id = plate_run_id_event,
                                           plate_size = 384)


# add raw assay results to database
tbl(con, "raw_assay_result")
add_raw_assay_results(con, sherlock_results_event)
tbl(con, "raw_assay_result")

# generate thresholds from raw assay results
# thresholds is a variable you will need to pass to a later function
thresholds_event <- generate_threshold(con, plate_run_identifier = plate_run_id_event)

# update assay detection results (TRUE or FALSE for a sample and assay type)
# in the database
update_assay_detection(con, thresholds_event)

# view assay results.
# this table contains the sample IDs run, the assay type, and positive detection
tbl(con, "assay_result")

# see genetic run identification results
tbl(con, "genetic_run_identification")
# this is run type IDs and their associated run
tbl(con, "run_type")

# disconnect!
DBI::dbDisconnect(con)

