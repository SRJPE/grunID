# draft workflow to prepare for field season

# libraries
library(tidyverse)
library(readxl)
library(DBI)

# establish connection
cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = cfg$port,
                      user = cfg$username,
                      password = cfg$password)

# read in sample plan created for the season. this contains
# contains location codes, sample events, sample bins,
# min/max fork lengths, sample dates, and sample IDs.
# needs to be in tidy format
# TODO this needs to be made by users at the beginning of the season
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
# adds samples with ids to SAMPLE
# updates sample status for each to "created" (1)
# returns the number of IDs created and the unique sample IDs created
feather_61_total <- add_sample_plan(con, feather_61_sample_plan, verbose = TRUE)
feather_17_total <- add_sample_plan(con, feather_17_sample_plan, verbose = TRUE)

# open workbook to create field sheets
wb <- openxlsx::createWorkbook()

# use get_field_sheet_event_plan() to create a data frame containing content for the
# field sheets for sampling events
# get_field_sheet_event_plan() retrieves sampling event information from the database
# that is needed to prepare field sheets. Looks up based on sampling event ID
plan <- get_field_sheet_event_plan(con, sample_event_id = 2)

# append a field sheet to the workbook for that sample event
# create field sheet for sampling crews to use.
# Takes in get_field_sheet_sample_plan$field_sheet_sample_plan
# leaves columns "Date", "Time", "FL(mm)", "Field run ID", "Fin clip (Y/N)", and "Comments"
# blank intentionally so that they can be filled out in the field
wb <- create_field_sheet(wb = wb,
                         field_sheet_sample_plan = plan$field_sheet_sample_plan,
                         sample_event_number = plan$sample_event_number,
                         first_sample_date = plan$first_sample_date,
                         sample_location = plan$location_name,
                         sample_location_code = plan$location_code)

openxlsx::saveWorkbook(wb, paste0(field_sheet_filepath), overwrite = TRUE)



# step 3: send field sheets out to monitoring crews to gather samples

# step 4:
# read in filled-out field sheets and turn them into a tidy format with
# sample_id, datetime_collected, fork_length_mm, field_run_type_id, fin_clip, field_comment
filepath <- "data-raw/test_LS.xlsx"
field_data_clean <- process_field_sheet_samples(filepath)

# step 5:
# update database with biologial information from field sheets:
# this adds datetime collected, fork_length_mm, field_run_type_id, fin_clip, and field_comment
# by matching on sample_id.
update_field_sheet_samples(field_data_clean)

# now the biological samples are stored in the database with the
# appropriate sample IDs. Now onto assays :)
