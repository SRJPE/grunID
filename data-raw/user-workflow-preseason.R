# draft workflow to prepare for field season

# libraries
library(tidyverse)
library(readxl)
library(DBI)
library(grunID)


# step 1: establish connection --------------------------------------------
cfg <- config::get()
# at this point config has the creds
con <- DBI::dbConnect(RPostgres::Postgres(),
               dbname = cfg$dbname,
               host = cfg$host,
               port = 5432,
               user = cfg$username,
               password = cfg$password)

#con <- gr_db_connect()


# step 2: add sample plan and generate field sheets -------------------------------

# if starting with a raw sample plan similar to "data-raw/2024_raw_sample_plan.xlsx"
# use process raw sample plan function:
sample_plan_2024 <- process_raw_sample_plan("data-raw/2024_raw_sample_plan.xlsx", 2024)
sample_ids_2024 <- add_sample_plan(con, sample_plan_2024, verbose = TRUE)

# if already in tidy format, read in and then add sample plan
# this example code is for only one location
sample_plan_2022_final <- read_csv("data-raw/2022_sample_plan.csv") |> distinct_all()

# filter sample plan to locations (this isn't necessary but helpful for
# partitioning workflow)
feather_61_sample_plan <- sample_plan_2022_final |>
  filter(location_code == "F61") |>
  mutate(sample_event_number = as.integer(sample_event_number),
         min_fork_length = as.integer(min_fork_length),
         max_fork_length = as.integer(max_fork_length))

# add sample plans to database. this code:
# adds sample events to table SAMPLE_EVENT
# adds sample bins for each event SAMPLE_BIN
# adds samples with ids to SAMPLE
# updates sample status for each to "created" (1)
# returns the number of IDs created and the unique sample IDs created
feather_61_IDs <- add_sample_plan(con, feather_61_sample_plan, verbose = TRUE)

# create workbook containing multiple field sheets
create_multiple_field_sheets(con, 2024, "data-raw/2024_field_sheets_test.xlsx")

# step 3: send field sheets out to monitoring crews to gather samples

# step 4:
# read in filled-out field sheets and turn them into a tidy format with
# sample_id, datetime_collected, fork_length_mm, field_run_type_id, fin_clip, field_comment
filepath <- "data-raw/F61_test.xlsx"
field_data_clean <- process_field_sheet_samples(filepath)

# step 5:
# update database with biologial information from field sheets:
# this adds datetime collected, fork_length_mm, field_run_type_id, fin_clip, and field_comment
# by matching on sample_id.
update_field_sheet_samples(field_data_clean)

# now the biological samples are stored in the database with the
# appropriate sample IDs. Now onto assays :)
