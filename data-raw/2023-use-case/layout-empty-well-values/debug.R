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
