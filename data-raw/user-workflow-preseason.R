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

con <- gr_db_connect()


# 2026 - season creation --------------------------------------------------

sampling_dates_2026 <- readxl::read_xlsx("data-raw/2026-season/sampling-dates-2026.xlsx") |>
  mutate(first_sample_date = as_date(Start),
         sample_event_number = Event) |>
  select(sample_event_number, first_sample_date)


# if starting with a raw sample plan similar to "data-raw/2024_raw_sample_plan.xlsx"
# use process raw sample plan function:
sample_plan_2026 <- process_raw_sample_plan("data-raw/2026-season/2026_JPE_Sample_Sizes.xlsx", 2026, 14)
sample_plan_2026_with_dates <- sample_plan_2026 |>
  select(-first_sample_date) |>
  left_join(sampling_dates_2026, by = "sample_event_number") |>
  mutate(sample_event_number = as.integer(sample_event_number)) |>
  relocate(first_sample_date, .before = sample_bin_code)
sample_ids_2026 <- add_sample_plan(con, sample_plan_2026_with_dates, verbose = TRUE)

# create workbook containing multiple field sheets
create_season_field_sheets(con, 2026, "data-raw/2026-season/field-sheets-v3.xlsx")


# 2025 - season creation -------------
sampling_dates_2025 <- readxl::read_xlsx("data-raw/2025-season/sampling-dates.xlsx") |>
  mutate(first_sample_date = as_date(Start),
         sample_event_number = Event) |>
  select(sample_event_number, first_sample_date) |>
  add_row(sample_event_number = 15, first_sample_date = as_date("2025-01-01"))


# if starting with a raw sample plan similar to "data-raw/2024_raw_sample_plan.xlsx"
# use process raw sample plan function:
sample_plan_2025 <- process_raw_sample_plan("data-raw/2025-season/2025_JPE_Sample_Sizes_100224-with-x-bins.xlsx",
                                            2025, 15)
sample_plan_2025_with_dates <- sample_plan_2025 |>
  select(-first_sample_date) |>
  left_join(sampling_dates_2025, by = "sample_event_number") |>
  mutate(sample_event_number = as.integer(sample_event_number)) |>
  relocate(first_sample_date, .before = sample_bin_code)
sample_ids_2025 <- add_sample_plan(con, sample_plan_2025_with_dates, verbose = TRUE)

# create workbook containing multiple field sheets
create_season_field_sheets(con, 2025, "data-raw/2025-season/field-sheets-v1.xlsx")

# step 2: add sample plan and generate field sheets -------------------------------

# read in sampling dates
sampling_dates_2024 <- readxl::read_xlsx("data-raw/2024-use-case/2024_sampling_dates.xlsx") |>
  mutate(sample_event_number = as.numeric(substr(`...1`, 7, 8))) |>
  mutate(first_sample_date = ymd(`Date Start`)) |>
  select(first_sample_date, sample_event_number) |>
  glimpse()

# if starting with a raw sample plan similar to "data-raw/2024_raw_sample_plan.xlsx"
# use process raw sample plan function:
sample_plan_2024 <- process_raw_sample_plan("data-raw/2024-use-case/2024_raw_sample_plan.xlsx", 2024)
sample_plan_2024_with_dates <- sample_plan_2024 |>
  select(-first_sample_date) |>
  left_join(sampling_dates_2024, by = "sample_event_number") |>
  mutate(sample_event_number = as.integer(sample_event_number)) |>
  relocate(first_sample_date, .before = sample_bin_code)
sample_ids_2024 <- add_sample_plan(con, sample_plan_2024_with_dates, verbose = TRUE)


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
create_season_field_sheets(con, 2024, "data-raw/2024-use-case/2024_field_sheets_test2.xlsx",
                           n_extra_bins = 5, bin_code_extra_bins = "X")

# step 3: send field sheets out to monitoring crews to gather samples

# step 4:
# read in filled-out field sheets and turn them into a tidy format with
# sample_id, datetime_collected, fork_length_mm, field_run_type_id, fin_clip, field_comment
filepath <- "data-raw/2024-use-case/2024_field_sheets_test_filled.xlsx"
field_data_clean <- process_field_sheet_samples(filepath)

# step 5:
# update database with biologial information from field sheets:
# this adds datetime collected, fork_length_mm, field_run_type_id, fin_clip, and field_comment
# by matching on sample_id.
update_field_sheet_samples(con, field_data_clean)

# now the biological samples are stored in the database with the
# appropriate sample IDs. Now onto assays :)


# con <- gr_db_connect()

additional_samples_2024 <- expand_grid(
  location_code = "BTC",
  sample_event_number = 2,
  first_sample_date = lubridate::as_date("2024-01-01"),
  sample_bin_code = LETTERS[1:5],
  min_fork_length = 1,
  max_fork_length = 200,
  expected_number_of_samples = 10
) |>
  mutate(sample_event_number = as.integer(sample_event_number),
         min_fork_length = as.integer(min_fork_length),
         max_fork_length = as.integer(max_fork_length))


add_sample_plan(con , additional_samples_2024, verbose = TRUE)

add_sample(con, location_code = "BTC", sample_event_number = 2, first_sample_date = lubridate::as_date("2024-01-01"),
           sample_bin_code = "A", min_fork_length = 1, max_fork_length = 200, expected_number_of_samples = 10)
