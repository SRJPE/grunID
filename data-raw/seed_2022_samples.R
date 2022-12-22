library(readr)
library(DBI)
library(dplyr)

# set up connection
cfg <- config::get()

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = cfg$port,
                      user = cfg$username,
                      password = cfg$password)


# read in 2022 Sample numbers to seed the "sample" table with
sample_ids_2022 <- read_csv(here::here("data-raw", "2022-use-case", "JPE_2022_Sample_ID_List.txt"))$`Sample ID`

# get associated location, sample_bin_code, year, and sample_number
# load sample locations
sample_locations <- dplyr::tbl(con, "sample_location") |>
  collect()
# load sample events
sample_events <- dplyr::tbl(con, "sample_event") |>
  collect()

sample_information <- dplyr::tbl(con, "sample_bin") |>
  collect() |>
  rename(sample_bin_id = id) |>
  left_join(sample_events |>
              rename(sample_event_id = id),
            by = "sample_event_id") |>
  left_join(sample_locations |>
              rename(sample_location_id = id,
                     location_code = code),
            by = "sample_location_id") |>
  mutate(year = year(first_sample_date)) |>
  select(sample_bin_code,
         location_code, # missing sample number! need this to filter/join w/ 2022 samples
         sample_event_number) |> glimpse()

# join based on sample ids from 2022 seed data
# TODO
seed_samples <- sample_information |>
  filter(sample_number %in% sample_ids_2022) |> glimpse()


# check if samples in the table
sample_table <- tbl(con, "sample") |> collect()
sum(sample_ids_2022 %in% sample_table$id)/length(sample_ids_2022)

# seed into Sample table on database
# this inserts the samples without checking if they already exist in the database
query <- glue::glue_sql("INSERT INTO sample(id, sample_bin_id)
                          VALUES ({seed_samples$sample_number}, {seed_samples$sample_bin_code});",
                        .con = con)
for(i in 1:length(query)) {
  res <- DBI::dbSendQuery(con, query[i])
  DBI::dbClearResult(res)
}


