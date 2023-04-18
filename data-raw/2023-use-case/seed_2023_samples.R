library(readr)
library(DBI)
library(dplyr)
library(grunID)

# set up connection
# connect to runID database
con <- gr_db_connect()


# read in 2023 Sample numbers to seed the "sample" table with
sample_ids_2023 <- read_csv(here::here("data-raw", "2023-use-case", "2023_JPE_Sample_IDs_04-10-2023_v2.csv"))$`Sample ID`
sample_ids_2023 <- tibble(sample_ids = sample_ids_2023)
seed_components_raw <- sample_ids_2023 |>
  tidyr::separate(sample_ids, sep = "_",
                  into = c("location_and_date", "sample_event_number",
                           "sample_bin_code", "sample_number"),
                  remove = FALSE) |>
  mutate(location_code = substr(location_and_date, 1, 3),
         first_sample_year = substr(location_and_date, 4, 5),
         first_sample_date = ifelse(first_sample_year == "", NA_character_, paste0("20", first_sample_year, "-01-01"))) |>
  filter(!is.na(first_sample_date)) |>
  select(-location_and_date) |>
  glimpse()

# get sample location ids
sample_location_ids <- dplyr::tbl(con, "sample_location") |>
  select(location_code = code, location_id = id) |>
  collect() |>
  glimpse()

# combine
seed_components <- seed_components_raw |>
  mutate(sample_event_number = as.numeric(sample_event_number)) |>
  left_join(sample_location_ids, by = "location_code") |>
  mutate(permit_id = 2,
         min_fork_length = 1,
         max_fork_length = 200) |> # TODO placeholders
  glimpse()

# TODO add permit information into database

# now insert into database ------------------------------------------------

# seed into Sample table on database

# sample event
query <- glue::glue_sql("INSERT INTO sample_event(sample_event_number, sample_location_id, first_sample_date)
                          OVERRIDING SYSTEM VALUE VALUES ({seed_components$sample_event_number},
                        {seed_components$location_id},
                        {seed_components$first_sample_date});",
                        .con = con)

for(i in 1:length(query)) {
  res <- DBI::dbSendQuery(con, query[i])
  DBI::dbClearResult(res)
}

# get sample event ids
sample_event_ids <- dplyr::tbl(con, "sample_event") |>
  collect() |>
  select(sample_event_number, sample_event_id = id)

seed_components_with_sample_event_ids <- left_join(seed_components, sample_event_ids, by = "sample_event_number") |>
  glimpse()

# sample bin
query <- glue::glue_sql("INSERT INTO sample_bin(sample_event_id, sample_bin_code, min_fork_length, max_fork_length)
                         VALUES ({seed_components_with_sample_event_ids$sample_event_id}, {seed_components_with_sample_event_ids$sample_bin_code}, {seed_components_with_sample_event_ids$min_fork_length}, {seed_components_with_sample_event_ids$max_fork_length});",
                        .con = con)
for(i in 1:length(query)) {
  res <- DBI::dbSendQuery(con, query[i])
  DBI::dbClearResult(res)
}

# get sample bin ids
sample_bin_ids <- dplyr::tbl(con, "sample_bin") |>
  collect() |>
  mutate(sample_bin_code = as.character(sample_bin_code)) |>
  filter(sample_bin_code %in% seed_components_with_sample_event_ids$sample_bin_code) |>
  select(sample_bin_code, sample_bin_id = id) |>
  glimpse()

seed_components_with_sample_bin_ids <- left_join(seed_components_with_sample_event_ids,
                                                 sample_bin_ids, by = "sample_bin_code")

# sample
query <- glue::glue_sql("INSERT INTO sample(id, sample_bin_id)
                          VALUES ({seed_components_with_sample_bin_ids$sample_ids}, {seed_components_with_sample_bin_ids$sample_bin_id});",
                        .con = con)
for(i in 1:length(query)) {
  res <- DBI::dbSendQuery(con, query[i])
  DBI::dbClearResult(res)
}


