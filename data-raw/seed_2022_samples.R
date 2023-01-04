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
sample_ids_2022 <- tibble(sample_ids = sample_ids_2022)
seed_components <- sample_ids_2022 %>%
  tidyr::separate(sample_ids, sep = "_",
                  into = c("location_and_date", "sample_event_number",
                           "sample_bin_code", "sample_number")) |>
  mutate(location_code = substr(location_and_date, 1, 3),
         first_sample_year = substr(location_and_date, 4, 5),
         first_sample_year = ifelse(first_sample_year == "", NA_character_, paste0("20", first_sample_year))) |>
        select(-location_and_date) |>
  glimpse()

write_csv(seed_components, here::here("data-raw", "2022-use-case", "2022_seed_components.csv"))



# now insert into database ------------------------------------------------

seed_components <- read_csv(here::here("data-raw", "2022-use-case", "2022_seed_components.csv"))

# seed into Sample table on database
# this inserts the samples without checking if they already exist in the database
query <- glue::glue_sql("INSERT INTO sample(id, sample_bin_id)
                          VALUES ({seed_samples$sample_number}, {seed_samples$sample_bin_code});",
                        .con = con)
for(i in 1:length(query)) {
  res <- DBI::dbSendQuery(con, query[i])
  DBI::dbClearResult(res)
}


# notes from pair w emanuel 1-4-2023
# sample ids already contain information about bin, sample event id, year
# decompose those into separate columns in the seed csv
# and then update those the query
# open pull request

