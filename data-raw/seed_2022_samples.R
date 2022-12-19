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

# read in 2022 Sample numbers
sample_ids_2022 <- read_csv(here::here("data-raw", "2022-use-case", "JPE_2022_Sample_ID_List.txt"))$`Sample ID`

# check if samples in the table
sample_table <- tbl(con, "sample") |> collect()
sum(sample_ids_2022 %in% sample_table$id)/length(sample_ids_2022)

# seed into Sample table on database
# this inserts the samples without checking if they already exist in the database
query <- glue::glue_sql("INSERT INTO sample(id)
                          VALUES ({sample_ids_2022});",
                        .con = con)
for(i in 1:length(query)) {
  res <- DBI::dbSendQuery(con, query[i])
  DBI::dbClearResult(res)
}


