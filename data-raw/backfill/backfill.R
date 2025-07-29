library(tidyverse)

data_2023 <- readxl::read_excel("data-raw/backfill/2023_JPE_GT_Summary_grunID 1.xlsx")
parsed_data_2023 <- data_2023 |>
  tidyr::separate(SampleID, sep = "_",
                  into = c("location_and_date", "sample_event_number",
                           "sample_bin_code", "sample_number"),
                  remove = FALSE)

all_run_types <- grunID::get_run_types(con)
all_run_types <- all_run_types |> mutate(run_name_upper = toupper(run_name)) |> select(id, run_name_upper)

run_ids_2023 <- parsed_data_2023 |> left_join(all_run_types, by=c("Pop_Structure_ID" = "run_name_upper")) |>
  select(sample_id = SampleID, run_type_id = id)

data_2023 |> distinct(Pop_Structure_ID)


parsed_data_2023 |>
  group_by(location_and_date, sample_event_number, sample_bin_code) |>
  tally()


# 1. insert samples and all FK's
# for each of the rows in the df above, do a function call with `add_sample`
add_sample(con, "BTC", 10, "2023-01-01", "A", 1, 200, 2)

# 2. modify field spreadsheet to look like current format and upload uisng app
# use the function `process_field_sheet_samples2` and point it to the season field data

# 3. insert into the run id table,
# convert run id in spreadsheet to the id's we use in the database
# do database insert using DBI package
run_ids_2023_to_insert <- run_ids_2023 |>
  filter(sample_id %in% c("BTC23_10_A_2",
                          "BTC23_10_A_1"))

insert_statement <- glue::glue_sql("INSERT INTO genetic_run_identification(sample_id, run_type_id)
                                   VALUES ({run_ids_2023_to_insert$sample_id}, {run_ids_2023_to_insert$run_type_id})",
                                   .con = con)

for(i in 1:length(insert_statement)) {
  res <- DBI::dbSendQuery(con, insert_statement[i])
  DBI::dbClearResult(res)
}






