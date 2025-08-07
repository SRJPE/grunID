library(tidyverse)

data_2023 <- readxl::read_excel("data-raw/backfill/2023_JPE_GT_Summary_grunID 1.xlsx")
parsed_data_2023 <- data_2023 |>
  tidyr::separate(SampleID, sep = "_",
                  into = c("location_and_date", "sample_event_number",
                           "sample_bin_code", "sample_number"),
                  remove = FALSE) |>
  mutate(location_code = substr(location_and_date, 1, 3),
         date = substr(location_and_date, 4, 5)) |>
  relocate(location_code, .after = location_and_date) |>
  relocate(date, .after = location_code) |>
  select(-location_and_date)

all_run_types <- grunID::get_run_types(con)
all_run_types <- all_run_types |>
  mutate(run_name_upper = toupper(run_name)) |>
  select(id, run_name_upper)

run_ids_2023 <- parsed_data_2023 |>
  left_join(all_run_types,
            by=c("Pop_Structure_ID" = "run_name_upper")) |>
  select(sample_id = SampleID, run_type_id = id)

data_2023 |>
  distinct(Pop_Structure_ID)

max(as.numeric(parsed_data_2023$sample_number), na.rm = T)
to_upload <- parsed_data_2023 |>
  filter(date == "23") |> # this is just a check
  group_by(location_code, date, sample_event_number, sample_bin_code) |>
  summarise(n = max(as.numeric(sample_number))) |>
  ungroup() |>
  filter(!is.na(n)) |> # TODO sample event number coded as "A"
  mutate(first_sample_date = as.Date(paste0("20", date, "-01-01")),
         min_fork_length = 1,
         max_fork_length = 200) |>
  tail(-1)


# 1. insert samples and all FK's
# for each of the rows in the df above, do a function call with `add_sample`
purrr::pmap(list(location_code = to_upload$location_code,
                 sample_event_number = to_upload$sample_event_number,
                 sample_bin_code = to_upload$sample_bin_code,
                 first_sample_date = to_upload$first_sample_date,
                 min_fork_length = to_upload$min_fork_length,
                 max_fork_length = to_upload$max_fork_length,
                 expected_number_of_samples = to_upload$n),
            grunID::add_sample,
            con = con)

# add_sample(con, "BTC", 10, "2023-01-01", "A", 1, 200, 2)

# 2. modify field spreadsheet to look like current format and upload uisng app
# use the function `process_field_sheet_samples2` and point it to the season field data

field_season_data_backfill <- process_field_sheet_samples2(filepath = "data-raw/backfill/2023_JPE_Sample_Data2_clean.xlsx")

data_with_all_run_types <- parsed_data_2023 |>
  left_join(field_season_data_backfill |>
              select(sample_id, field_run_type_id),
            by = c("SampleID" = "sample_id"))

# which ones don't align with a sample id?
parsed_data_2023$SampleID[!parsed_data_2023$SampleID %in% field_season_data_backfill$sample_id]

# 3. insert into the run id table,
# convert run id in spreadsheet to the id's we use in the database
# do database insert using DBI package
run_ids_2023_to_insert <- run_ids_2023 |>
  filter(!sample_id %in% c("BTC23_10_A_2",
                           "BTC23_10_A_1",
                           "MIL23_11_E_A"))

insert_statement <- glue::glue_sql("INSERT INTO genetic_run_identification(sample_id, run_type_id)
                                   VALUES ({run_ids_2023_to_insert$sample_id}, {run_ids_2023_to_insert$run_type_id})",
                                   .con = con)

for(i in 1:length(insert_statement)) {
  print(paste0(i, " of ", length(insert_statement)))
  res <- DBI::dbSendQuery(con, insert_statement[i])
  DBI::dbClearResult(res)
}






