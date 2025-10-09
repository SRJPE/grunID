library(tidyverse)


# process the sherlock side of things
sherlock_all_data <- readxl::read_excel("data-raw/backfill/JPE_2022-2024_Genetic_Data_FC_05-2025_v2.xlsx")

con <- grunID::gr_db_connect()
all_run_types <- grunID::get_run_types(con)
all_run_types <- all_run_types |>
  mutate(run_name_upper = toupper(run_name)) |>
  select(id, run_name_upper)

# 2023 -----------------------

gt_seq_data_2023 <- readr::read_tsv("data-raw/backfill/2023/JPE_2023_Reanalysis_10-2025_summary.tsv")
sherlock_2023_samples_ids <- sherlock_all_data |> filter(Year == 2023) |> pull(SampleID)
gtseq_2023_ids <- gt_seq_data_2023 |> pull(SampleID)

dplyr::intersect(sherlock_2023_samples_ids, gtseq_2023_ids) |> length()
length(unique(sherlock_2023_samples_ids))
length(unique(gtseq_2023_ids))

# ok so it looks like gtseq and the sherlock stuff pretty much 100  percent overlap
dplyr::setdiff(sherlock_2023_samples_ids, gtseq_2023_ids)
# i think the plan should be lets tackle the overlaps since they have all the data, then
# we can tackle the ids that exist in each of the two datesets only

# TODO: what to do with samples in gtseq and not in sherlock --- XTRA clr24_1_1a MILL_MAIN_EVENT
gtseq_results_2023  <- readr::read_tsv("data-raw/backfill/2023/JPE_2023_Reanalysis_10-2025_summary.tsv")
sherlock_results_2023 <- sherlock_all_data |> filter(Year == 2023) |> select(-`Gtseq Chr28 geno`)

samples_in_both_2023 <- dplyr::intersect(gtseq_results_2023$SampleID, sherlock_results_2023$SampleID)
samples_only_in_gtseq_2023 <- dplyr::setdiff(gtseq_results_2023$SampleID, sherlock_results_2023$SampleID)
samples_only_in_sherlock_2023 <- dplyr::setdiff(sherlock_results_2023$SampleID, gtseq_results_2023$SampleID)


## insert samples that are in both gtseq and sherlock ----------------
full_data_2023 <- sherlock_results_2023 |>
  left_join(
    gtseq_results_2023,
    by=c("SampleID" = "SampleID"),
    suffix = c("_sherlock", "_gtseq"))


full_data_2023 |> glimpse()
parsed_data_2023 <- full_data_2023 |>
  tidyr::separate(SampleID, sep = "_",
                  into = c("location_and_date", "sample_event_number",
                           "sample_bin_code", "sample_number"),
                  remove = FALSE) |>
  mutate(location_code = substr(location_and_date, 1, 3),
         date = substr(location_and_date, 4, 5)) |>
  relocate(location_code, .after = location_and_date) |>
  relocate(date, .after = location_code) |>
  select(-location_and_date)

data_2023_final_designation <- parsed_data_2023 |> mutate(
  final_run_designation = case_when(
    !is.na(Pop_Structure_ID) ~ Pop_Structure_ID,
    !is.na(`SHLCK Run Designation`) ~ `SHLCK Run Designation`,
    TRUE ~ "UNKNOWN"
  ),
  final_run_designation = case_when(
    final_run_designation == "FALL OR LATE FALL" ~ "FALL/LATEFALL",
    final_run_designation == "SPRING OR WINTER" ~ "SPRING/WINTER",
    TRUE ~ final_run_designation
  )
)

run_ids_2023 <- data_2023_final_designation |>
  left_join(all_run_types,
            by=c("final_run_designation" = "run_name_upper")) |>
  select(sample_id = SampleID, run_type_id = id)


run_ids_2023 |> filter(is.na(run_type_id))

to_upload <- data_2023_final_designation |>
  filter(date == "23") |> # this is just a check
  group_by(location_code, date, sample_event_number, sample_bin_code) |>
  summarise(n = max(as.numeric(sample_number))) |>
  ungroup() |>
  filter(!is.na(n)) |> # TODO sample event number coded as "A"
  mutate(first_sample_date = as.Date(paste0("20", date, "-01-01")),
         min_fork_length = 1,
         max_fork_length = 200)

# add samples to database
purrr::pmap(list(location_code = to_upload$location_code,
                 sample_event_number = to_upload$sample_event_number,
                 sample_bin_code = to_upload$sample_bin_code,
                 first_sample_date = to_upload$first_sample_date,
                 min_fork_length = to_upload$min_fork_length,
                 max_fork_length = to_upload$max_fork_length,
                 expected_number_of_samples = to_upload$n),
            grunID::add_sample,
            con = con)


insert_statement <- glue::glue_sql("INSERT INTO genetic_run_identification(sample_id, run_type_id)
                                   VALUES ({run_ids_2023$sample_id}, {run_ids_2023$run_type_id})",
                                   .con = con)

walk(1:length(insert_statement), function(i) {
  print(paste0(i, " of ", length(insert_statement)))
  res <- DBI::dbSendQuery(con, insert_statement[i])
  DBI::dbClearResult(res)
})

# 2022 ----------------------------------

gt_seq_data_2022 <- readr::read_tsv("data-raw/backfill/2022/JPE_2022_Reanalysis_10-2025_summary.tsv")
sherlock_2022_samples_ids <- sherlock_all_data |> filter(Year == 2022) |> pull(SampleID)
gtseq_2022_ids <- gt_seq_data_2022 |> pull(SampleID)

dplyr::intersect(sherlock_2022_samples_ids, gtseq_2022_ids) |> length()
length(unique(sherlock_2022_samples_ids))
length(unique(gtseq_2022_ids))

dplyr::setdiff(sherlock_2022_samples_ids, gtseq_2022_ids)

# TODO: what to do with samples in gtseq and not in sherlock --- XTRA clr24_1_1a MILL_MAIN_EVENT
gtseq_results_2022  <- readr::read_tsv("data-raw/backfill/2022/JPE_2022_Reanalysis_10-2025_summary.tsv")
sherlock_results_2022 <- sherlock_all_data |> filter(Year == 2022) |> select(-`Gtseq Chr28 geno`)

samples_in_both_2022 <- dplyr::intersect(gtseq_results_2022$SampleID, sherlock_results_2022$SampleID)
samples_only_in_gtseq_2022 <- dplyr::setdiff(gtseq_results_2022$SampleID, sherlock_results_2022$SampleID)
samples_only_in_sherlock_2022 <- dplyr::setdiff(sherlock_results_2022$SampleID, gtseq_results_2022$SampleID)

full_data_2022 <- sherlock_results_2022 |>
  left_join(
    gtseq_results_2022,
    by=c("SampleID" = "SampleID"),
    suffix = c("_sherlock", "_gtseq"))


full_data_2022 |> glimpse()
parsed_data_2022 <- full_data_2022 |>
  # correct weird Tisdale coded as TISS
  mutate(Site = ifelse(Site == "TISS", "TIS", Site),
         SampleID = ifelse(SampleID == "TISS22_9_D_5", "TIS22_9_D_5", SampleID)) |>
  tidyr::separate(SampleID, sep = "_",
                  into = c("location_and_date", "sample_event_number",
                           "sample_bin_code", "sample_number"),
                  remove = FALSE) |>
  mutate(location_code = substr(location_and_date, 1, 3),
         date = substr(location_and_date, 4, 5)) |>
  relocate(location_code, .after = location_and_date) |>
  relocate(date, .after = location_code) |>
  select(-location_and_date)

data_2022_final_designation <- parsed_data_2022 |> mutate(
  final_run_designation = case_when(
    !is.na(Pop_Structure_ID) ~ Pop_Structure_ID,
    !is.na(`SHLCK Run Designation`) ~ `SHLCK Run Designation`,
    TRUE ~ "UNKNOWN"
  ),
  final_run_designation = case_when(
    final_run_designation == "FALL OR LATE FALL" ~ "FALL/LATEFALL",
    final_run_designation == "SPRING OR WINTER" ~ "SPRING/WINTER",
    TRUE ~ final_run_designation
  )
)

run_ids_2022_raw <- data_2022_final_designation |>
  left_join(all_run_types,
            by=c("final_run_designation" = "run_name_upper")) |>
  select(sample_id = SampleID, run_type_id = id)


to_check_run_ids <- run_ids_2022_raw |> filter(is.na(run_type_id))
run_ids_2022 <- run_ids_2022_raw |>
  filter(!is.na(run_type_id))

to_upload_2022 <- data_2022_final_designation |>
  filter(date == "22", # just a check
         !SampleID %in% to_check_run_ids$sample_id) |> # these didn't match with a run_id in db
  group_by(location_code, date, sample_event_number, sample_bin_code) |>
  summarise(n = max(as.numeric(sample_number))) |>
  ungroup() |>
  filter(!is.na(n)) |> # TODO sample event number coded as "A"
  mutate(first_sample_date = as.Date(paste0("20", date, "-01-01")),
         min_fork_length = 1,
         max_fork_length = 200)

# add samples to database
purrr::pmap(list(location_code = to_upload_2022$location_code,
                 sample_event_number = to_upload_2022$sample_event_number,
                 sample_bin_code = to_upload_2022$sample_bin_code,
                 first_sample_date = to_upload_2022$first_sample_date,
                 min_fork_length = to_upload_2022$min_fork_length,
                 max_fork_length = to_upload_2022$max_fork_length,
                 expected_number_of_samples = to_upload_2022$n),
            grunID::add_sample,
            con = con)


insert_statement <- glue::glue_sql("INSERT INTO genetic_run_identification(sample_id, run_type_id)
                                   VALUES ({run_ids_2022$sample_id}, {run_ids_2022$run_type_id})",
                                   .con = con)

walk(1:length(insert_statement), function(i) {
  print(paste0(i, " of ", length(insert_statement)))
  res <- DBI::dbSendQuery(con, insert_statement[i])
  DBI::dbClearResult(res)
})



# 2024 ----------------------------------

gt_seq_data_2024 <- readr::read_tsv("data-raw/backfill/2024/JPE_2024_Reanalysis_10-2025_summary.tsv")
sherlock_2024_samples_ids <- sherlock_all_data |> filter(Year == 2024) |> pull(SampleID)
gtseq_2024_ids <- gt_seq_data_2024 |> pull(SampleID)

dplyr::intersect(sherlock_2024_samples_ids, gtseq_2024_ids) |> length()
length(unique(sherlock_2024_samples_ids))
length(unique(gtseq_2024_ids))

dplyr::setdiff(sherlock_2024_samples_ids, gtseq_2024_ids)

# TODO: what to do with samples in gtseq and not in sherlock --- XTRA clr24_1_1a MILL_MAIN_EVENT
gtseq_results_2024  <- readr::read_tsv("data-raw/backfill/2024/JPE_2024_Reanalysis_10-2025_summary.tsv")
sherlock_results_2024 <- sherlock_all_data |> filter(Year == 2024) |> select(-`Gtseq Chr28 geno`)

samples_in_both_2024 <- dplyr::intersect(gtseq_results_2024$SampleID, sherlock_results_2024$SampleID)
samples_only_in_gtseq_2024 <- dplyr::setdiff(gtseq_results_2024$SampleID, sherlock_results_2024$SampleID)
samples_only_in_sherlock_2024 <- dplyr::setdiff(sherlock_results_2024$SampleID, gtseq_results_2024$SampleID)

full_data_2024 <- sherlock_results_2024 |>
  left_join(
    gtseq_results_2024,
    by=c("SampleID" = "SampleID"),
    suffix = c("_sherlock", "_gtseq"))


full_data_2024 |> glimpse()
parsed_data_2024 <- full_data_2024 |>
  tidyr::separate(SampleID, sep = "_",
                  into = c("location_and_date", "sample_event_number",
                           "sample_bin_code", "sample_number"),
                  remove = FALSE) |>
  mutate(location_code = substr(location_and_date, 1, 3),
         date = substr(location_and_date, 4, 5)) |>
  relocate(location_code, .after = location_and_date) |>
  relocate(date, .after = location_code) |>
  select(-location_and_date)

data_2024_final_designation <- parsed_data_2024 |> mutate(
  final_run_designation = case_when(
    !is.na(Pop_Structure_ID) ~ Pop_Structure_ID,
    !is.na(`SHLCK Run Designation`) ~ `SHLCK Run Designation`,
    TRUE ~ "UNKNOWN"
  ),
  final_run_designation = case_when(
    final_run_designation == "FALL OR LATE FALL" ~ "FALL/LATEFALL",
    final_run_designation == "SPRING OR WINTER" ~ "SPRING/WINTER",
    TRUE ~ final_run_designation
  )
)

run_ids_2024_raw <- data_2024_final_designation |>
  left_join(all_run_types,
            by=c("final_run_designation" = "run_name_upper")) |>
  select(sample_id = SampleID, run_type_id = id)


to_check_run_ids <- run_ids_2024_raw |> filter(is.na(run_type_id))
run_ids_2024 <- run_ids_2024_raw |>
  filter(!is.na(run_type_id))

to_upload_2024 <- data_2024_final_designation |>
  filter(date == "24", # just a check
         !SampleID %in% to_check_run_ids$sample_id) |> # these didn't match with a run_id in db
  group_by(location_code, date, sample_event_number, sample_bin_code) |>
  summarise(n = max(as.numeric(sample_number))) |>
  ungroup() |>
  filter(!is.na(n)) |> # TODO sample event number coded as "A"
  mutate(first_sample_date = as.Date(paste0("20", date, "-01-01")),
         min_fork_length = 1,
         max_fork_length = 200)

# add samples to database
purrr::pmap(list(location_code = to_upload_2024$location_code,
                 sample_event_number = to_upload_2024$sample_event_number,
                 sample_bin_code = to_upload_2024$sample_bin_code,
                 first_sample_date = to_upload_2024$first_sample_date,
                 min_fork_length = to_upload_2024$min_fork_length,
                 max_fork_length = to_upload_2024$max_fork_length,
                 expected_number_of_samples = to_upload_2024$n),
            grunID::add_sample,
            con = con)


insert_statement <- glue::glue_sql("INSERT INTO genetic_run_identification(sample_id, run_type_id)
                                   VALUES ({run_ids_2024$sample_id}, {run_ids_2024$run_type_id})",
                                   .con = con)

walk(1:length(insert_statement), function(i) {
  print(paste0(i, " of ", length(insert_statement)))
  res <- DBI::dbSendQuery(con, insert_statement[i])
  DBI::dbClearResult(res)
})


# identify erroneous sample ids -------------------------------------------










# old ---------------------------------------------------------------------
#
# data_2023 <- readxl::read_excel("data-raw/backfill/2023_JPE_GT_Summary_grunID 1.xlsx")
# parsed_data_2023 <- data_2023 |>
#   tidyr::separate(SampleID, sep = "_",
#                   into = c("location_and_date", "sample_event_number",
#                            "sample_bin_code", "sample_number"),
#                   remove = FALSE) |>
#   mutate(location_code = substr(location_and_date, 1, 3),
#          date = substr(location_and_date, 4, 5)) |>
#   relocate(location_code, .after = location_and_date) |>
#   relocate(date, .after = location_code) |>
#   select(-location_and_date)
#
# all_run_types <- grunID::get_run_types(con)
# all_run_types <- all_run_types |>
#   mutate(run_name_upper = toupper(run_name)) |>
#   select(id, run_name_upper)
#
# run_ids_2023 <- parsed_data_2023 |>
#   left_join(all_run_types,
#             by=c("Pop_Structure_ID" = "run_name_upper")) |>
#   select(sample_id = SampleID, run_type_id = id)
#
# data_2023 |>
#   distinct(Pop_Structure_ID)
#
# max(as.numeric(parsed_data_2023$sample_number), na.rm = T)
# to_upload <- parsed_data_2023 |>
#   filter(date == "23") |> # this is just a check
#   group_by(location_code, date, sample_event_number, sample_bin_code) |>
#   summarise(n = max(as.numeric(sample_number))) |>
#   ungroup() |>
#   filter(!is.na(n)) |> # TODO sample event number coded as "A"
#   mutate(first_sample_date = as.Date(paste0("20", date, "-01-01")),
#          min_fork_length = 1,
#          max_fork_length = 200) |>
#   tail(-1)
#
#
# # 1. insert samples and all FK's
# # for each of the rows in the df above, do a function call with `add_sample`
# purrr::pmap(list(location_code = to_upload$location_code,
#                  sample_event_number = to_upload$sample_event_number,
#                  sample_bin_code = to_upload$sample_bin_code,
#                  first_sample_date = to_upload$first_sample_date,
#                  min_fork_length = to_upload$min_fork_length,
#                  max_fork_length = to_upload$max_fork_length,
#                  expected_number_of_samples = to_upload$n),
#             grunID::add_sample,
#             con = con)
#
# # add_sample(con, "BTC", 10, "2023-01-01", "A", 1, 200, 2)
#
# # 2. modify field spreadsheet to look like current format and upload uisng app
# # use the function `process_field_sheet_samples2` and point it to the season field data
#
# field_season_data_backfill <- process_field_sheet_samples2(filepath = "data-raw/backfill/2023/2023_JPE_Sample_Data2_clean.xlsx")
#
# data_with_all_run_types <- parsed_data_2023 |>
#   left_join(field_season_data_backfill |>
#               select(sample_id, field_run_type_id),
#             by = c("SampleID" = "sample_id"))
#
# # which ones don't align with a sample id?
# parsed_data_2023$SampleID[!parsed_data_2023$SampleID %in% field_season_data_backfill$sample_id]
#
# # 3. insert into the run id table,
# # convert run id in spreadsheet to the id's we use in the database
# # do database insert using DBI package
#
# samples_seeded <- tbl(con, "sample") |>
#   collect() |>
#   pull(id)
#
# samples_not_seeded <- run_ids_2023$sample_id[!run_ids_2023$sample_id %in% samples_seeded]
#
# run_ids_2023_to_insert <- run_ids_2023 |>
#   filter(!sample_id %in% samples_not_seeded)
#
# insert_statement <- glue::glue_sql("INSERT INTO genetic_run_identification(sample_id, run_type_id)
#                                    VALUES ({run_ids_2023_to_insert$sample_id}, {run_ids_2023_to_insert$run_type_id})",
#                                    .con = con)
#
# for(i in 1:length(insert_statement)) {
#   print(paste0(i, " of ", length(insert_statement)))
#   res <- DBI::dbSendQuery(con, insert_statement[i])
#   DBI::dbClearResult(res)
# }
#
#
# # 2022 --------------------------------------------------------------------
#
# gtseq_results_2022 <- readxl::read_excel("data-raw/backfill/2022_JPE_GT_summary_grunID.xlsx")
# sherlock_results_2022 <- sherlock_all_data |> filter(lubridate::year(Date) == 2022)
#
# samples_in_both <- dplyr::intersect(gtseq_results_2022$SampleID, sherlock_results_2022$SampleID)
# samples_only_in_gtseq <- dplyr::setdiff(gtseq_results_2022$SampleID, sherlock_results_2022$SampleID)
# samples_only_in_sherlock <- dplyr::setdiff(sherlock_results_2022$SampleID, gtseq_results_2022$SampleID)
#
# ## insert samples that are in both gtseq and sherlock ----------------
#
# full_data_2022 <- sherlock_results_2022 |>
#   inner_join(
#     gtseq_results_2022,
#     by=c("SampleID" = "SampleID"),
#     suffix = c("_sherlock", "_gtseq"))
#
# full_data_2022 |> glimpse()
# parsed_data_2022 <- full_data_2022 |>
#   tidyr::separate(SampleID, sep = "_",
#                   into = c("location_and_date", "sample_event_number",
#                            "sample_bin_code", "sample_number"),
#                   remove = FALSE) |>
#   mutate(location_code = substr(location_and_date, 1, 3),
#          date = substr(location_and_date, 4, 5)) |>
#   relocate(location_code, .after = location_and_date) |>
#   relocate(date, .after = location_code) |>
#   select(-location_and_date)
#
# all_run_types <- grunID::get_run_types(con)
# all_run_types <- all_run_types |>
#   mutate(run_name_upper = toupper(run_name)) |>
#   select(id, run_name_upper)
#
# run_ids_2022 <- parsed_data_2022 |>
#   left_join(all_run_types,
#             by=c("Pop_Structure_ID" = "run_name_upper")) |>
#   select(sample_id = SampleID, run_type_id = id)
#
# to_upload <- parsed_data_2022 |>
#   filter(date == "22") |> # this is just a check
#   group_by(location_code, date, sample_event_number, sample_bin_code) |>
#   summarise(n = max(as.numeric(sample_number))) |>
#   ungroup() |>
#   filter(!is.na(n)) |> # TODO sample event number coded as "A"
#   mutate(first_sample_date = as.Date(paste0("20", date, "-01-01")),
#          min_fork_length = 1,
#          max_fork_length = 200)
#
# # add samples to database
# purrr::pmap(list(location_code = to_upload$location_code,
#                  sample_event_number = to_upload$sample_event_number,
#                  sample_bin_code = to_upload$sample_bin_code,
#                  first_sample_date = to_upload$first_sample_date,
#                  min_fork_length = to_upload$min_fork_length,
#                  max_fork_length = to_upload$max_fork_length,
#                  expected_number_of_samples = to_upload$n),
#             grunID::add_sample,
#             con = con)
#
#
# insert_statement <- glue::glue_sql("INSERT INTO genetic_run_identification(sample_id, run_type_id)
#                                    VALUES ({run_ids_2022$sample_id}, {run_ids_2022$run_type_id})",
#                                    .con = con)
#
# walk(1:length(insert_statement), function(i) {
#   print(paste0(i, " of ", length(insert_statement)))
#   res <- DBI::dbSendQuery(con, insert_statement[i])
#   DBI::dbClearResult(res)
# })
#
