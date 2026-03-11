library(tidyverse)

test <- read_csv("data-raw/backfill/results/genetics_query_for_dashboard_2022-2025_2026-03-10.csv")

test |>
  glimpse()

added_ids <- test$sample_id

gtseq_data <- bind_rows(gt_seq_data_2022,
                        gt_seq_data_2023) |>
  bind_rows(gt_seq_data_2024)

gtseq_data |>
  filter(!SampleID %in% added_ids) |>
  View()

get_cases <- generate_final_run_assignment(con)

case_ids <- get_cases$diagnostic

gtseq_non_adds_with_reason <- gtseq_data |>
  filter(!SampleID %in% added_ids) |>
  left_join(case_ids |>
              select(sample_id, remove_case),
            by = c("SampleID" = "sample_id")) |>
  mutate(reason_not_added = case_when(if_all(`Gtseq_Chr28_Geno`:`SacWin`, ~is.na(.)) ~ "MISSING DATA",
                                      remove_case == "NO CASE" ~ "MISSING DATA", # this is hard coded because i checked and they're all missing data except 1 col, so not getting caught by logic in genetic_identification.R
                                      !is.na(remove_case) ~ remove_case,
                                      TRUE ~ "NONSTANDARD SAMPLE ID")) |>
  select(-remove_case) |>
  janitor::clean_names() |>
  glimpse()

write_csv(gtseq_non_adds_with_reason, "data-raw/backfill/results/gtseq_non_adds_with_reason_2026-03-11.csv")
