library(tidyverse)

raw_data <- readxl::read_excel("data-raw/2024-use-case/scanned-in-samples-from-field/2024_JPE_Sample_Check_In_022924.xlsx",
                               col_names = c("sample_id", "received_sample", "event", "entered_by", "verified_by", "alias", "comments"),
                               skip = 1)


raw_data |>
  filter(!is.na(sample_id)) |>
  group_by(received_sample) |> tally()






