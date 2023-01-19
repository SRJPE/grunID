library(readxl)
library(tidyverse)
library(purrr)
library(grunID)

sampling_events <- read_xlsx("data-raw/FL_min_max_for_bin_planning_at_sampling_sites_2022.xlsx",
                             sheet = "Sampling Events") |>
  transmute(sample_event_number = `Sampling Event`,
            first_sample_date = lubridate::as_date(`First Sampling Date`))



all_sheets <- readxl::excel_sheets("data-raw/field_sheets_2022.xlsx")

all_data <- map_df(all_sheets, function(s) {
  read_xlsx("data-raw/field_sheets_2022.xlsx", sheet = s) |>
    mutate(sheet_name = s)
})

all_data |> glimpse()


sample_plan_2022 <- all_data |>
  transmute(
    location_code = str_split(sheet_name, "-", simplify = TRUE)[,1],
    sample_event_number = as.integer(str_match(sheet_name, "-([0-9]+)")[,2]),
    sample_bin_code = Bin,
    min_fork_length = as.integer(str_match(`Bin FL Range (mm)`, "([0-9]+)-([0-9]+)")[1,2]),
    max_fork_length = as.integer(str_match(`Bin FL Range (mm)`, "([0-9]+)-([0-9]+)")[1,3])
  ) |>
  left_join(sampling_events) |>
  select(
    location_code,
    sample_event_number,
    first_sample_date,
    sample_bin_code,
    min_fork_length,
    max_fork_length
  )


sample_plan_2022 |> glimpse()
sample_plan_template |> glimpse()



sampling_plan_raw_sheets <- excel_sheets("data-raw/FL_min_max_for_bin_planning_at_sampling_sites_2022.xlsx")
sampling_sheets <- sampling_plan_raw_sheets[7:length(sampling_plan_raw_sheets)]

all_sampling_plan_data <- map_df(sampling_sheets, function(s) {
  read_xlsx("data-raw/FL_min_max_for_bin_planning_at_sampling_sites_2022.xlsx", skip = 2, sheet = s) |>
    transmute(Date, `Min and max FL`, `Bin #`, `FL range`, total_in_bin = as.numeric(`# in bin`), sheet_name = s,
              location_code)
}) |>
  tidyr::fill(Date) |>
  mutate(expected_number_of_samples = tidyr::replace_na(total_in_bin, 0),
         Date = lubridate::as_date(Date),
         Date = lubridate::`year<-`(Date, 2022)) |>
  left_join(sampling_events, by = c("Date" = "first_sample_date")) |>
  select(first_sample_date = Date, expected_number_of_samples, location_code, sample_bin_code = `Bin #`)

all_sampling_plan_data |> glimpse()


sample_plan_2022_final <- sample_plan_2022 |>
  left_join(all_sampling_plan_data) |>
  mutate(sample_event_number = as.integer(sample_event_number),
         location_code = case_when(
           location_code == "F17" ~ "FTH_RM17",
           location_code == "F61" ~ "FTH_RM61",
           TRUE ~ location_code
         ))



sample_plan_2022_final |> glimpse()
sample_plan_template |> glimpse()

write_csv(sample_plan_2022_final, "data-raw/2022_sample_plan.csv")
