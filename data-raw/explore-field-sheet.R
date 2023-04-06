library(readxl)
library(tidyverse)
library(purrr)
library(grunID)

# get first sample dates to correspond to sample event number
sampling_events <- read_xlsx("data-raw/FL_min_max_for_bin_planning_at_sampling_sites_2022.xlsx",
                             sheet = "Sampling Events") |>
  transmute(sample_event_number = `Sampling Event`,
            first_sample_date = lubridate::as_date(`First Sampling Date`))


# read in field sheets Adie/FW created for teams (sheet for each site/event number)
all_sheets <- readxl::excel_sheets("data-raw/field_sheets_2022.xlsx")

all_data <- map_df(all_sheets, function(s) {
  read_xlsx("data-raw/field_sheets_2022.xlsx", sheet = s) |>
    mutate(sheet_name = s)
})

all_data |> glimpse()

# create location code, sample_event_number; get bin and fork length ranges as columns
# joins sample event number to table to get first sample date
sample_plan_2022 <- all_data |>
  transmute(
    location_code = str_split(sheet_name, "-", simplify = TRUE)[,1],
    sample_event_number = as.integer(str_match(sheet_name, "-([0-9]+)")[,2]),
    sample_bin_code = Bin,
    `Bin FL Range (mm)`) |>
  tidyr::separate(`Bin FL Range (mm)`,
                  into = c("min_fork_length", "max_fork_length"), convert = TRUE) |>
  left_join(sampling_events) |>
  select(
    location_code,
    sample_event_number,
    first_sample_date,
    sample_bin_code,
    min_fork_length,
    max_fork_length
  )
# TODO why do we not keep sample IDs?

sample_plan_2022 |> glimpse()
# check against sample plan template (usethis)
sample_plan_template |> glimpse()


# read in sheets for each trib to get expected number of samples column
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

# join expected number of samples column to sample plan
sample_plan_2022_final <- sample_plan_2022 |>
  left_join(all_sampling_plan_data) |>
  mutate(sample_event_number = as.integer(sample_event_number))



sample_plan_2022_final |> glimpse()
sample_plan_template |> glimpse()

write_csv(sample_plan_2022_final, "data-raw/2022_sample_plan.csv")
