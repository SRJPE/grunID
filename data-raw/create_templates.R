protocol_template <- data.frame(software_version = "3.11.19",
                                reader_type = "Synergy H1",
                                reader_serial_number = "21092224",
                                plate_type = "Greiner 384 F bottom",
                                set_point = 37,
                                preheat_before_moving = TRUE,
                                runtime = "02:00:00",
                                interval = "00:03:00",
                                read_count = 41,
                                run_mode = "Kinetic",
                                excitation = 484,
                                emissions = 530,
                                optics = "Top",
                                gain = 100,
                                light_source = "Xenon Flash",
                                lamp_energy = "High",
                                read_height = 9,
                                name = "new name")

usethis::use_data(protocol_template, overwrite = TRUE)

library(tidyverse)
library(lubridate)
raw_sample_plan <- read_csv("data-raw/example_sample_plan.csv")

#' sample_plan <- tibble(location_code = c("BTC", "BUT"),
#'                       sample_event_number = 1:2,
#'                       first_sample_date = "2020-01-01",
#'                       sample_bin_code = "A",
#'                       min_fork_length = 10,
#'                       max_fork_length = 95,
#'                       expected_number_of_samples = 10
#'                       )

glimpse(raw_sample_plan)

sample_plan_template <- raw_sample_plan |>
  tidyr::fill(Date) |>
  dplyr::mutate(
    location_code = "FTH_RM17",
    first_sample_date = lubridate::ydm(paste(2022, Date, sep = "-"))) |>
  tidyr::separate(`FL range`, into = c("min_fork_length", "max_fork_length"),
                  sep = "-", convert = TRUE) |>
  dplyr::group_by(first_sample_date) |>
  dplyr::mutate(sample_event_number = cur_group_id(),
                expected_number_of_samples = tidyr::replace_na(`# in bin`, 0)) |>
  dplyr::ungroup() |>
  dplyr::select(location_code, sample_event_number,
                first_sample_date, sample_bin_code = `Bin #`,
                min_fork_length, max_fork_length,
                expected_number_of_samples)

usethis::use_data(sample_plan_template, overwrite = TRUE)
