# load packages 
library(tidyverse)
library(readxl)
library(stringr)
library(hms)

# helpers ---
# 702 columns
excel_column_index <- c(LETTERS, 
                        expand.grid(LETTERS, LETTERS) %>% 
                          transmute(columns = paste0(Var1, Var2)) %>% 
                          pull(columns) %>% 
                          sort())

# metadata ---- 
metadata <- read_excel("data-raw/exampleoutput_synergyH1trial_data_092021.xlsx", 
                       range = "A2:B27", col_names = c("key", "value")) %>% 
  fill(key)

runtime_metadata <- metadata %>% filter(key == "Start Kinetic") %>% pull(value) %>% 
  str_split(",") %>% unlist() %>% str_trim() %>% 
  set_names("Runtime", "Interval", "Number of Reads")

number_of_rows <- parse_number(runtime_metadata["Number of Reads"])

# mapping of plate layout location to sample identifier
plate_layout <- read_excel("data-raw/exampleoutput_synergyH1trial_data_092021.xlsx", 
                           range = "C32:N39", col_names = as.character(1:12)) %>% 
  mutate(letter = letters[1:8]) %>% 
  pivot_longer(names_to = "number", values_to = "sample_id", !letter) %>% 
  transmute(location = toupper(paste0(letter, number)), sample_id)

# raw fluorescence ----
column_index <- sum(!is.na(plate_layout$sample_id)) + 3 # offset 3 columns
start_raw_fluorescence <- 43
end_row_raw_fluorescence <- start_raw_fluorescence + number_of_rows
end_raw_fluorescence <- paste0(excel_column_index[column_index], end_row_raw_fluorescence)
raw_fluorescence <- read_excel("data-raw/exampleoutput_synergyH1trial_data_092021.xlsx", 
                               range = paste0("B", start_raw_fluorescence,":", end_raw_fluorescence)) %>% 
  mutate(Time = as_hms(Time)) %>% 
  mutate_all(as.character) %>% 
  select(-starts_with("T°")) %>% 
  pivot_longer(names_to = "location", values_to = "fluorescence", !Time) %>%
  left_join(plate_layout)

# background values ---
start_background_fluorescence <- end_row_raw_fluorescence + 4
end_row_background_fluorescence <- start_background_fluorescence + number_of_rows
end_background_fluorescence <- paste0(excel_column_index[column_index - 1], end_row_background_fluorescence)
background_fluorescence <- read_excel("data-raw/exampleoutput_synergyH1trial_data_092021.xlsx", 
                                      range = paste0("B", start_background_fluorescence,":", 
                                                     end_background_fluorescence)) %>% 
  mutate(Time = as_hms(Time)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(names_to = "location", values_to = "background_fluorescence", !Time) %>%
  left_join(plate_layout)

# results ---
start_results <- end_row_background_fluorescence + 4
end_row_results <- start_results + 4*8
end_results <- paste0(excel_column_index[15], end_row_results)
results <- read_excel("data-raw/exampleoutput_synergyH1trial_data_092021.xlsx", 
                                      range = paste0("B", start_results,":", end_results), 
           col_types = "text") %>% 
  fill(...1) %>% 
  pivot_longer(names_to = "number_location", values_to = "RFU", !c(...1, ...14)) %>% 
  transmute(location = paste0(...1, number_location), metric = ...14, RFU) %>% 
  left_join(plate_layout) %>% 
  filter(!is.na(sample_id)) %>% 
  mutate(metric = str_remove(metric, "\\[.+\\]")) %>% 
  arrange(location)

View(results)
  
