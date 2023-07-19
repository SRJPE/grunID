library(grunID)
library(shiny)
library(tidyverse)
library(shinyBS)
library(DT)

config_path <- Sys.getenv("CONFIG_PATH")
cfg <- config::get(file = config_path)
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = 5432,
                      user = cfg$username,
                      password = cfg$password)


all_protocols <- get_protocols(con) |> collect()
all_labs <- get_laboratories(con) |> select(id, code, laboratory_name, description) |> collect()
all_gen_methods <- get_genetic_methods(con) |> select(id, code, method_name, description) |> collect()
all_sample_status <- dplyr::tbl(con, "sample_status") |>
  dplyr::left_join(dplyr::tbl(con, "status_code") |>
                     select(status_code_id = id, status = status_code_name),
                   by = "status_code_id") |>
  dplyr::mutate(created_at = as.Date(created_at),
                updated_at = as.Date(updated_at)) |>
  dplyr::select(sample_id, status, created_at, updated_at) |>
  dplyr::collect()
sample_status_options <- dplyr::tbl(con, "status_code") |>
  dplyr::distinct(status_code_name) |>
  dplyr::collect() |>
  dplyr::pull()
