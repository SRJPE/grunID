library(grunID)
library(shiny)
library(tidyverse)
library(shinyBS)
library(DT)

config_path <- Sys.getenv("CONFIG_PATH")
print(config_path)
in_dev_mode <- Sys.getenv("GRUNID_IS_DEV")

if (!is.na(in_dev_mode) && in_dev_mode == 1) {

  message("in development mode, using local database")
  cfg <- config::get(file = config_path)
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = cfg$dbname,
                        host = cfg$host,
                        port = 5432,
                        user = cfg$username,
                        password = cfg$password)

} else {
  cfg <- config::get(file = config_path)
  con <- grunID::gr_db_connect(username = cfg$username, host = cfg$host, dbname = cfg$dbname)
}

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

# sample_status_options <- dplyr::tbl(con, "status_code") |>
#   dplyr::distinct(status_code_name) |>
#   dplyr::collect() |>
#   dplyr::pull() |>
#   dput()

colors <- c(
  "green" = "#afdeab",
  "orange" = "#b58653",
  "red" = "#b55353",
  "purple" = "#b09ab8",
  "blue" = "#6c97cc",
  "yellow" = "#fbfcc5"
)

sample_status_options <- c(
  "other lab" = as.character(colors["purple"]),
  "need ots28" = as.character(colors["orange"]),
  "out to field" = as.character(colors["purple"]),
  "prepped" = as.character(colors["orange"]),
  "returned from field" = as.character(colors["blue"]),
  "analysis complete" = as.character(colors["green"]),
  "need ots16" = as.character(colors["orange"]),
  "archived" = as.character(colors["purple"]),
  "ots28 in progress" = as.character(colors["orange"]),
  "ots16 complete" = as.character(colors["green"]),
  "ots16 inprogress" = as.character(colors["orange"]),
  "ots28 complete" = as.character(colors["green"]),
  "created" = as.character(colors["yellow"])
)

sample_status_colors = c(rep("#ead8d5", 2), rep("#e7f2f1", 3),
                         "#d5ead5", "#ead8d5", rep("#e7f2f1", 6))
all_locations <- dplyr::tbl(con, "sample_location") |>
  dplyr::distinct(code) |>
  dplyr::filter(!code %in% c("CONTROL", "TEST2", "TEST")) |>
  dplyr::pull(code)
