library(grunID)
library(shiny)
library(tidyverse)
library(shinyBS)
library(DT)

config_path <- Sys.getenv("CONFIG_PATH")
in_dev_mode <- as.logical(Sys.getenv("GRUNID_IS_DEV"))

if (!is.na(in_dev_mode) && in_dev_mode) {

  cfg <- config::get(file = config_path)
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = cfg$dbname,
                        host = cfg$host,
                        port = 5432,
                        user = cfg$username,
                        password = cfg$password)

} else {
  con <- grunID::gr_db_connect()
}

all_protocols <- get_protocols(con) |> collect()
all_labs <- get_laboratories(con) |> select(id, code, laboratory_name, description) |> collect()
all_gen_methods <- get_genetic_methods(con) |> select(id, code, method_name, description) |> collect()
