library(grunID)
library(shiny)
library(tidyverse)
library(shinyBS)
library(DT)

renderTableWithScrollOnX <- function(...) {
  tags$div(
    style = 'overflow-x: scroll',
    renderTable(...)
  )
}

config_path <- Sys.getenv("CONFIG_PATH")
in_dev_mode <- Sys.getenv("GRUNID_IS_DEV")

if (!is.na(in_dev_mode) && in_dev_mode == 1) {
  # logger::log_threshold(level = logger::INFO) # Just always capture all the input?
  run_mode_log_message <- "app started in development mode"
  cfg <- config::get(file = config_path)
  config_file_log_message <- glue::glue("using config file found at: {config_path}")
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = cfg$dbname,
                        host = cfg$host,
                        port = 5432,
                        user = cfg$username,
                        password = cfg$password)

} else {
  run_mode_log_message <- "app started in production mode"
  config_file_log_message <- glue::glue("using config file found at: {config_path}")
  cfg <- config::get(file = config_path)
  con <- grunID::gr_db_connect(username = cfg$username, host = cfg$host, dbname = cfg$dbname)
}

logger::log_appender(logger::appender_console, index = 1)
logger::log_appender(function(lines) {
  DBI::dbAppendTable(con, name = "shiny_app_logs", value = data.frame(line = lines, created_at = Sys.time()))
}, index = 2)

logger::log_info(run_mode_log_message)
logger::log_info(config_file_log_message)

all_protocols <- get_protocols(con) |> collect()
all_labs <- get_laboratories(con) |> select(id, code, laboratory_name, description) |> collect()
all_gen_methods <- get_genetic_methods(con) |> select(id, code, method_name, description) |> collect()

# the actual current status for a given sample is always the latest update
DB_get_sample_status <- function() {
  DBI::dbGetQuery(
    con,
    "SELECT t1.sample_id, sc.status_code_name as status, t1.updated_at, t1.comment as plate_comment FROM sample_status AS t1
      INNER JOIN (
        SELECT sample_id, MAX(id) AS max_id
        FROM sample_status
      GROUP BY sample_Id
    ) AS t2 ON t1.id = t2.max_id join public.status_code sc on sc.id = t1.status_code_id;"
  )
}

# Constants ----------------------------------------------------------------

colors <- c(
  "green" = "#77dd7740",
  "orange" = "#ffb34740",
  "red" = "#fa675f40",
  "purple" = "#b09ab840",
  "blue" = "#aec6cf40",
  "yellow" = "#fdfd9640"
)

sample_status_options <- c(
  # "other lab" = as.character(colors["purple"]),
  "created" = as.character(colors["yellow"]),
  "analysis complete" = as.character(colors["green"]),
  "need ots28" = as.character(colors["orange"]),
  "ots28 in progress" = as.character(colors["orange"]),
  "need ots16" = as.character(colors["orange"]),
  "ots16 inprogress" = as.character(colors["orange"]),
  "ots16 complete" = as.character(colors["green"]),
  "ots28 complete" = as.character(colors["green"])
  # "out to field" = as.character(colors["purple"]),
  # "prepped" = as.character(colors["orange"]),
  # "returned from field" = as.character(colors["blue"]),
  # "archived" = as.character(colors["purple"]),
)

sample_status_colors = c(rep("#ead8d5", 2), rep("#e7f2f1", 3),
                         "#d5ead5", "#ead8d5", rep("#e7f2f1", 6))


# Database Handles -------------------------------------------------

all_locations <- dplyr::tbl(con, "sample_location") |>
  dplyr::distinct(code) |>
  dplyr::filter(!code %in% c("CONTROL", "TEST2", "TEST")) |>
  dplyr::pull(code)

available_years <- dplyr::tbl(con, "sample_event") |>
  dplyr::mutate(year = lubridate::year(first_sample_date)) |>
  dplyr::distinct(year) |>
  dplyr::collect() |>
  dplyr::pull(year)


