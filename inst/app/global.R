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

# the actual current status for a given sample is always the latest update
all_sample_status <- function() {

  DBI::dbGetQuery(
    con,
    "SELECT t1.sample_id, sc.status_code_name as status, t1.updated_at, t1.comment as plate_comment
FROM sample_status AS t1
INNER JOIN (
    SELECT sample_id, MAX(id) AS max_id
    FROM sample_status
    GROUP BY sample_Id
) AS t2 ON t1.id = t2.max_id join public.status_code sc on sc.id = t1.status_code_id;"
  )
}

flagged_sample_status <- function() {
  DBI::dbGetQuery(
    con,
    "SELECT ar.plate_run_id, ar.sample_id, sc.status_code_name, status.comment, a.assay_name, ar.raw_fluorescence, ar.threshold, ar.positive_detection, pr.active AS active_plate_run
FROM assay_result AS ar
LEFT JOIN(
	SELECT sample_id, comment, status_code_id
	FROM sample_status
	WHERE comment LIKE '%MANUAL%'
) AS status ON ar.sample_id = status.sample_id
LEFT JOIN (
	SELECT id, active
	FROM plate_run
) AS pr ON ar.plate_run_id = pr.id
JOIN public.status_code sc ON sc.id = status.status_code_id
JOIN public.assay a ON a.id = ar.assay_id;"
  )
}
# sample_status_options <- dplyr::tbl(con, "status_code") |>
#   dplyr::distinct(status_code_name) |>
#   dplyr::collect() |>
#   dplyr::pull() |>
#   dput()

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
all_locations <- dplyr::tbl(con, "sample_location") |>
  dplyr::distinct(code) |>
  dplyr::filter(!code %in% c("CONTROL", "TEST2", "TEST")) |>
  dplyr::pull(code)

available_years <- dplyr::tbl(con, "sample_event") |>
  dplyr::mutate(year = lubridate::year(first_sample_date)) |>
  dplyr::distinct(year) |>
  dplyr::collect() |>
  dplyr::pull(year)



