cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = 5432,
                      user = cfg$username,
                      password = cfg$password)

add_new_plate_results(
  con = con,
  filepath = "data-raw/sherlock-example-outputs/2024/salvage/061223_JPE24_E+L_E1-2_P1_SH_RH.xlsx",
  protocol_name = "default",
  genetic_method = "SHLK",
  laboratory = "DWR_GeM",
  date_run = lubridate::today(),
  lab_work_performed_by = "emanuel",
  sample_type = "mucus",
  layout_type = "split_plate_early_late",
  plate_size = 384, description = "",
  samples_source = "salvage",
  .control_id = "EBK",
  run_gen_id = TRUE)
