cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
               dbname = cfg$dbname,
               host = cfg$host,
               port = 5432,
               user = cfg$username, password = cfg$password)

library(grunID)
add_new_plate_results(
  con = con,
  protocol_name = "default",
  genetic_method = "SHLK",
  laboratory = "DWR_GeM",
  lab_work_performed_by = "Emanuel",
  description = "test",
  date_run = lubridate::today(),
  filepath = "data-raw/sherlock-example-outputs/2024/ebk-ids/061223_JPE24_E+L_E1-2_P1_SH_RH - FAIL.xlsx",
  sample_type = "fin_clip",
  layout_type = "split_plate_early_late",
  plate_size = 384,
  .control_id = "EBK",
  selection_strategy = "recent priority",
  run_gen_id = TRUE
  # samples_source = "jpe"
)


text <- "MANUAL EBK VALUE CHECK: plate_run_id = 393 Values = EBK-1(13000);EBK-2(13000)"
matches <- str_match_all(text, "EBK-\\d+\\((\\d+)\\)")
values <- matches[[1]][, 1]

matches <- str_match_all(text, "EBK-\\d+-\\d+\\((\\d+)\\)")
values <- matches[[1]]

