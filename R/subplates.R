#' @title Get Subplates for Run
#' @export
get_all_subplates_for_run <- function(con, plate_run_id, .assay_result_table = "assay_result") {
  tbl(con, .assay_result_table) |>
    filter(plate_run_id == !!plate_run_id, !str_detect(sample_id, "EBK|NTC|DNA")) |>
    distinct(sub_plate) |>
    collect() |>
    pull()
}

#' @title Remove subplate
#' @description
#' Given a plate run, delete all of the plate data in database for this subplate
#'
#' @export
remove_subplates_from_run <- function(con, plate_run_id, sub_plate) {
  # first we get samples that are in this subplate and plate run
  sample_ids_in_subplate <- tbl(con, "assay_result") |>
    filter(plate_run_id == !!plate_run_id, !str_detect(sample_id, "EBK|NTC|DNA")) |>
    collect() |>
    pull(sample_id)

  sub_plate <- as.integer(sub_plate)

  if (length(sample_ids_in_subplate) > 0) {


    # first remove from genid
    sql_stm <- glue::glue_sql(
      "DELETE FROM genetic_run_identification where (early_plate_id = {plate_run_id} or late_plate_id = {plate_run_id} or winter_plate_id = {plate_run_id} or spring_plate_id = {plate_run_id}) and sample_id IN ({sample_ids_in_subplate*})", .con = con)
    DBI::dbExecute(con, sql_stm)

    # remove from status
    sql_stm <- glue::glue_sql(
      "DELETE FROM sample_status where plate_run_id = {plate_run_id} and sample_id IN ({sample_ids_in_subplate*})", .con = con)
    DBI::dbExecute(con, sql_stm)

    # remove from results
    sql_stm <- glue::glue_sql(
      "DELETE FROM assay_result where plate_run_id = {plate_run_id} and sample_id IN ({sample_ids_in_subplate*})", .con = con)
    DBI::dbExecute(con, sql_stm)

    # remove from raw results
    sql_stm <- glue::glue_sql(
      "DELETE FROM raw_assay_result where plate_run_id = {plate_run_id} and sample_id IN ({sample_ids_in_subplate*})", .con = con
    )
    DBI::dbExecute(con, sql_stm)

  }

  # determine if we can delete the plate run
  samples_remaining <- tbl(con, "assay_result") |>
    filter(plate_run_id == !!plate_run_id) |>
    collect() |>
    pull(sample_id)

  samples_not_threshold_types <- which(!(samples_remaining %in% c("EBK-1-1", "EBK-1-2", "EBK-1-3", "EBK-1-4", "POS-DNA-1", "POS-DNA-2",
                                 "POS-DNA-3", "NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3", "NTC-1",
                                 "NTC-2", "NTC-3")))

  if (length(samples_not_threshold_types) == 0) {

    sql_stm <- glue::glue_sql("DELETE FROM assay_result where plate_run_id = {plate_run_id}", .con = con)
    DBI::dbExecute(con, sql_stm)
    sql_stm <- glue::glue_sql("DELETE FROM raw_assay_result where plate_run_id = {plate_run_id}", .con = con)
    DBI::dbExecute(con, sql_stm)

    sql_stm <- glue::glue_sql("DELETE FROM plate_run where id = {plate_run_id}", .con = con)
    DBI::dbExecute(con, sql_stm)


  }


}
