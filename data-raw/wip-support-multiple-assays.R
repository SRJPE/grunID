add_new_plate_results(con, protocol_name = "new protocol", genetic_method = "SHLK",
                      laboratory = "DWR_GeM", lab_work_performed_by = "user",
                      description = "a test run", date_run = "2023-07-12",
                      filepath = "../misc/sherlock_results_part_2.xlsx",
                      sample_type = "mucus",
                      layout_type = "split_plate_early_late",
                      plate_size = 384)



dplyr::tbl(con, "assay_result") |>
  dplyr::filter(sample_id %in% sample_identifiers) |>
  dplyr::select(sample_id, assay_id, positive_detection) |>
  dplyr::collect() |>
  dplyr::mutate(assay_id_name = dplyr::case_when(assay_id == 1 ~ "ots_28_e",
                                                 assay_id == 2 ~ "ots_28_l",
                                                 assay_id == 3 ~ "ots_16_s",
                                                 assay_id == 4 ~ "ots_16_w"),
                assay_id_name = factor(assay_id_name, levels = c("ots_28_e","ots_28_l","ots_16_s","ots_16_w"))) |>
  dplyr::select(-assay_id) |>
  tidyr::pivot_wider(names_from = "assay_id_name", values_from = "positive_detection", names_expand = TRUE)


x <- dplyr::tbl(con, "assay_result") |>
  dplyr::filter(sample_id %in% sample_identifiers) |>
  dplyr::select(sample_id, assay_id, positive_detection, plate_run_id) |>
  dplyr::collect()

x |>
  group_by(plate_run_id, assay_id) |>
  summarise(
    total = n()
  )


first_assay <- tbl(con, "assay_result") |>
  filter(sample_id == "F1722_3_A_1") |>
  filter(assay_id == 1) |>
  collect()


run_assay_detection <- function(con, sample_id, first_plate = NULL, second_plate = NULL) {
  assay_results <- tbl(con, "assay_result") |>
    filter(sample_id == !!sample_id)

  first_assay <- assay_results |>
    filter(assay_id == 1) |>
    collect()

  if (!is.null(first_plate)) {
    first_assay <- first_assay |> filter(plate_run_id == first_plate)
  }


  total_first_assay <- nrow(first_assay)
  if (total_first_assay > 1) {
    cli::cli_alert_warning("more than one assay found, please pass additional argument")
    cli::cli_alert_info("use the following to identify which plate runs to use:")
    code_msg <- glue::glue("grunID::get_plate_run(con, id %in% c({glue::glue_collapse(first_assay$plate_run_id, sep = ', ')}))")
    cli::cli_code(code_msg)
  }

  second_assay <- assay_results |>
    filter(assay_id == 2) |>
    collect()

  if (nrow(second_assay) ==1) {
    cli::cli_abort(c(
      "x" = "{.var sample_id} = {sample_id} does not have a second assay associated with it"
    ))
  }

}

run_assay_detection(con, "F1722_3_A_1", first_plate = 83)






