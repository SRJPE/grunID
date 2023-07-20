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


ots_early_late_detection <- function(con, sample_id,
                                     strategy = c("positive priority", "recent priority")) {

  strategy <- match.arg(strategy)

  assay_results <- tbl(con, "assay_result") |>
    filter(sample_id == !!sample_id)

  assays_for_existing_for_sample <- assay_results |> dplyr::distinct(assay_id) |> dplyr::pull()

  assay_needed_not_found <- which(!(1:2 %in% assays_for_existing_for_sample ))

  if (length(assay_needed_not_found)) {
    cli::cli_abort(c(
      "x" = "late and early assay needed to run early/late detection but assay = {assay_needed_not_found} was not found for sample {sample_id}"
    ))
  }

  ots_early <- assay_results |> filter(assay_id == 1)

  if (nrow(collect(ots_early)) > 1) {
    cli::cli_alert_info("using '{strategy}' to identify a unique assay run")
    if (strategy == "positive priority") {
      ots_early_priority_results <-
        ots_early |> filter(positive_detection) |>
        collect()
      if (nrow(ots_early_priority_results) > 1) {
        cli::cli_abort(c(
          "x" = "'positive priority' did not identify a unique assay run",
          "i" = "try a different strategy or pass plate id of run to use for identification"
        ))
      }
    } else if (strategy == "recent priority") {
      ots_early_priority_results <-
        ots_early |> arrange(desc(created_at)) |>
        head(1) |>
        collect()
    }
  } else {
    ots_early_priority_results <- collect(ots_early)
  }




  ots_late <- assay_results |> filter(assay_id == 2)

  if (nrow(collect(ots_late)) > 1) {
    cli::cli_alert_info("using '{strategy}' to identify a unique assay run")
    if (strategy == "positive priority") {
      ots_late_priority_results <-
        ots_late |> filter(positive_detection) |>
        collect()
      if (nrow(ots_late_priority_results) > 1) {
        cli::cli_abort(c(
          "x" = "'positive priority' did not identify a unique assay run",
          "i" = "try a different strategy or pass plate id of run to use for identification"
        ))
      }
    } else if (strategy == "recent priority") {
      ots_late_priority_results <-
        ots_late |> arrange(desc(created_at)) |>
        head(1) |>
        collect()
    }
  } else {
    ots_late_priority_results <- collect(ots_late)
  }

  if (!ots_early_priority_results$positive_detection && ots_late_priority_results$positive_detection) {
    # sample is fall or late-fall
    return(list(sample_Id = sample_id, status_code = "analysis complete", run_type="FAL"))
  } else if (ots_early_priority_results$positive_detection && ots_late_priority_results$positive_detection) {
    return(list(sample_Id = sample_id, status_code = "analysis complete", run_type="UNK"))
  } else if (ots_early_priority_results$positive_detection && !ots_late_priority_results$positive_detection) {
    return(list(sample_Id = sample_id, status_code = "need ots16", run_type = NA))
  } else {
    return(list(sample_Id = sample_id, status_code = NA, run_type = NA))
  }

}

safe_version <- safely(ots_early_late_detection)
out <- map(ids, ~safe_version(con, ., strategy = "recent"))

out_2 <- out |> transpose()

out_2$result


w <- safe_version(con, "F1722_3_A_7", strategy = "recent")

x <- safely(\(x) cli::cli_abort(c("fdsafdsa")))
y <- x(10)



microbenchmark::microbenchmark(
  suppressMessages(map(c("F1722_3_A_1", "F1722_3_A_2"), ~ots_early_late_detection(con, ., strategy = "recent")))
)




run_assay_detection <- function(con, sample_id, first_plate = NULL, second_plate = NULL) {
  assay_results <- tbl(con, "assay_result") |>
    filter(sample_id == !!sample_id)

  if (all(c(1, 2)))

  first_assay <- assay_results |>
    filter(assay_id == 1) |>
    collect()

  if (!is.null(first_plate)) {
    first_assay <- first_assay |> filter(plate_run_id == first_plate)
  }


  total_first_assay <- nrow(first_assay)
  if (total_first_assay > 1) {
    code_msg <- glue::glue("use the following to identify which plate runs to use: grunID::get_plate_run(con, id %in% c({glue::glue_collapse(first_assay$plate_run_id, sep = ', ')}))")
    cli::cli_abort(c(
      "x" = "more than one assay found, please pass additional argument run species identification",
      "i" = code_msg
    ))
  }

  second_assay <- assay_results |>
    filter(assay_id == 2) |>
    collect()

  if (nrow(second_assay) == 0) {
    cli::cli_abort(c(
      "x" = "{.var sample_id} = {sample_id} does not have a second assay associated with it"
    ))
  }

  if (nrow(second_assay) > 1) {
    code_msg <- glue::glue("use the following to identify which plate runs to use: grunID::get_plate_run(con, id %in% c({glue::glue_collapse(second_assay$plate_run_id, sep = ', ')}))")
    cli::cli_abort(c(
      "x" = "more than one assay found, please pass additional argument run species identification",
      "i" = code_msg
    ))
  }



}

run_assay_detection(con, "F1722_3_A_1",                   = 83)






