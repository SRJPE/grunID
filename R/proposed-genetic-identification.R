#' @title Determine Run identifcation after Early and Late Assays
#' @param con a database connection
#' @param sample_id the sample id to perform run identification on
#' @param strategy the strategy to use when multi same assays are found for a sample see "details" for more
#' @export
ots_early_late_detection <- function(con, sample_id,
                                     selection_strategy = c("positive priority", "recent priority")) {

  selection_strategy <- match.arg(selection_strategy)

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
    cli::cli_alert_info("using '{selection_strategy}' to identify a unique assay run")
    if (selection_strategy == "positive priority") {
      ots_early_priority_results <-
        ots_early |> filter(positive_detection) |>
        collect()
      if (nrow(ots_early_priority_results) > 1) {
        cli::cli_abort(c(
          "x" = "'positive priority' did not identify a unique assay run",
          "i" = "try a different strategy or pass plate id of run to use for identification"
        ))
      }
    } else if (selection_strategy == "recent priority") {
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
    cli::cli_alert_info("using '{selection_strategy}' to identify a unique assay run")
    if (selection_strategy == "positive priority") {
      ots_late_priority_results <-
        ots_late |> filter(positive_detection) |>
        collect()
      if (nrow(ots_late_priority_results) > 1) {
        cli::cli_abort(c(
          "x" = "'positive priority' did not identify a unique assay run",
          "i" = "try a different strategy or pass plate id of run to use for identification"
        ))
      }
    } else if (selection_strategy == "recent priority") {
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


#' @export
ots_winter_spring_detection <- function(con, sample_id,
                                        selection_strategy = c("positive priority", "recent priority")) {
  selection_strategy <- match.arg(selection_strategy)

  assay_results <- dplyr::tbl(con, "assay_result") |>
    dplyr::filter(sample_id == !!sample_id)

  assays_for_existing_for_sample <- assay_results |> dplyr::distinct(assay_id) |> dplyr::pull()

  assay_needed_not_found <- which(!(3:4 %in% assays_for_existing_for_sample ))

  if (length(assay_needed_not_found)) {
    cli::cli_abort(c(
      "x" = "late and early assay needed to run early/late detection but assay = {assay_needed_not_found} was not found for sample {sample_id}"
    ))
  }

  ots_spring <- assay_results |> dplyr::filter(assay_id == 3)

  if (nrow(collect(ots_spring)) > 1) {
    cli::cli_alert_info("using '{selection_strategy}' to identify a unique assay run")
    if (selection_strategy == "positive priority") {
      ots_spring_priority_results <-
        ots_spring |> filter(positive_detection) |>
        collect()
      if (nrow(ots_early_priority_results) > 1) {
        cli::cli_abort(c(
          "x" = "'positive priority' did not identify a unique assay run",
          "i" = "try a different strategy or pass plate id of run to use for identification"
        ))
      }
    } else if (selection_strategy == "recent priority") {
      ots_spring_priority_results <-
        ots_spring |> arrange(desc(created_at)) |>
        head(1) |>
        collect()
    }
  } else {
    ots_spring_priority_results <- collect(ots_spring)
  }




  ots_winter <- assay_results |> filter(assay_id == 4)

  if (nrow(collect(ots_winter)) > 1) {
    cli::cli_alert_info("using '{selection_strategy}' to identify a unique assay run")
    if (selection_strategy == "positive priority") {
      ots_winter_priority_results <-
        ots_winter |> filter(positive_detection) |>
        collect()
      if (nrow(ots_winter_priority_results) > 1) {
        cli::cli_abort(c(
          "x" = "'positive priority' did not identify a unique assay run",
          "i" = "try a different strategy or pass plate id of run to use for identification"
        ))
      }
    } else if (selection_strategy == "recent priority") {
      ots_winter_priority_results <-
        ots_winter |> arrange(desc(created_at)) |>
        head(1) |>
        collect()
    }
  } else {
    ots_winter_priority_results <- collect(ots_winter)
  }

  if (!ots_spring_priority_results$positive_detection && ots_winter_priority_results$positive_detection) {
    # sample is winter
    return(list(sample_Id = sample_id, status_code = "analysis complete", run_type="WIN"))
  } else if (ots_spring_priority_results$positive_detection && ots_winter_priority_results$positive_detection) {
    return(list(sample_Id = sample_id, status_code = "analysis complete", run_type="HET"))
  } else if (ots_spring_priority_results$positive_detection && !ots_winter_priority_results$positive_detection) {
    return(list(sample_Id = sample_id, status_code = "analysis complete", run_type = "SPR"))
  } else {
    return(list(sample_Id = sample_id, status_code = NA, run_type = NA))
  }


}
