#' @title assign status codes
#' @description helper function called within `add_genetic_identification`
#' @param assay_detections table of assay detections
#' @details `assign_status_codes` takes in assay detections and assigns a status code
#' based on assay results logic.
#' @returns a table with `status_code_id` column updated
#' @export
assign_status_codes <- function(assay_detections) {
  assay_detections |>
    dplyr::mutate(
      status_code_id = dplyr::case_when(
        # for cases where only one assay (1 or 2) was run:
        is.na(ots_28_e) & (ots_28_l | !ots_28_l) ~ 6,
        is.na(ots_28_l) & (ots_28_e | !ots_28_e) ~ 6,
        # for cases where assays 1 and 2 have been run:
        ots_28_e & !ots_28_l ~ 8,
        ots_28_l & !ots_28_e ~ 11,
        ots_28_e & ots_28_l ~ 11,
        !ots_28_e & !ots_28_l ~ 7,
        # for cases where one of assay (3, 4) was run:
        is.na(ots_16_s) & (ots_16_w | !ots_16_w) ~ 9,
        is.na(ots_16_w) & (ots_16_s | !ots_16_s) ~ 9,
        # for cases where assays 1, 2, 3, and 4 have been run:
        ots_16_s & !ots_16_w ~ 11,
        ots_16_w & !ots_16_s ~ 11,
        ots_16_s & ots_16_w ~ 11,
        !ots_16_s & !ots_16_w ~ 10
      ))
}

#' @title assign run types
#' @description helper function called within `add_genetic_identification`
#' @param assay_detections table of assay detections
#' @details `assign_run_types` takes in assay detections and assigns a run type ID
#' based on assay results logic and status codes.
#' @returns a table with `run_type_id` column
#' @export
assign_run_types <- function(assay_detections_with_status_codes) {
  assay_detections_with_status_codes |>
    dplyr::mutate(run_type_id = dplyr::case_when(
      # assign run type based on status code and specific assay results
      # see tables "run_type" and "status_code"
      # for heterozygotes:
      status_code_id == 11 & ots_16_s & ots_16_w ~ 8,
      status_code_id == 11 & ots_28_e & ots_28_l ~ 8,
      # for spring/winter:
      status_code_id == 11 & ots_28_e ~ 6,
      status_code_id == 11 & ots_16_s & !ots_16_w ~ 1,
      status_code_id == 11 & ots_16_w & !ots_16_s ~ 4,
      # for fall/late fall:
      status_code_id == 11 & ots_28_l & !ots_28_e ~ 5,
      # for unknowns:
      status_code_id == 7 ~ 7,
      status_code_id == 10 ~ 7,
      TRUE ~ 0
    ))
}

