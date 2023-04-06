#' Get Samples
#' @export
get_samples <- function(con, ...) {
  is_valid_connection(con)

  assays <- dplyr::tbl(con, "sample") |>
    dplyr::filter(...) |>
    dplyr::collect()

  return(assays)
}

#' Generate Subsample
#' @description `get_subsample()` selects sample IDs from the field for assay workflow
#' @param con
#' @param sample_event_id
#' @examples
#' @export
#' @md

get_subsample <- function(con, sample_event_id) {

  # select sample IDs from "sample" where sample_bin_id = sample_event_id

  # if no_samples <= 150, run all

  # else:

  # no_samples_F61 = 0

  # if location != F61:

  # for each sampling event:

  # group_by(bin_id)

  # sort by lowest no_samples_in_bin

  # if no_samples_in_bin < 13: no_samples_F61 += (13 - no_samples_in_bin)

  # no_samples_per_bin = round(13 * (no_sample_in_bin/total_no_samples_in_event), 0)

  # round up to the nearest integer (not down)

  # sample from samples_in_bin randomly, n = no_samples_per_bin

  # then do F61 samples


}
