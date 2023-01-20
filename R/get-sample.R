#' Get Samples
#' @export
get_samples <- function(con, ...) {
  is_valid_connection(con)

  assays <- dplyr::tbl(con, "sample") |>
    dplyr::filter(...) |>
    dplyr::collect()

  return(assays)
}
