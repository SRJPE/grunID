#' @importFrom magrittr %>%
NULL

.onAttach <- function(libname, pkgname) {
  options(cli.progress_show_after = 0)
  options(cli.progress_clear = FALSE)
}
