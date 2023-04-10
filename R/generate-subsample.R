# get subsample for assays
#' Set Status Code
#' @description Set the status code for existing samples
#' @param con connection to the database
#' @param sample_event_ids vector of sample event IDs to subsample
#' @param project project (either `JPE` or `salvage`)
#' @examples
#' # example database connection
#' cfg <- config::get()
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       dbname = cfg$dbname,
#'                       host = cfg$host,
#'                       port = cfg$port,
#'                       user = cfg$username,
#'                       password = cfg$password)
#'
#' sample_locations <- set_sample_locations(con,
#'                                          sample_event_ids = c(9, 10),
#'                                          project = "JPE")
#' @export
#' @md
generate_subsample <- function(con, sample_event_ids,
                               project) {
  # TODO what is the argument? sample event? location?

  project <- tolower(project)

  if(!project %in% c("jpe", "salvage")) {
    stop("Project argument must be either JPE or Salvage")
  }

  if(!is.numeric(sample_event_id)) {
    stop("Sample event IDs must be in the correct format")
  }

  # initialize subsample vector
  subsamples <- vector("character")
  max_no_samples <- 13
  max_no_samples_f61 <- max_no_samples

  # get unique sample event IDs
  event_ids <- unique(sample_event_ids)

  # get sample event number and location code
  sample_event_information <- tbl(con, "sample_event") |>
    filter(id %in% event_ids) |>
    collect() |>
    left_join(tbl(con, "sample_location") |>
                collect() |>
                select(sample_location_id = id, code),
              by = "sample_location_id") |>
    select(sample_event_number, sample_event_id = id, code)

  # get sample bin information
  sample_bin_information <- tbl(con, "sample_bin") |>
    collect()  |>
    filter(sample_event_id %in% sample_event_information$sample_event_id) |>
    select(sample_bin_id = id, sample_event_id, sample_bin_code)

  # get sample IDs
  sample_id_information <- tbl(con, "sample") |>
    collect() |>
    filter(sample_bin_id %in% sample_bin_information$sample_bin_id) |>
    select(sample_id = id, sample_bin_id)

  # join sample bin, sample event number, and location code to sample IDs
  all_samples <- left_join(sample_id_information,
                           sample_bin_information,
                           by = "sample_bin_id") |>
    left_join(sample_event_information,
              by = "sample_event_id") |>
    mutate(sample_bin_code = as.character(sample_bin_code)) |>
    select(sample_id, bin = sample_bin_code, location = code,
           sample_event_id)

  all_samples_with_sample_size <- all_samples |>
    group_split(location, sample_event_id) |>
    purrr::map(function(x) {
      new_x <- x |>
        add_count(location) |>
        rename(no_samples_from_site = n) |>
        add_count(bin) |>
        rename(no_samples_from_bin = n) |>
        arrange(no_samples_from_bin) |>
        mutate(samples_to_take = ifelse(location == "F61", 0, ceiling(max_no_samples * (no_samples_from_bin / no_samples_from_site))),
               samples_for_f61 = (max_no_samples - samples_to_take),
               samples_to_take = ifelse(location == "F61", samples_to_take + samples_for_f61, samples_to_take))
      # TODO change samples_to_take to max_no_samples - samples_to_take for highest bin count

    }) |>
    list_rbind()


  samples <- all_samples_with_sample_size |>
    group_split(sample_event_id, location, bin) |>
    purrr::map(function(y) {
      y |>
        sample_n(size = unique(samples_to_take), replace = TRUE)
    }) |>
    list_rbind() |>
    pull(sample_id)

  return(samples)

}

