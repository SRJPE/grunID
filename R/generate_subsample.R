# get subsample for assays

# libraries
library(tidyverse)
library(readxl)
library(DBI)

# establish connection
cfg <- config::get()
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = cfg$dbname,
                      host = cfg$host,
                      port = cfg$port,
                      user = cfg$username,
                      password = cfg$password)

# for a sample event

generate_subsample <- function(sample_event_ids,
                               project) {

  project <- tolower(project)

  if(!project %in% c("jpe", "salvage")) {
    stop("Project argument must be either JPE or Salvage")
  }

  if(!is.numeric(sample_event_id)) {
    stop("Sample event IDs must be in the correct format")
  }

  # initialize subsample
  subsamples <- vector("character")

  # pull sample IDs, bin, and sites
  event_ids <- unique(sample_event_ids)

  sample_event_information <- tbl(con, "sample_event") |>
    filter(id %in% event_ids) |>
    collect() |>
    left_join(tbl(con, "sample_location") |>
                collect() |>
                select(sample_location_id = id, code),
              by = "sample_location_id") |>
    select(sample_event_number, sample_event_id = id, code)

  sample_bin_information <- tbl(con, "sample_bin") |>
    collect()  |>
    filter(sample_event_id %in% sample_event_information$sample_event_id) |>
    select(sample_bin_id = id, sample_event_id, sample_bin_code)

  sample_id_information <- tbl(con, "sample") |>
    collect() |>
    filter(sample_bin_id %in% sample_bin_information$sample_bin_id) |>
    select(sample_id = id, sample_bin_id)

  all_samples <- left_join(sample_id_information,
                           sample_bin_information,
                           by = "sample_bin_id") |>
    left_join(sample_event_information,
              by = "sample_event_id") |>
    mutate(sample_bin_code = as.character(sample_bin_code)) |>
    select(sample_id, bin = sample_bin_code, location = code,
           sample_event_id)

  if(project == "jpe") {
    max_no_samples <- 13
    no_samples_f61 <- max_no_samples

    if(length(unique(all_samples$sample_id)) <= 150) {
      subsamples <- unique(all_samples$sample_id)
      return(subsamples)
    } else {

      # save F61 for last
      if(!str_detect("F61", all_samples$sample_id)) {


      }
    }

  }
  # if project = JPE:

  subsample <- vector()
  max_no_samples <- 13

  # pull sample IDs from sample event id, also pull sample bin IDs, and site

  # if length(sample IDs) <= 150 { subsample = sample_IDs}

  no_samples_f61 <- max_no_samples

  # else:

  # group_by(site) and sort by tally of samples per site
  # make this into a table with sample event number, site, bin,
  # no_samples_from_site, and no_samples_from_bin
  # sort by no_samples_from_bin low to high

  # if no_samples_from_site < max_no_samples:

  # add all samples from that site to the subsample &
  # no_samples_f61 += (max_no_samples - no_samples_from_site)

  # else:

  # group_by(bin)
  # for each bin:
  # no_sample_per_bin = max_no_samples * (no_samples_from_bin/no_samples_from_site)
  # for the bin with the highest no samples, just take the remainder so:
  # no_sample_highest_bin = no_samples_from_site - sum(no_sample_per_bin)
  # round this up to the nearest integer

  # then get sample IDs:
  # sample_IDs_for_assays <- sample(no_sample_per_bin, samples_by_bin)
  # subsample <- append(subsample, sample_IDs_for_assays)


  # then do this logic for no_samples_f61


}

