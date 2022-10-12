results$raw_assay_results


# get the last time step for blank values
plate_run_uid

# get the last two values for the blank
get_protocols(con) |>
  filter(id == protocol_id) |> glimpse()

library(hms)

as_hms("00:00:00") + (("00:03:00") * 31)

a <- as_hms("00:00:00")

for (i in 1:31) {
  a <- a + as_hms("00:01:00")
  print(as_hms(a))
}

clock::as_

clock::add_minutes("00:00:00", 1)

results$raw_assay_results |>
  glimpse()


a <- hms::as_hms("00:00:00")
b <- hms::as_hms
