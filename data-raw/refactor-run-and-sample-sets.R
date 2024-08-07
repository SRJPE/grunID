res <- DBI::dbGetQuery(con,
               "SELECT *
FROM (
  SELECT *,
         ROW_NUMBER() OVER (PARTITION BY sample_id, assay_id ORDER BY updated_at DESC) as rn
  FROM assay_result
  WHERE active = true
) subquery
WHERE rn = 1;") |> as_tibble()

res |>
  select(sample_id, assay_id, positive_detection) |>
  pivot_wider(names_from = assay_id, values_from = positive_detection) |>
  mutate(
    sample_state = case_when(

    )
  )


res |>
  select(sample_id, assay_id, positive_detection) |>
  complete(sample_id, assay_id = 1:4) |>  # This ensures all assay_ids from 1 to 4 are present
  pivot_wider(
    names_from = assay_id,
    values_from = positive_detection,
  ) |>
  rename("early" = `1`, "late" = `2`, "spring" = `3`, "winter" = `4`) |>
  mutate(sample_state = case_when(
    early & !late & spring & winter ~ "SW-HET;analysis complete",
    early & !late & !spring & winter ~ "WIN;anaysis complete",
    early & !late & spring & !winter ~ "SPR;analysis complete",
    early & !late & !spring & !winter ~ "UNK;SW-failed",
    !early & late & is.na(spring) & is.na(winter) ~ "FAL;analysis complete",
    early & late & is.na(spring) & is.na(winter) ~ "EL-HET;analysis complete",
    early & !late & is.na(spring) & is.na(winter) ~ "SPW;need ots16",
    !early & !late & is.na(spring) & is.na(winter) ~ "UNK;EL-failed"
  )) |>
  separate(sample_state, into=c("run", "sample_status"), sep = ";")
