library(tidyverse)

con <- gr_db_connect()

query <- glue::glue_sql("
                           SELECT
    --- gri.id AS genetic_run_id,
    gri.sample_id,
    s.event_number AS event,
    --- gri.run_type_id AS genetic_run_type_id,
    rt_genetic.run_name AS genetic_run_name,
    s.fork_length_mm,
    s.datetime_collected,
    rt_field.run_name AS field_run_name,
    gri.early_plate_id,
    gri.late_plate_id,
    gri.winter_plate_id,
    gri.spring_plate_id
    --- gri.created_at AS genetic_run_created_at
FROM (
    SELECT *,
           ROW_NUMBER() OVER (PARTITION BY sample_id ORDER BY created_at DESC) as rn
    FROM genetic_run_identification
) gri
JOIN run_type rt_genetic ON gri.run_type_id = rt_genetic.id
JOIN sample s ON gri.sample_id = s.id
LEFT JOIN run_type rt_field ON s.field_run_type_id = rt_field.id
WHERE gri.rn = 1
ORDER BY gri.sample_id;")

query_2 <-                            "
                           SELECT
    --- gri.id AS genetic_run_id,
    gri.sample_id,
    s.event_number AS event,
    --- gri.run_type_id AS genetic_run_type_id,
    rt_genetic.run_name AS genetic_run_name,
    s.fork_length_mm,
    s.datetime_collected,
    rt_field.run_name AS field_run_name,
    gri.early_plate_id,
    gri.late_plate_id,
    gri.winter_plate_id,
    gri.spring_plate_id
    --- gri.created_at AS genetic_run_created_at
FROM (
    SELECT *,
           ROW_NUMBER() OVER (PARTITION BY sample_id ORDER BY created_at DESC) as rn
    FROM genetic_run_identification
) gri
JOIN run_type rt_genetic ON gri.run_type_id = rt_genetic.id
JOIN sample s ON gri.sample_id = s.id
LEFT JOIN run_type rt_field ON s.field_run_type_id = rt_field.id
WHERE gri.rn = 1
ORDER BY gri.sample_id;
                           "

res <- DBI::dbGetQuery(con, query_2)
res |>
  glimpse()

res |>
  mutate(season = substr(sample_id, 4, 5)) |>
  group_by(season) |>
  summarise(rows = n(),
            na_rows = sum(if_any(datetime_collected, is.na)),
            pct_na = (na_rows / rows))

query_all <- res |>
  mutate(season = substr(sample_id, 4, 5)) |>
  filter(season != "-5") |>
  select(sample_id, genetic_run_name, datetime_collected, fork_length_mm, field_run_name)

write_csv(query_all, "~/Downloads/sample_query_for_badhia_11-18-2025.csv")
write_csv(query_all, "~/Downloads/sample_edi_query.csv")
