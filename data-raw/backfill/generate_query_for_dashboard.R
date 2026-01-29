library(tidyverse)
library(grunID)

# generate query for data dashboard, bind together 2022-2024
# 2022-2024 are in staging as of Dec 2025; backfilled and query written in backfill.R
early_seasons_result <- read_csv("data-raw/backfill/results/genetics_query_for_dashboard_2022-2024_2026-01-28.csv") |>
  glimpse()

early_seasons_result |>
  mutate(season = substr(sample_id, 4, 5)) |>
  distinct(season)

# now 2025
# ENSURE CONNECTION IS TO PRODUCTION
con_prod <- gr_db_connect()

run_query <- generate_final_run_assignment(con_prod)
final_runs_2025 <- run_query$results |>
  mutate(season = substr(sample_id, 4, 5),
         coleman_f = as.numeric(coleman_f)) |>
  filter(season == 25) |>
  select(-season)

final_runs_2025 |>
  mutate(season = substr(sample_id, 4, 5)) |>
  distinct(season)

final_query_all_seasons <- bind_rows(early_seasons_result,
                                     final_runs_2025) |>
  mutate(final_run_designation = ifelse(final_run_designation == "UNKNOWN", "GREB1L HETEROZYGOTE", final_run_designation))

final_query_all_seasons |>
  mutate(fake_year = ifelse(month(datetime_collected) %in% 10:12, 1970, 1971),
         fake_date = as.Date(paste0(fake_year, "-", month(datetime_collected), "-", day(datetime_collected))),
         season = as.factor(paste0("Season: 20", substr(sample_id, 4, 5)))) |>
  ggplot(aes(x = fake_date, fill = final_run_designation)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~season) +
  theme_minimal() +
  labs(x = "Datetime collected") +
  theme(legend.position = "bottom")


write_csv(final_query_all_seasons, paste0("data-raw/backfill/results/genetics_query_for_dashboard_2022-2025_", Sys.Date(), ".csv"))
