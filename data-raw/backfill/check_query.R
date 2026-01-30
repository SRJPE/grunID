library(tidyverse)

test <- read_csv("data-raw/backfill/results/genetics_query_for_dashboard_2022-2025_2026-01-28.csv") |>
  mutate(season = substr(sample_id, 4, 5))

# only 4 run names should exist
test |>
  distinct(final_run_designation)

# check for high numbers of unknowns
test |>
  group_by(season, final_run_designation) |>
  tally()

# test for gt seq results vs. sherlock results
test |>
  mutate(has_gtseq = ifelse(is.na(pop_structure_id), F, T),
         has_shlk = ifelse(is.na(shlk_run_designation), F, T)) |>
  group_by(season, has_gtseq, has_shlk) |>
  tally() |>
  ungroup()

test |>
  mutate(fake_year = ifelse(month(datetime_collected) %in% 10:12, 1970, 1971),
         fake_date = as.Date(paste0(fake_year, "-", month(datetime_collected), "-", day(datetime_collected))),
         season = as.factor(paste0("Season: 20", substr(sample_id, 4, 5)))) |>
  ggplot(aes(x = fake_date, fill = final_run_designation)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~season) +
  theme_minimal() +
  labs(x = "Datetime collected") +
  theme(legend.position = "bottom")

test |>
  mutate(season = substr(sample_id, 4, 5)) |>
  distinct(season, final_run_designation,
           gtseq_chr28_geno, pop_structure_id,
           shlk_chr28_genotype,
           shlk_chr16_genotype, shlk_run_designation
           ) |>
  arrange(season, final_run_designation) |>
  View()

test |>
  mutate(season = substr(sample_id, 4, 5)) |>
  filter(final_run_designation == "GREB1L HETEROZYGOTE") |>
  distinct(season, final_run_designation,
           gtseq_chr28_geno, pop_structure_id,
           shlk_chr28_genotype,
           shlk_chr16_genotype, shlk_run_designation,
           cv_fall, cv_late_fall, cv_spring, cv_winter) |>
  arrange(season, final_run_designation) |>
  View()
