# Excel columns lookups
excel_column_index <- c(LETTERS,
                        expand.grid(LETTERS, LETTERS) %>%
                          transmute(columns = paste0(Var1, Var2)) %>%
                          pull(columns) %>%
                          sort())


usethis::use_data(excel_column_index)


evens <- seq(2, 24, by = 2)
odds <- seq(1, 24, by = 2)
plate_letters <- LETTERS[1:16]
plate_letters_odds <- as.character(na.omit(plate_letters[odds]))
plate_letters_evens <- as.character(na.omit(plate_letters[evens]))

plate_v4_mapping <- expand_grid(rows = LETTERS[1:16], cols = 1:24) |>
  mutate(idx = paste0(rows, cols),
         plate = case_when(
           cols %in% odds & rows %in% plate_letters_odds ~ 1,
           cols %in% odds & rows %in% plate_letters_evens ~ 3,
           cols %in% evens & rows %in% plate_letters_evens ~ 4,
           cols %in% evens & rows %in% plate_letters_odds ~ 2
         ))


usethis::use_data(plate_v4_mapping, overwrite = TRUE)
