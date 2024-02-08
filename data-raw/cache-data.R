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

# this keeps track of where a sample goes (in sample blocks, small clusters of 4)
plating_blocks <- tibble("rows" = c(rep(c("A", "A", "B", "B"), 12),
                                    rep(c("C", "C", "D", "D"), 12),
                                    rep(c("E", "E", "F", "F"), 12),
                                    rep(c("G", "G", "H", "H"), 12),
                                    rep(c("I", "I", "J", "J"), 11),
                                    rep(c("K", "K", "L", "L"), 11),
                                    rep(c("M", "M", "N", "N"), 11),
                                    rep(c("O", "O", "P", "P"), 11)),
                         "cols" = c(rep(c(1, 2, 1, 2, 3, 4, 3, 4, 5, 6, 5, 6, 7, 8,
                                        7, 8, 9, 10, 9, 10, 11, 12, 11, 12, 13, 14,
                                        13, 14, 15, 16, 15, 16, 17, 18, 17, 18, 19,
                                        20, 19, 20, 21, 22, 21, 22, 23, 24, 23, 24), 4),
                                    rep(c(1, 2, 1, 2, 3, 4, 3, 4, 5, 6, 5, 6, 7, 8,
                                          7, 8, 9, 10, 9, 10, 11, 12, 11, 12, 13, 14,
                                          13, 14, 15, 16, 15, 16, 17, 18, 17, 18, 19,
                                          20, 19, 20, 21, 22, 21, 22), 4)),
                         "sample_blocks" = as.character(rep(1:92, each = 4)))

control_blank_mapping <- tibble("control_blocks" = c("EBK-1-1", "EBK-2-1", "EBK-3-1", "EBK-4-1",
                                                     "NEG-DNA-1", "POS-DNA-1", "NEG-DNA-2", "POS-DNA-2",
                                                     "NEG-DNA-3", "POS-DNA-3", NA, "NTC-1", NA,
                                                     "NTC-2", NA, "NTC-3"),
                                "rows" = c(rep("I", 2), rep("J", 2), rep("K", 2),
                                           rep("L", 2), rep("M", 2), rep("N", 2),
                                           rep("O", 2), rep("P", 2)),
                                "cols" = rep(c(23, 24), 8))

plate_v4_mapping <- expand_grid(rows = LETTERS[1:16], cols = 1:24) |>
  mutate(idx = paste0(rows, cols),
         plate = case_when(
           cols %in% odds & rows %in% plate_letters_odds ~ 1,
           cols %in% odds & rows %in% plate_letters_evens ~ 3,
           cols %in% evens & rows %in% plate_letters_evens ~ 4,
           cols %in% evens & rows %in% plate_letters_odds ~ 2
         )) |>
  left_join(plating_blocks) |>
  left_join(control_blank_mapping) |>
  mutate(sample_blocks = ifelse(is.na(sample_blocks), control_blocks, sample_blocks)) |>
  select(-control_blocks)


usethis::use_data(plate_v4_mapping, overwrite = TRUE)
