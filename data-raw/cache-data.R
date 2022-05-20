# Excel columns lookups
excel_column_index <- c(LETTERS,
                        expand.grid(LETTERS, LETTERS) %>%
                          transmute(columns = paste0(Var1, Var2)) %>%
                          pull(columns) %>%
                          sort())


usethis::use_data(excel_column_index)
