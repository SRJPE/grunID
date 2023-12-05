get_all_thresholds_funcs <- function(con) {
  statement = glue::glue_sql("SELECT name, description, func_text as fn, created_by FROM threshold_strategy;", .con = con)
  res <- DBI::dbGetQuery(con, statement)

  return(res)

}

