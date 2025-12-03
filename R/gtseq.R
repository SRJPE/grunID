#' @title Read GT-Seq
#' @export
read_gtseq <- function(filepath) {
  gtseq_data <- readr::read_tsv(filepath, show_col_types = FALSE)
}

#' @export
insert_gtseq_raw_results <- function(con, gtseq_data) {

  # TODO need a cleaner way to do this
  db_sample_ids <- tbl(con, "sample") |>
    select(id) |>
    collect()

  # data filtered to those existing in db (in "sample" table)
  insert_data <- gtseq_data |>
    filter(SampleID %in% db_sample_ids$id)

  samples_not_inserted <- gtseq_data |>
    filter(!SampleID %in% db_sample_ids$id)

  if(nrow(samples_not_inserted) > 0) {
    cli::cli_alert_danger("Some samples are not present in the database. Please add them to the database
                          before continuing.")

  }

  values_clause <- paste(sprintf("('%s', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)",
                                 insert_data$SampleID,
                                 insert_data$Gtseq_Chr28_Geno,
                                 insert_data$Pop_Structure_ID,
                                 insert_data$CV_Fall,
                                 insert_data$CV_Late_Fall,
                                 insert_data$CV_Spring,
                                 insert_data$CV_Winter,
                                 insert_data$Tributary,
                                 insert_data$ButteFall,
                                 insert_data$FRHfall,
                                 insert_data$FRHsp,
                                 insert_data$MillDeerFall,
                                 insert_data$SanJoaquinFall,
                                 insert_data$ButteSp,
                                 insert_data$MillDeerSp,
                                 insert_data$ColemanLF,
                                 insert_data$SacWin, NA), collapse = ", ")

  insert_statements <- glue::glue_sql("INSERT INTO gtseq_results (sample_id, gtseq_chr28_geno, pop_structure_id, cv_fall, cv_late_fall, cv_spring, cv_winter, tributary, buttefall, frh_fall, frh_sp, mill_deer_fall, san_joaquin_fall, butte_sp, mill_deer_sp, coleman_f, sac_win, season) VALUES
                                     ({insert_data$SampleID}, {insert_data$Gtseq_Chr28_Geno}, {insert_data$Pop_Structure_ID},
                                     {insert_data$CV_Fall}, {insert_data$CV_Late_Fall}, {insert_data$CV_Spring},
                                     {insert_data$CV_Winter}, {insert_data$Tributary}, {insert_data$ButteFall},
                                     {insert_data$FRHfall}, {insert_data$FRHsp}, {insert_data$MillDeerFall},
                                     {insert_data$SanJoaquinFall}, {insert_data$ButteSp}, {insert_data$MillDeerSp},
                                     {insert_data$ColemanLF}, {insert_data$SacWin}, {2025} )", .con =con)

  res <- map(1:length(insert_statements), function(i) {
    res <- DBI::dbSendQuery(con, insert_statements[i])
    DBI::dbClearResult(res)
  },
  .progress = T)

  # update status to complete
  update_status_query <- glue::glue_sql("UPDATE sample_status
                                         SET status_code_id = '11'
                                         WHERE sample_id IN ({insert_data$SampleID*});",
                                        .con = con)

  DBI::dbExecute(con, update_status_query)

  cli::cli_bullets(paste0(nrow(insert_data), " samples inserted into database."))
  cli::cli_bullets(paste0(nrow(samples_not_inserted), " samples not inserted into database because they were not initialized in the database sample table."))

  return("samples_not_inserted" = samples_not_inserted$SampleID)
}

