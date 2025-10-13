#' @title Read GT-Seq
#' @export
read_gtseq <- function(filepath) {
  gtseq_data <- readr::read_tsv(filepath)
}

#' @export
insert_gtseq_raw_results <- function(con, gtseq_data) {

  # TODO need a cleaner way to do this
  sherlock_sample_ids <- tbl(con, "sample") |>
    select(id) |>
    collect()

  # data filtered to those with sherlock results in db (in "sample" table)
  insert_data <- gtseq_data |>
    filter(SampleID %in% sherlock_sample_ids$id)

  samples_not_inserted <- gtseq_data |>
    filter(!SampleID %in% sherlock_sample_ids$id)

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

  cli::cli_bullets(paste0(nrow(insert_data), " samples inserted into database. ",
                          nrow(samples_not_inserted), " samples not inserted; must be
                          inserted into the sample table first."))

   return("samples_not_inserted" = samples_not_inserted$SampleID)
}



# TODO:
# 1. store raw results
# send sampleid and popstructu id to run_identification_V2 function for assignment
