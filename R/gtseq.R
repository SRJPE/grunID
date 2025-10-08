#' @title Read GT-Seq
#' @export
read_gtseq <- function(filepath) {
  gtseq_data <- readr::read_tsv(filepath)
}

#' @export
insert_gtseq_raw_results <- function(con, gtseq_data) {
  values_clause <- paste(sprintf("('%s', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)",
                                 gtseq_data$SampleID, gtseq_data$Gtseq_Chr28_Geno, gtseq_data$Pop_Structure_ID,
                                 gtseq_data$CV_Fall, gtseq_data$CV_Late_Fall, gtseq_data$CV_Spring,
                                 gtseq_data$CV_Winter, gtseq_data$Tributary, gtseq_data$ButteFall,
                                 gtseq_data$FRHfall, gtseq_data$FRHsp, gtseq_data$MillDeerFall,
                                 gtseq_data$SanJoaquinFall, gtseq_data$ButteSp, gtseq_data$MillDeerSp,
                                 gtseq_data$ColemanLF, gtseq_data$SacWin, NA), collapse = ", ")

  insert_statements <- glue::glue_sql("INSERT INTO gtseq_results (sample_id, gtseq_chr28_geno, pop_structure_id, cv_fall, cv_late_fall, cv_spring, cv_winter, tributary, buttefall, frh_fall, frh_sp, mill_deer_fall, san_joaquin_fall, butte_sp, mill_deer_sp, coleman_f, sac_win, season) VALUES
                                    ( {gtseq_data$SampleID}, {gtseq_data$Gtseq_Chr28_Geno}, {gtseq_data$Pop_Structure_ID},
                                 {gtseq_data$CV_Fall}, {gtseq_data$CV_Late_Fall}, {gtseq_data$CV_Spring},
                                 {gtseq_data$CV_Winter}, {gtseq_data$Tributary}, {gtseq_data$ButteFall},
                                 {gtseq_data$FRHfall}, {gtseq_data$FRHsp}, {gtseq_data$MillDeerFall},
                                 {gtseq_data$SanJoaquinFall}, {gtseq_data$ButteSp}, {gtseq_data$MillDeerSp},
                                 {gtseq_data$ColemanLF}, {gtseq_data$SacWin}, {2025} )", .con =con)

  res <- map(1:length(insert_statements), function(i) {
    print(paste0("at step ", i))
    print(insert_statements[i])
    res <- DBI::dbSendQuery(con, insert_statements[i])
    DBI::dbClearResult(res)
  })
   return(res)
}



# TODO:
# 1. store raw results
# send sampleid and popstructu id to run_identification_V2 function for assignment
