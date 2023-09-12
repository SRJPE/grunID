# offline user workflow

# for each spreadsheet
offline_sherlock_results <- process_sherlock(
  filepath = "inst/sherlock_results_template.xlsx",
  sample_type = "mucus",
  layout_type = "single_assay_ots28_late",
  plate_size = 384)

offline_thresholds <- generate_threshold_offline(offline_sherlock_results$data)

offline_detections <- generate_assay_detection(offline_thresholds,
                                               offline_sherlock_results$data)

# for each spreadsheet
offline_sherlock_results_2 <- process_sherlock(
  filepath = "inst/sherlock_results_template.xlsx",
  sample_type = "mucus",
  layout_type = "single_assay_ots28_early",
  plate_size = 384)

offline_thresholds_2 <- generate_threshold_offline(offline_sherlock_results_2$data)

offline_detections_2 <- generate_assay_detection(offline_thresholds_2,
                                                 offline_sherlock_results_2$data)

# now compare

offline_assay_detections <- generate_genetic_detection(offline_detections, offline_detections_2)
