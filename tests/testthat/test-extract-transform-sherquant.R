fixture <- system.file("sherquant_results_template.xlsx", package = "grunID")
fake_plate_run <- structure(list(plate_run_id = 1L), class = "plate_run")

test_that("extract_sherquant_protocol parses metadata correctly", {
  md <- extract_sherquant_protocol(fixture)

  expect_equal(md$plate_size, 384)
  expect_equal(md$read_count, 41)
  expect_equal(md$runtime, hms::as_hms(2 * 3600))
  expect_match(md$instrument_type, "QuantStudio")
  expect_equal(md$chemistry, "TAQMAN")
})

test_that("process_well_sample_details_sherquant maps assay_id from Target Name", {
  details <- process_well_sample_details_sherquant(fixture, "mucus", 1L)

  expect_setequal(
    names(details$data),
    c("location", "sample_id", "assay_id", "plate_run_id", "sample_type_id")
  )
  expect_false(anyNA(details$data$assay_id))
  expect_equal(nrow(details$data), 48)
  # Early/Late/Spring/Winter assays are evenly represented (12 wells each)
  expect_equal(as.vector(table(details$data$assay_id)), rep(12, 4))
})

test_that("process_sherquant returns the expected raw_assay_result contract", {
  result <- process_sherquant(fixture, sample_type = "mucus", plate_run_id = fake_plate_run)

  expect_s3_class(result, "sherquant_output")
  expect_setequal(
    names(result$data),
    c("sample_id", "sample_type_id", "assay_id", "raw_fluorescence",
      "plate_run_id", "well_location", "background_value", "time", "sub_plate")
  )
  expect_equal(nrow(result$data), 48)
  expect_true(all(is.na(result$data$background_value)))
  expect_true(all(result$data$time == hms::as_hms(2 * 3600)))
  expect_true(all(result$data$sub_plate == 1))
  expect_false(anyNA(result$data$raw_fluorescence))
})

test_that("process_sherquant rejects a plate_run_id not created by add_plate_run", {
  expect_error(
    process_sherquant(fixture, sample_type = "mucus", plate_run_id = 1L)
  )
})

# --- Known gaps, documented so they don't regress silently -------------------
# These two tests are expected to PASS today. If either one starts failing,
# it means the underlying assumption has changed (e.g. a real production file
# or a code change), which is exactly when `new-machine-integration.md`'s open
# caveats about control-sample naming should get revisited.

test_that("KNOWN GAP: SherQuant control sample_ids don't match SHERLOCK's naming convention", {
  # SHERLOCK suffixes control replicates (NTC-1/2/3, NEG-DNA-1/2/3, ...) so that
  # (sample_id, assay_id) stays unique per plate. SherQuant's raw `Sample Name`
  # values carry no such suffix. validate_results() and the run_gen_id exclusion
  # list in add_new_plate_results() both key off the SHERLOCK naming convention,
  # so neither currently recognizes SherQuant's controls as controls.
  details <- process_well_sample_details_sherquant(fixture, "mucus", 1L)
  sherlock_style_controls <- c(
    "NTC-1", "NTC-2", "NTC-3", "NEG-DNA-1", "NEG-DNA-2", "NEG-DNA-3",
    "POS-DNA-1", "POS-DNA-2", "POS-DNA-3"
  )
  expect_true(length(intersect(details$data$sample_id, sherlock_style_controls)) == 0)
})

test_that("KNOWN GAP: replicate controls collide on (sample_id, assay_id)", {
  # Every control in this example file is replicated 3x (triplicate plate
  # quadrants) under the *same* sample_id and assay_id. add_raw_assay_results()
  # enforces uniqueness on (sample_id, assay_id) per plate_run, so running this
  # exact file through the full pipeline would fail with a duplicate-key error
  # on every one of these groups - not a soft QA/QC gap, a hard insert failure.
  # Confirm with a real production file whether SherQuant's sample_ids get a
  # per-replicate suffix (like SHERLOCK's) before relying on this path.
  result <- process_sherquant(fixture, sample_type = "mucus", plate_run_id = fake_plate_run)
  dup_groups <- result$data |>
    dplyr::count(sample_id, assay_id) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(dup_groups), 16)
  expect_equal(sum(dup_groups$n), nrow(result$data)) # every single row is in a colliding group
})
