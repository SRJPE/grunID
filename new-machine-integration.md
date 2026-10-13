# Support new fluorescence-reader machine

Background: today `raw_assay_result` is fed exclusively by the Synergy H1 / SHERLOCK
pipeline (`R/extract-transform-sherlock.R` → `R/load-sherlock.R`). Everything
downstream of `raw_assay_result` (thresholding, QA/QC, genetic ID assignment) is
already instrument-agnostic. The goal of this issue is to add a second ingest path
for the new machine's output file(s) without touching steps 8–14 of the existing
pipeline.

New machine / file format is **SherQuant** (QuantStudio-style qPCR export), example
file: `data-raw/sherquant/2025-02-05 101813 SherQuant Tissue Run 1 PL 1.xlsx`
(also committed as the test fixture `inst/sherquant_results_template.xlsx`).

**Status: code, DB schema, tests, and docs are in place. Blocked only on getting
a real (non-controls-only) production file to validate the open assumptions
below.**

## 0. Discovery — done

- [x] Example output file obtained (controls-only validation plate; still need
      a real production plate — see "Open assumptions" below)
- [x] Confirmed kinetic series vs. endpoint: has a real per-cycle series, but we
      only use the max-cycle row as the endpoint read (team decision)
- [x] Confirmed no background subtraction — raw `FAM` used as-is
- [x] Confirmed plate geometry — 384-well, same `A1...` convention as SHERLOCK
- [x] Confirmed file format — xlsx, 4 sheets, long/tidy format, 45-row metadata
      block repeated at the top of every sheet
- [x] Confirmed no embedded layout/grid — well → sample mapping comes directly
      from `Sample Setup`'s `Well Position`/`Sample Name` columns
- [x] Confirmed assay set unchanged — `Target Name` gives the assay per well
      directly, no `layout_type` needed
- [x] Instrument/genetic_method naming decided and added to the DB (team confirmed)

### Decisions locked in
- Raw `FAM` values, no background subtraction.
- 41 cycles = 2 hour runtime (hardcoded single data point).
- No `layout_type` — assay comes from `Target Name` via direct lookup.
- `run_mode = "Standard Curve"` (real value, read from the file's `Experiment
  Type` field — not yet wired into `extract_sherquant_protocol()`'s parsed
  field list, currently only used in the manually-drafted protocol row).
- `software_version` has no real value anywhere in the file — set to
  `"unknown"` as an explicit placeholder pending the real QuantStudio software
  version from the team.
- Protocol row **confirmed by the team** and added to the `protocol` table.

## 1. Extract/transform module — done

- [x] `R/extract-transform-sherquant.R`: `process_sherquant()`,
      `process_well_sample_details_sherquant()`, `print.sherquant_output()` —
      all bugs fixed, verified against the fixture file (see Testing below)
- [x] `extract_sherquant_protocol()` (`R/helpers.R`) — parses `plate_size`,
      `read_count`, the 41-cycles→2hr `runtime` rule, and the rest of the
      metadata fields
- [x] `background_value = NA_real_`, `time = protocol$runtime` for every row
- [x] `sub_plate` hardcoded to `1` for every row (see "Open assumptions")

## 2. Pluggable orchestrator — done

- [x] `instrument` argument added to `add_new_plate_results()` (`R/plate-run.R`),
      validated against `c("sherlock", "sherquant")`
- [x] Dispatches via `switch()` to `process_sherlock()` vs `process_sherquant()`
- [x] Shared result variable renamed `reader_results_event` (also updated in
      `data-raw/user-workflow.R`)

## 3. Lookup tables / schema — done

- [x] `genetic_method` row added
- [x] `protocol` row added, team-confirmed values — confirmed present in
      **production** (`id = 10`) as of 2026-10-13. (Earlier attempts silently
      failed against production while schema fixes were accidentally applied
      to staging instead — if the protocol dropdown still looks empty for
      SherQuant in the app, restart the Shiny process so `global.R` reloads
      `all_protocols` with this row.)
- [x] `is_valid_protocol()` (`R/lu-protocol.R`) relaxed: `run_mode` now accepts
      `"Kinetic"` or `"Standard Curve"`; `optics`/`light_source`/`lamp_energy`
      now also accept `NA` (SHERLOCK's original values still required when
      non-`NA`, so existing SHERLOCK validation is unaffected)
- [x] DB schema changes required to actually insert the row (run manually
      against the DB, not tracked as a repo migration since this project has no
      migrations folder):
  - `add_enum_value(con, "run_mode_enum", "Standard Curve")`
  - `ALTER TABLE protocol ALTER COLUMN <col> DROP NOT NULL` for `set_point`,
    `preheat_before_moving`, `interval`, `excitation`, `emissions`, `optics`,
    `gain`, `light_source`, `lamp_energy`, `read_height` — all fields that
    don't apply to a qPCR reader

## 4. Threshold + QA/QC parameterization — partially done

- [x] Placeholder RFU cutoff: `sherquant_rfu_threshold_placeholder <- 20000`
      (`R/plate-run.R`), documented as a stand-in
- [ ] Not yet wired into `validate_results()`'s actual logic — needs an
      `instrument` column on `plate_run` first
- [ ] Calibrated RFU cutoff still pending from the wet lab team — needs a real
      production plate with known positive/negative results to calibrate against

## 5. Plate map generation — skipped

Plate geometry and control layout unchanged from SHERLOCK; contingent on the
sub_plate/pooling question below if that assumption turns out to be wrong.

## 6. Shiny app — done

- [x] Instrument selector in `inst/app/ui.R`
- [x] `layout_type`/`plate_size` hidden via `conditionalPanel` when SherQuant
      is selected
- [x] `inst/app/server.R` dispatches on `input$instrument`, passes
      `layout_type`/`plate_size` as `NULL` for SherQuant
- [x] Confirmed `inst/app/global.R`'s `genetic_method`/`protocol` dropdowns
      aren't SHERLOCK-filtered, so the new rows show up automatically
- [x] `protocol`, `genetic_method`, and `control_blank` dropdowns now actually
      filter/default based on the selected instrument (`server.R`'s
      `observeEvent(input$instrument, ...)`): `protocol` filters on
      `reader_type` ("Synergy" vs "QuantStudio"); `control_blank` is limited to
      `"NTC"` for SherQuant (`"NEG-DNA"`/`"EBK"` don't exist as sample names in
      SherQuant output, so `generate_threshold()` would error on them); a
      help-text warning shows under `control_blank` for SherQuant noting this
      is provisional

## 7. Tests and fixtures — done

- [x] `inst/sherquant_results_template.xlsx` committed (the controls-only
      example file)
- [x] `tests/testthat/test-extract-transform-sherquant.R` — covers
      `extract_sherquant_protocol()`, `process_well_sample_details_sherquant()`,
      `process_sherquant()`'s full output contract, and the `plate_run` class
      guard. All pass against the fixture (20/20).
- [x] Two gaps documented as explicit "KNOWN GAP" tests rather than silently
      passed over (see "Open assumptions" below) — these are expected to pass
      *today*; if either starts failing it means the underlying assumption
      changed and these notes should be revisited
- [x] `tests/testthat/test-plate-run-instrument-dispatch.R` — instrument
      validation, `layout_type` required only for SHERLOCK, SherQuant exempt
- [x] Added `RSQLite` and `mockery` to `DESCRIPTION` Suggests (both were
      already relied on by the existing test suite without being declared)
- Note: a full end-to-end test (`process_sherquant()` →
  `add_raw_assay_results()` → `generate_threshold()` → `validate_results()` →
  `run_genetic_identification_v3()`) is still blocked on a real production
  file — see below
- **Found while building tests, unrelated to this work:** running the full
  suite (`test_dir()`) surfaces pre-existing failures in `test-protocol.R` and
  `test-run-type.R` (a `cli::cli_abort()` message-format mismatch, and a
  `protocol_template` class mismatch likely from an old `stringsAsFactors`
  serialization). Confirmed these fail identically without any of the
  SherQuant test files present — not something introduced here, left alone.

## 8. Docs — done

- [x] Added a "SherQuant Workflow" section to
      `vignettes/process_and_add_assay_results.Rmd` (rather than a separate
      vignette, since the workflow is a small diff from SHERLOCK's) covering
      `add_new_plate_results(instrument = "sherquant", ...)`, the multi-step
      equivalent, and the open caveats below
- [x] `devtools::document()` run — `NAMESPACE` and `man/*.Rd` regenerated for
      all new exports

## Open assumptions — the reason we're not fully done

These are the specific things to confirm once a real production SherQuant file
is available. Everything else (code, schema, tests, docs) is ready to go.

1. **`sample_id` mapping:** we assume `Sample Setup`'s `Sample Name` is the real
   sample_id. Only verified against a controls-only file so far.
2. **Control sample naming:** SherQuant's control wells keep the instrument's
   raw `Sample Name` (`"NTC"`, `"(EMG)+"`, etc.) rather than SHERLOCK's suffixed
   convention (`"NTC-1"`, `"NEG-DNA-1"`). Concretely, every replicate control in
   the example file shares an identical `(sample_id, assay_id)` across its 3
   replicate wells — this would hit `add_raw_assay_results()`'s uniqueness
   constraint and fail outright, not just silently skip QA/QC checks. Need to
   confirm whether real production control wells get a disambiguating suffix.
3. **`sub_plate`:** hardcoded to `1` everywhere, assuming SherQuant plates
   aren't pooled from multiple 96-well source plates the way some SHERLOCK
   plates are. Unconfirmed.
4. **RFU cutoff:** placeholder value, not calibrated, not wired into
   `validate_results()`.
5. **Genetic method code:** `server.R`'s instrument-based default for the
   `genetic_method` dropdown assumes the code added for SherQuant is
   `"SHQT"` — unconfirmed against the actual DB value (no DB access to check
   from here). If it's different, update the `selected = "SHQT"` line in the
   `observeEvent(input$instrument, ...)` block in `inst/app/server.R`.
