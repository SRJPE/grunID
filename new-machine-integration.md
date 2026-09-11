# Support new fluorescence-reader machine

Background: today `raw_assay_result` is fed exclusively by the Synergy H1 / SHERLOCK
pipeline (`R/extract-transform-sherlock.R` → `R/load-sherlock.R`). Everything
downstream of `raw_assay_result` (thresholding, QA/QC, genetic ID assignment) is
already instrument-agnostic. The goal of this issue is to add a second ingest path
for the new machine's output file(s) without touching steps 8–14 of the existing
pipeline.

New machine / file format is **SherQuant** (QuantStudio-style qPCR export), example
file: `data-raw/sherquant/2025-02-05 101813 SherQuant Tissue Run 1 PL 1.xlsx`.

## 0. Discovery (do first, blocks everything else)

- [x] Get a real sample output file from the new machine — have one, but it's a
      controls-only validation/QC plate (NTC + marker +/- controls only, no real
      fish sample IDs). Still need an actual production plate to confirm the
      `Sample Name` → `sample_id` mapping (see item 1 below).
- [x] Confirm: kinetic time series or single endpoint read? — Neither cleanly:
      `Multicomponent Data` has a real per-cycle series (Cycle 1–41, `FAM`/`ROX`
      per well), but per team decision we only use the max-cycle row as the
      endpoint read (see decisions below).
- [x] Confirm: background/blank read? — **Decision: use raw `FAM` values as-is,
      no background subtraction.** `background_value` is always `NA_real_` for
      SherQuant, same code path SHERLOCK already uses when there are no BLK wells.
- [x] Confirm plate geometry — 384-well (`Block Type` = "384-Well Block"), same
      `A1...` well-numbering convention as SHERLOCK.
- [x] Confirm file format — xlsx, 4 sheets (`Sample Setup`, `Amplification Data`,
      `Multicomponent Data`, `Results`), long/tidy format (one row per well, or
      per well×cycle) — NOT the fixed-cell-offset wide format SHERLOCK uses. One
      file per plate, same 45-row metadata block repeated at the top of every sheet.
- [x] Confirm: does the file embed its own plate layout/grid? — No `Layout`
      section and no `plate_map` sheet convention. Well → sample mapping comes
      directly from `Sample Setup`'s `Well Position`/`Sample Name` columns
      (long format, no pseudo-ID layer like SHERLOCK's `SPL1`/`BLK`).
- [x] Confirm assay set / layout types — assay set is the same (OTS28
      early/late, OTS16 spring/winter). **Decision: no `layout_type` needed** —
      `Sample Setup`'s `Target Name` column (`Early`/`Late`/`Spring`/`Winter`)
      gives the assay per well directly; map it straight to `assay_id` instead
      of inferring from well position.
- [ ] Decide instrument/genetic_method naming (new `genetic_method` row, or reuse existing?)

### Decisions locked in (2026-09)
- Raw `FAM` values, no background subtraction.
- **41 cycles = 2 hour runtime.** Hardcoded single data point for now — if a
  plate ever comes back with a different cycle count we don't yet know its
  runtime; revisit if/when that happens.
- No `layout_type` — assay comes from `Target Name` via a direct lookup
  (`Early` → 1, `Late` → 2, `Spring` → 3, `Winter` → 4).
- Placeholder RFU cutoff added (`sherquant_rfu_threshold_placeholder <- 20000`
  in `R/plate-run.R`) — not wired into `validate_results()` yet. Team is
  providing a calibrated value once we can show them results from a real
  production plate (see item 4).
- Still unconfirmed: whether `Sample Name` in a real production run actually
  holds the true `sample_id`, or whether we still need an external mapping
  step. Blocked on getting a non-controls-only output file.
- Still unconfirmed: whether SherQuant plates get pooled from multiple 96-well
  source plates the same way SHERLOCK's do. Code currently hardcodes
  `sub_plate = 1` for every row (see item 1) pending an answer.

## 1. New extract/transform module — mostly done, a few bugs to fix

- [x] Create `R/extract-transform-sherquant.R`
- [x] `extract_sherquant_protocol()` (`R/helpers.R`) — parses `plate_size`,
      `read_count`, the 41-cycles→2hr `runtime` rule, and the rest of the
      SherQuant metadata fields into a tibble, mirroring
      `extract_sherlock_protocol()`'s shape.
- [x] `process_sherquant()` (`R/extract-transform-sherquant.R`) — the three
      original bugs are fixed (`process_sherquant <- function(...)` syntax,
      `max_cycle` referencing `raw_results$cycle`, `skip = 46`), and it now
      returns `sample_id, sample_type_id, assay_id, plate_run_id,
      raw_fluorescence, background_value, time, well_location, sub_plate`,
      wrapped in a `"sherquant_output"` object with a working `print` method.
      Two things still need fixing:
  - **Bug:** the final `structure(list(...), ..., plate_size = metadata$plate_size, sub_plate)`
    call has a bare, unnamed `sub_plate` at the end — there's no local
    variable called `sub_plate` in `process_sherquant()`'s scope (it only
    exists as a column inside `raw_assay_results`), so this will throw
    `object 'sub_plate' not found` at runtime. Just remove that trailing
    `sub_plate` from the `structure()` call.
  - **Cleanup:** the old stub's leftover `# needs to return...` comment and
    `return(data)` after the real `return(...)` are dead code (unreachable,
    and `data` no longer exists) — delete both.
  - Minor nit: `case_when` inside `process_well_sample_details_sherquant()`'s
    `mutate()` isn't namespaced (`dplyr::case_when`), unlike everything else
    in the file.
- [x] Write `process_well_sample_details_sherquant()` — reads `Sample Setup`,
      maps `location`/`sample_id`/`assay_id` (via `Target Name`), no
      `layout_type` arg, output matches `expected_layout_colnames()`.
- [x] Join endpoint-FAM table to sample-details table by well
- [x] `background_value = NA_real_`, `time = protocol$runtime` — both decided and implemented
- [ ] `sub_plate` is hardcoded to `1` for every row with a `# TODO confirm we
      won't have sub-plates?` comment — **not yet joined to
      `dual_assay_plate_mapping_V1`/`single_assay_plate_mapping_V5`.** This is
      fine as a placeholder as long as SherQuant plates are never pooled from
      multiple 96-well source plates; if they are, this needs the real join
      like SHERLOCK does. Confirm with the wet lab team.

## 2. Make the reader pluggable in the orchestrator — done

- [x] Added `instrument` argument to `add_new_plate_results()` (`R/plate-run.R`),
      validated against `c("sherlock", "sherquant")`, with a guard that
      `layout_type` is required when `instrument == "sherlock"`
- [x] Branches via `switch(instrument, "sherlock" = process_sherlock(...), "sherquant" = process_sherquant(...))`
      inside the existing `tryCatch`/plate-run-cleanup-on-error wrapper
- [x] Renamed the shared result variable from `sherlock_results_event` to
      `reader_results_event` (also updated in `data-raw/user-workflow.R`)
- [ ] `sherlock_output` class itself is still named `sherlock_output`, not
      renamed to something instrument-neutral — low priority since
      `"sherquant_output"` already exists as its own class rather than a
      subclass, so nothing currently depends on a shared neutral name
- Typo to fix: `logger::log_info("Processing {instrumnet} data")` —
  `instrumnet` → `instrument` (this will error at runtime since `logger`
  glue-interpolates the string and no variable named `instrumnet` exists)

## 3. Lookup tables / schema

- [ ] Add new `genetic_method` row if needed (`add_genetic_method()`, `R/lu-genetic-method.R:53`)
- [ ] Add new `protocol` row(s) via `add_protocol()` / `add_protocol_based_on()` (`R/lu-protocol.R`)
- [ ] `is_valid_protocol()` (`R/lu-protocol.R:328-347`) hard-rejects
      `run_mode != "Kinetic"`, `optics != "Top"`, `light_source != "Xenon Flash"`
      — none of these fields exist in SherQuant's metadata (it has `Passive
      Reference`, `Quantification Cycle Method`, `Chemistry`, etc. instead).
      Needs a relaxed/extended check or a separate validation path.
- [ ] Check `protocol_template` class-matching in `is_valid_protocol()` still
      works, or write a SherQuant-specific template
- [ ] Consider adding an `instrument` column on `plate_run` — needed once both
      machines share result tables and you want to filter/re-threshold by
      instrument (this is also the prerequisite for item 4 below)

## 4. Threshold + QA/QC parameterization

- [x] Confirmed new machine's fluorescence values are **not** on a comparable
      scale to SHERLOCK RFUs (raw, unsubtracted qPCR fluorescence vs. SHERLOCK's
      isothermal RFUs) — cutoff must be parameterized per instrument
- [x] Placeholder RFU cutoff added: `sherquant_rfu_threshold_placeholder <- 20000`
      in `R/plate-run.R`, documented as a stand-in pending a calibrated value
      from the wet lab team
- [ ] Wire the placeholder into `validate_results()`'s actual logic — blocked
      on item 3's `instrument`/`reader_type` dispatch existing
- [ ] Confirm `generate_threshold()` (`R/load-sherlock.R`)'s control-well math
      (built around `BLK` wells) works for SherQuant, which has `NTC` +
      marker-specific +/- controls instead of `BLK` — may need a SherQuant-specific
      threshold `strategy`
- [ ] Get a calibrated RFU cutoff from the team once we can show them a plate
      with real positive/negative production results (blocked on the same
      real-production-file gap as item 0/1)

## 5. Plate map generation (only if layout/controls differ)

- [ ] **Skip for now** — plate geometry (384-well, `A1...`) and control layout
      are unchanged from SHERLOCK, no new subplate mapping table needed
      (contingent on the sub_plate/pooling question in item 1).

## 6. Shiny app — done

- [x] Added instrument selector to `inst/app/ui.R` (`selectInput("instrument",
      ...)`, right under "Enter Plate Run")
- [x] Wrapped `layout_type` (and its info button + custom-layout help text) and
      `plate_size` in `shiny::conditionalPanel(condition = "input.instrument == 'sherlock'", ...)`
      so they're hidden when SherQuant is selected
- [x] Branched the `add_new_plate_results()` calls in `inst/app/server.R`
      (`observeEvent(input$yes_upload | input$no_upload, ...)`) on `input$instrument`:
      passes `instrument = input$instrument`, and passes `layout_type`/`plate_size`
      as `NULL` when instrument isn't `"sherlock"` (the "custom layout" branch
      is now also gated on `input$instrument == "sherlock"`)
- [x] Updated the upload-confirmation modal to show the selected instrument,
      and only show "Layout Selected" when instrument is `"sherlock"`
- [ ] Not done / out of scope for this pass: the `fileInput("sherlock_results", ...)`
      id/label and the tab title ("Upload Sherlock Results") still say
      "Sherlock" even though the tab now accepts both instruments — cosmetic,
      left alone to avoid touching the (unrelated, pre-existing) references to
      `input$sherlock_results` elsewhere in `server.R`
- Pre-existing, unrelated bug noticed while in here: `input$custom_layout_file`
  is referenced in `server.R` but there's no corresponding
  `fileInput("custom_layout_file", ...)` in `ui.R` — the custom-layout upload
  path looks broken independent of anything in this effort.

## 7. Tests and fixtures

- [ ] Commit a real SherQuant output as `inst/sherquant_results_template.xlsx`
      (analogous to `inst/sherlock_results_template.xlsx`) — ideally a real
      production plate once we have one, not just the controls-only example
      currently in `data-raw/sherquant/`
- [ ] Write `tests/testthat/test-load-sherquant.R` (model: `tests/testthat/test-load-sherlock.R`)
- [ ] End-to-end test: `process_sherquant()` → `add_raw_assay_results()` →
      `generate_threshold()` → `add_plate_thresholds()` → `validate_results()` →
      `run_genetic_identification_v3()` against a test DB

## 8. Docs

- [ ] Update/add a vignette analogous to `vignettes/process_and_add_assay_results.Rmd`
      for the SherQuant workflow
- [ ] Document new `layout_type`/instrument options in relevant `@param` roxygen docs
