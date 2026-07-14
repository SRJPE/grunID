# Hamilton Cherry Picking — How It Works

## Overview

`grunID` is an R package (see `DESCRIPTION`), not a web-service backend. The
"UI" is a **Shiny app** bundled at `inst/app/` (`ui.R` + `server.R` +
`global.R`), meant to be run locally (e.g. from RStudio) against the
prod/staging Postgres database. There is no `downloadHandler` in the app —
generated files are written directly to a local directory configured in
`config.yml` (`data_output`, e.g. `~/Downloads/grunid-224-testing`), and the
user gets a Shiny notification with the path rather than a browser download.

Entry point for launching the app: `run_app()` in `R/run-app.R:11-42`, which
calls `shiny::runApp(system.file("app", package = "grunID"))`.

## 1. Where the user generates the files (UI)

Tab: **"Generate Plate" → "Generate Hamilton Plate Layouts"**.

`inst/app/ui.R:224-234`:
```r
tabPanel("Generate Hamilton Plate Layouts",
  tags$div(
    style = "padding: 10px;",
    tagList(
      tags$p("Only events with at least one sample set to 'Need Ots 16' are selectable..."),
      uiOutput("gen_ham_plate_events_UI"),
      selectInput("gen_ham_destination", "Destnation", choices = c("Sherlock"="sherlock", "GT-Seq" = "gtseq")),
      actionButton("save_ham_plates", "Generate Cherry Pick Files"),
      tableOutput("gen_ham_plate_samples_preview")
    )
  )),
```

The user:
1. Selects one or more `event_number`s from a dropdown.
2. Picks a destination assay: `sherlock` or `gtseq`.
3. Reviews a preview table of candidate samples.
4. Clicks **"Generate Cherry Pick Files"**.

Supporting reactives that populate the dropdown and preview
(`inst/app/server.R:580-613`):
- `events_with_need_ots16_or_gtseq_samples` (580-583) — pulls current-season
  sample statuses filtered to `need ots16` / `need gtseq`.
- `need_ots16_or_gtseq_events` (585-589) — distinct event numbers from those.
- `output$gen_ham_plate_events_UI` (595-597) — renders the event `selectInput`.
- `output$gen_ham_plate_samples_preview` (607-613) — renders the preview table,
  filtered by the chosen destination and selected events.

## 2. The click handler

`inst/app/server.R:636-655`:
```r
observeEvent(input$save_ham_plates, {
  if (length(input$gen_ham_plate_events) == 0) {
    shiny::showNotification("you must select at least one event to generate a plate", type = "warning")
    return(NULL)
  }

  res <- make_sw_plate_maps(
    con,
    events = input$gen_ham_plate_events,
    destination = input$gen_ham_destination,
    output_dir = cfg$data_output,
    season = get_current_season()
  )

  if (res$success) {
    shiny::showNotification(glue::glue("file created and saved to: {cfg$data_output} ..."), type = "message")
  }
})
```

This is the Shiny equivalent of an API route handler — it validates the
selection and dispatches to the core generator, `make_sw_plate_maps()`.

(For comparison, the sibling "Generate Archive Plate Layouts" tab has its own
handler at `server.R:617-634` calling `make_archive_plate_maps_by_event()` —
a separate, earlier stage in the pipeline, not part of the cherry-pick path.)

## 3. Core generation logic — `R/plate-maps.R`

Entry function: **`make_sw_plate_maps(con, events, destination, season, output_dir)`**
— `R/plate-maps.R:637-915`. Doc comment: *"Plate maps will be generated for
input into the Hamilton cherry picking machine."*

### Step 1 — Get candidate samples

`get_sw_plates_candidates()` (`plate-maps.R:613-629`), called at
`plate-maps.R:651-656`.

- Pulls sample statuses via `get_sample_status()` (`R/sample-status.R:94-135`),
  filtered to `status_code_name %in% c("need ots16", "need gtseq")` and the
  selected `event_number`s.
- `destination == "gtseq"`: keeps samples where
  `status_code_name == "need gtseq"` OR `sample_location %in% c("KNL","TIS","DEL")`.
  (i.e. samples at these three locations are routed to GT-Seq even if they're
  still flagged "need ots16" — a lab-routing rule based on physical location.)
- `destination == "sherlock"`: keeps samples where
  `status_code_name == "need ots16"` AND NOT in `c("KNL","TIS","DEL")`.
- Returns a vector of `sample_id`.

### Step 2 — Look up each sample's archive-plate source location

Direct query at `plate-maps.R:660-662`:
```r
tbl(con, "sample_archive_plates") |> filter(sample_id %in% candidate_samples) |> collect()
```
This gives `sample_id, arc_plate_id, arc_well_id` — the physical archive-plate
and well that each candidate sample currently sits in. These rows exist because
they were written earlier by `insert_archive_plate_ids()` (`plate-maps.R:512-529`)
when an archive plate was registered via the "Register plates" tab
(`register_arc_plate()`, `plate-maps.R:1261-1273`).

If no archive-plate records are found for any candidate, the function
`stop()`s with an error listing the candidate sample IDs (`plate-maps.R:664-669`)
rather than silently producing an empty file.

### Step 3 — Sort and chunk samples into 96-sample groups

This is the part with the most non-obvious behavior, so it's worth walking
through carefully. From `plate-maps.R:695-710` (gtseq branch; the sherlock
branch at `781-796` is identical):

```r
archive_plate_for_candidates |>
  select(sample_id, arc_plate_id, arc_well_id) |>
  separate(arc_well_id, into = c("well_id_row", "well_id_col"), sep = 1, remove = FALSE) |>
  mutate(well_id_col = as.numeric(well_id_col)) |>
  arrange(arc_plate_id, well_id_col, well_id_row) |>
  select(-well_id_col, -well_id_row) |>
  mutate(grp = ceiling(row_number() / 96)) |>
  group_by(grp) |>
  mutate(destination_well_id = hamilton_letters[1:n()]) |>
  ungroup() |>
  group_by(grp) |>
  mutate(plate_id = dense_rank(arc_plate_id)) |>
  ungroup() |>
  ...
```

Breaking down what actually happens:

- **All candidate samples across every selected event are combined into one
  list first**, sorted by `arc_plate_id`, then well column, then well row
  (column-major order — `A1, B1, ... H1, A2, B2, ...` — matching physical
  plate fill order).
- `row_number()` is a **global**, sequential index over that entire sorted
  list. It does **not** reset per archive plate.
- `grp = ceiling(row_number() / 96)` slices that global list into consecutive
  chunks of (at most) 96 rows. Each `grp` becomes one output "Hamilton plate"
  — its own trio of files (`_CP_inputfile.txt`, `_CP_platekey.txt`, `_DNA.xlsx`),
  numbered `P1`, `P2`, `P3`, etc. via `groups_in_cherry_pick <- distinct(grp)`
  (`plate-maps.R:757` / `843`) and a `walk()` over each group value.

**What this means for >96 samples:** there's no cap and no error — more
samples just produce more numbered output files. 250 candidates → 3 groups
(96, 96, 58) → 3 sets of files (`P1`, `P2`, `P3`).

**The gotcha — groups can straddle archive plates.** Because the 96-row
cutoff is applied to the *global* sorted list, not per archive plate, a group
boundary does not necessarily land where one archive plate ends and the next
begins. Example: archive plate A has 60 samples, archive plate B has 40.
`grp=1` (rows 1–96) contains all 60 of A's samples *plus* the first 36 of B's
— mixing two physical source plates into a single Hamilton destination
plate/run.

This is exactly why `plate_id` is computed **within each group**, not
globally:
```r
group_by(grp) |> mutate(plate_id = dense_rank(arc_plate_id)) |> ungroup()
```
`dense_rank(arc_plate_id)` is scoped to `group_by(grp)`, so within `grp=1`
the first distinct archive plate encountered becomes `Plate1`, the second
becomes `Plate2`, and so on — **restarting at 1 for every group/output
file.** The final `PlateID` column (`paste0("Plate", plate_id)`,
`plate-maps.R:721` / `807`) is therefore only meaningful *within a single
output file*. `"Plate1"` in the `P1` platekey file and `"Plate1"` in the `P2`
platekey file are almost certainly two different physical archive plates —
never cross-reference `PlateID` between files without also checking
`arc_plate_id`.

**Destination well positions also reset per group.** `hamilton_letters`
(`plate-maps.R:687`) is the fixed 96-position sequence
`paste0(LETTERS[1:8], rep(1:12, each = 8))` = `A1, B1, ..., H1, A2, ..., H12`.
Because `destination_well_id = hamilton_letters[1:n()]` is also scoped
`group_by(grp)`, every group's destination plate starts fresh at `A1` —
correct, since each group represents a brand-new physical destination plate
on the Hamilton deck.

**Partial last group.** If the total candidate count isn't a multiple of 96
(e.g. 250 → last group has 58), that final group's `_CP_inputfile.txt` /
`_CP_platekey.txt` simply has fewer rows. For the visual `_DNA.xlsx` layout,
`make_plate_layout()` (`plate-maps.R:18-42`) pads the remainder with `NA`
(blank wells):
```r
pad_amount <- layout_size - length(samples)
raw <- matrix(c(samples, rep(NA, pad_amount)), nrow = 8, ncol = 12, byrow = FALSE)
```

> **Dead code note:** `plate-maps.R:871-909` contains a large commented-out
> block referencing different constants (`sample_cap_for_single_assay <- 368`,
> `sample_cap_for_dual_assay <- 176`) and an alternate single-vs-dual-assay
> splitting scheme. This is **not active** — the only chunk size in effect
> in the current code path is 96. Don't confuse it with real behavior if
> reading further into the file.

### Step 4 — Assemble the final columns

After chunking, each branch produces `hamilton_cherry_pick`
(`plate-maps.R:711-726` / `797-812`) with columns:

```
SampleID, PlateID, WellIDSource, WellIDDestination, grp, arc_plate_id
```

and a companion `plate_map_keys <- hamilton_cherry_pick |> distinct(arc_plate_id, PlateID, grp)`
(`plate-maps.R:728` / `814`).

### Step 5 — Write the visual plate-layout Excel files

Via `make_plate_layout()` (`plate-maps.R:18-42`, builds an 8x12 matrix) and
`write_layout_to_file()` (`plate-maps.R:548-564`, uses `openxlsx`), named:

```
JPE{season}_{events}_{GT|SW}_P{n}_DNA.xlsx
```

### Step 6 — Write the actual Hamilton input files (the key artifact)

For each `grp` value (`plate-maps.R:760-771` gtseq / `846-857` sherlock):

```r
platekey_filename  <- glue::glue("{output_dir}/JPE{season}_{events}_{label}_P{i}_CP_platekey.txt")
cp_input_filename  <- glue::glue("{output_dir}/JPE{season}_{events}_{label}_P{i}_CP_inputfile.txt")
write_csv(hamilton_cherry_pick |> filter(grp == i) |> select(-grp, -arc_plate_id), cp_input_filename)
write_csv(plate_map_keys |> filter(grp == i) |> select(-grp), platekey_filename)
```

- **`..._CP_inputfile.txt`** — the literal file the Hamilton robot consumes.
  Columns: `SampleID, PlateID, WellIDSource, WellIDDestination`. Despite the
  `.txt` extension it's comma-delimited (`readr::write_csv`).
- **`..._CP_platekey.txt`** — a lookup for that same group only, mapping the
  local `PlateID` (`Plate1`, `Plate2`, ...) back to the real `arc_plate_id`,
  columns `arc_plate_id, PlateID` (one row per distinct archive plate that
  contributed to this group — not one row per sample).

### Step 7 — Sherlock destination only: dual-assay layout

An additional dual-assay Sherlock plate layout is produced via
`make_dual_ots_16_from_cherry_pick()` (`plate-maps.R:1243-1257`) and written
to `..._SH.xlsx` (`plate-maps.R:862-868`).

### Step 8 — Return value

`list(success = TRUE, message = "process complete")` (`plate-maps.R:911-914`)
— this is what the Shiny handler checks before showing the "file created"
notification back in `server.R`.

### Notable gap: Hamilton plate assignment isn't persisted to the DB

`insert_hamilton_plate_ids()` (`plate-maps.R:532-544`) would `INSERT` into a
`sample_hamilton_plates` table (`sample_id -> ham_plate_id`), mirroring how
`insert_archive_plate_ids()` persists archive-plate assignments. It is
**defined but never called** anywhere in the codebase (verified via
repo-wide grep) — the Hamilton plate assignment produced by this feature is
written to the flat files only, not saved back to the database (unlike the
archive-plate equivalent, which *is* persisted from `register_arc_plate()`,
`plate-maps.R:1263`).

## 4. Generated files at a glance

Per plate group (one group per 96 samples, numbered `P1`, `P2`, ...):

| File | Naming pattern | Format | Contents |
|---|---|---|---|
| Cherry-pick input file | `JPE{season}_{events}_{GT\|SW}_P{n}_CP_inputfile.txt` | CSV (`.txt` ext) | `SampleID, PlateID, WellIDSource, WellIDDestination` — the actual robot input |
| Plate key file | `JPE{season}_{events}_{GT\|SW}_P{n}_CP_platekey.txt` | CSV | `arc_plate_id, PlateID` — maps this group's local `PlateID` labels back to real archive plates. **`PlateID` numbering restarts at 1 in every group's file** — don't compare `PlateID` across files without also checking `arc_plate_id` |
| Plate layout workbook | `JPE{season}_{events}_{GT\|SW}_P{n}_DNA.xlsx` | Excel (8x12 grid) | Human-readable visual layout of the source DNA plate |
| Sherlock dual-assay layout (Sherlock destination only) | `JPE{season}_{events}_SW_P{n}_SH.xlsx` | Excel (8x12 grid) | Visual layout for the dual OTS-16 Sherlock destination plate |

Where `{GT|SW}` is `GT` for GT-Seq or `SW` for Sherlock, and `{events}` is a
range label like `E12-15`.

## 5. Database objects touched

| Object | Role | Reference |
|---|---|---|
| `sample` / sample status join | Source of `sample_id`, `status_code_name`, `event_number`, `sample_location` | `R/sample-status.R:94-135` |
| `status_code` | Human-readable status names (`need ots16`, `need gtseq`) | joined in `get_sample_status` |
| `sample_archive_plates` | Maps `sample_id → arc_plate_id, arc_well_id` (source wells) | read at `plate-maps.R:660-662`; written by `insert_archive_plate_ids()` (`plate-maps.R:512-529`) |
| `sample_hamilton_plates` | Would map `sample_id → ham_plate_id`; **currently unused/dead code path** | `plate-maps.R:532-544` |

## 6. What comes after (not part of this feature)

Once the Hamilton run and downstream genotyping come back, results feed into
genetic-ID logic (`R/genetic-identification.R:608-722`), which computes fields
like `shlk_run_designation` and `shlk_chr28_genotype`/`shlk_chr16_genotype`.
That logic consumes the *results* of the Sherlock/GT-Seq assay run — it does
not participate in generating the cherry-pick input file itself.

## Quick file:line index

| Concern | File | Lines |
|---|---|---|
| Shiny app entry point | `R/run-app.R` | 11-42 |
| App bootstrap / DB connect / config | `inst/app/global.R` | 1-90 |
| UI tab & button | `inst/app/ui.R` | 224-234 |
| Event dropdown / preview reactives | `inst/app/server.R` | 580-613 |
| Button click handler | `inst/app/server.R` | 636-655 |
| Core generator function | `R/plate-maps.R` | 637-915 |
| Candidate sample query | `R/plate-maps.R` | 613-629 |
| Sample status query | `R/sample-status.R` | 94-135 |
| Archive plate DB read | `R/plate-maps.R` | 660-669 |
| Sort/chunk into 96-sample groups | `R/plate-maps.R` | 695-710 / 781-796 |
| GT-Seq cherry-pick build + file write | `R/plate-maps.R` | 689-774 |
| Sherlock/OTS-16 cherry-pick build + file write | `R/plate-maps.R` | 776-868 |
| Plate layout matrix builder | `R/plate-maps.R` | 18-42 |
| Excel writer helper | `R/plate-maps.R` | 548-564 |
| Dual OTS-16 Sherlock layout from cherry pick | `R/plate-maps.R` | 1243-1257 |
| Insert archive plate ids | `R/plate-maps.R` | 512-529 |
| Insert Hamilton plate ids (defined, unused) | `R/plate-maps.R` | 532-544 |
| Dead code: alternate single/dual-assay split (commented out) | `R/plate-maps.R` | 871-909 |
| Config file (output dir) | `config.yml` | 1-8 |
