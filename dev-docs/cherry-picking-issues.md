# Cherry Picking — Known Issues

Tracking issues found while reviewing the Hamilton cherry-pick workflow
("Generate Plate" → "Generate Hamilton Plate Layouts" in the Shiny app, backed by
`make_sw_plate_maps()` in `R/plate-maps.R`). See `cherry-picking-summary.md` for
how the workflow works end to end.

## Open

- [ ] **Help text on the tab is wrong** (`inst/app/ui.R:228`)
  - Says generating "will associate the given sample to the hamilton plate name
    generated", but nothing is written to the database
    (`insert_hamilton_plate_ids()`, `R/plate-maps.R:532-544`, is never called).
  - Only mentions "Need Ots 16"; the dropdown also includes `need gtseq` events.
  - **Plan:** update the text.

- [ ] **Candidates without an archive-plate record are silently dropped**
  (`R/plate-maps.R:660-669`)
  - `make_sw_plate_maps()` only `stop()`s when *no* candidate has an
    archive-plate record. Any individual candidate missing one is skipped with no
    warning.
  - **Plan:** address — surface missing samples to the user.

- [ ] **A sample on more than one archive plate is picked more than once**
  (`R/plate-maps.R:660-662`)
  - `sample_archive_plates` is keyed on `(sample_id, arc_plate_id)`, so one sample
    can have several rows, and each row becomes a separate pick.
  - **Plan:** address — decide which plate to pick from (or error).

- [ ] **Archive plates are sorted as text** (`R/plate-maps.R:689`, `775`)
  - `arrange(arc_plate_id, ...)` sorts as text, so `_P10_` comes before `_P2_`.
    This changes how plates are packed into 96-sample groups.
  - **Plan:** address, or confirm the current order is intentional.

- [ ] **An error in the generator disconnects the Shiny app**
  (`inst/app/server.R:636-655`)
  - The `save_ham_plates` handler has no `tryCatch`, so the `stop()` in
    `make_sw_plate_maps()` ends the session.
  - **Plan:** address — wrap in `tryCatch` and show an error notification.

- [ ] **GT-Seq and Sherlock branches are near-duplicates that have drifted**
  (`R/plate-maps.R:680-869`)
  - The GT-Seq branch records file paths with `<<-`; the Sherlock branch uses
    `<-`, so its "Saving … to" messages print `0`s.
  - Both print `nrow(data)`, where `data` is R's built-in function rather than
    the samples, so the sample-count message is blank.
  - Dead code: `plate_to_arc_plate_lookup`, `events_name`, `n_layout_groups`,
    `group_ids`, and the large commented-out single/dual-assay block
    (`R/plate-maps.R:871-909`).
  - **Plan:** address — merge into one code path that varies only by destination
    label and the extra Sherlock `_SH.xlsx` output.

## Noted (no action planned yet)

- **Event range in file names may depend on an unconfirmed column**
  (`R/plate-maps.R:671-680`)
  - The `E{min}-{max}` label comes from `sample_archive_plates.event_number`, but
    `insert_archive_plate_ids()` never writes that column. It may be generated in
    the database; the schema isn't in this repo.

- **The preview doesn't match what gets generated**
  (`inst/app/server.R:607-613` vs `R/plate-maps.R:613-629`)
  - The preview filters only on status, but the generator also uses location:
    KNL/TIS/DEL samples go to GT-Seq even when marked `need ots16`. The events
    dropdown also ignores the chosen destination.
  - The preview may be removed, so this is low priority.

## Not an issue

- **Sample statuses don't change after generating.** This is intended: statuses
  stay `need ots16` / `need gtseq` until the lab results are loaded back into
  the system.
