# Pipeline mechanics

Read when changing how targets are wired, debugging reruns or memory, moving
work between the laptop and Spartan, validating a pipeline edit, or rerunning a
check in `extras/`.

## A refit reaches everything below it

- `model_fit` and `preds` return paths that never change, which used to stop a
  refit at the fit.
- `model_fit_image` is `model_fit` as a `format = "file"` target, so it follows
  the image's contents (~3 s to rehash). Validation, predictions and the
  coefficient draws read `model_fit_image`, never `model_fit`. Anything new that
  reads the image must depend on `model_fit_image`.
- Keep the image target separate from the fit: with `format = "file"` on the fit
  itself, a moved or deleted image would relaunch the ~10 h fit, and a file
  target drops its names (breaking `preds$p`).
- `preds` returns its three raster paths plus their md5s. Any target that
  returns a fixed path needs the same treatment. Design note:
  `extras/refit_cascade_fix.md`.

## Memory: targets that load the fit image

- **`greta::calculate()` on the loaded fit image keeps ~5.2 GB of R memory
  after the calling function returns.** Neither `gc()`, Python's `gc.collect()`
  nor TensorFlow's `clear_session()` frees it. targets runs every target in one
  R process, so in the 2026-10-16 run `preds` went first and `resids_and_rhats`
  and `coef_draws` then hit "vector memory limit of 16.0 Gb reached" within
  20 s. Each succeeded in its own `tar_make()`.
- Measured in a fresh Rscript: `load()` of the image peaks at 7.8 GB and frees
  everything; `predict_lambda()` peaks at 12.4 GB and keeps 5.2 GB;
  `extract_cv_draws()` peaks at 15.5 GB on top of that and keeps nothing.
- **The fix:** `in_fresh_process(fun, list(...))` (`R/in_fresh_process.R`,
  3fc3700) runs a function from `R/` in a callr child, which returns all its
  memory when it exits. It attaches the pipeline's packages, sources `R/`, sets
  the target's seed, and wraps and unwraps terra arguments.
  `resids_and_rhats` and `coef_draws` run through it since 2026-11-09 (the
  user's choice); their plain versions are parked in
  `extras/parked_targets/process_variants.R`.
- **Pass the function, not its name as a string:**
  `in_fresh_process(validation_and_checking, ...)`. targets finds dependencies
  from the symbols in a command, so with a string an edit to the function
  wouldn't rerun the target (checked with `tar_deps()`). The child calls the
  function it is sent and finds what that calls in its own freshly sourced
  `R/`. The parked `_os` targets use strings (untracked).
- **`preds` still runs in the pipeline's process** (its wrapped version is
  parked in `extras/parked_targets/process_variants.R`), so it leaves ~5.2 GB
  held for the rest of a run. Swapping it in rebuilds `preds` and everything
  below (~1 h). In a scratch pipeline the rasters came out value-identical
  (same seed; md5s differ, from file metadata), and the parent held 47 MB
  against 5,250 MB. If a `tar_make()` after a refit runs out of memory, this is
  the next step.
- Run any script that loads the fit image as its own `Rscript`, not in a
  session already holding memory.
- **Upstream:** a reprex and issue text for greta are in `outputs/greta_issue/`
  (git-ignored), not filed. On greta 0.6.0 a small model with a likelihood
  keeps ~190 MB per `calculate()` call, adding up with each call.

## Stores, machines and needless reruns

- `_targets/` is not in git. The laptop and Spartan (HPC, user `ryange`) keep
  separate stores. Never share `_targets/meta/meta` between machines: the
  objects won't match, and targets rebuild from the root. (The meta was tracked
  until b10436d, 2026-09-28. A Spartan meta with `user_is_gerry_spartan` TRUE
  made the laptop rebuild `expert_maps`, and flipping `user_is_*` also switches
  the `offsets_raw` path, which changes `project_mask_5`.)
- `_targets.R` pins GDAL's `OGR_CURRENT_DATE` as an environment variable (so it
  reaches workers; 9f7c20e). Without it GDAL stamps each GeoPackage with its
  write time, so every `tar_terra_vect` target (`expert_maps`, `oneearth_vect`,
  the hulls) gets a new hash on each rebuild and the change cascades, e.g. into
  `expert_offset_maps`.
- To copy to Spartan before building there (git-ignored):
  `data/vector-atlas-logo.pdf` and `data/vector/other_expert_maps/wiebe2017/`.
  officer and flextable are not installed there.
- `tar_option_set()` has `error = "continue"` and `workspace_on_error = TRUE`,
  so one failed target doesn't stop the rest.

## Validating a pipeline edit without touching the store

- Splice the proposed `tar_target()` calls into a copy of `_targets.R` in a
  scratch folder and run `tar_manifest()`, `tar_network()` or `tar_validate()`:
  that builds the graph without running anything.
- Point targets at the copy with the **`TAR_CONFIG` environment variable**
  (`TAR_CONFIG=<temp yaml> Rscript ...`), not `options(tar_config = )`. The
  option doesn't reach the callr subprocess, so `tar_manifest()` silently reads
  the real `_targets.R` and passes falsely. Assert that the new target is in the
  manifest and that `tar_config_get("script")` is the copy.
- `parse()` the spliced file: a block pasted mid-`list()` needs a trailing
  comma. Afterwards, confirm `_targets/meta/meta`'s mtime predates the session.
- Tiny fits only: 2 chains × 20 + 20 in a scratch store, run in the
  foreground. `extras/overhaul_checks/03_smoke_pipeline.R` runs the real target
  definitions in a scratch store.
- Before saying what a change invalidates, check `tar_network()` or
  `tar_outdated()`.

## Long runs

- The Mac sleeps when idle, even on mains power: run long jobs under
  `caffeinate` (example: `extras/run_keepre_caffeinated.sh`, resumable).
- An agent's background jobs can be killed when its session sits idle for more
  than ~1–2 h; keep a periodic wake-up for long unattended runs.
- greta runs all chains in one process. In July 2026, 50 chains at once ran out
  of the 16 GB and were run as batches of ~10 and merged. Save draws as a plain
  coda `mcmc.list` (~4.5× smaller than a `greta_mcmc_list`, which stores a copy
  of the model).

## Checks in `extras/`

- `check_gelman_rhat.R`: `gelman_rhat()` gives bit-identical results to
  `coda::gelman.diag()`, at a fraction of the memory.
- `check_maxsss.R`: `maxsss_thresholds()` gives thresholds identical to
  `PresenceAbsence::optimal.thresholds()`.
- `check_bg_method_average.R`: the background-method fix on two tiny fits of
  the real data (model data identical across seeds; every background row gets
  exactly the averaged term, error ~1e-15). Passes.
- `check_uncounted_pa.R`: the 2026-11-06 data-type fix, on a tiny fit of
  `model_data_spatial_cx` patched in memory.
  `Rscript extras/check_uncounted_pa.R <image>`, ~1 min. Passes.
- `check_masked_presences.R` → `outputs/mask_check/masked_presences.csv`: what
  hides each presence cell on the maps (`maps.md`).
- `check_os_model.R`: the os model and its comparison functions on tiny fits
  (`os_model.md`).
- `overhaul_checks/`: the 2026-10-14 overhaul checks (`history.md`), plus
  `check_cv_fold_resume.R` (the CV fold resume guard, without fitting). **Don't
  regenerate the overhaul snapshot** (`01_snapshot_before.R` →
  `outputs/overhaul_check/snapshot_before.rds`): the fit images it was taken
  from are deleted. `04_compare_after_run.R` loads the fit image (~7.8 GB): run
  it as its own `Rscript`.
- `digitise_wiebe2017_coluzzii.R` is not a check: it makes the coluzzii map
  (`maps.md`).
