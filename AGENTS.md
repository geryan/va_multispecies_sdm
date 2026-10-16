# Vector Atlas multispecies SDM: notes for agents

## What this is

A Bayesian multispecies point-process model, written in greta, of the distribution
and relative abundance of 18 *Anopheles* species across Africa.

- **Data:** Vector Atlas count, presence-absence and presence-only records, plus
  background points.
- **Inputs:** environmental covariates, a mechanistic *An. gambiae* offset, and
  expert range maps.
- **Layout:** the pipeline is `_targets.R` (targets + geotargets); functions live in
  `R/` and are sourced by `tar_source()`; prototypes and one-off checks live in
  `extras/`.

**Target species** (`target_spp()`): *arabiensis*, *coluzzii*, *coustani*,
*funestus*, *gambiae*, *leesoni*, *maculipalpis*, *melas*, *merus*, *moucheti*,
*nili*, *pharoensis*, *pretoriensis*, *quadriannulatus*, *rivulorum*, *rufipes*,
*squamosus*, *ziemanni*.

## Working in this repo

- **The user owns `_targets.R` and runs the pipeline.**
  - Don't run `tar_make()`, fit models, or render heavy outputs such as the atlas
    PDFs unless asked.
  - Edit `_targets.R` only when asked.
  - Read-only calls (`tar_read`, `tar_meta`, `tar_manifest`, `tar_network`) are fine.
- **Test new code on the stored objects** (`tar_read()` of the target). An in-memory
  rebuild is not enough, because the geotargets round trip changes structure: for
  example, a terra categorical raster's ID column comes back named `value` rather
  than `ID`. Keep scratch work out of the repo.
- **The `_targets/` store is not tracked in git.** The laptop and Spartan (HPC, user
  `ryange`) keep separate stores. Never share `_targets/meta/meta` between machines:
  the objects won't match, and targets rebuild from the root.
- **`_targets.R` pins GDAL's `OGR_CURRENT_DATE`,** so the GeoPackages behind
  `tar_terra_vect` targets are byte-stable. Otherwise every rebuild changes their
  hash and cascades downstream, for example into the ~40 min `expert_offset_maps`.
- **Build everything but the CV** with `targets::tar_make(!starts_with("cv_"))`,
  which is what the user runs. The five CV folds take about 2 h each, so they are
  built separately. `tar_option_set()` has `error = "continue"` and
  `workspace_on_error = TRUE`, so one failed target does not stop the rest.
- **Resources:**
  - `model_fit` takes about 12 h (40 chains, 2000 warm-up + 1000 samples,
    8 cores; 11.9 h on 2026-10-16);
  - the fit image is about 6.75 GB;
  - R's vector memory limit on the laptop is 16 GB. Targets that load the fit
    image can go over it when they share a `tar_make()`: see "Memory", below.

## Pipeline, in file order

1. **Spatial inputs**
   - Mechanistic offsets, monthly (`offsets_raw`; path chosen by `user_is_*`).
   - ESA CCI land-cover class proportions, annual 1992–2022.
   - Mu et al. human footprint, annual 2000–2024.
   - `prox_to_sea`, and One Earth bioregions (fractional).
   - Travel time from research facilities: `bias_tt_raw` in minutes; `bias_tt_5` is
     the reversed 0–1 version the model uses.
   - Expert range maps: `sinka_expert_maps` + `pharoensis_expert_map` →
     `expert_maps`, and `expert_maps_buffer_1000` (all maps dissolved, plus 1000 km).
   - Country boundaries for the atlases (`atlas_*`).
2. **Records**
   - `raw_data` (`data/tabular/va.data_20260616.csv`) → `full_data_records`
     (`clean_full_data_records()`, `clean_species()`).
   - → `model_data_records` (`generate_model_data_records()`: assigns data types and
     infers zeros).
   - → `model_data_spatial`: background points added; offset matched on year-month;
     land cover and footprint matched on each record's year.
3. **The model:** one set, on `model_data_spatial_cx`. See below.
4. **Abundance cubes (`abundance_cubes`).** Monthly relative abundance, one file
   per species with one layer per month, for the last 24 months of the offset
   series. It is the model's abundance without the offset, scaled by the expert
   offset and (1 − bare) as `pred_lambda` is, times each month's offset, so the
   mean of a species' last 12 layers is its `pred_lambda` layer.
5. **Spatial block cross-validation (`cv_*`),** on `model_data_spatial_cx`. Only
   the fold fits exist (`cv_folds_file`, `cv_fold_draws`), and neither has been
   built. The scoring targets are still to be written; their helpers already
   exist (`predict_cv_heldout()`, `extract_cv_draws()`, `split_cv_fold()`,
   `cv_designmat()`). `fit_cv_fold()` reuses an existing fold file only when its
   stored fingerprint matches the data, folds, settings and fit function.

## The model

- **Fit function:** `fit_model_multispecies_pp_count()`, in the file of that name.
  Until 2026-10-14 it was `fit_model_multispecies_pp_count_source_effect_reparam()`;
  notes and scripts in `extras/` still use the old name. Equations are in
  `extras/model_equations_sre_reparam.md`; that note describes 10 covariates and
  180 columns, where the model now has 11 and 198.
- **Design:** 11 covariates (9 ESA land-cover classes, `footprint`, `prox_to_sea`)
  plus their interactions with 17 bioregions, giving 198 columns per species. The
  per-species intercept is `alpha`, added separately; `bare` and `sparse` are left
  out because the classes sum to 1.
- **Likelihoods:**
  - counts: negative binomial (dispersion `gamma_nb`, exponential prior);
  - presence-absence: Bernoulli via icloglog;
  - presence-only + background: Poisson, with a travel-time bias;
  - shared across all three: a sampling-method effect `sampling_re`, one per method
    for all species;
  - counts only: a per-source effect `zeta`.
- **Background sampling methods are drawn at random** inside the fit, from the
  target's seed. Renaming the fit target changes the draw, so two fits of the
  same data differ there even before the MCMC.
- **Design rows:** one per distinct (latitude, longitude, `model_date`), the key the
  time-varying covariates and offset were joined on. This is `distinct_idx`,
  mirrored in `cv_designmat()`.
- **Predictions:** `predict_lambda()` (was `predict_lambda_reparam()`) samples
  parameter draws, then does the matrix multiply in R. Running `greta::calculate()` over all pixels runs out of
  memory. Predictions are at 10 km, for indoor human landing catch.

## The model set in `_targets.R`

There is one model set. Until 2026-10-14 there were four (the original, `_sre`,
`_rep` and `_rep_cx`); only `_rep_cx` was kept, and its targets lost the suffix.

- **Data:** `model_data_spatial_cx`, built by `drop_contradicted_inferred_zeros()`
  from `model_data_spatial`.
  - It drops inferred zeros for a species at locations where that species, or a
    complex or group containing it, was recorded present at any time.
  - The membership table is `complex_member_defs()`, from Harbach's Mosquito
    Taxonomic Inventory (2024) and WRBU.
  - It drops 16,503 of 96,270 inferred rows.
  - It keeps its `_cx` name because `model_data_spatial` still exists as its
    parent. Some side targets (novelty maps, older tables and plots) still read
    `model_data_spatial`.
- **Removed sets, for reference:** `_rep` was the same model on
  `model_data_spatial`. `_noinf` (all inferred rows dropped) was fitted and
  removed earlier; it showed the inferred zeros matter, especially for species
  with few data.

**Paths** come from five targets: `run_tag` (dates the plot and validation
folders), `fit_image`, `pred_prefix`, `validation_dir` and `plot_dir`. The fit
depends on `fit_image` alone, so a new `run_tag` does not refit.

**The set runs:**
1. fit (`model_fit`), then `model_fit_image`;
2. validation (`resids_and_rhats`, using the memory-light `gelman_rhat()`);
3. predictions (`preds`);
4. masked rasters (`pred_p`, `pred_lambda`, `pred_cv`);
5. plots: per-species, panels (3×6 grid, plus featured landscape and portrait),
   dominant species (also masked to `expert_maps_buffer_1000`), and maxSSS binary
   ranges (`thresholds_maxsss`, `pred_binary`);
6. the species atlas PDF, and per-country GeoTIFFs plus the country atlas PDF, for
   50 countries (WHO AFRO plus Sudan, Somalia and Djibouti).

**A refit reaches everything below it.** `model_fit` and `preds` return paths
that never change, which used to stop a refit at the fit.
- `model_fit_image` is `model_fit` as a file target, so it follows the image's
  contents. Validation, predictions and the coefficient draws read
  `model_fit_image`, never `model_fit`.
- `preds` carries the md5s of its three rasters alongside their paths.
- Keep the image target separate from the fit: if the image is moved or deleted,
  only the cheap target fails and the fit is not relaunched.

**Also in the set:**
- the RGB composition map of the main vectors (`rel_abund_rgb`): red arabiensis,
  green gambiae + coluzzii, blue funestus;
- `coef_draws`: 1,000 coefficient draws pulled out of the fit image once
  (`extract_cv_draws()`), so the plots below never load the ~6.8 GB image;
- response plots (`plot_response_curves`): one greyscale figure per species, from
  the main effects. The user dropped the all-species-per-covariate versions as
  useless;
- bioregion multipliers (`plot_bioregion_multipliers`): per species, each
  bioregion's multiplier at its average covariates, with 90% intervals and record
  counts, greyscale; and the multiplier maps (`bioregion_effect`);
- the country atlas opens with a continental "Africa" section
  (`species_atlas_pages` + `traveltime_page`);
- `data_summary_table_cx` (+ `_csv`).

**Why there are no covariate-slope figures** (from
`outputs/session_20261001/response_review/REVIEW.md`):
- Land cover is compositional with bare and sparse left out, so a class's slope on
  its own compares it with pure bare ground, which almost no data cover.
- The interaction terms act as species × bioregion intercepts, so slope heatmaps
  showed bioregion levels rather than covariate effects.

**Data and covariate figures** (outside the model set): `plot_records_traveltime`
(records next to travel time) and `plot_covariate_panels` (landscape 5×3, and
portrait sized for an A4 page).

**Atlas rendering.** The atlas pages are `cairo_pdf`s with the map embedded as an
image, one pixel per 10 km cell. PDF viewers (Preview, browsers) smooth small
images whatever the PDF's no-interpolate flag says, so small countries came out
as a blur. `sharpen_raster_grobs()` (`R/raster_for_pdf.R`) enlarges the built map
image by pixel repetition before saving. It is pixel-identical and adds about
0.3–0.8 s per page, against about 2.5 s for disaggregating the cells before
plotting. `raster_for_pdf()` averages down layers too big for the page (the ~1 km
travel time). tidyterra thins anything over 500,000 cells by dropping rows and
columns, so the atlas draws with `maxcell = Inf`; `distplot()` still defaults to
5e5.

**Outputs:**
- fit image: `outputs/images/model_fit.RData`
- prediction rasters: `outputs/rasters/multispecies_pp{,_p,_p_cv}.tif`
- abundance cubes: `outputs/rasters/abundance_cubes_masked/`
- plots: `outputs/figures/distribution_plots/distn_<run_tag>/`, including
  `species_atlas.pdf`, `country_atlas.pdf`, `country_pages/<ISO3>/`,
  `response_curves/`, `bioregion_multipliers/`
- validation: `outputs/figures/validation/<run_tag>/`
- country rasters: `outputs/rasters/countries/<ISO3>/`
- coefficient draws: `outputs/draws/coef_draws.rds`

## Memory: targets that load the fit image

**`greta::calculate()` on the loaded fit image keeps about 5.2 GB of R memory
after the calling function returns.** Neither `gc()`, Python's `gc.collect()`
nor TensorFlow's `clear_session()` releases it. targets runs every target in one
R process, so in the 2026-10-16 run `preds` (which calls `calculate()`) went
first, and `resids_and_rhats` and `coef_draws` then hit "vector memory limit of
16.0 Gb reached" within 20 s. Each succeeded when rerun in its own `tar_make()`.
The earlier fix (`gelman_rhat()` in place of `coda::gelman.diag()`, ef9f7ba) is
in place; it was not the cause.

Measured in a fresh Rscript: `load()` of the image peaks at 7.8 GB and frees
everything; `predict_lambda()` peaks at 12.4 GB and keeps 5.2 GB;
`extract_cv_draws()` peaks at 15.5 GB on top of that and keeps nothing.

**The fix, ready but not active:** `R/in_fresh_process.R`.
`in_fresh_process("fun", list(...))` runs a function from `R/` in a `callr`
child, which returns all its memory when it exits. It attaches the pipeline's
packages, sources `R/`, sets the target's seed, and wraps and unwraps terra
arguments. In `_targets.R`, wrapped versions of `preds`, `resids_and_rhats` and
`coef_draws` sit commented out under the active targets, with a note on how to
swap them in.
- **Swap in `preds` and `resids_and_rhats`,** the two that call `calculate()`.
  `coef_draws` does not need it.
- **Swapping changes the commands,** so `preds` and everything below it rebuild
  (about 1 h). In a scratch pipeline the rasters came out value-identical to the
  unwrapped ones (same seed); their md5s differ, from file metadata. The parent
  process held 47 MB after the wrapped `preds`, against 5,250 MB unwrapped.
- **Without the swap,** rebuild the image-loading targets in separate
  `tar_make()` calls, e.g. `tar_make(resids_and_rhats)`.

**Upstream:** a reprex and issue text for greta are in `outputs/greta_issue/`
(git-ignored), not yet filed. On greta 0.6.0 a small model with a likelihood keeps
about 190 MB per `calculate()` call, and the amount adds up with each call.

## The 2026-10-14 overhaul

`_targets.R` went from 232 targets to 186, on branch `reset_cx_to_main_pipeline`.

- **Removed (50 targets):** the original chain (`model_fit`, `resids_and_rhats`),
  `_sre`, the whole `_rep` section, and the stale tail after the CV section.
- **Replaced (1):** the bioregion slope heatmaps, by `plot_bioregion_multipliers`.
- **Renamed (38):** `X_rep_cx` → `X`, plus `model_fit_sre_rep_cx` → `model_fit`,
  `resids_and_rhats_sre_rep_cx` → `resids_and_rhats`, `preds_sm_rep_cx` → `preds`,
  and `rep_tag_cx`, `rep_fit_image_cx`, `rep_pred_prefix_cx`,
  `rep_validation_dir_cx`, `rep_plot_dir_cx` → `run_tag`, `fit_image`,
  `pred_prefix`, `validation_dir`, `plot_dir`.
- **Added (4):** `model_fit_image`, and the expert-range dominant maps
  (`pred_dominant_expert`, `pred_dominant_001_expert`,
  `plot_pred_dominant_expert`), carried over from `_rep`.
- **Relinked (3):** `abundance_cubes` (new predictions, masked, tracked as files)
  and the two CV targets (now on `model_data_spatial_cx`).
- **`R/`:** the fit and predict functions were renamed (see "The model"). Deleted:
  the two older fit functions, the refit function, the old `predict_lambda()`,
  `inits()`, `split_sims()`, `scale_predictions()`, `make_species_atlas()`, and
  the slope-heatmap code. `render_species_atlas_pages()` moved to a file of its
  own. A further 67 functions in `R/` already had no caller and were left.
- **Output paths** lost `_rep_cx` and the run tag is `20261014`, so the earlier
  outputs are untouched on disk.

**State on the laptop (2026-10-16).** The overhaul is committed (3b2c00c). The
user's `tar_make(!starts_with("cv_"))` built every target outside the CV:
`model_fit` finished at 04:29, and `resids_and_rhats` and `coef_draws`, which
ran out of memory in that run (see "Memory"), were rebuilt on their own by
09:48 and 10:26. `tar_outdated()` lists nothing outside the CV. Of the CV,
only `cv_tag`, `cv_k`, `cv_dir` and `cv_fold_ids` are built. Until `tar_prune()`
is run, the store still holds the `_rep_cx` objects.

**Checks** are in `extras/overhaul_checks/`, with results in
`outputs/overhaul_check/`:
- `01_snapshot_before.R`: the pipeline, the store and the data inside the old
  fit image, before the change (`snapshot_before.rds`). Do not regenerate it.
- `02_check_wiring.R`: every carried-over command equals the old one under the
  rename map apart from the listed deliberate edits; the graph, the removals and
  the additions are as intended; nothing upstream became outdated. Passes.
- `03_smoke_pipeline.R`: the model core end to end on a 2-chain, 20-sample fit in
  a scratch store, including a refit to the same path cascading to the rasters
  and cubes. Passes. Its fit image held the same data as the old production
  image in 48 of 57 objects (model data, design matrix, count and
  presence-absence tables); the other 9 are run settings and the background
  sampling-method draw.
- `check_cv_fold_resume.R`: the CV fold resume guard, without fitting. Passes.
- `04_compare_after_run.R`: for after the user's run. Stored inputs and the data
  inside the new fit image against the snapshot, cubes against `pred_lambda`,
  and a report of old against new results. Tried on the smoke store; not yet
  run on the real one, though the run it needs is done. It loads the fit image
  (~7.8 GB), so run it as its own `Rscript`, not in a session holding memory.
- `list_duplicate_outputs.R` → `duplicate_outputs.csv`: superseded output files
  and orphaned store objects, for the user to check and delete.

**Side targets, listed for the user and not removed.** None feeds the model,
the cubes or the CV. Awaiting a decision on which to drop. All of them were
built, and are current, after the 2026-10-14/16 run.
- *First built in that run:* the hull and range-offset chain (`bioregions_v`,
  `convex_hulls`, `bioregion_hulls`, `point_hulls_*`, `convex_offset_maps`,
  `bioregion_offset_maps`, `range_offsets`; the two offset maps took 20 and
  41 min), `bioregions_all`, `species_unique_presence_location_records`,
  `mod_dat_pts`.
- *Nothing downstream:* the ten `africa_mess*` / `africa_exdet*` novelty maps
  (on the pre-`_cx` data; `africa_mess_br` and `africa_exdet_br` fail now and
  then with a terra `writeRaster` bug, "too many values", and passed on
  2026-10-15. `R/mess_safe.R` avoids it but is not wired in); `bg_points`; `bioregion_names_mask` →
  `bioregion_mask_5` → `bioregion_mask_10` → `landscape_mask_10`; `offsets_10`,
  `offsets_avg_p_5`, `offsets_avg_p_10`; `expert_offset_maps_500` →
  `expert_offset_maps_10_500`; `esa_landcover_year` → `esa_landcover_all`, and
  `esa_landcover_lookup`; `bioregion_stack`; `west_africa_extent`, `nga`, `cod`,
  `kentzauga`, `ken`, `uga`; `exploration_full_data_records`;
  `presences_outside_expert_range` (+ `_plot`); `species_unique_location_records`
  (+ `_presence_records`); `data_summary_table`, `pa_data_table`, `covs_plots`,
  `bias_offset_plots`, `simple_point_plots`.
- *In the chain but unused by the model:* the soil layers (`soiltype_*`;
  `soiltype_names` is `NULL`), `subrealm_layers` and `subrealm_names`,
  `record_data_spatial_subsample` (a pass-through), `offset_names` (`NULL`).
  Removing these changes `covariate_rast_5_all` and invalidates everything below
  it, so do it only alongside a planned refit.

## Open items (as of 2026-10-16)

- **Run `extras/overhaul_checks/04_compare_after_run.R`** now that the model set
  is built.
- **Decide whether to swap in the fresh-process `preds` and `resids_and_rhats`**
  (see "Memory"). Until then, a refit needs the image-loading targets rebuilt in
  separate `tar_make()` calls.
- **Old outputs and store objects** are listed in
  `outputs/overhaul_check/duplicate_outputs.csv` for the user to delete. The old
  fit image, prediction rasters and coefficient draws are needed by check 04, so
  they go last.
- **CV scoring targets** are not written yet.
- **`pred_cv` scales the CV down** by the expert offset and by (1 − bare), which
  makes uncertainty look lower near range edges. It should probably be masked
  instead of scaled.
- **`clean_species()` misses two labels.** It maps "funestus-like" and
  "rivulorum-like" with hyphens, but the data spell them with spaces. Fixing this
  invalidates everything downstream.
- **Model structure may disadvantage some species, e.g. funestus.** Sampling-method
  effects are shared across species, with no species × method term, and the offset
  is *An. gambiae*-based for every species.
- **Travel time has no data** for some island states (Cabo Verde, Seychelles). Cabo
  Verde also lies outside the prediction grid.
- **Convergence.** The 2026-10-16 fit (2000 warm-up) is close: max R-hat 1.106,
  with 4 of 3,907 parameters over 1.1 and 72 over 1.05, all in `beta_raw` (64)
  and `alpha` (8, max 1.091). Every other group is under 1.03. The fit before
  (2026-09-30, 1000 warm-up) had max R-hats of 1.14 (gate) and 1.16 (beta).
  The values are in `tar_read(resids_and_rhats)$rhats` and
  `outputs/figures/validation/20261014/last_rhats.csv`.
- **No rootograms are drawn.** `rootgram_data()` fails with "subscript out of
  bounds" (`yrep_matrix[, 1:ncol_yrep]`), and `try()` in
  `validation_and_checking()` hides it. This predates the overhaul: the
  2026-09-28 validation folder has none either.
- **The continental PNG figures are thinned:** `distplot()` defaults to tidyterra's
  500,000-cell cap, and the 10 km grid is 732,511 cells. Setting the default to
  `Inf` was offered and not yet decided.
- **Weak data behind some peak abundances** (`outputs/session_20261001/`
  `abundance_drivers.txt`):
  - 244 of the 263 surveyed locations with ≥80% grassland are in Madagascar, so the
    grassland effect (funestus, maculipalpis, rufipes) is largely a Madagascar
    effect;
  - the mangrove effect (melas, squamosus) rests on 2 locations;
  - the shrubland effect for leesoni and rivulorum rests on 3 detections each.
- **Uncommitted work** (the overhaul itself is 3b2c00c):
  - `_targets.R`: `error = "continue"` (the user's) and the three commented-out
    fresh-process targets;
  - new, untracked: `R/in_fresh_process.R`, `extras/overhaul_checks/`,
    `extras/refit_cascade_fix.md`, which 3b2c00c left out;
  - `R/get_bioregions.R`: the user's change of the GeoJSON path to
    `data/vector/`;
  - this file.
- **Session artefacts** (git-ignored): `outputs/session_20261001/`, with test
  renders, bioregion prototypes and final test figures, the abundance-driver
  analysis and its scripts; `outputs/overhaul_check/`; `outputs/greta_issue/`.

## Checks in `extras/`

- `check_gelman_rhat.R`: `gelman_rhat()` gives bit-identical results to
  `coda::gelman.diag()`, at a fraction of the memory.
- `check_maxsss.R`: `maxsss_thresholds()` gives thresholds identical to
  `PresenceAbsence::optimal.thresholds()`.
- `overhaul_checks/`: the checks on the 2026-10-14 pipeline overhaul, described
  under it above.

## Coding preferences

- **Default to tidyverse** (dplyr, tidyr, tibble, ggplot2, stringr, purrr) for
  tabular and functional work.
- **Use terra** for all spatial work. Prefer `terra::` over `raster::` or `sp::`.
- **Maps:** ggplot2 + tidyterra.
- **Bayesian models:** greta.
- **Style:** use the base pipe `|>`. Comment only where the reasoning is not obvious.

## Coding conventions

Where a function takes more than one argument, write it over several lines. Do this:

```r
runif(
  n,
  min = 0,
  max = 1
)

sapply(
  X,
  FUN,
  simplify = TRUE,
  USE.NAMES = TRUE
)
```

Not this:

```r
runif(n, min = 0, max = 1)
sapply(X, FUN, simplify = TRUE, USE.NAMES = TRUE)
```

This is easier to read, and new lines are free.
