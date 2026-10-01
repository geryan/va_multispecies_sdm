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
- **Resources:**
  - `model_fit_sre_rep*` takes about 10 h (40 chains, 2000 warm-up + 1000 samples,
    8 cores);
  - each fit image is about 6.5 GB;
  - R's vector memory limit on the laptop is 16 GB.

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
3. **Model sections:** see below.
4. **Spatial block cross-validation (`cv_*`).** Only `cv_fold_draws` exists so far.
   The scoring targets are still to be written; their helpers already exist
   (`predict_cv_heldout()`, `extract_cv_draws()`, `split_cv_fold()`,
   `cv_designmat()`).
5. **Old chains, stale and unmaintained.** Some reference targets that no longer
   exist. Don't build them.
   - `model_fit`, `model_fit_sre`, `refit_model_fit_sre`, `resids_and_rhats` and
     `resids_and_rhats_sre`, before the reparameterisation section;
   - `preds_sm`, `pred_p`, ... and `abundance_cubes`, after the CV section.

## The model

- **Fit function:** `R/fit_model_multispecies_pp_count_source_effect_reparam.R`.
  Equations are in `extras/model_equations_sre_reparam.md`; that note describes 10
  covariates and 180 columns, where the model now has 11 and 198.
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
- **Design rows:** one per distinct (latitude, longitude, `model_date`), the key the
  time-varying covariates and offset were joined on. This is `distinct_idx`,
  mirrored in `cv_designmat()`.
- **Predictions:** `predict_lambda_reparam()` samples parameter draws, then does the
  matrix multiply in R. Running `greta::calculate()` over all pixels runs out of
  memory. Predictions are at 10 km, for indoor human landing catch.

## Model sections in `_targets.R`

- **`_rep`, the main section:** `model_fit_sre_rep` on `model_data_spatial`.
- **`_cx`:** the same code on `model_data_spatial_cx`, built by
  `drop_contradicted_inferred_zeros()`.
  - It drops inferred zeros for a species at locations where that species, or a
    complex or group containing it, was recorded present at any time.
  - The membership table is `complex_member_defs()`, from Harbach's Mosquito
    Taxonomic Inventory (2024) and WRBU.
  - It drops 16,503 of 96,270 inferred rows.
- **`_noinf`** (all inferred rows dropped) was fitted and then removed. It showed the
  inferred zeros matter, especially for species with few data.

**Each section runs:**
1. fit;
2. validation (`resids_and_rhats_*`, using the memory-light `gelman_rhat()`);
3. predictions;
4. masked rasters (`pred_p_*`, `pred_lambda_*`, `pred_cv_*`);
5. plots: per-species, panels (3×6 grid, plus featured landscape and portrait),
   dominant species, and maxSSS binary ranges (`thresholds_maxsss_*`,
   `pred_binary_*`).

**`_rep` only:**
- dominant species masked to `expert_maps_buffer_1000`;
- the species atlas PDF;
- per-country GeoTIFFs and the country atlas PDF, for 50 countries (WHO AFRO plus
  Sudan, Somalia and Djibouti).

**Outputs:**
- fit images: `outputs/images/model_fit_test_source_re_rep{,_cx}.RData`
- prediction rasters: `outputs/rasters/reparam_multispecies_pp_rep{,_cx}*.tif`
- plots: `outputs/figures/distribution_plots/distn_<rep_tag>_rep{,_cx}/`
- validation: `outputs/figures/validation/sre_rep{,_cx}_<rep_tag>/`
- country rasters: `outputs/rasters/countries_rep/<ISO3>/`

## Open items (as of 2026-10-01)

- **Refits don't reach the maps.** The fit and prediction targets return constant
  paths, so a refit doesn't propagate to the prediction rasters or maps. A fix is
  written in `extras/refit_cascade_fix.md` but not applied.
- **CV scoring targets** are not written yet.
- **`pred_cv_*` scales the CV down** by the expert offset and by (1 − bare), which
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
- **The atlas targets are written and smoke-tested but not built.** The country atlas
  is ~3,650 pages: about 40–45 min and 150–250 MB.
- **Uncommitted work:** many new `R/` files and changes to `_targets.R` were
  uncommitted when this was written.

## Checks in `extras/`

- `check_gelman_rhat.R`: `gelman_rhat()` gives bit-identical results to
  `coda::gelman.diag()`, at a fraction of the memory.
- `check_maxsss.R`: `maxsss_thresholds()` gives thresholds identical to
  `PresenceAbsence::optimal.thresholds()`.

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
