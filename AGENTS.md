# Vector Atlas multispecies SDM: notes for agents

A Bayesian multispecies point-process model, in greta, of the distribution and
relative abundance of 18 *Anopheles* species across Africa. It uses Vector
Atlas count, presence-absence and presence-only records plus background points,
environmental covariates and a mechanistic *An. gambiae* offset; expert range
maps mask the maps only. The pipeline is `_targets.R` (targets + geotargets);
functions live in `R/` (sourced by `tar_source()`); prototypes and one-off
checks live in `extras/`.

**Species** (`target_spp()`): arabiensis, coluzzii, coustani, funestus,
gambiae, leesoni, maculipalpis, melas, merus, moucheti, nili, pharoensis,
pretoriensis, quadriannulatus, rivulorum, rufipes, squamosus, ziemanni.

This file is loaded in every session, so keep it short. Detail is in
`agent_notes/`, read on demand (index at the end). When updating, put detail in
the note it belongs to, **replace** the State section rather than appending,
and move superseded facts to `agent_notes/history.md`.

## Rules

- **The user owns `_targets.R` and runs the pipeline.** Don't run `tar_make()`,
  fit models, or render heavy outputs such as the atlas PDFs unless asked. Edit
  `_targets.R` only when asked; otherwise deliver a function in `R/` plus an
  example `tar_target()`. Read-only calls (`tar_read`, `tar_meta`,
  `tar_manifest`, `tar_network`, `tar_outdated`) are fine. Commit only when
  asked.
- **Never drop a model term to improve mixing or R-hats** (the user: "it's a
  core part of the structure of the data and system").
- **Test on the stored objects** (`tar_read()` of the target), not an in-memory
  rebuild: the geotargets round trip changes structure (a terra categorical
  raster's ID column comes back named `value`, so index `levels()` by
  position). Test fits are tiny (2 chains × 20 + 20) in a scratch store, never
  the real store. Keep scratch work out of the repo.
- **Before saying what a change invalidates** (a refit is 9–12 h), check
  `tar_network()` or `tar_outdated()`.
- **Don't read `_targets.R` whole** (~2,660 lines): `grep -n` for the target,
  or `tar_manifest(names = ...)`. Commented-out targets are parked in
  `extras/parked_targets/`, with a pointer comment where each was.
- Name groupings of records by their columns ("location-date: source × method ×
  detailed method × location × start date × duration"), never "survey".

## Costly traps

- Never share `_targets/meta/meta` between the laptop and Spartan (HPC, user
  `ryange`). Each keeps its own store (`_targets/` is not in git), and a
  foreign meta rebuilds everything from the root.
- Keep the `OGR_CURRENT_DATE` pin at the top of `_targets.R`. It makes the
  GeoPackages behind `tar_terra_vect` targets byte-stable; without it every
  rebuild cascades, e.g. into `expert_offset_maps` (~30 min).
- Never put `format = "file"` on `model_fit` itself. Anything that reads the
  fit image depends on `model_fit_image` (`agent_notes/pipeline.md`).
- `in_fresh_process()`: pass the function itself, not its name as a string, or
  targets loses the dependency.
- Load the ~6.75 GB fit image only in its own `Rscript`: R's vector memory
  limit here is 16 GB.
- Don't delete `data/raster/esa_landcover/*.zip` (14 GB): they are the files
  of the live `esa_landcover_zip` target, and losing them invalidates the
  land-cover chain and the fit.
- These invalidate everything down to the fit, so only alongside a planned
  refit: any change to `generate_model_data_records()`; fixing
  `clean_species()`; removing the unused soil and subrealm layers.
- Don't regenerate the overhaul snapshot
  (`extras/overhaul_checks/01_snapshot_before.R`): its source images are gone.

## Running the pipeline

- The user builds everything but the CV with
  `targets::tar_make(!starts_with("cv_"))`. `tar_option_set()` has
  `error = "continue"` and `workspace_on_error = TRUE`.
- Costs: `model_fit` 9–12 h (40 chains, 2000 warm-up + 1000 samples, 8 cores);
  fit image ~6.75 GB; `expert_offset_maps` ~30 min.
- `resids_and_rhats` and `coef_draws` run in fresh R processes, because
  `greta::calculate()` leaves ~5.2 GB held; `preds` still runs in the
  pipeline's process.
- Switched off by the user; leave them so: the CV targets (`cv_*`, since
  2026-10-19: no CV for now) and the offset-strength model (`_os`, since
  2026-11-06). Both are parked in `extras/parked_targets/`.

## Pipeline

1. **Spatial inputs** (`agent_notes/covariates.md`): the monthly mechanistic offset
   (`offsets_raw`, path set by `user_is_*`); ESA CCI land-cover class
   proportions 1992–2022; Mu et al. human footprint 2000–2024; `prox_to_sea`;
   One Earth bioregions (fractional); travel time (`bias_tt_5`); expert range
   maps (`expert_maps`: Sinka + pharoensis + coluzzii; maps only); country
   boundaries (`atlas_*`).
2. **Records** (`agent_notes/data.md`): `raw_data` → `full_data_records` →
   `model_data_records` (data types, inferred zeros) → `model_data_spatial`
   (background points; covariates matched to each record's date) →
   `model_data_spatial_cx` (inferred zeros contradicted by a presence of the
   species or its complex dropped): the model's data.
3. **Model** (`agent_notes/model.md`): `model_fit` → `model_fit_image` (file target) →
   `resids_and_rhats`, `preds`, `coef_draws`.
4. **Maps** (`agent_notes/maps.md`): `pred_p`, `pred_lambda`, `pred_cv` (scaled by the
   expert offset and 1 − bare), thresholds and binary ranges, dominant species,
   `abundance_cubes`.
5. **Figures and atlases** (`agent_notes/outputs.md`): maps and panels, response curves,
   bioregion multipliers, the species atlas, 50 country atlases, and the data
   summary tables.

## The model

- Fit: `fit_model_multispecies_pp_count()`. Predictions: `predict_lambda()`,
  which samples parameter draws and multiplies in R (`calculate()` over all
  pixels crashes).
- Design: 9 ESA land-cover classes (`bare` and `sparse` left out, since the
  classes sum to 1) + `footprint` + `prox_to_sea`, plus their interactions with
  17 bioregions: 198 columns per species. The per-species intercept `alpha` is
  outside the matrix. One design row per (latitude, longitude, `model_date`).
- Likelihoods: counts negative binomial (NB1, dispersion `gamma_nb`);
  presence-absence Bernoulli via icloglog; presence-only + background Poisson
  with a travel-time bias. A sampling-method effect `sampling_re` (one per
  method, all species) is shared by all three; counts also get a per-source
  `zeta`. Background points take the method effects averaged over the methods'
  shares of the records.
- Maps are at 10 km, for indoor human landing catch, with 2022 land cover and
  2024 footprint.

## Outputs

Fit image `outputs/images/model_fit.RData`; rasters
`outputs/rasters/multispecies_pp{,_p,_p_cv}.tif`; cubes
`outputs/rasters/abundance_cubes_masked/`; maps and atlases
`outputs/figures/distribution_plots/distn_<run_tag>/`; validation
`outputs/figures/validation/<run_tag>/`; draws `outputs/draws/coef_draws.rds`.
`run_tag` is still `20261014`, so each refit overwrites those folders.

## State (2026-11-26)

- **Current fit:** 2026-11-22 (9.1 h), after the user's filter dropping coluzzii
  records from sources 2676 and 1001743. Classic R-hat max 1.088, none over 1.1,
  84 over 1.05.
- **Everything is built**, including the coluzzii expert map and all below it
  (2026-11-23), except `africa_exdet_br`, which fails now and then with a terra
  bug (`agent_notes/outputs.md`).
- The manuscript data paragraph was redone with the current numbers on
  2026-11-24 (the 2026-11-20 ones predated the coluzzii filter):
  `agent_notes/data.md`.
- **New raw data, not in the pipeline yet:** `va.data_20261116.csv` (replaces
  `va.data_20260616.csv`, which `raw_data_file` still points to) and
  `va.new.COMBINED_20261103.csv` (extra records). `extras/check_source_ids.R`
  lists their source and duplicate-row problems for the data team
  (`agent_notes/data.md`).
- **All committed** and merged to `main` (PR #55, 2026-11-26).

## Open items

The user's decisions:
- The count model (analysed 2026-11-25, to be returned to): positive counts are
  badly under-predicted. Mainly, NB1 makes catches too rare (3% predicted
  against 13% observed), and no term absorbs the source or duration variation.
  Evidence and options: `extras/count_misfit/README.md`.
- The offset-strength model: κ collapsed to ~0 and LOO preferred it, but it
  mixes badly and the ranges spread (`agent_notes/os_model.md`).
- Counts under the "other" sampling method (larvae and 7 trap types) are kept
  only as presence-absence; the user thinks they should be counts. Don't
  change this unprompted (`agent_notes/data.md`).
- Which side targets to drop (`agent_notes/outputs.md`).
- The atlas cover: the working subtitle, and *Anopheles* is not italic in the
  titles.
- `distplot()` default `maxcell = Inf` (the continental PNGs are thinned).
- Whether to swap in the fresh-process `preds`; whether to file the greta
  memory issue (`outputs/greta_issue/`).
- CV scoring targets are not written (no CV for now).

Known issues, not fixed:
- `clean_species()` misses "funestus like" and "rivulorum like" (spaces in the
  data, hyphens in the code).
- `pred_cv` is scaled down by the expert offset and (1 − bare) rather than
  masked, so uncertainty looks lower near range edges.
- No rootograms are drawn: `rootgram_data()` errors and `try()` hides it.
- Method effects are shared across species, and the offset is gambiae-based
  for every species: this may disadvantage some, e.g. funestus.
- Travel time has no data for some islands (Cabo Verde, Seychelles).
- Records to check: funestus 342,645 in one light trap, a window-exit count of
  200,170, and ziemanni in Morocco (`agent_notes/data.md`).

## Notes in `agent_notes/` (read when relevant)

| File | Read when working on |
|---|---|
| `model.md` | the fit, likelihoods, priors, design matrix, predictions, convergence, CV, known misfit |
| `data.md` | records, data types, inferred zeros, `_cx`, summary tables, manuscript numbers, the "other"-counts item, the new raw data and its source checks |
| `covariates.md` | covariate layers, matching records to dates, prediction years, land-cover and footprint traps |
| `maps.md` | masks, expert ranges (incl. coluzzii), thresholds, hidden presences, abundance cubes |
| `outputs.md` | figures, atlases, output paths, side targets, disk clean-up, the country extracts |
| `pipeline.md` | the refit cascade, memory and fresh processes, laptop vs Spartan, validating edits, the checks in `extras/` |
| `os_model.md` | the offset-strength alternative model and its results |
| `history.md` | what changed when, old names (scripts in `extras/` use them), past fits |

`nigeria_environmental_space/` and `uganda_environmental_space/` are covariate
extracts set aside for the user's other work, not part of this project. Don't
extend, regenerate or wire them in unless asked.

## Coding

- tidyverse (dplyr, tidyr, tibble, ggplot2, stringr, purrr) for tabular and
  functional work; terra for all spatial work (`terra::` over `raster::` or
  `sp::`); maps with ggplot2 + tidyterra; Bayesian models in greta.
- Base pipe `|>`. Comment only where the reasoning is not obvious.
- Where a function takes more than one argument, write it over several lines:

```r
runif(
  n,
  min = 0,
  max = 1
)
```

not `runif(n, min = 0, max = 1)`. It is easier to read, and new lines are free.
