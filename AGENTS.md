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
    8 cores; 11.9 h on 2026-10-16, 12.3 h on 2026-11-07);
  - the fit image is about 6.75 GB;
  - R's vector memory limit on the laptop is 16 GB. Targets that load the fit
    image went over it when they shared a `tar_make()`; since 2026-11-09
    `resids_and_rhats` and `coef_draws` run in their own R processes: see
    "Memory", below.

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
     infers zeros). Data types are set per location-date (source × sampling
     method × detailed method × location × start date × duration), the same
     grouping the zeros are inferred in. A row with its own count is a count,
     zeros included. Where some species were counted and others recorded
     without a count, the uncounted rows (those presences and the inferred
     zeros) are presence-absence (since 2026-11-06; before, they were zero
     counts). Where everything recorded was counted, inferred zeros are zero
     counts.
   - → `model_data_spatial`: background points added; offset matched on year-month;
     land cover and footprint matched on each record's year.
3. **The model:** one set, on `model_data_spatial_cx`. See below.
4. **Abundance cubes (`abundance_cubes`).** Monthly relative abundance, one file
   per species with one layer per month, for the last 24 months of the offset
   series. It is the model's abundance without the offset, scaled by the expert
   offset and (1 − bare) as `pred_lambda` is, times each month's offset, so the
   mean of a species' last 12 layers is its `pred_lambda` layer.
5. **Spatial block cross-validation (`cv_*`),** on `model_data_spatial_cx`. The
   user commented these targets out on 2026-10-19 and wants no CV for now: leave
   them so. Only the fold fits were written (`cv_folds_file`, `cv_fold_draws`),
   and neither was built. The scoring targets are still to be written; their
   helpers already exist (`predict_cv_heldout()`, `extract_cv_draws()`,
   `split_cv_fold()`, `cv_designmat()`). `fit_cv_fold()` reuses an existing fold
   file only when its stored fingerprint matches the data, folds, settings and
   fit function.
6. **The alternative model (`_os` targets),** at the end of the file: a
   per-species offset strength, its maps and a comparison with the current
   model. See "The alternative model: offset strength". **Commented out on
   2026-11-06** at the user's request: only the main pipeline runs for now.

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
- **Background points have no sampling method** (since 2026-10-19). In the
  presence-only likelihood each takes the method effects averaged over the
  methods' shares of the records, `sampling_re_bg = log(sum_m p_m *
  exp(sampling_re[m]))`. Until then each point got one method drawn at random
  from the target's seed, which moved the level of every map; see the first
  open item. The data the model sees no longer depend on the seed.
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
2. validation (`resids_and_rhats`, using the memory-light `gelman_rhat()`, in
   a fresh R process);
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
- `data_summary_table_cx` (+ `_csv`), from `make_data_summary_table()`. Per
  species: `count`, `count_p`, `count_0`, `count_imp`, `pa`, `pa_1`, `pa_0`,
  `pa_imp`, `po`, `p_n` (all present records) and `p_unique` (distinct
  latitude-longitude pairs of those). `count_p`, `pa_1`, `p_n` and `p_unique`
  were added 2026-11-06.

**Why there are no covariate-slope figures** (from
`outputs/session_20261001/response_review/REVIEW.md`):
- Land cover is compositional with bare and sparse left out, so a class's slope on
  its own compares it with pure bare ground, which almost no data cover.
- The interaction terms act as species × bioregion intercepts, so slope heatmaps
  showed bioregion levels rather than covariate effects.

**Data and covariate figures** (outside the model set): `plot_records_traveltime`
(records next to travel time) and `plot_covariate_panels`. It makes four
figures in `outputs/figures/`:
- `covariate_panels_landscape.png` (5×3) and `covariate_panels_portrait.png`
  (sized for an A4 page);
- since 2026-11-06, each part alone for the A4 manuscript, through
  `make_covariate_panels(panels = )`: `covariate_panels_bioregions.png` (map
  with the key on the right, no title, 16 × 11 cm) and
  `covariate_panels_covariates.png` (3 rows × 4, colour bar in the last cell,
  16 × 13.1 cm). Both have 7 pt text at printed size, `maxcell = Inf` (every
  10 km cell) and 600 dpi. The new arguments (`panels`, `maxcell`, `dpi`,
  `width`, `height`) default to the old behaviour, so the first two are
  pixel-identical to before; the portrait is still thinned and 300 dpi.

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

**Atlas cover, footer and citations** (`make_atlas_pdf()`, so all three atlas
targets; added 2026-10-16, cfaf971). The cover has the Vector Atlas logo, the
title and subtitle, and "Please cite as" with two citations between green
rules: Vector Atlas (2026), and Ryan, Sinka & Golding (2026) on Figshare,
doi 10.26188/34044441. Every later page's footer has a small logo, the same two
citations, the "Contents" link and the page number. Both URLs are clickable.
The contents starts on page 2.
- The citations are the `citations` argument's default, written as LaTeX.
- The logo is `data/vector-atlas-logo.pdf` (the `logo` argument), made from the
  SVG with `rsvg-convert -f pdf`: xelatex can't include an SVG, and the PDF is
  5.7 KB against the PNG's 104 KB. It is embedded once and reused on every page,
  so the species atlas grew by only 41 KB.
- `data/` is git-ignored, so the logo PDF must be copied to Spartan before the
  atlases are built there. targets doesn't track it: a new logo needs the
  atlases invalidated by hand.
- The cover subtitle and title come from `_targets.R`. The subtitle still reads
  "Reparameterised multispecies model, contradicted inferred zeros removed, run
  …", and *Anopheles* in the title is not italic.

**Outputs:**
- fit image: `outputs/images/model_fit.RData`
- prediction rasters: `outputs/rasters/multispecies_pp{,_p,_p_cv}.tif`
- abundance cubes: `outputs/rasters/abundance_cubes_masked/`
- plots: `outputs/figures/distribution_plots/distn_<run_tag>/`, including
  `species_atlas.pdf`, `country_atlas.pdf`, `country_pages/<ISO3>/`,
  `country_atlases/<ISO3>_<Country>.pdf`, `response_curves/`,
  `bioregion_multipliers/`
- validation: `outputs/figures/validation/<run_tag>/`
- country rasters: `outputs/rasters/countries/<ISO3>/`
- coefficient draws: `outputs/draws/coef_draws.rds`

## The alternative model: offset strength (`os`)

Added 2026-10-19 at the user's request. Fitted 2026-10-20 (`model_fit_os`,
11.3 h), with the comparison built; see "Results (2026-10-20)" below.

**Commented out 2026-11-06** at the user's request: the whole `_os` block at
the end of `_targets.R`, so `tar_manifest()` has no `_os` targets. `R/os_*.R`
are unchanged and still sourced, but nothing calls them. The fit image and
outputs are still on disk; the store keeps their entries until `tar_prune()`
is run. Its results below are on the data before the 2026-11-06 data-type
fix (see the open items).

**Why.** The *An. gambiae* offset hides 127 presence cells (see the open item on
hidden presences). The offset is there to keep species out of the Sahara, where
no data say so. On the maps the bare-ground mask does most of that, so a plain
per-species power on the offset is safe there (see the same open item).

**The model.** It is `model_fit`'s model with one change:

    log λ_s = α_s + x·β_s + κ_s · log(offset),   κ_s = exp(b_s),   b_s ~ Student-t(3, 0, 0.05)

- κ_s = 1 is the current model.
- Without evidence, κ_s stays within about 0.95–1.05 (61% of the prior). The
  heavy tails let a species whose data clearly favour another value move there.
  The penalty, in log-likelihood units, is about 4 at κ = 0.8, 8 at 0.5 and 11
  at 0.25.
- The user chose the Student-t over a horseshoe, which would have the funnel
  geometry greta's sampler struggles with. They chose scale 0.05 over 0.1 for a
  stronger penalty.
- The fit applies κ to each record's monthly offset.

**The user's rules for this work:**
- Don't modify the existing functions. The os versions are copies with an
  `os_` prefix: `os_fit_model_multispecies_pp_count()`, `os_predict_lambda()`
  and `os_extract_cv_draws()`.
- No CV, no R-hat comparisons, and no long fits in the comparison. (On
  2026-10-21 the user asked for `resids_and_rhats_os`, the same validation as the
  current model's.)
- Only the distribution and abundance maps for this model.

**Predictions** (`os_predict_lambda()`):
- For each species and draw, the effective offset is the mean over 2024's 12
  months (`offsets_2024_10`) of offset^κ. With κ = 1 this is `offsets_avg_10`
  (checked: relative difference ≤ 1.1e-7).
- `<prefix>.tif` is λ **including** the offset (`preds_os$lambda`), since a
  per-species power can't be applied afterwards. `pred_lambda_os` and
  `pred_p_os` take the same masks as the current model.

**The comparison** goes to `outputs/model_comparison/<run_tag>/` and needs both
fits.
- **`compare_loo_os`** (`os_compare_loo()`): PSIS-LOO on every record (counts,
  presence-absence, presence-only + background; 105,314 rows). It gives the
  difference os − current with its SE, overall, by data type and by species.
  It uses `os_record_loglik()`, the fit's three likelihoods rebuilt in R from a
  draws file; κ = 1 when the file has none.
  - `os_record_loglik()` matches greta's expected values to about 1e-14 for
    counts and PO/bg.
  - For presence-absence it uses `-expm1(-λ)`, more accurate than the fit's
    `logit_icloglog_safe()` at tiny λ, with the same clamp on log λ.
- **`compare_maps_os`** (`os_compare_maps()`):
  - presence cells hidden and newly shown;
  - predicted range by offset level;
  - area over the threshold before the masks where the offset is < 0.01.
- **`plot_kappa_os`**: the κ table, a dot-and-interval figure and the
  offset^κ curves.
- **`plot_range_change_os`**: per-species maps of both / current only / os
  only, with the records.

**Outputs:**
- fit image: `outputs/images/model_fit_os.RData`
- rasters: `outputs/rasters/multispecies_pp_os{,_p,_p_cv}.tif`
- maps: `outputs/figures/distribution_plots/distn_os_<run_tag>/`
- validation: `outputs/figures/validation/os_<run_tag>/` (`resids_and_rhats_os`,
  `validation_and_checking()` unchanged, on `model_fit_image_os`)
- draws: `outputs/draws/coef_draws_os.rds`

`preds_os`, `coef_draws_os` and `resids_and_rhats_os` run in fresh processes
(`in_fresh_process()`).

**Tested** on tiny fits (2 chains × 20 + 20) of the real data with
`extras/check_os_model.R`: all checks pass. The tiny os fit already moved κ far
from 1 for some species (pharoensis, funestus and arabiensis about 0.3–0.4;
moucheti about 1.6), but 20 warm-up iterations say nothing about the real
posterior.

**Results (2026-10-20),** in `outputs/model_comparison/20261014/`:
- **κ collapsed towards 0 for 17 of 18 species.** Posterior medians run from
  0.0005 (pharoensis, coustani) to 0.19 (rivulorum); gambiae, whose offset it
  is, 0.11 (90% 0.10–0.13). Only moucheti went up (2.1, 90% 1.8–2.4). The
  Student-t's tails let this through: the penalty grows only logarithmically
  (about 18 log-likelihood units at κ = 0.0005). Below κ ≈ 0.01, offset^κ ≈ 1,
  so the chains disagree on log κ there (pharoensis −13 to −7 by chain) without
  it mattering. α drops by 1–3 for most species to take up the offset's level.
- **LOO prefers os by 17,744 elpd (SE 433, z = 41),** and in every data type:
  presence-absence +10,151, presence-only + background +5,345, counts +2,248.
  Worse only for moucheti (counts −9, presence-absence −33) and ziemanni counts
  (−18). Pareto k > 0.7 on 302 rows for os against 115 for current, 189 of them
  moucheti presence-absence, so moucheti's LOO is unreliable.
- **Maps:** with the offset gone, the ranges grow into low-offset land the masks
  don't cover. Area over threshold where the 2024 offset < 0.01, after the
  masks: leesoni 472,500 km², quadriannulatus 410,000, squamosus 397,000,
  rivulorum 364,000, pretoriensis 245,000 (current: 0 for these). arabiensis
  gains 45,212 cells (Horn of Africa, Namibia/Botswana, southern Africa) and
  loses 3,476; leesoni reaches the Mediterranean coast and Nile delta. gambiae
  shrinks (1.7 M km² less) and hides 169 presence cells against 118. Hidden
  presence cells fall for most other species (arabiensis 321 → 151).
- **Abundance ratio maps** (2026-10-21, `os_plot_abundance_difference()`, run on
  the stored `pred_lambda` and `pred_lambda_os`; not a target yet):
  `abundance_difference_<species>.png` (current | os | log2 ratio) and
  `abundance_ratio_all.png`. os is higher almost everywhere, by ×16–64+ where
  the offset is low: the Maghreb coast, the northern Sahel edge, the Ethiopian
  highlands and Horn, and Namibia/South Africa. It's the same pattern for most
  species. pharoensis is ×16–64 nearly everywhere (its α didn't drop to
  compensate). moucheti is the only one with large lower areas.
- **The os fit mixes much worse** (`resids_and_rhats_os`, built 2026-10-21, 1 min
  53 s): max classic R-hat 1.253, 49 over 1.1 and 1,352 over 1.05, against the
  current fit's 1.077, 0 and 55. The worst is `log_kappa` (9 of 18 over 1.1:
  merus 1.25, pharoensis 1.24, rufipes 1.20, pretoriensis 1.20, funestus 1.18,
  leesoni 1.17, arabiensis 1.15, melas 1.15, nili 1.09). Then `alpha` (3 over
  1.1), `beta_raw` (37 over 1.1, 1,246 over 1.05) and `zeta_raw` (76 over 1.05,
  max 1.08). Once κ is near 0, the likelihood is nearly flat in log κ, so the
  chains drift along the prior's tail, and α and β, which trade off against
  κ·log(offset), drift with them. The maps depend on offset^κ, which stays ≈ 1,
  so they are less affected than the R-hats suggest. The LOO and β-based
  results rest on a fit that hasn't converged.
- **Count residuals, arabiensis and funestus** (2026-10-21, scratch analysis;
  figure `count_residuals_arabiensis_funestus.png` in the comparison folder).
  Both models' "underprediction" comes mainly from the NB1 dispersion, not the
  offset. With `gamma_nb` 176 (arabiensis) and 314 (funestus), the model gives
  P(0) 0.86 / 0.95 against observed zero shares of 0.60 / 0.69. A record that
  caught anything had on average a 20% / 11% predicted chance of catching ≥ 1.
  So 53% / 73% of positive counts sit above the predictive 97.5% quantile
  (expected: 2.5%). os cuts the share of count records above it from 21.6 to
  17.1% (arabiensis) and 23.2 to 19.6% (funestus). The gain is where the offset
  is 0.01–0.1 (55 → 20%, 49 → 19%) and in Madagascar (26 → 7%, 29 → 10%). It is
  a little worse where the offset is > 1. Neither model reaches the counts over
  500 (50 and 44 records, above all 100 simulations). The largest is funestus
  342,645 in one indoor CDC light trap (Mozambique, 2020-05), against an
  expected ~1: worth checking whether it is a pooled count.
- Not decided: whether the offset should stay, at what strength, or with a
  tighter prior; the user has the results.

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

**The fix, active for two targets since 2026-11-09:** `R/in_fresh_process.R`.
`in_fresh_process(fun, list(...))` runs a function from `R/` in a `callr`
child, which returns all its memory when it exits. It attaches the pipeline's
packages, sources `R/`, sets the target's seed, and wraps and unwraps terra
arguments. The function was committed in 3fc3700.
- **`resids_and_rhats` and `coef_draws` run through it** (the user's choice,
  2026-11-09), so after a refit they no longer need their own `tar_make()`
  calls. Their plain versions sit commented out under them.
- **`preds` still runs in the pipeline's process,** so it still leaves ~5.2 GB
  held for the rest of the run. Its wrapped version is commented out under it.
  Swapping it in would rebuild `preds` and everything below it (about 1 h). In
  a scratch pipeline the rasters came out value-identical to the unwrapped ones
  (same seed); their md5s differ, from file metadata. The parent process held
  47 MB after the wrapped `preds`, against 5,250 MB unwrapped.
- **Pass the function itself, not its name as a string:**
  `in_fresh_process(validation_and_checking, ...)`. targets finds dependencies
  from the symbols in a command, so with a string an edit to the function
  would not rerun the target (checked with `tar_deps()`). The child calls the
  function it is sent, and finds what that calls in its own freshly sourced
  `R/`. Strings still work, untracked; the commented-out `_os` targets use
  them.
- **The swap changed the two commands,** so both rebuild on the next
  `tar_make()` (about 2 min each, the first since the swap). The four plots
  below `coef_draws` show as outdated too. `extract_cv_draws()` has no random
  step, so they should be skipped if the draws file comes out the same.

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
09:48 and 10:26. Of the CV, only `cv_tag`, `cv_k`, `cv_dir` and `cv_fold_ids`
are built.

**State on the laptop (2026-10-19).** Later on 2026-10-16 the user rebuilt the
three atlas targets with the new cover and footer (14:22–14:25), ran
`tar_prune()` (the store holds no `_rep_cx` objects), and committed 446ede1,
3fc3700 and cfaf971. `tar_outdated()` listed only `cv_folds_file` and
`cv_fold_draws`, until the background-method fix later on 2026-10-19 outdated
`model_fit` and everything below it.

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
  and a report of old against new results. Run on the real store on
  2026-10-19: all checks pass (results in `compare_after_run.rds`). Its section
  D report reads as "MCMC noise", but it hides a level shift: see the open item
  on the background sampling-method draw. It loads the fit image (~7.8 GB), so
  run it as its own `Rscript`, not in a session holding memory.
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
  then with a terra `writeRaster` bug, "too many values". Both passed on
  2026-10-15; `africa_mess_br` failed again on 2026-11-09. `R/mess_safe.R` avoids it but is not wired in); `bg_points`; `bioregion_names_mask` →
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

## Country data extracts (set aside; not part of this project)

Two folders of covariates cut out for the user to keep for other work. Nothing
in the pipeline, `R/` or the model reads them, and they are not to be developed
further here. Don't extend, regenerate or wire them in unless asked.

- **`nigeria_environmental_space/`** (made 2026-10-19 by
  `extras/make_nigeria_environmental_space.R`): land cover (11 ESA class
  proportions) and Mu footprint for 2013–2022, plus 51 *An. gambiae* presence
  records and a plotting script. It has a `README.md` with citations and an
  `AGENTS.md` handoff note that describes the data for an agent working in that
  folder.
- **`uganda_environmental_space/`** (made 2026-10-23 by
  `extras/make_uganda_environmental_space.R`): the same land cover for every
  year (1992–2022) and footprint for every year (2000–2024), with no records.
  It has a `README.md` with citations.
- Both are cropped and masked to GADM 4.1 level 0 (the `nga` and `uga`
  targets), on the 5 km project grid, read from `outputs/raster/` and the
  store. That is the finest the project holds: the raw 1 km footprint was not
  kept, and the 300 m ESA source holds one class per cell, not proportions.
- The `.tif` and `.csv` files are git-ignored. The boundary `.gpkg`s, the
  scripts and the notes are untracked, not ignored.

## Open items (as of 2026-11-09)

**State on the laptop (2026-11-09).**
- **Refitted after the data-type fix** ("Present records with no count",
  below). `model_fit` finished 2026-11-07 05:30 (12.3 h), and everything
  below it was rebuilt by 05:57. `resids_and_rhats`, `coef_draws` and the four
  coefficient plots were rebuilt on 2026-11-09, 09:04–09:07, before the swap.
- **These are the fits to keep.** `run_tag` is still `20261014`, so this fit
  overwrote the 2026-10-20 fit's image, rasters, `distn_20261014/` and
  validation folder.
- **The fresh-process swap is made** for `resids_and_rhats` and `coef_draws`
  (see "Memory"). `tar_outdated()` lists:
  - those two, whose commands changed;
  - the four plots below `coef_draws`, which should be skipped;
  - `africa_mess_br`, which failed again on 2026-11-09 with the terra
    `writeRaster` bug (see the side targets; `africa_exdet_br` passed).
- **The new data:** `model_data_spatial_cx` has 15,911 / 2,437 counts
  (absent / present), 66,886 / 12,130 presence-absence, 3,468 presence-only
  and 249 background rows. No present count row has a count of 0.
- **Convergence** is in the convergence item below.
- **Still the user's decision:** the offset strength model (see "The
  alternative model"). Its comparison was made on the data before the fix.

- **The background sampling-method draw set the level of every map. Fixed in
  code 2026-10-19 (uncommitted), refitted 2026-10-20.** Found by check 04.
  - The fit gave each background point one sampling method, at random. Those
    points are the presence-only integral (weights ~120,000 km² each against 1
    for a presence), and `sampling_re` is shared by all three likelihoods, so
    the draw moved every method effect.
  - The 2026-09-30 and 2026-10-16 fits have identical data apart from this
    draw (and warm-up). `sampling_re` is well mixed in both (R-hat ≤ 1.02), yet
    the posterior means differ by up to 5.5 posterior SDs: outdoor resting
    1.03 → 0.07; indoor HLC, the method the maps predict for, −0.32 → +0.03.
  - So every species' abundance rose by about ×1.4 (median ×1.1–1.6 by
    species) and mean probability by 15–60%. The correlations in check 04
    (≥ 0.99) hide it, since a shared scale change keeps the correlation. Relative
    comparisons between species are mostly unaffected.
  - Each method's share of background area hardly changed (indoor HLC 8.5% →
    8.2%), so it is where a method's few points fall, not how many there are.
  - The counts did not hold the level: the shift is the same (×1.3–1.4) at
    the cells with indoor-HLC counts. They are very overdispersed (count
    variance ~100–300× the mean for arabiensis, gambiae and funestus), so they
    say little about the mean.
  - **The fix:** background rows get `sampling_re_bg`, the average over the
    method shares (see "The model"), in place of one drawn method. It equals the
    old draw on average and is the same as one copy of each point per method with
    weight w × p_m. Also in `predict_cv_heldout()`, using the fit's shares, which
    `extract_cv_draws()` now saves as `sm_prop`.
  - **Tested** (`extras/check_bg_method_average.R`, two 2-chain × 20 + 20 fits
    on the real data): the model data are identical
    across seeds; the built model gives every background row exactly the
    averaged term (error ~1e-15) and every presence-only row its own method's;
    `extract_cv_draws()`, `predict_cv_heldout()` and `validation_and_checking()`
    run on the result. The validation plots by method now show background rows
    as an "NA" panel.
  - **Refitted 2026-10-20** (`model_fit` 10.9 h, finished 14:06; everything
    below rebuilt by 15:13, no errors, `tar_outdated()` empty). Method effects
    now sit between the two earlier fits: indoor HLC −0.04 (SD 0.18), outdoor
    resting 0.65. Against the 2026-09-30 fit, median abundance is ×1.0–1.6 by
    species (most ×1.3) and mean probability ×1.1–1.4; correlations of log λ
    are ≥ 0.99. Superseded by the 2026-11-07 refit after the data-type fix,
    which overwrote its outputs.
- **Presence records the maps hide** (`extras/check_masked_presences.R`,
  2026-10-19, on the 2026-10-16 fit; 4,471 species × 10 km presence cells).
  "Hidden" = below the species' maxSSS threshold on `pred_p`.
  - *The expert-range masks hide none* (expert factor ≥ 0.57 at every hidden
    presence cell). *Bare ground hides 4* (arabiensis 2, coluzzii 2).
  - *The An. gambiae offset hides 127 cells in 12 species*, where it is at least
    10× below the species' usual: arid margins (Sudan, Mauritania, Niger,
    Botswana, Namibia), highlands (Ethiopia, Kenya, Rwanda) and North Africa
    (pharoensis in Egypt, 55 records; ziemanni in Morocco). Most: arabiensis 40,
    pharoensis 20, ziemanni 17, funestus 15. Not a 2024 artefact: the records'
    own survey months had an even lower offset (median 0.0043, against 0.9 at
    other presences), so the fit never fitted them either.
  - *A plain per-species exponent on the offset* (`κ_s log(offset)`): what it
    would do to the Sahara, from the 2026-10-16 fit with habitat held fixed and
    each species re-levelled (scratch calculation, 2026-10-19; maps for 7
    species in `outputs/kappa_sahara_preview/`). The offset is never 0 (median
    1e-4 in a 17–30°N, 13°W–34°E box), but the maps barely change there: the
    bare-ground mask blanks it (96% of the box is over 90% bare). At κ = 0.5 the
    box gains 7 cells over arabiensis' threshold, 19 for coluzzii, all at
    17–20°N, and pharoensis 148 more, mostly in the Nile valley. Before the
    masks, arabiensis' median Sahara probability goes 0.0002 → 0.016
    (threshold 0.59). coluzzii is the exception: its fitted habitat there is
    high, and 13% of the box would pass its threshold before masking. The
    larger change from a low κ is outside the Sahara: arabiensis' binary range
    spreads over the Horn of Africa and southern Africa at κ ≤ 0.5. 23 of the
    127 cells have a 2024 offset below 0.01 (the Nile valley, some Ethiopian
    highland cells, part of Morocco). Fitted as the `_os` model (see "The
    alternative model: offset strength"): κ went to ~0, not 0.5.
  - *The ziemanni Morocco records* are 11 presence-only locations from one source
    (1002703), each entered twice (indoor HLC and "other"). Worth checking the
    identification: ziemanni is not usually reported from North Africa.
  - *The water mask drops 129 presence cells (402 records) before the model*:
    coastal and lakeshore records in a 5 km cell `water_mask_5` calls water.
    gambiae 28 cells, coluzzii 22, melas 22 (a coastal species), arabiensis 19.
    58 have land in the same 10 km cell. The user does not consider these a
    worry (2026-10-19): leave them.
  - 13 cells are off the offset's area altogether: arabiensis on Réunion (10
    cells, 78 records) and Cabo Verde (2), and one coluzzii on the Cameroon coast.
- **The fresh-process `preds` is not swapped in** (see "Memory"). Since
  2026-11-09 `resids_and_rhats` and `coef_draws` run in fresh processes, so
  a refit runs in one `tar_make()`. Untested in a full run until the next
  `tar_make()`.
- **Old outputs:** `tar_prune()` has cleared the store, but the output files in
  `outputs/overhaul_check/duplicate_outputs.csv` are still on disk: 65 items,
  about 30 GB (groups "older", "removed set" and "renamed", plus 3 tiny
  orphaned objects in `_targets/objects`, group "store"). Check 04 has run, so
  the 5 marked `keep_until_check_04` (the old fit image, prediction rasters and
  coefficient draws) can go too. The old image is the only other draw of the
  background methods, so keep it if a comparison across draws is wanted.
- **Atlas cover text:** whether to replace the working subtitle and italicise
  *Anopheles* in the titles (both in `_targets.R`; see "Atlas cover, footer and
  citations").
- **CV scoring targets** are not written yet, and the CV targets are commented
  out (the user, 2026-10-19: no CV for now).
- **`pred_cv` scales the CV down** by the expert offset and by (1 − bare), which
  makes uncertainty look lower near range edges. It should probably be masked
  instead of scaled.
- **Present records with no count were fitted as zero catches. Fixed in code
  2026-11-06 (uncommitted), refitted 2026-11-07.**
  - *The error:* species recorded present (not inferred) at a location-date
    where other species were counted, but with no count of their own
    (`occurrence_n` NA). `generate_model_data_records()` made them counts with
    `count = 0`, so `n = 0` was the fit's response for a species that was
    there. 1,113 rows in `model_data_spatial_cx`, 32% of the present count
    rows: arabiensis 359, funestus 294, pharoensis 176, gambiae 65 the most.
    The inferred zeros at those location-dates were zero counts too.
  - *The fix (the user's rule):* per location-date, the grouping the zeros are
    inferred in. If every recorded row there has its own count, inferred zeros
    stay zero counts; otherwise the uncounted rows (presences and inferred
    zeros) are presence-absence. Recorded counts, zeros included, stay counts
    (the user's choice). The new `full_count` rule in the `data_type`
    `case_when`.
  - *Effect on `model_data_records`:* 1,115 presences go count → pa, and 6,063
    inferred zeros count → pa 0, at 437 location-dates from 67 sources. 30 of
    the 6,063 are at 2 location-dates where the uncounted row is a recorded
    absence. Nothing else changes: same rows and keys, same `presence`, and
    `count` and `n` change only on those rows. No present count row has a
    count of 0 any more. Counts 26,677 / 3,603 (absent / present) →
    20,614 / 2,488; presence-absence 77,675 / 11,484 → 83,738 / 12,599.
  - *Tested:* the edited function against the stored `model_data_records`, and
    a 2-chain × 20 + 20 fit on `model_data_spatial_cx` patched the same way in
    memory (`extras/check_uncounted_pa.R`; passes). Its stored responses are
    exact. Don't check greta data with `calculate()`: it simulates them.
  - Rebuilt, so `make_data_summary_table()`'s `count_p` is now the number of
    counts > 0.
- **`clean_species()` misses two labels.** It maps "funestus-like" and
  "rivulorum-like" with hyphens, but the data spell them with spaces. Fixing this
  invalidates everything downstream.
- **Model structure may disadvantage some species, e.g. funestus.** Sampling-method
  effects are shared across species, with no species × method term, and the offset
  is *An. gambiae*-based for every species.
- **Travel time has no data** for some island states (Cabo Verde, Seychelles). Cabo
  Verde also lies outside the prediction grid.
- **Convergence.** The 2026-11-07 fit (after the data-type fix): max classic
  R-hat 1.109 (`beta_raw[1,12]`), 7 over 1.1 (all beta_raw), 69 over 1.05
  (beta_raw 60, alpha 9); every other group under 1.04 (zeta_raw 1.039). On
  the split measure it is 1.16 (gate; worst alpha[3,1], coustani) and 1.18
  (beta), min bulk ESS 199 (gate) and 180 (beta). That is about where the
  2026-10-16 fit was, and a little worse than the 2026-10-20 fit.
- The 2026-10-20 fit (after the background-method fix): max
  classic R-hat 1.077, none over 1.1, 55 over 1.05 (beta_raw 50, alpha 5);
  every other group under 1.035. On the split measure it is 1.18 (gate) and
  1.20 (beta), bulk ESS 174, a little worse than before.
- The 2026-10-16 fit (2000 warm-up) was close: max R-hat 1.106,
  with 4 of 3,907 parameters over 1.1 and 72 over 1.05, all in `beta_raw` (64)
  and `alpha` (8, max 1.091). Every other group is under 1.03. The values are
  in `tar_read(resids_and_rhats)$rhats` and
  `outputs/figures/validation/20261014/last_rhats.csv`.
  - Doubling the warm-up barely helped. On the same measure (classic R-hat,
    `gelman_rhat()`) the fit before (2026-09-30, 1000 warm-up) had max 1.098,
    0 over 1.1 and 100 over 1.05.
  - Two R-hats are in use. `resids_and_rhats` is classic Gelman–Rubin;
    `coef_draws$convergence` (`extract_cv_draws()`) is rank-normalised split
    R-hat, which runs higher. On the split measure both fits are 1.14 (gate)
    and 1.16 (beta), with bulk ESS 187 → 201. Don't compare across the two.
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
- **Uncommitted work (2026-11-09):** everything up to 5d9e074 is committed,
  including the background-method fix, the os model, the country extracts,
  the summary-table columns, the covariate panel figures and the earlier
  checks. Not committed:
  - the data-type fix (`R/generate_model_data_records.R`) and its check
    (`extras/check_uncounted_pa.R`, untracked), from 2026-11-06;
  - in `_targets.R`: the `_os` block commented out (2026-11-06) and the
    fresh-process swap of `resids_and_rhats` and `coef_draws` (2026-11-09);
  - the note on passing functions in `R/in_fresh_process.R` (2026-11-09);
  - this file.
- **Session artefacts** (git-ignored): `outputs/session_20261001/`, with test
  renders, bioregion prototypes and final test figures, the abundance-driver
  analysis and its scripts; `outputs/overhaul_check/`; `outputs/greta_issue/`.

## Checks in `extras/`

- `check_gelman_rhat.R`: `gelman_rhat()` gives bit-identical results to
  `coda::gelman.diag()`, at a fraction of the memory.
- `check_maxsss.R`: `maxsss_thresholds()` gives thresholds identical to
  `PresenceAbsence::optimal.thresholds()`.
- `check_bg_method_average.R`: the background-method fix, on two tiny fits of
  the real data (see the first open item). Passes.
- `check_uncounted_pa.R`: the 2026-11-06 data-type fix. A tiny fit on
  `model_data_spatial_cx` patched in memory, checking the count and PA
  responses. `Rscript extras/check_uncounted_pa.R <image>`, about 1 min.
  Passes.
- `check_masked_presences.R` → `outputs/mask_check/masked_presences.csv`: every
  species × 10 km cell with a presence record, what hides it on the maps (or
  drops it before the model) and why. See the open item on hidden presences.
- `check_os_model.R`: the os model and its comparison functions, on tiny fits of
  the real data. It checks that:
  - κ enters `log_lambda` exactly, and at κ = 1 it is the current model;
  - `os_record_loglik()` reproduces greta's expected values for both models;
  - `os_predict_lambda()` matches an independent reconstruction;
  - the comparison runs end to end.
  `Rscript extras/check_os_model.R fit <seed> <image>`, then `... check
  <image_os> <image_current> <outdir>`. Passes.
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
