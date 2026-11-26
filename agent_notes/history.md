# History

Read when you need the back-story: what changed when and why, old names,
superseded settings, past fits. Current facts are in `AGENTS.md` and the other
notes. The full AGENTS.md as it was before the split is in
`history/agents_md_snapshot_2026-11-24.md`.

When a fact in `AGENTS.md` or a topic note is superseded, move a dated line
here rather than keeping both.

## Old names (before the 2026-10-14 overhaul)

Notes and scripts in `extras/` still use these.

- Targets: `X_rep_cx` → `X`; `model_fit_sre_rep_cx` → `model_fit`;
  `resids_and_rhats_sre_rep_cx` → `resids_and_rhats`; `preds_sm_rep_cx` →
  `preds`; `rep_tag_cx`, `rep_fit_image_cx`, `rep_pred_prefix_cx`,
  `rep_validation_dir_cx`, `rep_plot_dir_cx` → `run_tag`, `fit_image`,
  `pred_prefix`, `validation_dir`, `plot_dir`. Kept: `model_data_spatial_cx`,
  `data_summary_table_cx(_csv)`, `complex_members`.
- Functions: `fit_model_multispecies_pp_count_source_effect_reparam()` →
  `fit_model_multispecies_pp_count()`; `predict_lambda_reparam()` →
  `predict_lambda()`. The older functions of those names were deleted.
- Model sets: the original, `_sre`, `_rep` (the model on `model_data_spatial`)
  and `_rep_cx` (the one kept, on `model_data_spatial_cx`). `_noinf` (all
  inferred rows dropped) was fitted and removed earlier.

## Superseded settings

- Land cover: WorldCover, 8 classes (`landcover_covs`), until 2026-09-03; then
  9 ESA CCI classes, time-varying. Footprint: geodata, static (`footprint_raw`),
  until 2026-09-03; then Mu et al., annual.
- Covariates 10 → 11 and columns 180 → 198 (2026-09-03, shrubland split out of
  grassland).
- HMC: `Lmin = 3, Lmax = 25` (original) → 30 / 150 (2026-07-24) → 5 / 60 (the
  user, by 2026-08-25).
- NB dispersion: `log_size ~ N(2, 2)` (the July "V3") → a PC prior with
  theta = 0.061 (wired in, never fitted, rejected by a prior predictive check)
  → `gamma_nb ~ Exponential(≈3.0)` (2026-07-31, 8b40e7f).
- Design rows keyed on coordinates alone until 2026-09-28.
- Background points got one sampling method drawn at random until 2026-10-19.
- Present records with no count were zero counts until 2026-11-06.
- `resids_and_rhats` and `coef_draws` ran in the pipeline's process until
  2026-11-09.
- coluzzii's maps used the gambiae (Sinka) expert map until 2026-11-23.

## Timeline

- **2025-07-25** (3032ecf): counted "other"/larvae rows made presence-absence.
- **2026-07:** the source-effect model wouldn't mix (every R-hat over 1.1). The
  cause was sampler geometry: variance funnels, an NB-dispersion funnel at the
  Poisson limit, a flat `zeta`↔`sampling_re` ridge, and ~733 prior-only `zeta`
  dimensions. Four fixes were adopted (V1 longer HMC trajectories, V2 `zeta`
  over count sources only, V3 NB dispersion on the log scale, V5 centred
  hyperparameters); V4, dropping `sampling_re` from the count likelihood, was
  rejected by the user. 30 pooled chains on a subsample then converged (max
  R-hat 1.036). The production function was written 2026-07-24. The
  prediction-tensor fix to `predict_lambda_reparam()` was made 2026-07-25.
  An Lmax sweep (2026-07-29) found an optimum near 75 on effective samples per
  gradient; long static-HMC trajectories waste gradients, and R-hat per
  iteration flatters them. Records: `extras/sre_mixing_exploration.R`,
  `extras/sre_keepre_vs_original.md`, `extras/sre_lmax_sweep*` (their draws
  were deleted 2026-11-23).
- **2026-07-27:** the `africa_mess_br` terra bug diagnosed; `R/mess_safe.R`.
- **2026-07-29/31:** NB overdispersion PC prior derived; theta = 0.061 failed a
  prior predictive check (0 of 4 summary statistics), and NB1 itself may be
  misspecified (`extras/pc_prior_nb_overdispersion.md`). The user then set
  theta ≈ 3.0.
- **2026-08-28:** ESA land-cover proportion targets added and built.
  **2026-08-31:** year-matched land cover on the records
  (`model_data_spatial_landcover`, later folded into `model_data_spatial`).
- **2026-09-03:** WorldCover replaced by the 9 ESA classes (shrubland split,
  f93a0ee; 63 targets invalidated) and the static footprint by Mu et al. annual
  footprint (67 invalidated). Condition numbers measured; bioregion columns
  found to be fractional.
- **2026-09-28** (03bca90): design rows keyed on (lat, lon, `model_date`);
  refit 2026-09-29 (36,053 s); 3,703 → 5,730 rows.
- **2026-09-29:** why `expert_offset_maps` kept rerunning: GeoPackage
  timestamps plus the meta shared between machines through git. Fixed by the
  `OGR_CURRENT_DATE` pin (9f7c20e) and untracking the meta (b10436d,
  2026-09-28). `expert_maps` split into Sinka + pharoensis. Blank
  dominant-species plots traced to the geotargets `ID` → `value` rename.
- **2026-09-30:** fit with 1000 warm-up: max classic R-hat 1.098, 0 over 1.1,
  100 over 1.05. `gelman_rhat()` replaced `coda::gelman.diag()` (ef9f7ba).
- **2026-10-01/02:** `_cx` made the primary results. Response-curve and
  bioregion review: the user chose response curves "R5" and multipliers "B2",
  greyscale; per-species figures only.
- **2026-10-14:** the pipeline overhaul (below).
- **2026-10-16:** first fit under the new names (2000 warm-up; finished 04:29):
  max classic R-hat 1.106, 4 of 3,907 parameters over 1.1, 72 over 1.05
  (`beta_raw` 64, `alpha` 8, max 1.091); split R-hat 1.14 (gate) / 1.16 (beta),
  bulk ESS 187 → 201. `resids_and_rhats` and `coef_draws` ran out of memory
  after `preds` and were rebuilt alone (09:48, 10:26). Atlas cover, footer and
  citations (cfaf971, rebuilt 14:22–14:25). `in_fresh_process()` written
  (3fc3700); bioregions path fixed (446ede1). `tar_prune()` run: the store
  holds no `_rep_cx` objects. Of the CV, only `cv_tag`, `cv_k`, `cv_dir` and
  `cv_fold_ids` were built (they are still in the store).
- **2026-10-19:** the background sampling-method draw found and fixed (below);
  hidden presences checked; the os model built; the user commented out the CV
  targets; the Nigeria extract made.
- **2026-10-20:** refits. `model_fit` 10.9 h (finished 14:06; everything below
  by 15:13): max classic R-hat 1.077, none over 1.1, 55 over 1.05 (`beta_raw`
  50, `alpha` 5), every other group under 1.035; split 1.18 (gate) / 1.20 (beta), bulk ESS 174. Indoor HLC
  −0.04 (SD 0.18), outdoor resting 0.65. Against the 2026-09-30 fit: median
  abundance ×1.0–1.6 by species (most ×1.3), mean probability ×1.1–1.4, log λ
  correlations ≥ 0.99. `model_fit_os` 11.3 h (`os_model.md`).
- **2026-10-21:** `resids_and_rhats_os`; abundance ratio maps; count residual
  analysis.
- **2026-10-23:** the Uganda extract.
- **2026-11-06:** the data-type fix (`data.md`); the `_os` block commented out
  (2dec65c); the A4 covariate panels; summary-table columns `count_p`, `pa_1`,
  `p_n`, `p_unique`.
- **2026-11-07:** refit after the data-type fix: 12.3 h, finished 05:30,
  everything below by 05:57. Max classic R-hat 1.109 (`beta_raw[1,12]`), 7 over
  1.1 (all `beta_raw`), 69 over 1.05 (`beta_raw` 60, `alpha` 9); every other
  group under 1.04 (`zeta_raw` 1.039). Split 1.16 (gate; worst `alpha[3,1]`,
  coustani) / 1.18 (beta), min bulk ESS 199 / 180. It overwrote the 2026-10-20
  fit's outputs (same `run_tag`). Its data, `model_data_spatial_cx`: counts
  15,911 / 2,437 (absent / present), pa 66,886 / 12,130, po 3,468, background
  249.
- **2026-11-09:** `resids_and_rhats` and `coef_draws` swapped to
  `in_fresh_process()` (0f08590; first built that way 2026-11-18, 10:05 and
  10:07). The "other"-method counts found. The data-type fix committed
  (03bab1b).
- **2026-11-11:** notes committed (8263013); MOZ and AGO cube crops.
- **2026-11-18:** `plot_pred_panel_main_vectors`.
- **2026-11-19/20:** data-summary docx (Node prototype, then the targets);
  larvae checked; manuscript numbers given to the user.
- **2026-11-21/22:** the user's filter dropping coluzzii records from sources
  2676 and 1001743; `model_data_records` rebuilt 2026-11-21 21:29; refit 9.1 h,
  finished 2026-11-22 07:38; `resids_and_rhats` 2026-11-23 09:19 (max 1.088,
  none over 1.1, 84 over 1.05).
- **2026-11-23:** disk cleanup (82 → 47 GB; `outputs.md`). coluzzii expert map
  added, then built with everything below it (13:07–16:21, no refit).
  `africa_mess_br` built at 20:51.
- **2026-11-24:** Nigeria TerraClimate extract. These notes split into
  `AGENTS.md` + `agent_notes/`. The commented-out targets in `_targets.R` (15
  blocks, 719 lines: the CV and `_os` sections, the WorldCover and Malaria
  Atlas Project covariates, PCA, switched-off plots, and the other-process
  variants of `resids_and_rhats`, `preds` and `coef_draws`) moved verbatim to
  `extras/parked_targets/` (7 files), each leaving a pointer comment; the
  parsed pipeline is unchanged (3,333 → 2,658 lines). The fit function's header
  comments brought up to date: they still described the July sampler (30 /
  150), `log_size` dispersion and 2000 samples. The three Kenya memories moved
  to the kenya_mess project's memory.
- **2026-11-25:** the count under-prediction analysed, read-only, on the
  2026-11-22 fit (`extras/count_misfit/`). That replaced `model.md`'s
  2026-10-21 bullet, which covered both models on the data before the
  2026-11-06 data-type fix:
  - `gamma_nb` was 176 (arabiensis) and 314 (funestus);
  - P(0) was 0.86 / 0.95, against observed zero shares of 0.60 / 0.69;
  - so 53% / 73% of positive counts sat above the predictive 97.5% quantile;
  - neither model reached the counts over 500.

  Figure: `count_residuals_arabiensis_funestus.png` in
  `outputs/model_comparison/20261014/`.

## The background sampling-method draw (found and fixed 2026-10-19)

- Found by overhaul check 04. The fit gave each background point one sampling
  method, at random. Those points are the presence-only integral (weights
  ~120,000 km² each against 1 for a presence), and `sampling_re` is shared by
  all three likelihoods, so the draw moved every method effect.
- The 2026-09-30 and 2026-10-16 fits had identical data apart from this draw
  (and warm-up). `sampling_re` was well mixed in both (R-hat ≤ 1.02), yet the
  posterior means differed by up to 5.5 posterior SDs: outdoor resting
  1.03 → 0.07; indoor HLC, the method the maps predict for, −0.32 → +0.03.
- So every species' abundance rose by about ×1.4 (median ×1.1–1.6 by species)
  and mean probability by 15–60%. The correlations in check 04 (≥ 0.99) hid it,
  since a shared scale change keeps the correlation. Relative comparisons
  between species were mostly unaffected.
- Each method's share of background area hardly changed (indoor HLC 8.5% →
  8.2%): it is where a method's few points fall, not how many there are.
- The counts did not hold the level: the shift was the same (×1.3–1.4) at cells
  with indoor-HLC counts, which are very overdispersed (variance ~100–300× the
  mean for arabiensis, gambiae and funestus).
- Fix: the averaged method effect (`model.md`). Tested with
  `extras/check_bg_method_average.R`; refitted 2026-10-20, when the method
  effects came out between the two earlier fits.

## The 2026-10-14 overhaul

`_targets.R` went from 232 targets to 186, on branch
`reset_cx_to_main_pipeline`; committed as 3b2c00c.

- **Removed (50 targets):** the original chain (`model_fit`,
  `resids_and_rhats`), `_sre`, the whole `_rep` section, and the stale tail
  after the CV section.
- **Replaced (1):** the bioregion slope heatmaps, by
  `plot_bioregion_multipliers`.
- **Renamed (38):** see "Old names".
- **Added (4):** `model_fit_image`, and the expert-range dominant maps
  (`pred_dominant_expert`, `pred_dominant_001_expert`,
  `plot_pred_dominant_expert`), carried over from `_rep`.
- **Relinked (3):** `abundance_cubes` (new predictions, masked, tracked as
  files) and the two CV targets (now on `model_data_spatial_cx`).
- **`R/`:** the fit and predict functions renamed; deleted the two older fit
  functions, the refit function, the old `predict_lambda()`, `inits()`,
  `split_sims()`, `scale_predictions()`, `make_species_atlas()` and the
  slope-heatmap code; `render_species_atlas_pages()` moved to its own file. A
  further 67 functions in `R/` already had no caller and were left.
- **Output paths** lost `_rep_cx`, and the run tag became `20261014`.
- **Checks** (`extras/overhaul_checks/`, results in `outputs/overhaul_check/`):
  - `01_snapshot_before.R`: the pipeline, the store and the data inside the old
    fit image before the change (`snapshot_before.rds`). Don't regenerate it.
  - `02_check_wiring.R`: every carried-over command equals the old one under
    the rename map apart from listed deliberate edits; the graph, removals and
    additions are as intended; nothing upstream became outdated. Passes.
  - `03_smoke_pipeline.R`: the model core end to end on a 2-chain, 20-sample
    fit in a scratch store, including a refit to the same path cascading to the
    rasters and cubes. Passes. Its fit image held the same data as the old
    production image in 48 of 57 objects; the other 9 are run settings and the
    background sampling-method draw.
  - `check_cv_fold_resume.R`: the CV fold resume guard, without fitting.
    Passes.
  - `04_compare_after_run.R`: stored inputs and the new fit image's data
    against the snapshot, cubes against `pred_lambda`, and old-against-new
    results. Run on the real store 2026-10-19: all checks pass
    (`compare_after_run.rds`). Its section D report reads as "MCMC noise" but
    hid the background-draw level shift above.
  - `list_duplicate_outputs.R` → `duplicate_outputs.csv`: superseded output
    files and orphaned store objects (most deleted 2026-11-23).
