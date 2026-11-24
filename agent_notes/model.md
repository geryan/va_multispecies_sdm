# The model

Read when working on the fit, likelihoods, priors, the design matrix,
predictions, convergence, cross-validation or known misfit. Past fits and the
mixing work are in `history.md`.

## Fit function and settings

- `fit_model_multispecies_pp_count()` (`R/fit_model_multispecies_pp_count.R`).
  Until 2026-10-14 it was `fit_model_multispecies_pp_count_source_effect_reparam()`;
  notes and scripts in `extras/` still use the old names (map in `history.md`).
- Equations: `extras/model_equations_sre_reparam.md`. It describes 10
  covariates and 180 columns; the model now has 11 and 198.
- `model_fit`: 40 chains, 2000 warm-up + 1000 samples, 8 cores,
  `hmc(Lmin = 5, Lmax = 60)` (set inside the function). 9–12 h on the laptop
  (11.9 h 2026-10-16, 12.3 h 2026-11-07, 9.1 h 2026-11-22). Fit image ~6.75 GB.
- greta 0.6.0 has static HMC only (no NUTS) and a diagonal mass matrix. It runs
  all chains in one process, and sampling is not seed-reproducible (TensorFlow
  randomness).

## Design

- 11 covariates: 9 ESA land-cover classes (crop_other, irrigated, tree,
  shrubland, grassland, wetland, mangrove, urban, water), `footprint` and
  `prox_to_sea`, plus their interactions with 17 bioregions: 198 columns per
  species. Layers and record matching are in `covariates.md`.
- **The intercept is `alpha`, not a design column.** It is per species
  (hierarchical `alpha_mean`, `alpha_sd`) and added with `sweep()`. "Dropping
  the intercept" would mean dropping `alpha`, the only cross-species pooling of
  overall abundance; the betas have none.
- `bare` and `sparse` are left out because the classes sum to exactly 1. With
  all classes the design is singular against `alpha` (condition number 2.3e8).
  Intercept + the 11 model covariates: condition number 88.6 raw, 53.3
  unit-scaled (WorldCover's were 54.0 / 33.3).
- **Bioregion columns are fractional, not 0/1, and not a strict partition**
  (an earlier claim that they were was wrong). On the 117,584-row
  `model_data_spatial` (2026-09-03): 114,586 rows sum to 1, 868 sum to exactly
  0 (in no bioregion), ~2,130 lie between. So the interaction block is severely
  ill-conditioned rather than singular, and the priors (`beta_sd` on main
  effects, `beta_regularised_sd = 0.1` on interactions) decide how the shared
  part is split. 5 interaction columns are all zero over the distinct
  locations. Don't quote condition indices for the full design (an ~1e6 vs ~3e3
  comparison was never explained).
- **Design rows:** one per distinct (latitude, longitude, `model_date`),
  `distinct_idx`, mirrored in `cv_designmat()`. It is the key the time-varying
  covariates and offset are joined on. A coordinate-only key (before
  2026-09-28) gave every record at a site the first record's values (40,082 of
  117,584 rows had the wrong offset). A year key isn't enough (the offset is
  monthly); year-month would save only ~2% of rows (5,630 vs 5,730).

## Likelihoods and priors

- **Counts:** negative binomial, NB1. `gamma_nb := mu / size`, so
  var = mu (1 + gamma_nb) and gamma_nb = 0 is Poisson. Prior
  `gamma_nb ~ Exponential(theta)` per species, theta = −log(0.05) / (2 − 1) ≈ 3.0,
  i.e. Pr(variance > 2 × Poisson) = 0.05 (the user's setting, 2026-07-31,
  8b40e7f). The derivation, a rejected theta of 0.061 and the doubts about NB1
  are in `extras/pc_prior_nb_overdispersion.md`. greta's
  `inverse_gamma(alpha, beta)` takes beta as the scale. No count is filtered
  out: the original's `count < 1000` filter is commented out (81 of the 18,340
  count rows are ≥ 1,000, up to 342,645). The header of
  `R/os_fit_model_multispecies_pp_count.R` still says the filter applies.
- **Presence-absence:** Bernoulli via icloglog (`logit_icloglog_safe()`).
- **Presence-only + background:** Poisson (a Berman–Turner quadrature; each
  background weight is ~120,000 km² against 1 for a presence), with a
  travel-time bias (`bias_tt_5`).
- **Shared by all three:** `sampling_re`, one effect per sampling method for
  all species. **Counts only:** per-source `zeta`, over count sources only.
- **Background points have no sampling method** (since 2026-10-19). In the
  presence-only likelihood each takes
  `sampling_re_bg = log(sum_m p_m * exp(sampling_re[m]))`, p_m being the
  methods' shares of the records. This equals one copy of each point per method
  with weight w × p_m. `predict_cv_heldout()` does the same with the fit's
  shares (`sm_prop`, saved by `extract_cv_draws()`). Before, each point got one
  method drawn at random, which moved every method effect and the level of
  every map (`history.md`). The validation plots by method show background rows
  as an "NA" panel. Test: `extras/check_bg_method_average.R`.
- **Never drop a model term to improve mixing or R-hats** (the user, rejecting
  "V4" in 2026-07: "you can't just drop a part of the model because it reduces
  R-hats; it's a core part of the structure of the data and system").

## Predictions

- `predict_lambda()` samples parameter draws with one small
  `calculate(beta, alpha, sampling_re, values = draws, nsim = )`, then does the
  matrix multiply in R, one species at a time (~5.5 GB, ~100 s at nsim = 100).
  Pushing the design through `calculate(nsim = )` tiles it into one
  nsim × pixels × columns tensor (~53 GB at nsim = 100 over 367,853 pixels), and
  TensorFlow aborts hard: a killed callr process, not an R memory error.
- Maps are at 10 km, for indoor human landing catch, with 2022 land cover,
  2024 footprint and the 2024 mean offset (`offsets_avg_10`). Masking is in
  `maps.md`.

## Convergence

- **Current fit (2026-11-22):** classic R-hat max 1.088, none over 1.1, 84 over
  1.05. Values: `tar_read(resids_and_rhats)$rhats` and
  `outputs/figures/validation/20261014/last_rhats.csv`.
- The worst parameters are always in `beta_raw` and `alpha`; every other group
  sits under ~1.04. Doubling the warm-up (1000 → 2000) barely helped.
- **Two R-hats are in use; don't compare across them.** `resids_and_rhats` is
  classic Gelman–Rubin (`gelman_rhat()`, bit-identical to `coda::gelman.diag()`
  at a fraction of the memory). `coef_draws$convergence` (`extract_cv_draws()`)
  is rank-normalised split R-hat, which runs higher.
- Earlier fits' numbers are in `history.md`.

## Known misfit and weak spots

- **Count dispersion** (2026-10-21, both models, on the data before the
  2026-11-06 data-type fix): with `gamma_nb` 176 (arabiensis) and 314 (funestus)
  the model gives P(0) 0.86 / 0.95 against observed zero shares 0.60 / 0.69, so
  53% / 73% of positive counts sit above the predictive 97.5% quantile. Neither
  model reaches the counts over 500. Figure
  `count_residuals_arabiensis_funestus.png` in `outputs/model_comparison/20261014/`.
- **Structure:** method effects are shared across species (no species × method
  term), and the offset is *An. gambiae*-based for every species. This may
  disadvantage some species, e.g. funestus.
- **Thin data behind some peak abundances**
  (`outputs/session_20261001/abundance_drivers.txt`): 244 of the 263 surveyed
  locations with ≥80% grassland are in Madagascar, so the grassland effect
  (funestus, maculipalpis, rufipes) is largely a Madagascar effect; the
  mangrove effect (melas, squamosus) rests on 2 locations; the shrubland effect
  for leesoni and rivulorum on 3 detections each.
- **From the 2026-10-01 response review**
  (`outputs/session_20261001/response_review/REVIEW.md`): bare/sparse is the
  best "class" for coluzzii, ziemanni and arabiensis (×21, ×8, ×4 against the
  typical recorded landscape), which the offset cancels on the maps; melas ×
  `prox_to_sea` has a slope of 23 per unit (near-separation); explicit
  bioregion intercepts would be near-collinear with the interactions.

## Why there are no covariate-slope figures

- Land cover is compositional with bare and sparse left out, so a class's slope
  on its own compares it with pure bare ground, which almost no data cover
  (0.66% of surveyed design rows are over 25% bare/sparse; that contrast trades
  off with `alpha`, posterior correlation median −0.72).
- The interaction terms act as species × bioregion intercepts, so slope
  heatmaps showed bioregion levels, not covariate effects.
- Drawn instead: response curves from the main effects and bioregion
  multipliers (`outputs.md`). For curves, use the main effects (in effect the
  average bioregion), not slopes at a species' typical location (they carry the
  bioregion levels), and +10 percentage points of a class rather than pure-cell
  contrasts (which extrapolate for rare classes).

## Cross-validation (commented out)

- The user commented out the `cv_*` targets on 2026-10-19 and wants no CV for
  now: leave them so. Since 2026-11-24 they are parked in
  `extras/parked_targets/cross_validation.R`. Only the fold fits were written (`cv_folds_file`,
  `cv_fold_draws`), never built; five folds of ~2 h each. No `cv_*` target is in the manifest
  now; the store still holds `cv_tag`, `cv_k`, `cv_dir` and `cv_fold_ids` from
  2026-10-16.
- Scoring targets are not written. Helpers exist: `predict_cv_heldout()`,
  `extract_cv_draws()`, `split_cv_fold()`, `cv_designmat()`.
- Folds are assigned to the ~250 background Voronoi cells, because the
  presence-only quadrature tiles the continent (`cv_spatial_folds()` and the
  comment block at the top of the parked CV targets).
- `fit_cv_fold()` reuses a fold file only when its stored fingerprint matches
  the data, folds, settings and fit function
  (`extras/overhaul_checks/check_cv_fold_resume.R`).
