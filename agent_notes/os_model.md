# The alternative model: offset strength (`os`)

Read when the offset-strength model, its comparison with the current model or
the decision on the offset comes up. **Its targets are commented out** (since
2026-11-06, at the user's request: only the main pipeline runs for now; the
whole `_os` block is parked in `extras/parked_targets/os_model.R`), so `tar_manifest()` has no `_os`
targets. `R/os_*.R` are unchanged and still sourced, but nothing calls them.
The fit image and outputs are still on disk, and the store keeps their entries
until `tar_prune()` is run. **All results below are on the data before the
2026-11-06 data-type fix.** Whether the offset should stay, at what strength or
with a tighter prior is the user's decision; they have the results.

## Why

The *An. gambiae* offset hides 127 presence cells (`maps.md`). The offset is
there to keep species out of the Sahara, where no data say so, but on the maps
the bare-ground mask does most of that, so a plain per-species power on the
offset looked safe there.

## The model

`model_fit`'s model with one change:

    log λ_s = α_s + x·β_s + κ_s · log(offset),   κ_s = exp(b_s),   b_s ~ Student-t(3, 0, 0.05)

- κ_s = 1 is the current model. Without evidence κ_s stays within about
  0.95–1.05 (61% of the prior); the heavy tails let a species whose data
  clearly favour another value move there. The penalty, in log-likelihood
  units, is about 4 at κ = 0.8, 8 at 0.5 and 11 at 0.25.
- The user chose the Student-t over a horseshoe (whose funnel geometry greta's
  sampler struggles with), and scale 0.05 over 0.1 for a stronger penalty.
- The fit applies κ to each record's monthly offset.

## The user's rules for this work

- Don't modify the existing functions: the os versions are copies with an
  `os_` prefix (`os_fit_model_multispecies_pp_count()`, `os_predict_lambda()`,
  `os_extract_cv_draws()`), plus `os_record_loglik()`, `os_compare_loo()`,
  `os_compare_maps()`, `os_plot_kappa()`, `os_plot_range_change()`,
  `os_plot_abundance_difference()`.
- No CV, no R-hat comparisons and no long fits in the comparison. (On
  2026-10-21 the user asked for `resids_and_rhats_os`, the same validation as
  the current model's.)
- Only the distribution and abundance maps for this model.
- The user runs the fit in the pipeline; tiny test fits (2 chains × 20) in the
  foreground only.

## Predictions (`os_predict_lambda()`)

- For each species and draw, the effective offset is the mean over 2024's 12
  months (`offsets_2024_10`) of offset^κ. With κ = 1 this is `offsets_avg_10`
  (relative difference ≤ 1.1e-7).
- `<prefix>.tif` is λ **including** the offset (`preds_os$lambda`), since a
  per-species power can't be applied afterwards. `pred_lambda_os` and
  `pred_p_os` take the same masks as the current model.
- `preds_os`, `coef_draws_os` and `resids_and_rhats_os` ran in fresh processes
  (`in_fresh_process()`, called with strings).

## The comparison (`outputs/model_comparison/<run_tag>/`, needs both fits)

- **`compare_loo_os`** (`os_compare_loo()`): PSIS-LOO on every record (counts,
  presence-absence, presence-only + background; 105,314 rows): the difference
  os − current with its SE, overall, by data type and by species. It uses
  `os_record_loglik()`, the fit's three likelihoods rebuilt in R from a draws
  file (κ = 1 when the file has none), which matches greta's expected values to
  about 1e-14 for counts and PO/bg. For presence-absence it uses `-expm1(-λ)`,
  more accurate than the fit's `logit_icloglog_safe()` at tiny λ, with the same
  clamp on log λ.
- **`compare_maps_os`** (`os_compare_maps()`): presence cells hidden and newly
  shown; predicted range by offset level; area over threshold before the masks
  where the offset is < 0.01.
- **`plot_kappa_os`**: the κ table, a dot-and-interval figure and the offset^κ
  curves. **`plot_range_change_os`**: per-species maps of both / current only /
  os only, with the records.

Outputs: fit image `outputs/images/model_fit_os.RData`; rasters
`outputs/rasters/multispecies_pp_os{,_p,_p_cv}.tif`; maps
`outputs/figures/distribution_plots/distn_os_<run_tag>/`; validation
`outputs/figures/validation/os_<run_tag>/` (`resids_and_rhats_os`, on
`model_fit_image_os`); draws
`outputs/draws/coef_draws_os.rds`.

Tested on tiny fits (2 chains × 20 + 20) of the real data with
`extras/check_os_model.R`: κ enters `log_lambda` exactly and at κ = 1 it is the
current model; `os_record_loglik()` reproduces greta's expected values for both
models; `os_predict_lambda()` matches an independent reconstruction; the
comparison runs end to end. Run as
`Rscript extras/check_os_model.R fit <seed> <image>`, then
`... check <image_os> <image_current> <outdir>`. The tiny fit already moved κ
far from 1 for some species, but 20 warm-up iterations say nothing about the
real posterior.

## Results (fitted 2026-10-20; in `outputs/model_comparison/20261014/`)

`model_fit_os` took 11.3 h.

- **κ collapsed towards 0 for 17 of 18 species.** Posterior medians run from
  0.0005 (pharoensis, coustani) to 0.19 (rivulorum); gambiae, whose offset it
  is, 0.11 (90% 0.10–0.13). Only moucheti went up (2.1, 90% 1.8–2.4). The
  Student-t's tails let this through: the penalty grows only logarithmically
  (about 18 log-likelihood units at κ = 0.0005). Below κ ≈ 0.01 offset^κ ≈ 1,
  so the chains disagree on log κ there (pharoensis −13 to −7 by chain) without
  it mattering. α drops by 1–3 for most species to take up the offset's level.
- **LOO prefers os by 17,744 elpd (SE 433, z = 41)**, and in every data type:
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
- **Abundance ratio maps** (2026-10-21, `os_plot_abundance_difference()` on the
  stored `pred_lambda` and `pred_lambda_os`; not a target):
  `abundance_difference_<species>.png` (current | os | log2 ratio) and
  `abundance_ratio_all.png`. os is higher almost everywhere, by ×16–64+ where
  the offset is low: the Maghreb coast, the northern Sahel edge, the Ethiopian
  highlands and Horn, and Namibia/South Africa; the same pattern for most
  species. pharoensis is ×16–64 nearly everywhere (its α didn't drop to
  compensate). moucheti is the only one with large lower areas.
- **The os fit mixes much worse** (`resids_and_rhats_os`, 2026-10-21): max
  classic R-hat 1.253, 49 over 1.1 and 1,352 over 1.05, against the current
  fit's 1.077, 0 and 55 at the time. The worst is `log_kappa` (9 of 18 over
  1.1: merus 1.25, pharoensis 1.24, rufipes 1.20, pretoriensis 1.20, funestus
  1.18, leesoni 1.17, arabiensis 1.15, melas 1.15, nili 1.09), then `alpha` (3
  over 1.1), `beta_raw` (37 over 1.1, 1,246 over 1.05) and `zeta_raw` (76 over
  1.05, max 1.08). Once κ is near 0 the likelihood is nearly flat in log κ, so
  the chains drift along the prior's tail, and α and β, which trade off against
  κ·log(offset), drift with them. The maps depend on offset^κ, which stays ≈ 1,
  so they are less affected than the R-hats suggest. The LOO and β-based
  results rest on a fit that hasn't converged.
- **Count residuals, arabiensis and funestus** (2026-10-21; figure
  `count_residuals_arabiensis_funestus.png` in the comparison folder). Both
  models' under-prediction comes mainly from the NB1 dispersion, not the offset
  (`model.md`). os cuts the share of count records above the predictive 97.5%
  quantile from 21.6 to 17.1% (arabiensis) and 23.2 to 19.6% (funestus). The
  gain is where the offset is 0.01–0.1 (55 → 20%, 49 → 19%) and in Madagascar
  (26 → 7%, 29 → 10%); it is a little worse where the offset is > 1. A record
  that caught anything had on average a 20% / 11% predicted chance of catching
  ≥ 1. Neither model reaches the counts over 500 (50 and 44 records, above all
  100 simulations).
