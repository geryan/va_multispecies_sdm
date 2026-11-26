Title:

Counts badly under-predicted: NB1 makes catches ~4× too rare, and no term absorbs source or duration variation

Description (everything below the line):

---

**Fit:** 2026-11-22 (`coef_draws.rds` md5 `933170f1…`). Read-only analysis, 2026-11-25: nothing in the pipeline was changed or refitted.

## What we see

`redids_count_spp_spatial.png` (validation figures) shows many strong under-predictions of counts. I took the model's predictive distribution over all 1,000 posterior draws. 57% of the 2,436 positive counts lie beyond its 99th percentile, i.e. the model gave under a 1% chance of a count that large. It isn't only big counts: 97 of the 287 counts of exactly 1 had under a 1% predicted chance of any catch.

## Diagnosis

1. **Catches are predicted far too rarely; their size is closer.**
   - NB1 predicts a catch on 3.1% of the 18,340 count records; 13.3% have one.
   - On the 4,397 reported (non-inferred) records, it predicts 9.0% against 55.4% observed.
   - Given that a catch happened, only 12.8% of positive counts lie beyond the model's 99th percentile (1% if calibrated), against 57% overall.
2. **The presence-absence (PA) data set each species' level.**
   - PA records carry 94% of the Fisher information about each species' overall abundance, i.e. of how sharply the data pin it down. Counts carry 6%; if they were Poisson at the same means, they would carry 73%.
   - So λ is on a detection scale. The PA formula 1 − e^(−λ) gives a catch on 16.0% of the count records, and on 41.7% of the reported ones.
3. **NB1 ties the chance of a catch to its dispersion.**
   - Under NB1 (variance = mean × (1 + γ)), P(count > 0) = 1 − exp(−μ / d), with d = γ / ln(1 + γ).
   - Producing catches of tens at a mean near 1 takes γ ≈ 100–300 (funestus 313, arabiensis 173, gambiae 98).
   - That divides the chance of a rare catch by d = 54.5, 33.6 and 21.4.
4. **The extra variation is structured, and nothing absorbs it.**
   - Of the variance of log(count / μ) at positive counts, 49% is between sources and 28% between location-dates within a source. The remaining 23% is within location-dates.
   - A location-date is source × method × detailed method × location × start date × duration.
   - The variation rises with `study_months`: the slope on the log scale is 0.57 within sources and 0.92 overall.
   - ζ, the per-source count effect, has its SD fixed at 0.1: ±2 SD is ×0.82–1.22, and the fitted ζ means have SD 0.11. The sources' log(observed / predicted) totals have SD 2.56.
5. **The `gamma_nb` prior is binding.**
   - At the posterior, the count log-likelihood rises 2.74–3.00 per unit of γ for all 18 species. The prior falls 3.0 per unit.
   - With the means held fixed, the counts alone would choose γ 1.8–6.0× larger. That would make catches rarer still (1.9% predicted).

**Not the cause:**
- Duplicated records: 19% of positives. Dropping them leaves 57.0% beyond the 99th percentile.
- Inferred zeros: 76% of count records. Dropping them changes the dispersion the counts choose by 1–5% for the main species.

## Is it the distribution?

Partly. I held the fitted means fixed and chose only the dispersion per species from the counts alone, plus a multiplier on the means where stated. These rank the options; they don't predict a refit.

The first row is therefore not the model as fitted. As fitted (posterior γ), the model puts 57% of positives beyond the 99th percentile and predicts a catch on 3.1% of records. The γ the counts choose is 1.8–6.0× larger, which takes those to 63% and 1.9%.

| scenario | log-likelihood (higher is better) | positives beyond the 99th percentile | predicted share with a catch (observed 13.3%) |
|---|---|---|---|
| NB1, γ chosen by the counts | −24,830 | 63% | 1.9% |
| NB2 (variance = mean + mean²/k) | −38,624 | 99% | 0.1% |
| Poisson-lognormal | −22,961 | 58% | 4.7% |
| NB1, own count level | −20,267 | 22% | 8.9% |
| NB2, own count level | −19,499 | 3.3% | 12.2% |
| Poisson-lognormal, duration offset + level | −17,631 | 6.9% | 13.5% |

Freeing the count scale matters most. Once it is free, the shape matters too: Poisson-lognormal fits best, then NB2, then NB1.

## Options

Each is a 9–12 h refit, and none drops a term.

- [ ] Estimate ζ's SD instead of fixing it at 0.1. Watch for the flat ζ ↔ `sampling_re` ridge found in July.
- [ ] Give counts their own intercept and a `log(study_months)` term with its own coefficient. The coefficient is about 0.6 within sources, so not a fixed offset. Perhaps also a location-date effect.
  - No structured effort field exists in the data, and human biting rate is recorded mostly for complexes.
- [ ] Then NB2 or Poisson-lognormal. Ideally the PA link would come from the same distribution (under NB2, 1 − (1 + λ/k)^−k), or use a hurdle.
- [ ] Revisit the `gamma_nb` prior only alongside the above, if NB1 stays.
- [ ] Check records:
  - funestus 342,645 (source 1002459);
  - arabiensis 51,453 (1001684);
  - the duplicated pairs: pharoensis 9,573 (1098), moucheti 4,118 (1461) and melas 2,022 (2502).

## Record

In the working copy, not yet committed:
- `extras/count_misfit/README.md`: the full findings, terms and caveats.
- The scripts and `run_all.sh`, in `extras/count_misfit/`.
- The outputs and logs, in `outputs/count_misfit/fit_20261122/` (not in git).

Earlier work: `extras/pc_prior_nb_overdispersion.md` (July: NB1 suspected, NB2 suggested).
