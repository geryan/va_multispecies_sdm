# Count under-prediction: analysis of the 2026-11-22 fit

2026-11-25. The question (the user): `redids_count_spp_spatial.png` shows many
strong under-predictions of counts (purple). Why, and what could be done? Is it
that the counts are too overdispersed for the distributions used? Read-only:
nothing in the pipeline was changed and nothing was refitted.

Every number below comes from the logs in
`outputs/count_misfit/fit_20261122/logs/` (not in git), with the script that
prints it in brackets.

## Which fit this describes

Check these before comparing with the live store. The next refit overwrites
`coef_draws.rds`.

| | |
|---|---|
| `outputs/draws/coef_draws.rds` | 1,000 posterior draws; 29,848,648 bytes; 2026-11-22 07:40:09; md5 `933170f15be2cc07a07cf55053783200` |
| `model_fit` (`tar_meta`) | 2026-11-22 07:38:25 |
| `model_data_spatial_cx` | 2026-11-21 22:32:08, data hash `1b0fc8373d478fc1` |
| `resids_and_rhats` | 2026-11-23 09:19:58 (the figure's source) |

The figure: `outputs/figures/validation/20261014/redids_count_spp_spatial.png`
("redids" is its real name).

## Terms

- **Count record**: a row of `model_data_spatial_cx` with
  `data_type == "count"`. There are 18,340: 2,436 positive (a count above 0),
  1,961 recorded zeros, and 13,943 **inferred zeros**. An inferred zero is a
  zero added for a species not recorded at a location-date where the other
  species were all counted. It is dropped where the species or its complex was
  ever recorded present at that location (`agent_notes/data.md`).
- **Location-date**: source × method × detailed method × latitude × longitude ×
  start date × `study_months`, the grouping zeros are inferred in.
- **`study_months`**: (end − start in days) / 30 + 0.5, from dates recorded to
  the month (`R/clean_full_data_records.R`). It is the calendar span of the
  sampling, not trap-nights.
- **μ (expected count)**: the model's mean for a count record,
  λ × e^`sampling_re` × e^ζ. Rebuilt for each draw by `os_record_loglik()`,
  which matches greta's expected values to ~1e-14 (`agent_notes/os_model.md`).
- **NB1** (the model's count distribution): a negative binomial whose variance
  is the mean × (1 + γ), with γ = `gamma_nb`, one per species. Its chance of a
  zero is (1 + γ)^(−μ/γ).
- **NB2**: a negative binomial whose variance is the mean + mean²/k, so the
  spread grows with the mean.
- **Poisson-lognormal (PLN)**: a Poisson count whose rate varies log-normally
  between records. σ is the SD of the log rate, so e^σ is the factor one SD
  multiplies the rate by.
- **P(Y ≥ y)**: the model's chance of a count at least as large as the one
  observed. It is averaged over the 1,000 draws, computed exactly rather than
  simulated. Below 0.01 means the count is beyond the predictive 99th
  percentile.
- **PIT**: where the observed count falls in the predictive distribution, from
  0 (far below) to 1 (far above). If the model is right, it spreads evenly over
  0–1. Discrete counts get a random jitter (seeded).
- **PA formula**: the chance of a presence that the presence-absence
  likelihood gives, 1 − exp(−λ e^`sampling_re`) (icloglog). It is computed at
  the count records without ζ, which PA does not have.
- **Fisher information about a species' level**: how sharply a set of records
  pins down α, the species' overall log abundance. Shares by data type say
  which data set the level.
- **Plug-in**: computed at each record's posterior-mean μ and the posterior
  median γ, as if they were known.

## Short answer

Partly the distribution, but mostly not. The counts vary far more than the
model's means allow, but most of that variation is structured: it sits between
sources and location-dates, and it grows with the sampling duration. The model
has no term that can absorb it: ζ, the per-source count effect, has its SD
fixed at 0.1. NB1's shape then turns it into the pattern on the map:

- The PA records set each species' level (94% of the information). So λ is
  roughly on the scale of how often a species is caught.
- Catches are tens to thousands. NB1 can produce them at that mean only with
  γ ≈ 50–300.
- NB1 ties the chance of a catch to μ/γ. When catches are rare, a γ that large
  divides their chance by about γ / ln(1 + γ): 21–55 for gambiae, arabiensis
  and funestus, and 12 for coluzzii.

So the model is roughly right about the size of a catch given there is one,
and far wrong about how often there is one. That is why even counts of 1 come
out purple.

## Findings

### 1. What the purple dots are (02)

- The plot's residual is DHARMa's normal score: where the observed count falls
  among 100 simulated counts. It is capped at 4.75 when the count is above all
  100 (`R/validation_and_checking.R`). 1,214 records are capped, all of them
  positive counts.
- Over the 1,000 draws, of the 2,436 positive counts, P(Y ≥ y) is below 0.025
  for 69% (1,693), below 0.01 for 57% (1,393) and below 0.001 for 30% (726).
- It isn't only big counts. Of the positives beyond the 99th percentile, the
  median count is 37 and 21% are ≤ 5. 97 of the 287 counts of exactly 1 have
  P(Y ≥ 1) < 0.01: the model gave under a 1% chance of catching anything.
- The model's chance of any catch at the positive records: median 0.031,
  quartiles 0.0065–0.13.
- Totals are under-predicted too, over all records with zeros included:
  Σcount / Σμ = 36.2, and 4.78 without the largest 1% of counts. The median μ
  is 0.67 at the positive records, against a median positive count of 17.
- Green (over-prediction) is rare partly by construction. A zero cannot sit far
  below the prediction when P(0) ≈ 0.97.

### 2. The miss is mainly how often there is a catch (02, 06)

Share of count records with a catch:

| species | count records | observed | NB1 predicted | PA formula |
|---|---|---|---|---|
| arabiensis | 1,375 | 0.505 | 0.150 | 0.664 |
| funestus | 1,362 | 0.388 | 0.065 | 0.469 |
| pharoensis | 1,266 | 0.200 | 0.015 | 0.127 |
| gambiae | 594 | 0.285 | 0.073 | 0.426 |
| coluzzii | 588 | 0.264 | 0.089 | 0.343 |
| all 18 | 18,340 | 0.133 | 0.031 | 0.160 |

- The overall agreement of the PA formula (0.160 against 0.133) averages two
  opposite errors:
  - On the 4,397 reported records, 55.4% have a catch. The PA formula gives
    41.7% and NB1 9.0%.
  - On the 13,943 inferred zeros, the PA formula gives 7.9% and NB1 1.2%. Some
    inferred zeros sit where λ expects the species: arabiensis 150 records at
    54%, funestus 415 at 40%.
  - 1,141 of the 1,874 count location-dates hold an inferred zero.
- Given a catch, the size fits far better, but it is still not calibrated:
  - P(Y ≥ y | Y > 0) < 0.01 for 12.8% of the positives (nominal 1%), against
    57.2% unconditionally.
  - The top tenth of the PIT given a catch holds 25.4% of positives (10% if
    calibrated).
  - Given a catch, the model's median count is below the observed median:
    arabiensis 8 vs 22, funestus 10 vs 31, gambiae 6 vs 39.

### 3. Why NB1 makes catches rare (02, 05, 06)

- **The PA records set the level.** Of the Fisher information about the
  species' levels (plug-in), PA records carry 93.6% and counts 6.4%
  (arabiensis 11.7%, funestus 6.6%, gambiae 2.8%).
  - Per record, the median is 0.0043 for counts and 0.028 for PA.
  - Presence-only records carry none: their bias intercept enters additively
    with α.
  - If the counts were Poisson at the same means, they would carry 72.9%. It
    is the NB1 dispersion that takes their information away.
- **NB1 ties the chance of a catch to γ.** P(Y > 0) = 1 − exp(−μ/d) exactly,
  with d = γ / ln(1 + γ).
  - This is the PA formula with the mean divided by d. When μ is small, d is
    also about the mean size of a catch.
  - With the posterior γ, d is 54.5 for funestus (γ 313), 33.6 for arabiensis
    (173), 21.4 for gambiae (98), 19.7 for pharoensis (89) and 12.2 for
    coluzzii (47).
  - A large γ is the only way NB1 can make catches of tens when μ is near 1,
    and the same γ divides the chance of a catch by d.
- γ posterior medians: funestus 313, arabiensis 173, gambiae 98.2, coustani
  93.9, pharoensis 88.6, moucheti 73.9, nili 48.3, coluzzii 47.3, squamosus
  46.5, melas 40.8, rufipes 21.8, ziemanni 20.1, maculipalpis 16.9,
  quadriannulatus 12.8, pretoriensis 10.4, rivulorum 8.96, merus 6.67, leesoni
  4.86. The 90% intervals run from 302–324 (funestus) to 3.52–6.36 (leesoni).

### 4. The extra variation is structured (03, 06)

- **Where it sits.** The variance of log(count / μ) at the positives (lme4,
  random intercepts):
  - between sources: 49% (SD 2.50);
  - between location-dates within a source: 28% (SD 1.87);
  - within location-dates: 23% (SD 1.71).

  The raw SD is 3.62.
- **The count records.** There are 1,874 count location-dates (median 10
  records each), and 726 (39%) have no positive count. Of the 274 count
  sources, 29 have every count 0.
- **Per source.** log(Σcount / Σμ), over the 187 sources with ≥ 5 records and
  a positive: median 3.48, 5–95% −0.35 to 7.81, SD 2.56.
- **Duration.** log(count / μ) at the positives rises with log `study_months`:
  - slope 0.92 (SE 0.05) across all positives;
  - 0.62 (0.09) with a source intercept;
  - 0.57 (0.12) within sources (duration centred on each source's mean);
  - 0.70 (0.13) between sources.

  Only 37 of 228 sources have positives at more than one duration, so the
  within-source slope rests on few sources. The geometric mean of count / μ at
  the positives, by duration: ×9 (≤ 1 month), ×55 (1–3), ×89 (3–12), ×490
  (> 12). `study_months` is 0.5 for at least half the count records; 90th
  percentile 11.7, maximum 126.
- **By method.** Every method under-predicts at the positives: the geometric
  mean of count / μ runs from ×17 (indoor HLC) to ×200 (CDC light trap,
  unknown position). These ratios are biased upward by selecting the positives
  (×39 over all positives), so don't read them as the level gap; Σcount / Σμ in
  section 1 is not selected.
- **Spread.** The positives beyond the 99th percentile are spread over 226
  sources; the top 10 sources hold 19% of them.

### 5. ζ can't absorb it (02, 03, 08)

- `zeta = zeta_raw * 0.1`, with `zeta_raw ~ N(0, 1)`. That fixes ζ's SD at 0.1,
  so ±2 SD is ×0.82–1.22 on a source's counts.
- The fitted ζ means over the 274 count sources have SD 0.11 (range −0.12 to
  0.98). Over the 187 sources in section 4 their SD is 0.12, and their
  correlation with the source's log gap is −0.07. The gaps themselves have SD
  2.56.

### 6. The dispersion prior is binding (04, 08)

- The prior is γ ~ Exponential(θ = 2.996), so the log prior falls 3.0 per unit
  of γ. At the posterior γ, the count log-likelihood (plug-in) rises 2.74–3.00
  per unit for all 18 species. The data push γ up about as hard as the prior
  pulls it down.
- Holding the fitted means fixed, the counts alone choose γ 1.8–6.0× the
  posterior median: funestus 882 vs 313, arabiensis 312 vs 173, gambiae 277 vs
  98, moucheti 441 vs 74.
- Loosening the prior alone would make catches rarer still. At those γ, NB1
  predicts 1.9% of count records with a catch, against 3.1% at the posterior.

### 7. Other distributions, with the fitted means held fixed (04, 04b, 08)

These profiles rank the options; they don't predict a refit, in which λ, ζ and
`sampling_re` would move too. Every row keeps each record's fitted mean and
chooses each species' dispersion afresh from the counts alone, so the first
row is not the model as fitted. The model as fitted (posterior γ) puts 57% of
positives beyond the 99th percentile and predicts a catch on 3.1% of records.
The first row's γ is 1.8–6.0× larger (section 6), which takes those to 63% and
1.9%. "Own count level" also fits a multiplier on that species' fitted means.
"Duration offset" multiplies the means by `study_months` / 0.5, with a missing
`study_months` set to 0.5.

| scenario | log-likelihood | gain over the first row | positives beyond 99th pct | predicted share with a catch (obs. 13.3%) |
|---|---|---|---|---|
| NB1, γ chosen by the counts (means fixed) | −24,830 | 0 | 63% | 1.9% |
| NB2, fitted means | −38,624 | −13,794 | 99% | 0.1% |
| PLN, fitted means | −22,961 | 1,868 | 58% | 4.7% |
| NB1, own count level | −20,267 | 4,563 | 22% | 8.9% |
| NB2, own count level | −19,499 | 5,330 | 3.3% | 12.2% |
| NB1, duration offset + level | −20,420 | 4,409 | 25% | 7.7% |
| NB2, duration offset + level | −19,332 | 5,498 | 2.5% | 12.2% |
| PLN, duration offset + level | −17,631 | 7,198 | 6.9% | 13.5% |

- **Freeing the count scale does most.** With the scale free, the shape matters
  too: PLN fits best, then NB2, then NB1.
- **NB2 at the fitted means is far worse than NB1.** With means near 0, NB2
  needs a tiny k to produce large counts, and a tiny k puts nearly all the mass
  at 0 (arabiensis k = 0.0003).
- **The level multipliers can't be read as levels.** They are implausible and
  vary widely:
  - NB1, own level: 2.6–208, median 32;
  - NB2, own level: 8.8 to 4.0 million;
  - PLN: up to 10^52 (quadriannulatus), or 3.2 million (coluzzii) without
    it.

  Under a heavy tail the mean is set by the tail, so these profiles can't
  separate the count level from the spread.
- **PLN spread.** σ is 2.7–3.9 with the fitted means (×14–48 per SD) and
  4.2–6.7 with duration and level (×65–780 per SD). Both ranges leave out
  quadriannulatus (22 positives), at 4.2 and 17.4.
- **The duration offset has slope 1**, against about 0.57 within sources. It
  helps NB2 a little and hurts NB1 a little, so read those rows for direction
  only.
- **The PLN likelihood is checked.** It uses adaptive Gauss–Hermite
  quadrature. Against `integrate()`, the log pmf differs by at most 0.00067,
  and the pmf sums to 1 with mean μ (04b).

### 8. Not the cause (03, 06)

- **Inferred zeros.** Dropping them barely moves the dispersion the counts
  choose (means held fixed): γ falls 1–5% for the five main species. It falls
  more for small ones, e.g. leesoni ×0.56. At the γ the counts choose, the
  share of positives beyond the 99th percentile is unchanged (62.8% vs 62.9%).
- **Duplicates.** 464 positives (19.0%) are in a group of two or more records
  with the same species, location-date and count (963 of the 18,340 count
  records). Keeping one of each leaves 57.0% beyond the 99th percentile (57.2%
  with all).
- **Single records.** The most extreme include:
  - funestus 342,645 (source 1002459, indoor CDC light trap, 9.6 months,
    μ 1.1);
  - arabiensis 51,453 (1001684, HLC, 31 months);
  - arabiensis 26,399 (2667, outdoor CDC light trap, 11.6 months);
  - gambiae 11,268 and 9,911 (1000518);
  - duplicated pairs: pharoensis 9,573 (1098), moucheti 4,118 (1461), melas
    2,022 (2502).

  Some extremes are where μ is near 0: rufipes 672 and squamosus 974 (source
  1000035, outdoor resting, μ about 6e-5 and 4e-5). There the spatial pattern
  is off, not just the count model. The extremes are spread over 226 sources.

### 9. Effort in the raw data (07)

- `data/tabular/va.data_20260616.csv` has 41,055 rows, 13,190 of them with an
  occurrence count. No structured effort field exists for the counts:
  - `biting_number_of_sampling_nights_indoors`: filled on 30 rows, 6 of them
    with a count;
  - `..._outdoors`: 20 rows, none with a count;
  - `hbr_sampling_indoor`: 1,181 rows, 99 with a count;
  - `indoor_biting_total`: 246 rows, none with a count.
- Human biting rate (HBR) has a value on 2,456 rows (2,135 per person per
  night), from 150 sources in 26 countries. It is mostly recorded for
  complexes: gambiae complex 1,402, funestus complex 428, arabiensis 197.
- So `study_months` is the only effort proxy for the counts, and it is a
  calendar span.

## What might be done (the user's decision)

Each option is a refit (9–12 h), and none drops a term.

1. **Estimate ζ's SD** (e.g. `zeta_sd` with a half-normal prior, keeping the
   non-centred `zeta = zeta_raw * zeta_sd`) instead of fixing it at 0.1. It
   targets the 49% between sources. The risk is mixing: the July work found a
   flat `zeta`↔`sampling_re` ridge (`agent_notes/history.md`, 2026-07).
2. **Give counts their own scale:**
   - a count intercept (per species or shared), so that the count level need
     not equal the PA detection scale;
   - `log(study_months)` with its own coefficient (about 0.6 within sources,
     so not a fixed offset);
   - perhaps a location-date effect for counts (28% of the variance).
3. **Then the count distribution.** Once 1–2 free the scale, NB2 or PLN rank
   above NB1. Better still, take the PA link from the same distribution so that
   both data types read one λ. Under NB2 that link is
   P(present) = 1 − (1 + λ/k)^−k instead of 1 − e^−λ. A hurdle is another way
   (a detection part, then a size given a catch).
4. **The γ prior** (θ ≈ 3.0) is binding, but loosening it alone makes catches
   rarer. Revisit it only alongside 1–3, if NB1 stays.
5. **Check records** (data hygiene, not the cause):
   - funestus 342,645 (already on the list in `AGENTS.md`);
   - arabiensis 51,453;
   - the duplicated pairs;
   - the inferred zeros where λ expects the species.

Meanwhile, the counts carry about 6% of the information on the species'
levels. The maps' levels and patterns therefore rest on the PA and
presence-only data. A count model that fits would give the counts more weight,
and could move the maps.

## Relation to earlier work

- **`extras/pc_prior_nb_overdispersion.md`** (2026-07-29/31) ran a prior
  predictive check with a stand-in mean model. It found:
  - no NB1 θ reproduced the upper tail;
  - the best NB1 θ was 0.001–0.002 (E[γ] 500–1000);
  - NB2 was suggested instead.

  The user then set θ ≈ 3.0. These profiles refine the NB2 point: at the
  current fitted means NB2 is far worse than NB1, and it ranks above NB1 only
  once the count scale is free. The binding prior in section 6 agrees with that
  note's finding that the data want γ in the hundreds.
- **`agent_notes/os_model.md`** (2026-10-21): the offset-strength model didn't
  remove the under-prediction; both models' miss came mainly from the NB1
  dispersion.
- **The 2026-10-21 numbers** in `agent_notes/model.md`, from before the
  2026-11-06 data-type fix, are now in `agent_notes/history.md`.

## Files and rerunning

| script | what it does | reads |
|---|---|---|
| `01_count_predictive.R` | per-record predictive (P(0), P(Y ≥ y), PIT, PA formula); γ, ζ, `sampling_re` summaries | store (`model_data_spatial_cx`), `coef_draws.rds` |
| `02_overview.R` | check against DHARMa; tails; by species | store (`resids_and_rhats`), 01's output |
| `03_structure.R` | location-dates, variance shares, per-source gaps, duration, method, extreme records | 01's output |
| `04_distributions.R` | profiles: NB1 / NB2 / PLN × level × duration; the prior check | 01's output |
| `04b_check_pln.R` | the PLN quadrature against `integrate()` | the code of 04 |
| `05_information.R` | Fisher information, counts vs PA | store, `coef_draws.rds`, 01's output |
| `06_checks.R` | size given a catch; duplicates; duration within source; reported vs inferred; γ without inferred zeros | 01's output |
| `07_effort_fields.R` | effort fields in the raw csv | the raw csv |
| `08_profile_ranges.R` | the ranges and totals quoted from 04 | 04's and 05's outputs |
| `run_all.sh` | runs them all, printed output to `<outdir>/logs/` | |
| `github_issue.md` | the suggested GitHub issue (title and description), 2026-11-25 | |

- **To rerun on the fit now in the store**, from the project root:
  `sh extras/count_misfit/run_all.sh outputs/count_misfit/fit_<YYYYMMDD>`. It
  took 36 s; it writes only to the folder given and leaves
  `_targets/meta/meta` alone. 01's log prints the md5 of the draws file it
  read.
- **After a refit:**
  - 03, 04, 06 and 08 pointed at `outputs/count_misfit/fit_20261122/`
    reproduce this baseline.
  - 01, 02 and 05 describe whatever fit is in the store.
  - 01 and 05 hard-code NB1, a fixed-SD ζ and the current structure of
    `os_record_loglik()`. If the refit takes any of options 1–3, update them
    first, or they will describe the wrong model.
- **Seeds.** The PIT jitter is seeded (01: 20261124, 06: 20261125), so reruns
  are exact. Every object this run saved is identical (`all.equal`, tolerance
  0) to the scratch run of 2026-11-24.
- **Changes since the scratch run.** The scripts are kept as they were run, not
  restyled to the one-argument-per-line convention. Since then I added: usage
  lines; the draws fingerprint in 01; the seed and sections (d)–(e) in 06; and
  07, 08 and `run_all.sh`.
