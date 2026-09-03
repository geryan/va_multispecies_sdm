# PC prior on NB overdispersion — process record

**Date:** 2026-07-29. **Branch:** `study_random_effect`.

**What this is.** The derivation, simulation evidence and decision trail behind replacing
V3 of the reparam count likelihood (`log_size ~ N(2, 2)`) with a penalised-complexity
prior that puts mass at the Poisson limit, so overdispersion has to be driven by the data
rather than assumed. Code and runnable checks: `extras/pc_prior_nb_overdispersion.R`.
Related: `extras/sre_keepre_vs_original.md` (V1–V5), `extras/sre_nb_size_prior_comparison.R`.

**Motivation.** V3 was adopted purely to kill the `size -> Inf` boundary funnel in the
original `sqrt_inv_size ~ N(0, 0.5)+`, `size = (1/sqrt_inv_size)^2`. It worked (30 chains,
max R-hat 1.036) but explicitly traded away the original's PC prior, so `N(2, 2)` on
`log_size` was assuming a dispersion scale rather than letting the data argue for it.

---

## 1. Notation — the thing that caused the most confusion

```
gamma_nb := mu / size          var = mu * (1 + gamma_nb)          gamma_nb = 0 is Poisson
```

`gamma_nb` is the **linear overdispersion parameter itself**, one free parameter per
species, estimated. `theta` is the fixed elicited **rate of the PC prior** placed on it:
`gamma_nb ~ Exponential(theta)`.

**The original spec was internally inconsistent**, and I initially propagated the error by
quietly picking a self-consistent reading instead of flagging it. Since `E[1/size] = 1/beta`:

| spec | implies | Poisson limit |
|---|---|---|
| `size ~ InvGamma(1, mu * gamma_nb)` | `E[VIF-1] = 1/gamma_nb` | `gamma_nb -> +Inf` |
| `gamma_nb = mu/size` | `E[VIF-1] = gamma_nb` | `gamma_nb -> 0` |

These are reciprocals. The correct form under the definition above is

```
size ~ InverseGamma(1, mu / gamma_nb)        <- DIVIDED, not multiplied
```

and it must be this way round: a PC prior is exponential on the KLD distance from the base
model, and that distance is proportional to `gamma_nb` only when `gamma_nb -> 0` is Poisson.
An exponential prior on a parameter whose base model sits at `+Inf` is not a PC prior at all.

## 2. Why the `mu` belongs in `beta`

greta's `inverse_gamma(alpha, beta)` takes **beta as the SCALE** (verified numerically:
`mean(1/x) = alpha/beta`, `sd(1/x) = sqrt(alpha)/beta`), so `size ~ InvGamma(1, b)` is
exactly `psi = 1/size ~ Exponential(rate = b)`. With `b = mu / gamma_nb` the `mu` **cancels
out of the induced prior**:

```
VIF - 1 = mu * psi ~ Exponential(rate = 1 / gamma_nb)
```

Two consequences, and both are the point:

1. Every count record gets the same prior on its variance inflation regardless of its
   expected count — `gamma_nb` is scale-free.
2. It is centred on `size_i = mu_i / gamma_nb`, i.e. **NB1 / quasi-Poisson (linear)**
   overdispersion, not NB2's quadratic `var = mu + mu^2/size`.

**PC property (checked).** Expanding the Poisson-gamma mixture around `Lambda = mu`
(mixing variance `mu^2 psi`) gives `KLD(NB || Pois) ~ (mu*psi)^2 / 4 = gamma_nb^2 / 4`, so
`d = sqrt(2*KLD) = gamma_nb/sqrt(2)`. Exponential on `d` = exponential on `gamma_nb`.

**Elicitation.** `Pr(VIF > v) = exp(-theta(v-1))`, so `theta = -log(p)/(v-1)`, and
`E[gamma_nb] = 1/theta`. `theta = pc_theta(50, 0.05) = 0.061` was chosen provisionally —
**and is now known to be far too tight. See §4b.**

## 3. The three variants

| | form | free dispersion params | extra latent dims |
|---|---|---|---|
| **A** | `size_i = mu_i / gamma_nb` deterministic | `n_species` | 0 |
| **B** | `size ~ InvGamma(1, mu/gamma_nb)`, centred | `n_species` | **12,376** |
| **Bnc** | `e ~ Exp(1)`; `size = mu/(gamma_nb*e)`, non-centred | `n_species` | 12,376 |

B adds genuine per-record dispersion heterogeneity (`VIF_i - 1 ~ Exp(1/gamma_nb)`, SD =
mean) and heavier marginal tails. B and Bnc are the *same model*, different sampling
geometry.

## 4. Simulation results

All: n = 1500, 3 species, `gamma_true = c(30, 5, 0)`, `theta = 0.061`, 4 chains,
`hmc(15, 60)`, 1000 warmup / 1000 samples. Species 3 truth is **exactly Poisson**.
Reproduce with `run_check(gen = ..., which = ...)`.

**Data generated from A (NB1):**

| sp | truth | A | B |
|---|---|---|---|
| 1 | 30 | 29.9 [25.7, 34.9] | 41.5 [33.8, 50.5] |
| 2 | 5 | 5.5 [4.7, 6.4] | 6.5 [5.3, 8.0] |
| 3 | **0** | 0.06 [0.00, 0.17] | 0.07 [0.00, 0.17], **R-hat 1.11** |

**Data generated from B (per-record heterogeneity):**

| sp | truth | A | B | Bnc |
|---|---|---|---|---|
| 1 | 30 | 24.2 [20.7, 28.4] | 29.5 [24.0, 35.8] | 29.6 [24.2, 36.1] |
| 2 | 5 | 5.4 [4.7, 6.3] | 6.1 [5.0, 7.4] | 6.1 [5.0, 7.3] |
| 3 | **0** | 0.04, R-hat 1.00 | 0.03, **R-hat 1.50** | 0.04, **R-hat 1.00** |

Timings: A ~70–115s, B ~140s, Bnc ~150–165s.

### Readings

- **The PC prior does not manufacture overdispersion.** Under a prior with
  `E[gamma_nb] = 16.4`, the truly-Poisson species posterior came back at 0.03–0.06 in
  every variant. That is the property being bought. Note this was a *simulation* whose
  truths (0–30) sat near that prior; §4b shows the real data are 2–3 orders of magnitude
  away, so this result does not transfer to the production fit unexamined.
- **A vs B is a modelling question, not a computational one.** Each recovers its own
  generative truth and is biased on the other's (A gives 24.2 for a true 30 on B-data;
  B gives 41.5 for a true 30 on A-data). The question is whether dispersion genuinely
  varies *between* count records.
- **B's centred form funnels at exactly the Poisson limit the PC prior targets.** As
  `gamma_nb -> 0` the 12,376 latent `psi_i` are pulled to zero by a rate `mu_i/gamma_nb`
  that diverges, with `gamma_nb` the neck. R-hat on the Poisson species: 1.11 / 1.30 / 1.50
  across runs, while overdispersed species in the *same fits* sat at ~1.00.
- **Non-centring fixes it** — R-hat 1.003, identical recovery, ~5% more wall-clock. This is
  the *mirror* of V5: V5 went **centred** because those few-group effects were well informed
  by data; each `psi_i` here sees one observation and is prior-dominated, so it must go
  **non-centred**.

### Caveats on this evidence

1500 records and a trivial mean model (per-species intercept + one known covariate). The
real fit has **12,376** count records plus `beta` / `zeta` / `sampling_re`, so B's extra
dimensions are ~8x what was tested and sit alongside everything V1/V2/V5 were needed to
tame. **B has never been run at full scale.** Nothing here has been fitted to real data.

## 4b. Measured residual dispersion — the theta choice is wrong

`theta = 0.061` was picked on the assumption that the mean model / `zeta` / `sampling_re`
absorb most of the raw marginal VMR (30–500 by species). **That assumption was never
checked, and it is false.**

The converged 30-chain V3 run (`extras/sre_keepre_draws.rds`, 60,000 draws) traces
`log_size` directly, which measures residual dispersion *after* all the mean structure:

| species | `log_size` med | prior SDs below N(2,2) mean | `size` | implied NB1 `gamma_nb` |
|---|---|---|---|---|
| arabiensis | −6.50 | −4.3 | 0.0015 | 20,200 |
| coluzzii | −9.00 | −5.5 | 0.00012 | 54,700 |
| funestus | −7.38 | −4.7 | 0.00062 | 40,000 |
| gambiae | −7.92 | −5.0 | 0.00036 | 28,600 |
| melas | −5.63 | −3.8 | 0.0036 | 355 |
| merus | −4.63 | −3.3 | 0.0098 | 18.4 |
| moucheti | −6.90 | −4.5 | 0.0010 | 2,780 |
| nili | −6.71 | −4.4 | 0.0012 | 3,640 |

The posterior sits 3.3–5.5 **prior** SDs below the mean of its own prior — the data dragged
it there against a prior resisting. Residual dispersion is therefore *more* extreme than the
marginal VMR implied, not less, and `E[gamma_nb] = 1/theta = 16.4` is 2–3 orders of magnitude
too small for 7 of 8 species. Only *merus* is near it.

**Caveats on this measurement.** (a) Those draws are the `pa_frac = 0.20` subsample mixing
study, not a production posterior — all count records were retained so the count likelihood
is intact, but it is still a subsample run. (b) V3 is NB2 and this is NB1, so `mu/size` is a
matching-at-the-mean approximation, **not** an equivalence: NB2's VIF grows with `mu` while
NB1's is constant.

**Consequences — do not simply rescale theta and move on:**

- If NB1 is kept, `theta` must be far smaller — `Pr(VIF > 20000) = 0.5` needs `theta ≈ 3.5e-5`.
  At that point the PC prior barely constrains anything and "prior mass at Poisson" is
  largely cosmetic. There may be no theta that both anchors at Poisson and lets this data
  breathe, which would itself be worth knowing.
- **The bigger question is whether NB1 is the right likelihood at all.** The counts are ~75%
  zeros with a max of 968; V3's NB2 handles that with variance growing quadratically in `mu`,
  which a constant-VIF NB1 cannot mimic. A posterior predictive check of A against V3 is the
  next thing to run.

## 4c. Prior predictive check on theta (run 2026-07-29)

Script: `extras/pc_prior_theta_prior_predictive.R`. This is the check proposed as open
item 4 and it **rejects the wired-in `theta = 0.061`**.

**Method.** Plug-in `mu_i` from per-species **ridge**-penalised Poisson fits (`glmnet`,
alpha = 0, lambda = 0.05) on the same 10 covariates + `log(offset)`. Ridge, not plain
`glm()`: an unpenalised GLM extrapolates to fitted rates of 2e-16 here (the offset alone
spans 3e-7 to 3.3), which makes `(y-mu)^2/mu` explode and corrupts everything downstream —
that error produced nonsense moment estimates of 1e5–1e6 on a first pass. Then for each
theta, draw `gamma_nb ~ Exp(theta)` per species, simulate `y_i ~ NB(size = mu_i/gamma_nb,
mu = mu_i)`, **truncate at 1000 exactly as the data are** (`n == count`, and the 68 dropped
records are all exactly 1000, i.e. censored in the source), and compare summaries.
400 replicates per theta. `ok` = observed statistic inside the 5–95% prior predictive interval.

**Observed:** `zero_frac = 0.865`, `vmr = 357`, `q99 = 301`, `frac >= 100 = 0.0282`.

| theta | E[gamma_nb] | zero | ok | vmr | ok | q99 ok | ≥100 ok | **n ok** |
|---|---|---|---|---|---|---|---|---|
| **0.061** (wired in) | 16.4 | 0.578 | ✗ | 63 | ✗ | ✗ | ✗ | **0/4** |
| 0.020 | 50 | 0.661 | ✗ | 90 | ✗ | ✗ | ✓ | 1/4 |
| 0.010 | 100 | 0.720 | ✗ | 133 | ✗ | ✗ | ✓ | 1/4 |
| 0.005 | 200 | 0.776 | ✗ | 192 | ✗ | ✗ | ✓ | 1/4 |
| 0.003 | 333 | 0.820 | ✓ | 240 | ✗ | ✗ | ✓ | 2/4 |
| **0.002** | 500 | 0.852 | ✓ | 275 | ✓ | ✗ | ✓ | **3/4** |
| **0.001** | 1000 | 0.894 | ✓ | 326 | ✓ | ✗ | ✓ | **3/4** |
| 0.0005 | 2000 | 0.932 | ✓ | 377 | ✓ | ✗ | ✗ | 2/4 |
| 0.0001 | 10000 | 0.980 | ✗ | 445 | ✓ | ✗ | ✗ | 1/4 |

### Findings

1. **`theta = 0.061` fails every single statistic.** It generates 58% zeros against an
   observed 86.5%, and a VMR of 63 against 357. It is not merely tight, it is
   inconsistent with the data on every summary checked. **Do not ship it.**
2. **Best NB1 value is `theta ≈ 0.001–0.002`** (`E[gamma_nb]` 500–1000) — roughly 30–60x
   more permissive than what is wired in.
3. **No theta reproduces the upper tail.** `q99_ok` is FALSE at *every* theta. Observed
   q99 = 301; the best prior predictive interval reaches only [170, 280] (theta = 0.002),
   and shrinks again for smaller theta as truncation and the growing zero mass bite. NB1
   buys extra variance by making *more zeros plus a moderate tail*; the data want
   86.5% zeros **and** a heavy upper tail simultaneously. That is the signature of
   variance growing with `mu` — i.e. NB2, which is what V3 already fits.

### Caveat — this does not condemn NB1 outright

The plug-in mean model is a **stand-in**: no bioregion interactions, no `zeta`, no
`sampling_re`. The real model has more structure to absorb variation, and in particular
`zeta` (per-source) could supply exactly the tail the stand-in lacks if some sources
systematically report large counts. So finding 3 is *suggestive of NB1 misspecification,
not proof*. Findings 1 and 2 are more robust: extra mean structure would push the required
theta **up** from 0.001–0.002 toward 0.061, but it would have to absorb an implausible
amount to close a 30–60x gap.

**Recommendation:** do not fit at 0.061. Either move to `theta ≈ 0.002` for NB1, or —
better given finding 3 — put the PC prior on NB2's dispersion instead so the variance can
grow with `mu`, keeping V3's likelihood shape while regaining the Poisson-anchored prior
that was the whole point.

## 5. State as of this note

- **Option A is wired into** `R/fit_model_multispecies_pp_count_source_effect_reparam.R`
  (lines ~315–328): `pc_theta()` defined inline, `theta = pc_theta(50, 0.05)`,
  `gamma_nb <- exponential(theta, dim = n_species)`, `size_vec <- mu / gamma_nb[species_index]`,
  and `gamma_nb` traced in `model()`. The old V3 block is commented out above it.
- **Not run.** No fit has been done with the PC prior on real data.
- **The wired-in `theta = 0.061` is REJECTED by the prior predictive check (§4c): 0 of 4
  summary statistics covered.** Do not launch a production fit on it.

### Open items

1. **Stale header comments in the reparam function.** Lines 19, 44, 302–304 and 373 still
   describe V3 (`log_size ~ N(2,2)`, `sqrt_inv_size -> log_size`). The DOWNSTREAM NOTE
   should say `log_size -> gamma_nb`. Post-processing that `calculate()`s on `log_size`
   or `size` needs updating.
2. **`pc_theta()` is defined inside the fit function.** Fine, but it is also defined in
   `extras/pc_prior_nb_overdispersion.R`; if it is wanted in more than one place it should
   move to its own file in `R/`.
3. **Refit and check.** Needs the production settings (>= 20 chains, ~2000/2000). Watch
   `gamma_nb` R-hat specifically — under A it was clean in simulation, but A has never met
   the real posterior. If any species' `gamma_nb` posterior collapses toward 0, that is a
   *result* (that species is Poisson), not a bug.
4. **theta = 0.061 is rejected by the prior predictive check — 0/4 statistics (§4c).**
   Either `theta ~ 0.002` (best NB1 value, 3/4) or change likelihood. RESOLVED that 0.061
   must not ship; not yet resolved which replacement.
5. **Is NB1 the right likelihood? §4c says probably not.** No theta reproduces the observed
   q99, at any value. Strongly suggests putting the PC prior on NB2's dispersion instead —
   variance growing with `mu` is what the data show. Confirm with a real PPC against V3
   before committing, since the §4c mean model is a stand-in without `zeta`.
6. **A vs B on real data** — unresolved, and only worth opening after 4 and 5 settle.

## 6. Two corrections made during this work

Both were user pushbacks and both were right; recorded because they are easy to repeat.

1. **"Per-observation `size` is unidentified" — wrong.** `mu_i` is a deterministic function
   of parameters pinned by the rest of the likelihood, so each `size_i` has a proper prior
   and the posterior is proper. The real objection was only ever dimensionality/geometry.
   Check propriety before asserting non-identifiability.
2. **Asserting an unverified premise as fact — wrong.** "Much of the raw VMR is absorbed by
   the mean model" was stated as justification for `theta = 0.061` and never checked; the
   converged draws show the opposite (§4b). The measurement was cheap and available the
   whole time. Check load-bearing assumptions before they become a chosen constant.
3. **Silently resolving an inconsistent spec — wrong.** `beta = mu*gamma_nb` and
   `gamma_nb = mu/r` cannot both hold; I picked the self-consistent reading and renamed the
   user's quantity to `omega` without saying so, which cost a round trip. When a spec is
   internally inconsistent, say so explicitly.
