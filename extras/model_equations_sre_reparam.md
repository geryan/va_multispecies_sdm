---
title: "Multispecies point-process abundance model"
subtitle: "Mathematical specification of `fit_model_multispecies_pp_count_source_effect_reparam()`"
date: "2026-08-25"
---

# 1. Conventions

Distributions are written in the parameterisation used by `greta`, which is the
parameterisation actually used in the code:

- $\mathrm{Normal}(\mu, \sigma)$ takes a **standard deviation**, not a variance or a precision.
- $\mathrm{Normal}(\mu, \sigma)_{[0,\infty)}$ is that normal density truncated to the
  non-negative half-line and renormalised.
- $\mathrm{Exponential}(\theta)$ is parameterised by a **rate**, so its mean is $1/\theta$.
- $\mathrm{NegBinomial}(\kappa, p)$ is parameterised by a size (number of failures) $\kappa$
  and a success probability $p$, with mean $\kappa (1 - p) / p$ and
  variance $\kappa (1 - p) / p^{2}$.
- $\mathrm{Bernoulli}(\pi)$ and $\mathrm{Poisson}(\nu)$ are standard.

All parameters are estimated jointly; the statements below are the complete generative
model, and the joint posterior is proportional to the product of every line.

# 2. Indices and dimensions

| Symbol | Meaning | Size in the fitted model |
|---|---|---|
| $i = 1, \ldots, N$ | distinct spatial locations (pixels) in the data | $N = 3450$ |
| $s = 1, \ldots, S$ | species | $S = 18$ |
| $b = 1, \ldots, B$ | bioregions | $B = 17$ |
| $a = 1, \ldots, A$ | landscape / environmental covariates | $A = 10$ |
| $j = 1, \ldots, J$ | columns of the full abundance design matrix, $J = A + AB$ | $J = 180$ |
| $k = 1, \ldots, K$ | sampling methods | $K = 9$ |
| $g = 1, \ldots, G$ | data sources (studies) contributing **count** records | $G = 231$ |
| $r$ | an observation record | 12 444 count, 38 266 presence/absence, 2 471 presence-only, 249 background points |

Each record $r$ carries index maps to a location $i[r]$, a species $s[r]$, a sampling
method $k[r]$ and (for count records) a source $g[r]$.

# 3. Design matrices

The abundance design matrix $X$ ($N \times J$) is the landscape covariates
$X^{\mathrm{lc}}$ ($N \times A$) concatenated with every pairwise product of a landscape
covariate and a bioregion indicator $X^{\mathrm{bio}}$ ($N \times B$):

$$X = \left[\, X^{\mathrm{lc}} \,\middle|\, X^{\mathrm{int}} \,\right], \qquad X^{\mathrm{int}}_{i,\,(a-1)B + b} = X^{\mathrm{lc}}_{ia} \, X^{\mathrm{bio}}_{ib}.$$

So the first $A = 10$ columns of $X$ are landscape main effects and the remaining
$AB = 170$ columns are landscape-by-bioregion interactions.

Two further fixed quantities enter as data:

$$o_i = \log(\mathrm{offset}_i), \qquad z_i = \mathrm{travel\ time}_i,$$

where $\mathrm{offset}_i$ is the location- and date-matched prediction of an external
mechanistic *An. gambiae* adult life-cycle model, and $z_i$ is a travel-time accessibility
surface used as the single reporting-bias covariate.

# 4. Priors

## 4.1 Species log-abundance intercepts

$$\mu_\alpha \sim \mathrm{Normal}(0, 10)$$

$$\sigma_\alpha \sim \mathrm{Normal}(0, 1)_{[0,\infty)}$$

$$\alpha_s \sim \mathrm{Normal}(\mu_\alpha, \sigma_\alpha)$$

The prior standard deviation $10$ is $\sqrt{1 / \lambda_{\mathrm{int}}}$ for an $L_2$
intercept penalty $\lambda_{\mathrm{int}} = 10^{-2}$.

## 4.2 Species covariate coefficients

Each coefficient has an independent, mean-zero normal prior whose scale depends only on
whether the column is a main effect or an interaction:

$$\beta_{js} \sim \mathrm{Normal}(0, c_j), \qquad c_j = \begin{cases} \sqrt{10} & j \le A \quad \text{(landscape main effects)} \\ 0.1 & j > A \quad \text{(bioregion interactions)} \end{cases}$$

The main-effect scale $\sqrt{10}$ is $\sqrt{1 / \lambda_{\mathrm{sdm}}}$ for an $L_2$
penalty $\lambda_{\mathrm{sdm}} = 0.1$; the interaction scale $0.1$ is a fixed, manually
tuned ridge penalty that shrinks the bioregion-specific deviations towards the continental
main effect. In code this is implemented non-centred, as
$\beta_{js} = c_j \tilde{\beta}_{js}$ with $\tilde{\beta}_{js} \sim \mathrm{Normal}(0, 1)$,
which is the same prior.

## 4.3 Reporting bias

$$\mu_\gamma \sim \mathrm{Normal}(0, 10)$$

$$\sigma_\gamma \sim \mathrm{Normal}(0, 1)_{[0,\infty)}$$

$$\gamma_s \sim \mathrm{Normal}(\mu_\gamma, \sigma_\gamma)$$

$$\delta \sim \mathrm{Normal}(1, 0.5)_{[0,\infty)}$$

$\gamma_s$ is a species-specific presence-only reporting intensity and $\delta$ is a single
slope on log travel time, shared across species and constrained to be non-negative.

## 4.4 Sampling-method random effect

$$\sigma_\psi \sim \mathrm{Normal}(0, 1)_{[0,\infty)}$$

$$\psi_k \sim \mathrm{Normal}(0, \sigma_\psi)$$

## 4.5 Source random effect on counts

$$\zeta_g \sim \mathrm{Normal}(0, 0.1)$$

The scale $0.1$ is fixed rather than estimated, i.e. a regularised (ridge) source effect.
It is defined over the $G = 231$ sources that contribute count records only, because
$\zeta$ appears in no other likelihood. In code this is implemented non-centred, as
$\zeta_g = 0.1 \, \tilde{\zeta}_g$ with $\tilde{\zeta}_g \sim \mathrm{Normal}(0, 1)$.

## 4.6 Negative binomial overdispersion

Let $\phi_s$ be the excess of the variance-to-mean ratio over the Poisson value, i.e. the
variance inflation factor minus one. It is given a penalised-complexity prior, which is
exponential on $\phi_s$ with its mode at the Poisson limit $\phi_s = 0$:

$$\phi_s \sim \mathrm{Exponential}(\theta), \qquad \theta = \frac{-\log(0.05)}{2 - 1} \approx 2.996$$

The rate is chosen so that $\Pr(\phi_s > 1) = 0.05$, i.e. there is a 5% prior probability
that a species' count variance exceeds twice its mean.

# 5. Latent processes

## 5.1 Expected abundance

For every location $i$ and species $s$:

$$\log \lambda_{is} = o_i + \alpha_s + (X\beta)_{is}$$

where $(X\beta)_{is}$ is the $(i,s)$ element of the $N \times S$ matrix product of the
design matrix $X$ with the coefficient matrix $\beta$. Equivalently, $\lambda_{is}$ is a
relative larval-habitat abundance surface, $\exp(\alpha_s + (X\beta)_{is})$, multiplied by
the fixed mechanistic adult life-cycle offset $\exp(o_i)$.

## 5.2 Reporting bias

$$\log \eta_{is} = \gamma_s + \delta \log(z_i)$$

The slope on log travel time is shared across species; only the intercept $\gamma_s$
varies by species.

# 6. Observation models

Each of the three data types observes the same latent surface $\lambda$ through a
different link and a different set of nuisance effects.

## 6.1 Count data

For each count record $r$, with $y_r$ the number of mosquitoes caught:

$$\log \mu_r = \log \lambda_{i[r],\,s[r]} + \psi_{k[r]} + \zeta_{g[r]}$$

$$y_r \sim \mathrm{NegBinomial}\left(\kappa_r, \; p_r\right), \qquad \kappa_r = \frac{\mu_r}{\phi_{s[r]}}, \qquad p_r = \frac{1}{1 + \phi_{s[r]}}$$

This parameterisation gives

$$\mathrm{E}[y_r] = \mu_r, \qquad \mathrm{Var}[y_r] = \mu_r \left(1 + \phi_{s[r]}\right),$$

i.e. an NB1 (linear, quasi-Poisson-like) mean-variance relationship in which the variance
inflation factor $1 + \phi_s$ is constant within a species and $\phi_s \to 0$ recovers the
Poisson model. During data preparation, counts above 1000 are capped at 1000. Unlike
the original function, this version retains those capped records (68 of the 12 444 count
records), so they enter the likelihood as exact observations of $y_r = 1000$.

## 6.2 Presence / absence data

For each presence/absence record $r$, with $y_r \in \{0, 1\}$:

$$\log \Lambda_r = \log \lambda_{i[r],\,s[r]} + \psi_{k[r]}$$

$$\pi_r = 1 - \exp\left(-\exp(\log \Lambda_r)\right)$$

$$y_r \sim \mathrm{Bernoulli}(\pi_r)$$

The link is the complementary log-log, which is the probability that an inhomogeneous
Poisson process with rate $\Lambda_r$ yields at least one individual. It is evaluated on
the logit scale, and $\log \Lambda_r$ is clamped to
$[\log \varepsilon, \log x_{\max}]$ (the double-precision limits), for numerical stability.
No source effect $\zeta$ enters here.

## 6.3 Presence-only and background data

Presence-only records and background quadrature points are modelled jointly as a
down-weighted inhomogeneous Poisson point process. For each such record $r$, with
$y_r = 1$ for a presence-only record and $y_r = 0$ for a background point, and $A_r$ the
quadrature weight ($A_r = 1$ for presence-only records, the k-means cluster area for
background points):

$$\log \nu_r = \log \lambda_{i[r],\,s[r]} + \psi_{k[r]} + \log \eta_{i[r],\,s[r]} + \log A_r$$

$$y_r \sim \mathrm{Poisson}(\nu_r)$$

The observed presence-only intensity is therefore the product of true abundance
$\lambda$, the sampling-method effect, and the reporting bias $\eta$; the bias term
appears in this likelihood only, which is what identifies $\gamma$ and $\delta$
separately from $\alpha$ and $\beta$. Background points are replicated across all $S$
species, and each is assigned a sampling method drawn from the empirical frequency
distribution of the observed sampling methods.

# 7. Parameterisation notes

Three elements of the specification above differ substantively from the original
`fit_model_multispecies_pp_count_source_effect()`:

- **Overdispersion (Section 4.6).** The original placed
  $1/\sqrt{\kappa_s} \sim \mathrm{Normal}(0, 0.5)_{[0,\infty)}$ on a species-constant size
  $\kappa_s$, so that $\mathrm{Var}[y_r] = \mu_r + \mu_r^2 / \kappa_{s[r]}$ — an NB2, quadratic
  mean-variance relationship. The version above is a penalised-complexity prior on the
  variance inflation factor and gives the NB1, linear relationship.
- **Intercept hyperprior scale.** $\mu_\alpha \sim \mathrm{Normal}(0, 10)$ here, against
  $\mathrm{Normal}(0, 100)$ in the original: an $L_2$ intercept penalty of $10^{-2}$ rather
  than $10^{-4}$.
- **Large counts.** The original discarded every count record with $y_r \ge 1000$; this
  version retains them.

The remaining differences between the two functions are changes of computational
parameterisation that leave the joint density unchanged:

- $\alpha_s$, $\gamma_s$ and $\psi_k$ are sampled in **centred** form (as written above)
  rather than as standardised deviates multiplied by their scale. These few-group
  hierarchies are well identified by the data, so the funnel geometry sat in the
  non-centred form.
- $\beta_{js}$ and $\zeta_g$ are sampled in **non-centred** form, because their scales are
  fixed constants rather than estimated parameters.
- $\zeta$ is defined over the count sources only. Under the original definition over all
  sources, the roughly 730 sources that contribute no count data were pure-prior dimensions
  carrying no likelihood information.

# 8. Inference

The posterior is sampled with Hamiltonian Monte Carlo (`greta`, static integrator) using
50 chains, 1000 warmup iterations and 1000 retained iterations per chain, with a
leapfrog trajectory length drawn uniformly on $[L_{\min}, L_{\max}] = [5, 60]$ steps.

The traced parameters are
$\mu_\alpha$, $\sigma_\alpha$, $\alpha$,
$\mu_\gamma$, $\sigma_\gamma$, $\gamma$,
$\delta$,
$\tilde{\beta}$,
$\tilde{\zeta}$,
$\psi$, $\sigma_\psi$, and $\phi$.
