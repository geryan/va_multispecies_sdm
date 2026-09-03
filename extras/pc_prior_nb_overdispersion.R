# pc_prior_nb_overdispersion.R
#
# Penalised-complexity (PC) prior on negative-binomial overdispersion, in greta's
# (size, prob) parameterisation, for the count likelihood of
# fit_model_multispecies_pp_count_source_effect_reparam().
#
# GOAL: put prior mass at the Poisson limit, so overdispersion has to be argued
# for by the data rather than assumed by the prior. V3 of the reparam
# (log_size ~ N(2, 2)) deliberately traded the original PC prior away to kill the
# size -> Inf funnel; this restores a PC prior in a form that keeps the good
# sampling geometry.
#
# ---------------------------------------------------------------------------
# 0. NOTATION -- READ THIS FIRST
# ---------------------------------------------------------------------------
# gamma_nb is the LINEAR OVERDISPERSION PARAMETER itself:
#
#       gamma_nb := mu / size          var = mu * (1 + gamma_nb)
#
# so gamma_nb = 0 is Poisson and gamma_nb is the excess variance as a multiple of
# the Poisson variance. It is a FREE PARAMETER (one per species), estimated.
# theta is the RATE of the PC prior placed on it: gamma_nb ~ Exponential(theta).
# theta is a fixed elicited constant, not estimated.
#
# WARNING -- the original spec  size ~ InverseGamma(alpha = 1, beta = mu*gamma_nb)
# is NOT self-consistent with gamma_nb = mu/size. Since E[1/size] = 1/beta,
#       beta = mu * gamma_nb  =>  E[VIF - 1] = 1 / gamma_nb   (gamma_nb is a RATE,
#                                                              Poisson at +Inf)
#       gamma_nb = mu / size  =>  E[VIF - 1] = gamma_nb       (Poisson at 0)
# These are reciprocals. The correct form under the definition above is
#
#       size ~ InverseGamma(1, mu / gamma_nb)        <- DIVIDED, not multiplied
#
# and it has to be this way round, because a PC prior is exponential on the KLD
# distance from the base model and that distance is proportional to gamma_nb only
# when gamma_nb -> 0 is Poisson. An exponential prior on a parameter whose base
# model sits at +Inf is not a PC prior at all.
#
# ---------------------------------------------------------------------------
# 1. WHY  size ~ InverseGamma(1, mu / gamma_nb)  IS THE RIGHT PRIOR
# ---------------------------------------------------------------------------
# greta's negative_binomial(size, prob) has mean = size(1-prob)/prob, var = mean/prob.
# With prob = size/(size + mu) the mean is mu and
#     var = mu + mu^2 / size = mu * (1 + mu * psi),      psi := 1 / size
# so psi = 0 (size = Inf) IS the Poisson base model, and mu*psi = mu/size = gamma_nb.
# Define the variance inflation factor  VIF = var / mu = 1 + gamma_nb.
#
# In greta, inverse_gamma(alpha, beta) has beta as the SCALE, so
#     size ~ InverseGamma(1, b)   <=>   psi = 1/size ~ Exponential(rate = b)
# (verified numerically: mean(1/size) = alpha/b, sd(1/size) = sqrt(alpha)/b).
# With b = mu / gamma_nb the mu CANCELS out of the induced prior on the VIF:
#
#     VIF - 1  =  mu * psi  ~  Exponential(rate = 1 / gamma_nb)            (*)
#
# Two things follow, and both are the reason mu belongs in beta at all:
#
#   (i)  Every count record gets the SAME prior on its variance inflation,
#        regardless of its expected count. gamma_nb is scale-free.
#   (ii) The prior is centred on  size_i = mu_i / gamma_nb, i.e. on LINEAR
#        (NB1 / quasi-Poisson) overdispersion  var_i = mu_i (1 + gamma_nb),
#        not the quadratic  var = mu + mu^2/size  of a constant-size NB2.
#
# The PC property is a separate claim, and it holds. Expanding the Poisson-gamma
# mixture around Lambda = mu (mixing variance mu^2 psi) gives
#     KLD( NB(mu) || Poisson(mu) )  ~  (mu * psi)^2 / 4  =  gamma_nb^2 / 4
# so the PC distance is  d = sqrt(2 * KLD) = gamma_nb / sqrt(2).  PC priors are
# exponential on d, hence exponential on gamma_nb -- which is what section 2 sets.
#
# ---------------------------------------------------------------------------
# 2. ELICITING THE PC RATE theta
# ---------------------------------------------------------------------------
# gamma_nb = VIF - 1 is the excess variance as a multiple of the Poisson variance.
# Under the PC prior gamma_nb ~ Exponential(theta):
#       E[gamma_nb]     = 1 / theta          (and SD[gamma_nb] = 1/theta too)
#       Pr(VIF > v)     = exp(-theta (v - 1))
#       theta           = -log(p) / (v - 1)
# i.e. "prior probability the count variance exceeds v times the Poisson
# variance is p". theta -> Inf is the Poisson limit; theta small is permissive.
pc_theta <- function(v, p) {
  stopifnot(v > 1, p > 0, p < 1)
  -log(p) / (v - 1)
}
# pc_theta(v = 3,  p = 0.01) -> 2.30      pc_theta(v = 5,  p = 0.01) -> 1.15
# pc_theta(v = 10, p = 0.05) -> 0.33      pc_theta(v = 50, p = 0.05) -> 0.061
#
# *** theta = 0.061 WAS PROVISIONALLY CHOSEN AND IS NOW KNOWN TO BE FAR TOO TIGHT. ***
#
# It was picked on the assumption that the mean model / zeta / sampling_re absorb
# most of the raw marginal VMR (30-500 by species). MEASURED, that assumption is
# false. The converged 30-chain V3 run (extras/sre_keepre_draws.rds, 60k draws)
# has posterior log_size medians of -4.6 to -9.0, i.e. size = 1e-4 to 1e-2, which
# is 3.3 to 5.5 PRIOR SDs BELOW the mean of its own N(2, 2) prior -- the data
# dragged it there against a prior resisting. Implied NB1 gamma_nb = mu/size at
# each species' mean count:
#
#   arabiensis 20200   coluzzii 54700   funestus 40000   gambiae 28600
#   melas        355   merus     18.4   moucheti  2780   nili      3640
#
# So residual dispersion is MORE extreme than the marginal VMR suggested, not
# less, and E[gamma_nb] = 1/theta = 16.4 is 2-3 orders of magnitude too small for
# 7 of the 8 species. Only merus is near it.
#
# CAVEATS on that measurement: (a) those draws are the pa_frac = 0.20 subsample
# mixing study, not a production posterior (all count records were retained, so
# the count likelihood is intact, but it is still a subsample run); (b) V3 is NB2
# and this is NB1, so mu/size is a matching-at-the-mean approximation, NOT an
# equivalence -- NB2's VIF grows with mu while NB1's is constant.
#
# WHAT THIS MEANS -- unresolved, do not just rescale theta and move on:
#   * If NB1 is kept, theta must be far smaller. Pr(VIF > 20000) = 0.5 needs
#     theta ~ 3.5e-5. At that point the PC prior barely constrains anything and
#     the "prior mass at Poisson" goal is largely cosmetic.
#   * The bigger question is whether NB1 is the right likelihood at all. The count
#     data are ~75% zeros with a max of 968; V3's NB2 handles that with a variance
#     that grows quadratically in mu, which a constant-VIF NB1 cannot mimic.
#     A posterior predictive check of A against V3 is the thing to run next.

# ---------------------------------------------------------------------------
# 3. OPTION A -- deterministic NB1.  RECOMMENDED unless you specifically want
#    observation-level dispersion heterogeneity.
# ---------------------------------------------------------------------------
# Take the prior's centre  size_i = mu_i / gamma_nb  deterministically. Then
#       prob_i = size_i/(size_i + mu_i) = 1/(1 + gamma_nb)          [constant]
#       var_i  = mu_i (1 + gamma_nb)                                [linear]
# and the only free dispersion parameters are the n_species gamma_nbs. ZERO extra
# latent dimensions -- which matters here: V2 of the reparam fixed exactly this
# kind of problem by cutting zeta from n_sid down to count-sources-only.
if (FALSE) {

  theta <- pc_theta(v = 50, p = 0.05)   # = 0.061

  # gamma_nb = VIF - 1 = mu/size. PC prior, mode at the Poisson limit gamma_nb = 0.
  gamma_nb <- exponential(theta, dim = n_species)

  mu <- count_data_response_expected
  size_vec <- mu / gamma_nb[species_index]
  prob_vec <- size_vec / (size_vec + mu)      # = 1/(1 + gamma_nb), constant per species
  distribution(count_data_response) <- negative_binomial(size_vec, prob_vec)

  # ... and in model(): trace `gamma_nb` in place of `log_size`.
}

# ---------------------------------------------------------------------------
# 4. OPTION B -- the stochastic form,  size ~ InverseGamma(1, mu / gamma_nb)
# ---------------------------------------------------------------------------
# This is A plus per-observation dispersion heterogeneity: instead of every
# record having VIF = 1 + gamma_nb exactly, each draws VIF_i - 1 ~ Exp(1/gamma_nb),
# whose SD equals its mean. Heavier marginal tails than A. It is a proper model
# (proper prior per record, scale pinned by mu_i which is pinned by the rest of
# the likelihood), and on data actually generated this way it recovers gamma_nb
# where A underestimates it (see section 5). The cost is that `size` becomes a
# variable of dim n_count = 12,376, each element informed by ONE count.
#
# USE THE NON-CENTRED FORM. The centred form below funnels at exactly the place
# the PC prior concentrates: as gamma_nb -> 0 the 12,376 latent psi_i are all pulled
# to zero by a prior whose rate mu_i/gamma_nb diverges, and gamma_nb is the neck.
# Measured on the truly-Poisson species: R-hat 1.11 / 1.30 / 1.50 across three
# runs, while the overdispersed species in the same fits sat at ~1.00. This is
# the mirror of V5: V5 went CENTRED because those few-group effects were well
# informed by data; each psi_i here sees one observation and is prior-dominated,
# so it must go NON-CENTRED.
if (FALSE) {

  theta <- pc_theta(v = 50, p = 0.05)   # = 0.061

  gamma_nb <- exponential(theta, dim = n_species)
  mu <- count_data_response_expected

  # --- non-centred: standard Exp(1) innovations, scale applied deterministically.
  # e ~ Exp(1)  =>  psi_i = e_i*gamma_nb/mu_i ~ Exp(rate = mu_i/gamma_nb), which is
  # exactly size_i ~ InverseGamma(1, mu_i/gamma_nb) -- same model, no funnel.
  e <- exponential(1, dim = n_count)
  size_vec <- mu / (gamma_nb[species_index] * e)

  # --- centred equivalent, for reference. Same target, funnels at gamma_nb -> 0:
  # size_vec <- inverse_gamma(1, mu / gamma_nb[species_index])

  prob_vec <- size_vec / (size_vec + mu)
  distribution(count_data_response) <- negative_binomial(size_vec, prob_vec)

  # trace `gamma_nb` only -- NOT size_vec or e (12,376 columns each).
}
# DO NOT trace size / psi / e, and do not put the PC prior on a per-record
# quantity: the hyperparameter gamma_nb is what is identified and what to monitor.

# ---------------------------------------------------------------------------
# 5. CHECK
# ---------------------------------------------------------------------------
# Simulates counts including a species whose truth is EXACTLY Poisson, fits the
# variants, reports recovery + R-hat. All results below: n = 1500, 3 species,
# gamma_true = c(30, 5, 0), theta = 0.061, 4 chains, hmc(15, 60), 1000/1000.
#
#   run_check(gen = "A")   -- data are NB1 (A's own model)
#     sp  gamma_true   A: post [95%]          B: post [95%]
#     1       30       29.9 [25.7, 34.9]      41.5 [33.8, 50.5]   <- B biased, wrong model
#     2        5        5.5 [4.7,  6.4]        6.5 [5.3,  8.0]
#     3        0        0.06 [0.00, 0.17]      0.07 [0.00, 0.17]  rhat_B = 1.11
#
#   run_check(gen = "B")   -- data have per-record dispersion (B's own model)
#     sp  gamma_true   A: post [95%]          B: post [95%]      Bnc: post [95%]
#     1       30       24.2 [20.7, 28.4]      29.5 [24.0, 35.8]  29.6 [24.2, 36.1]
#     2        5        5.4 [4.7,  6.3]        6.1 [5.0,  7.4]    6.1 [5.0,  7.3]
#     3        0        0.04 [0.00, 0.12]      0.03 [0.00, 0.08]  0.04 [0.00, 0.12]
#     rhat sp3:         1.00                   1.50  <-- FUNNEL   1.00  <-- fixed
#   timings: A 70s, B 141s, Bnc 164s.
#
# Readings:
#  * The PC prior does NOT manufacture overdispersion. Under a prior with
#    E[gamma_nb] = 16.4, the truly-Poisson species posterior is 0.03-0.06 in every
#    variant. That is the property being bought.
#  * Each of A and B recovers its own generative truth and is biased on the
#    other's, so the A/B choice is a real modelling question about whether
#    dispersion varies BETWEEN count records, not a computational nicety.
#  * B's centred form fails only at gamma_nb -> 0 (R-hat 1.11/1.30/1.50 on the
#    Poisson species across runs); the non-centred form fixes it at ~5% more
#    wall-clock and identical recovery. If B is used, use Bnc.
#  * Caveat: this is 1500 records and a trivial mean model. The real fit has
#    12,376 records plus beta/zeta/sampling_re, so B's extra dimensions are ~8x
#    this and sit alongside everything V1/V2/V5 were needed to tame.
# `gen` selects the GENERATIVE model, so A and B can each be tested on their own
# truth as well as cross-tested:
#   gen = "A": NB1, VIF_i = 1 + gamma_nb exactly for every record.
#   gen = "B": VIF_i - 1 ~ Exp(rate = 1/gamma_nb) drawn per record (extra heterogeneity).
run_check <- function(n = 1500, n_sp = 3, gamma_true = c(30, 5, 0),
                      theta = pc_theta(50, 0.05), seed = 1, which = c("A", "B"),
                      gen = c("A", "B")) {

  set.seed(seed)
  library(greta)

  gen <- match.arg(gen)
  sp <- rep(seq_len(n_sp), length.out = n)
  log_mu <- rnorm(n, log(15), 1.2)
  mu_true <- exp(log_mu)
  y <- vapply(seq_len(n), function(i) {
    om <- gamma_true[sp[i]]
    # under gen = "B" each record draws its own VIF - 1 ~ Exp(1/om)
    if (gen == "B" && om > 0) om <- rexp(1, rate = 1 / om)
    if (om <= 0) rpois(1, mu_true[i])
    else rnbinom(1, size = mu_true[i] / om, mu = mu_true[i])
  }, numeric(1))
  cat(sprintf("\n--- data generated from model %s ---\n", gen))

  mean_model <- function() {
    a <- normal(0, 5, dim = n_sp); b <- normal(0, 1)
    exp(a[sp] + b * (log_mu - log(15)))
  }

  go <- function(label, build) {
    yy <- as_data(y); mu <- mean_model()
    gamma_nb <- exponential(theta, dim = n_sp)
    sz <- build(mu, gamma_nb)
    distribution(yy) <- negative_binomial(sz, sz / (sz + mu))
    t0 <- Sys.time()
    d <- mcmc(model(gamma_nb), warmup = 1000, n_samples = 1000, chains = 4,
              sampler = hmc(Lmin = 15, Lmax = 60), verbose = FALSE)
    po <- as.matrix(d); oc <- grep("^gamma_nb", colnames(po))
    cat(sprintf("\n=== %s === (%.0fs)\n", label, as.numeric(Sys.time() - t0, units = "secs")))
    print(data.frame(
      species = seq_len(n_sp), gamma_true = gamma_true,
      post = round(colMeans(po[, oc, drop = FALSE]), 3),
      q025 = round(apply(po[, oc, drop = FALSE], 2, quantile, .025), 3),
      q975 = round(apply(po[, oc, drop = FALSE], 2, quantile, .975), 3),
      rhat = round(coda::gelman.diag(d[, oc], multivariate = FALSE)$psrf[, 1], 3),
      row.names = NULL))
  }

  if ("A" %in% which) go("A: deterministic NB1", function(mu, g) mu / g[sp])
  if ("B" %in% which) go("B: stochastic InvGamma (centred)",
                         function(mu, g) inverse_gamma(1, mu / g[sp]))
  if ("Bnc" %in% which) go("Bnc: stochastic InvGamma (non-centred)", function(mu, g) {
    e <- exponential(1, dim = n)          # standard Exp(1), scale-free
    mu / (g[sp] * e)                      # size_i = mu_i / (gamma_nb * e_i)
  })
}
