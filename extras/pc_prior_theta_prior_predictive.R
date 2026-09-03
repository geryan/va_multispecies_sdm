# pc_prior_theta_prior_predictive.R
#
# Prior predictive check on theta, the PC-prior rate for the NB overdispersion
# parameter gamma_nb (see extras/pc_prior_nb_overdispersion.{R,md}).
#
# QUESTION: under gamma_nb ~ Exponential(theta), does the implied prior predictive
# distribution of counts look anything like the observed count records? theta = 0.061
# is currently wired into R/fit_model_multispecies_pp_count_source_effect_reparam.R
# and section 4b of the .md note argues it is orders of magnitude too tight.
#
# METHOD
#   1. Plug-in mean model. mu_i comes from per-species Poisson GLMs on the same 10
#      covariates + log(offset). Poisson QMLE is consistent for the MEAN even when the
#      data are badly overdispersed, so mu_i is usable here; the standard errors are
#      not, and are not used. This is a STAND-IN for the full model's mean structure
#      (no bioregion interactions, no zeta, no sampling_re), so it will attribute some
#      structure to noise and if anything OVERstates the dispersion the model must absorb.
#   2. Moment estimate of what NB1 dispersion the data actually want. Under NB1
#      var_i = mu_i(1 + gamma_nb), so
#           gamma_hat = mean( (y_i - mu_i)^2 / mu_i ) - 1
#      per species. This is direct, unlike the NB2 -> NB1 conversion in section 4b.
#   3. Prior predictive. For each theta and each replicate, draw gamma_nb ~ Exp(theta)
#      per species, simulate y_i ~ NB(size = mu_i/gamma_nb, mu = mu_i), and summarise
#      zero fraction / variance-to-mean / max / q99. Compare to observed.

suppressMessages({
  library(targets); library(dplyr); library(tidyr)
})

pc_theta <- function(v, p) -log(p) / (v - 1)

covs <- tar_read(target_covariate_names)

dat <- tar_read(model_data_spatial) |>
  filter(data_type == "count", count < 1000) |>
  select(species, n, offset, all_of(covs)) |>
  filter(!is.na(n), !is.na(offset), offset > 0) |>
  tidyr::drop_na()

species <- sort(unique(dat$species))
cat("count records used:", nrow(dat), " species:", length(species), "\n\n")

## ---- 1. plug-in mean model ------------------------------------------------
# RIDGE-penalised Poisson, not plain glm(). An unpenalised GLM extrapolates to fitted
# rates of 2e-16 on these covariates (the offset alone spans 3e-7 to 3.3), which makes
# (y-mu)^2/mu explode on any record with tiny mu and y > 0 and corrupts everything
# downstream. The real model has ridge priors on the coefficients (beta_sd = sqrt(10),
# beta_regularised_sd = 0.1), so a ridge fit is both better behaved and closer to it.
suppressMessages(library(glmnet))
X <- scale(as.matrix(dat[, covs]))
dat$mu <- NA_real_
for (s in species) {
  i <- which(dat$species == s)
  fit <- glmnet(X[i, ], dat$n[i], family = "poisson",
                offset = log(dat$offset[i]), alpha = 0, lambda = 0.05)
  dat$mu[i] <- as.numeric(predict(fit, newx = X[i, ],
                                  newoffset = log(dat$offset[i]), type = "response"))
}
# floor at a small fraction of the species mean: guards the moment estimator without
# materially changing any record the mean model actually fits.
dat <- dat |> group_by(species) |> mutate(mu = pmax(mu, 1e-3 * mean(n))) |> ungroup()
cat("fitted mu by species (ridge, lambda = 0.05):\n")
print(as.data.frame(dat |> group_by(species) |>
  summarise(min = signif(min(mu), 3), med = signif(median(mu), 3),
            max = signif(max(mu), 3), mean_y = signif(mean(n), 3), .groups = "drop")),
  row.names = FALSE)
cat("\n")

## ---- 2. what gamma_nb does the data want under NB1? -----------------------
obs <- dat |>
  group_by(species) |>
  summarise(
    n_rec      = n(),
    mean_y     = mean(n),
    zero_frac  = mean(n == 0),
    vmr_raw    = var(n) / mean(n),
    max_y      = max(n),
    # NB1 moment estimator: var_i = mu_i (1 + gamma)
    gamma_mom  = mean((n - mu)^2 / mu) - 1,
    .groups = "drop"
  )
cat("OBSERVED counts, and the NB1 dispersion they imply:\n")
print(as.data.frame(obs |> mutate(across(where(is.numeric), ~signif(.x, 3)))), row.names = FALSE)

cat("\nNB1 moment gamma_nb: range", paste(signif(range(obs$gamma_mom), 3), collapse = " - "),
    " median", signif(median(obs$gamma_mom), 3), "\n\n")

## ---- 3. prior predictive over theta ---------------------------------------
# The observed data are filtered `count < 1000`, and the 68 dropped records are all
# EXACTLY 1000 -- i.e. censored/capped values in the source data. The simulation must
# apply the same filter or the comparison is not apples-to-apples. Under that truncation
# `max` is uninformative (almost every replicate hits 999), so the tail is summarised by
# q99 and the fraction of records >= 100 instead.
sim_once <- function(theta) {
  g <- rexp(length(species), rate = theta)          # gamma_nb ~ Exp(theta), per species
  names(g) <- species
  gi <- g[dat$species]
  y <- rnbinom(nrow(dat), size = dat$mu / gi, mu = dat$mu)
  y <- y[y < 1000]                                  # same filter as the observed data
  c(zero_frac = mean(y == 0),
    vmr       = var(y) / mean(y),
    q99       = unname(quantile(y, 0.99)),
    ge100     = mean(y >= 100))
}

prior_pred <- function(theta, n_rep = 400, seed = 1) {
  set.seed(seed)
  r <- t(replicate(n_rep, sim_once(theta)))
  q <- function(v, p) unname(quantile(r[, v], p))
  ok <- function(v, o) o >= q(v, .05) && o <= q(v, .95)
  data.frame(
    theta    = theta,
    E_gamma  = signif(1 / theta, 4),
    zero     = signif(median(r[, "zero_frac"]), 3),
    zero_ci  = sprintf("[%.3f,%.3f]", q("zero_frac", .05), q("zero_frac", .95)),
    zero_ok  = ok("zero_frac", obs_zero),
    vmr      = signif(median(r[, "vmr"]), 3),
    vmr_ci   = sprintf("[%.0f,%.0f]", q("vmr", .05), q("vmr", .95)),
    vmr_ok   = ok("vmr", obs_vmr),
    q99_ci   = sprintf("[%.0f,%.0f]", q("q99", .05), q("q99", .95)),
    q99_ok   = ok("q99", obs_q99),
    ge100_ci = sprintf("[%.3f,%.3f]", q("ge100", .05), q("ge100", .95)),
    ge100_ok = ok("ge100", obs_ge100),
    row.names = NULL
  )
}

yo <- dat$n
obs_zero <- mean(yo == 0); obs_vmr <- var(yo)/mean(yo)
obs_q99  <- unname(quantile(yo, 0.99)); obs_ge100 <- mean(yo >= 100)

thetas <- c(0.061, 0.02, 0.01, 0.005, 0.003, 0.002, 0.001, 5e-4, 1e-4)

cat("PRIOR PREDICTIVE (400 draws each), simulation truncated at 1000 like the data.\n")
cat("OBSERVED:  zero_frac =", signif(obs_zero,3), " vmr =", signif(obs_vmr,3),
    " q99 =", obs_q99, " frac>=100 =", signif(obs_ge100,3), "\n")
cat("*_ok = observed statistic falls inside the 5-95% prior predictive interval.\n\n")
res <- do.call(rbind, lapply(thetas, prior_pred))
print(res, row.names = FALSE)

cat("\nthetas covering ALL FOUR statistics:",
    paste(res$theta[res$zero_ok & res$vmr_ok & res$q99_ok & res$ge100_ok], collapse=", "), "\n")
cat("n statistics covered, by theta:\n")
print(data.frame(theta = res$theta,
                 n_ok = res$zero_ok + res$vmr_ok + res$q99_ok + res$ge100_ok), row.names = FALSE)

## ---- 4. does the prior cover the moment estimates? -------------------------
cat("\nPrior mass above the per-species NB1 moment estimate, Pr(gamma_nb > gamma_mom):\n")
tail_prob <- sapply(thetas, function(th) exp(-th * obs$gamma_mom))
dimnames(tail_prob) <- list(obs$species, paste0("theta=", signif(thetas, 3)))
print(signif(tail_prob, 3))

invisible(list(obs = obs, prior_pred = res, dat = dat))
