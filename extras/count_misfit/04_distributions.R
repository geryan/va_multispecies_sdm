# Holding each count record's fitted mean fixed (posterior mean of mu), how do
# other count distributions, a duration offset and a free count level change
# the misfit? Profile likelihoods only: one or two numbers per species, no
# model refit. Plus the Fisher information each data type carries about each
# species' level (alpha).
suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
})
options(width = 220, pillar.sigfig = 3, dplyr.summarise.inform = FALSE)

args <- commandArgs(trailingOnly = TRUE)
outdir <- args[1]

cp <- readRDS(file.path(outdir, "count_predictive.rds")) |>
  mutate(
    months = ifelse(
      is.na(study_months),
      0.5,
      study_months
    ),
    effort = months / 0.5
  )
gnb <- readRDS(file.path(outdir, "gamma_nb.rds"))

# Physicists' Gauss-Hermite nodes and weights, for integrals against exp(-t^2)
gh_phys <- function(n){
  J <- matrix(0, n, n)
  off <- sqrt(seq_len(n - 1) / 2)
  J[cbind(1:(n - 1), 2:n)] <- off
  J[cbind(2:n, 1:(n - 1))] <- off
  e <- eigen(J, symmetric = TRUE)
  list(
    t = e$values,
    v = sqrt(pi) * e$vectors[1, ]^2
  )
}
ghp <- gh_phys(20)

lse_rows <- function(m){
  mx <- apply(m, 1, max)
  mx + log(rowSums(exp(m - mx)))
}

# Poisson-lognormal log pmf by adaptive Gauss-Hermite: log rate = log(mu) -
# s^2/2 + eps, eps ~ N(0, s^2), so the mean is mu. The quadrature is centred on
# each record's mode in eps and scaled by its curvature
ll_pln <- function(y, mu, s){
  a <- log(mu) - s^2 / 2
  s2 <- s^2
  eps <- s2 * (log(y + 0.5) - a) / (s2 + 1 / (y + 0.5))
  for (i in 1:60) {
    f <- y - exp(a + eps) - eps / s2
    fp <- -exp(a + eps) - 1 / s2
    step <- pmax(pmin(f / fp, 2), -2)
    eps <- eps - step
    if (max(abs(step)) < 1e-10) break
  }
  tau <- 1 / sqrt(exp(a + eps) + 1 / s2)
  e_j <- sweep(outer(tau, sqrt(2) * ghp$t), 1, eps, "+")
  g <- y * (a + e_j) - exp(a + e_j) - lgamma(y + 1) -
    e_j^2 / (2 * s2) - log(s * sqrt(2 * pi))
  log(sqrt(2) * tau) + lse_rows(sweep(g, 2, log(ghp$v) + ghp$t^2, "+"))
}
# P(Y >= y) on a fine grid in eps (only needed once, at the optimum)
tail_pln <- function(y, mu, s){
  out <- rep(1, length(y))
  pos <- which(y > 0)
  if (length(pos) == 0) return(out)
  z <- seq(-9, 9, length.out = 4001)
  w <- dnorm(z) * diff(z[1:2])
  w <- w / sum(w)
  a <- log(mu[pos]) - s^2 / 2
  rate <- exp(outer(a, s * z, "+"))
  pt <- ppois(matrix(y[pos] - 1, length(pos), length(z)), rate, lower.tail = FALSE)
  out[pos] <- as.vector(pt %*% w)
  out
}
p0_pln <- function(mu, s) exp(ll_pln(rep(0, length(mu)), mu, s))

# log-likelihoods and upper tails for the negative binomials, given means `mu`
ll_nb1 <- function(y, mu, g) dnbinom(y, size = mu / g, prob = 1 / (1 + g), log = TRUE)
ll_nb2 <- function(y, mu, k) dnbinom(y, size = k, mu = mu, log = TRUE)
tail_nb1 <- function(y, mu, g) ifelse(y > 0, pnbinom(y - 1, size = mu / g, prob = 1 / (1 + g), lower.tail = FALSE), 1)
tail_nb2 <- function(y, mu, k) ifelse(y > 0, pnbinom(y - 1, size = k, mu = mu, lower.tail = FALSE), 1)
p0_nb1 <- function(mu, g) dnbinom(0, size = mu / g, prob = 1 / (1 + g))
p0_nb2 <- function(mu, k) dnbinom(0, size = k, mu = mu)

# fit the dispersion (and optionally a level factor c) for one species
profile_one <- function(d, family, free_level, use_effort){
  y <- d$n
  mu0 <- d$mu_mean * if (use_effort) d$effort else 1
  llf <- switch(family, nb1 = ll_nb1, nb2 = ll_nb2, pln = ll_pln)
  negll <- function(par){
    disp <- exp(par[1])
    c <- if (free_level) exp(par[2]) else 1
    -sum(llf(y, mu0 * c, disp))
  }
  start <- switch(family, nb1 = log(50), nb2 = log(0.1), pln = log(2))
  if (free_level) {
    o <- optim(c(start, 0), negll, method = "Nelder-Mead", control = list(maxit = 2000, reltol = 1e-10))
  } else {
    o <- optimize(function(p) negll(p), interval = start + c(-12, 8))
    o <- list(par = o$minimum, value = o$objective)
  }
  disp <- exp(o$par[1])
  c <- if (free_level) exp(o$par[2]) else 1
  mu <- mu0 * c
  tailf <- switch(family, nb1 = tail_nb1, nb2 = tail_nb2, pln = tail_pln)
  p0f <- switch(family, nb1 = p0_nb1, nb2 = p0_nb2, pln = p0_pln)
  tl <- tailf(y, mu, disp)
  tibble(
    disp = disp,
    level = c,
    loglik = -o$value,
    pred_p_pos = mean(1 - p0f(mu, disp)),
    pos_tail_lt_01 = mean(tl[y > 0] < 0.01),
    n_pos_tail_lt_01 = sum(tl[y > 0] < 0.01)
  )
}

scenarios <- tribble(
  ~scenario,                          ~family, ~free_level, ~use_effort,
  "NB1, fitted means",                "nb1",   FALSE,       FALSE,
  "NB2, fitted means",                "nb2",   FALSE,       FALSE,
  "Poisson-lognormal, fitted means",  "pln",   FALSE,       FALSE,
  "NB1, own count level",             "nb1",   TRUE,        FALSE,
  "NB2, own count level",             "nb2",   TRUE,        FALSE,
  "NB1, duration offset + level",     "nb1",   TRUE,        TRUE,
  "NB2, duration offset + level",     "nb2",   TRUE,        TRUE,
  "PLN, duration offset + level",     "pln",   TRUE,        TRUE
)

res <- cp |>
  group_by(species) |>
  group_modify(function(d, key){
    pmap_dfr(
      scenarios,
      function(scenario, family, free_level, use_effort){
        profile_one(d, family, free_level, use_effort) |>
          mutate(scenario = scenario, .before = 1)
      }
    ) |>
      mutate(
        obs_p_pos = mean(d$n > 0),
        n_pos = sum(d$n > 0)
      )
  }) |>
  ungroup() |>
  left_join(gnb |> select(species, gamma_nb_med), by = "species")

saveRDS(res, file.path(outdir, "distribution_profiles.rds"))

cat("== all species, by scenario (dispersion and level per species) ==\n")
print(
  res |>
    group_by(scenario) |>
    summarise(
      loglik = sum(loglik),
      obs_share_pos = sum(obs_p_pos * 0 + n_pos) / nrow(cp),
      pred_share_pos = sum(pred_p_pos * table(cp$species)[species]) / nrow(cp),
      pos_tail_lt_01 = sum(n_pos_tail_lt_01) / sum(n_pos)
    ) |>
    arrange(match(scenario, scenarios$scenario)),
  width = 220
)

cat("\n== NB1 at the fitted means: profiled gamma against the posterior median ==\n")
print(
  res |>
    filter(scenario == "NB1, fitted means") |>
    select(species, disp, gamma_nb_med, obs_p_pos, pred_p_pos, pos_tail_lt_01) |>
    arrange(desc(gamma_nb_med)),
  n = 30
)

cat("\n== the main species, every scenario ==\n")
print(
  res |>
    filter(species %in% c("arabiensis", "funestus", "gambiae", "coluzzii", "pharoensis")) |>
    select(species, scenario, disp, level, loglik, obs_p_pos, pred_p_pos, pos_tail_lt_01),
  n = 60,
  width = 220
)

cat("\n== is the dispersion prior holding gamma_nb down? (plug-in means) ==\n")
# prior gamma_nb ~ Exponential(theta), theta = -log(0.05) / (2 - 1): the log
# prior falls by theta per unit of gamma_nb
theta <- -log(0.05) / (2 - 1)
prior_check <- cp |>
  group_by(species) |>
  group_modify(function(d, key){
    f <- function(g) sum(ll_nb1(d$n, d$mu_mean, g))
    g_post <- gnb$gamma_nb_med[gnb$species == key$species]
    g_hat <- exp(optimize(function(lg) -f(exp(lg)), c(log(0.5), log(1e5)))$minimum)
    h <- 0.01 * g_post
    tibble(
      gamma_post = g_post,
      gamma_profile = g_hat,
      slope_at_post = (f(g_post + h) - f(g_post - h)) / (2 * h),
      loglik_gain_to_profile = f(g_hat) - f(g_post),
      prior_cost_to_profile = theta * (g_hat - g_post)
    )
  }) |>
  ungroup() |>
  arrange(desc(gamma_post))
cat("theta (prior slope per unit gamma_nb):", round(theta, 3), "\n")
print(prior_check, n = 30)
