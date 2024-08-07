library(greta)
library(tidyverse)
library(targets)
library(bayesplot)
library(terra)
library(tidyterra)


## poiss approx
# use set of bg points, e.g. 10k
# calculate area of whole thing area of interest
# area of each background point = area of interest / n bg points
# for PO area is strongly negative offset, so tiny area
# (becomes -ve once logged)
# PA area assume 1?


# using mpp_rcrds_2
tar_load(mpp_rcrds_2)
tar_load(model_layers)


str(mpp_rcrds_2)

mppdat <- mpp_rcrds_2

all_locations <- bind_rows(
  mppdat$pa |>
    select(tcw, tcb, built_volume, lst_day, evi, rainfall, mech, bias),
  mppdat$bg |>
    select(tcw, tcb, built_volume, lst_day, evi, rainfall, mech, bias),
  tibble(po = mppdat$po) |>
    unnest(po) |>
    as.data.frame() |>
    select(tcw, tcb, built_volume, lst_day, evi, rainfall, mech, bias)
)

mech <- model_layers[["mech"]]
mechvals <- values(mech)
minmech <- min(mechvals[mechvals > 0], na.rm = TRUE)
mechvals[mechvals == 0] <- minmech

mech_alt <- mech
mech_alt[] <- mechvals

mech_log <- log(mech_alt)
plot(mech_log)


mech_dat <- all_locations$mech
mech_dat[mech_dat == 0] <- minmech
log_mech_dat <- log(mech_dat) |>
  matrix(data = _, ncol = 1)


npa <- nrow(mppdat$pa)
nbg <- nrow(mppdat$bg)
npo <- tibble(po = mppdat$po) |>
  unnest(po) |>
  nrow()

pa.samp <- 1:npa
bg.samp <- (npa + 1):(npa + nbg)
po.samp <- (npa + nbg +1):(npa + npo + nbg)

area_bg <- sum(!is.na(mechvals))/nbg
npospp <- sapply(mppdat$po, nrow)

x <- all_locations |>
  select(tcw, tcb, built_volume, lst_day, evi, rainfall) |>
  as.matrix()

z <- all_locations |>
  select(bias) |>
  as.matrix()

n.pixel <- nrow(x)

# numbers of covariates in use
n_cov_abund <- ncol(x)
n_cov_bias <- ncol(z)

# number of species to model
n_species <- 4

# tidy up PA data
pa <- mppdat$pa |>
  select(arabiensis, funestus, coluzzii, gambiae)

# generate po count matrix from all locations by assigning 1 to po per spp.
po.count <- matrix(
  data = 0,
  nrow = n.pixel,
  ncol = n_species,
  dimnames = list(NULL, names(pa))
)

po.count[(npa+nbg+1):(npa+nbg+npospp[1]), 1] <- 1
po.count[(npa+nbg+npospp[1]+1):(npa+nbg+npospp[1]+npospp[2]), 2] <- 1
po.count[(npa+nbg+npospp[1]+npospp[2]+1):(npa+nbg+npospp[1]+npospp[2]+npospp[3]), 3] <- 1
po.count[(npa+nbg+npospp[1]+npospp[2]+npospp[3]+1):(npa+nbg+npospp[1]+npospp[2]+npospp[3]+npospp[4]), 4] <- 1


# define parameters with normal priors, matching the ridge regression setup in
# multispeciesPP defaults
penalty.l2.sdm <- penalty.l2.bias <- 0.1
penalty.l2.intercept <- 1e-4

intercept_sd <- sqrt(1 / penalty.l2.intercept)
beta_sd <- sqrt(1 / penalty.l2.sdm)
delta_sd <- sqrt(1 / penalty.l2.bias)

# intercept and shared slope for selection bias
gamma <- normal(0, intercept_sd, dim = n_species)
delta <- normal(0, delta_sd, dim = c(n_cov_bias))

# intercept and slopes for abundance rate
alpha <- normal(0, intercept_sd, dim = n_species)
beta <- normal(0, beta_sd, dim = c(n_cov_abund, n_species))

# log rates across all sites

log_lambda_larval_habitat <- sweep(x %*% beta, 2, alpha, FUN = "+")

log_lambda_adults <- log_mech_dat

log_lambda <- sweep(log_lambda_larval_habitat, 1, log_lambda_adults, "+")

# can easily replace this model with something more interesting, like a low-rank
# GP on covariate space or something mechanistic

# bias across pixels (shared coefficient) and species (different intercepts)
log_bias_coef <- sweep(zeros(n.pixel, n_species), 1, z %*% delta, FUN = "+")
log_bias <- sweep(log_bias_coef, 2, gamma, FUN = "+")

# rates across all sites and species
lambda <- exp(log_lambda)

# offset stuff
bias <- exp(log_bias)

# define likelihoods

# compute probability of presence (and detection) at the PA sites, assuming
# area/effort of 1 in all these sites, for identifiability

### can compute separately for each PA PO and RELABUND
area_pa <- 1
#area_pa <- uniform(0,1)

p <- icloglog(log_lambda[pa.samp, ] + log(area_pa))
distribution(pa) <- bernoulli(p)

## compute (biased) expected numbers of presence-only observations across all
## presence and background sites, assuming presence-only count aggregation area
## of 1 (as in multispeciesPP definition). Not that when these are all the same,
## this value only changes all the gamma parameters by a fixed amount, and these
## are not the parameters of interest


area_po <- 1/nbg # 1/10000 # very small
po_idx <- which(po.count ==1, arr.ind = TRUE)
po_rate_po <- exp(log_lambda[po_idx] + log_bias[po_idx] + log(area_po))
distribution(po.count[po_idx]) <- poisson(po_rate_po)

# area_bg <- 3654.515 <- whole area / nnbg
po_rate_bg <- exp(log_lambda[bg.samp, ] + log_bias[bg.samp,] + log(area_bg))
distribution(po.count[bg.samp, ]) <- poisson(po_rate_bg)


# define and fit the model by MAP and MCMC
m <- model(alpha, beta, gamma, delta)

plot(m)

map <- opt(m, optimiser = adam(), max_iterations = 500)

inits <- function(){

  ina <- 0
  inb <- 0
  ing <- 0
  ind <- 0

  initials(
    alpha = rep(ina, 4),
    beta = matrix(
      data = rep(inb, 4*6),
      ncol = 4,
      nrow = 6
    ),
    gamma = rep(ing, 4),
    delta = ind
  )

}

calculate(p, values = inits())



draws <- mcmc(m, warmup = 1000, n_samples = 3000, initial_values = inits())


r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
max(r_hats$psrf[, 2])
r_hats

mcmc_trace(draws)

mcmc_intervals(draws)
# greta estimates and uncertainty
greta_map_estimates <- with(map$par, c(rbind(t(alpha), beta)))
greta_sims <- calculate(rbind(t(alpha), beta),
                        values = draws,
                        nsim = 1000)[[1]]
greta_mcmc_estimates <- c(apply(greta_sims, 2:3, mean))
greta_mcmc_upper <- c(apply(greta_sims, 2:3, quantile, 0.975))
greta_mcmc_lower <- c(apply(greta_sims, 2:3, quantile, 0.025))


## predict

model_layers

all_layer_values <- values(model_layers)
naidx <- is.na(all_layer_values[,1])


x_predict <- all_layer_values[!naidx, c("tcw", "tcb", "built_volume", "lst_day", "evi", "rainfall")]

log_lambda_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")

p_predict <- icloglog(log_lambda_predict[pa.samp, ] + log(area_pa))

preds <- calculate(p_predict, values = draws, nsim = 1)


# add in rel abund data where available as an additional likelihood
# use a multinomial obs model where p is from predicted abundances
#

