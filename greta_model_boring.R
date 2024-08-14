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
.libPaths("~/home/user/R/gr_lib/")
.libPaths()
library("dplyr")
library("tidyr")
library("targets")
library("greta")
library(terra)
tar_load(model_layers)

# using mpp_rcrds_2
# tar_load(mpp_rcrds_2)
# str(mpp_rcrds_2)
#mppdat <- mpp_rcrds_2

# using mpp_data - better
tar_load(mpp_data)
mppdat <- mpp_data

# all_locations <- bind_rows(
#   mppdat$pa |>
#     select(tcw, tcb, built_volume, lst_day, evi, rainfall, mech, bias),
#   mppdat$bg |>
#     select(tcw, tcb, built_volume, lst_day, evi, rainfall, mech, bias),
#   tibble(po = mppdat$po) |>
#     unnest(po) |>
#     as.data.frame() |>
#     select(tcw, tcb, built_volume, lst_day, evi, rainfall, mech, bias)
# )

all_locations <- bind_rows(
  mppdat$pa |>
    select(
      ag_microclim,
      research_tt_by_country,
      arid,
      built_volume,
      cropland,
      elevation,
      evi_mean,
      footprint,
      lst_day_mean,
      lst_night_mean,
      pop,
      pressure_mean,
      rainfall_mean,
      soil_clay,
      solrad_mean,
      surface_water,
      tcb_mean,
      tcw_mean,
      windspeed_mean,
      easting,
      northing
    ),
  mppdat$bg |>
    select(
      ag_microclim,
      research_tt_by_country,
      arid,
      built_volume,
      cropland,
      elevation,
      evi_mean,
      footprint,
      lst_day_mean,
      lst_night_mean,
      pop,
      pressure_mean,
      rainfall_mean,
      soil_clay,
      solrad_mean,
      surface_water,
      tcb_mean,
      tcw_mean,
      windspeed_mean,
      easting,
      northing
    ),
  tibble(po = mppdat$po) |>
    unnest(po) |>
    as.data.frame() |>
    select(
      ag_microclim,
      research_tt_by_country,
      arid,
      built_volume,
      cropland,
      elevation,
      evi_mean,
      footprint,
      lst_day_mean,
      lst_night_mean,
      pop,
      pressure_mean,
      rainfall_mean,
      soil_clay,
      solrad_mean,
      surface_water,
      tcb_mean,
      tcw_mean,
      windspeed_mean,
      easting,
      northing
    )
)


#
#
# mech <- model_layers[["ag_microclim"]]
# mechvals <- values(mech)
# minmech <- min(mechvals[mechvals > 0], na.rm = TRUE)
# mechvals[mechvals == 0] <- minmech
#
# mech_alt <- mech
# mech_alt[] <- mechvals
#
# mech_log <- log(mech_alt)
# plot(mech_log)
#
#
# mech_dat <- all_locations$ag_microclim
# mech_dat[mech_dat == 0] <- minmech
# log_mech_dat <- log(mech_dat) |>
#   matrix(data = _, ncol = 1)


npa <- nrow(mppdat$pa)
nbg <- nrow(mppdat$bg)
npo <- tibble(po = mppdat$po) |>
  unnest(po) |>
  nrow()

pa.samp <- 1:npa
bg.samp <- (npa + 1):(npa + nbg)
po.samp <- (npa + nbg +1):(npa + npo + nbg)

area_bg <- sum(!is.na(values(model_layers[[1]])))/nbg
npospp <- sapply(mppdat$po, nrow)

x <- all_locations |>
  #select(tcw, tcb, built_volume, lst_day, evi, rainfall) |>
  select(arid,
         built_volume,
         cropland,
         elevation,
         evi_mean,
         footprint,
         lst_day_mean,
         lst_night_mean,
         pop,
         pressure_mean,
         rainfall_mean,
         soil_clay,
         solrad_mean,
         surface_water,
         tcb_mean,
         tcw_mean,
         windspeed_mean,
         easting,
         northing) |>
  as.matrix()

z <- all_locations |>
  select(research_tt_by_country) |>
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

# trying others
# penalty.l2.sdm <- penalty.l2.bias <- 0.2
# penalty.l2.intercept <- 1e-2


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

# log_lambda_larval_habitat <- sweep(x %*% beta, 2, alpha, FUN = "+")
#
# log_lambda_adults <- log_mech_dat
#
# log_lambda <- sweep(log_lambda_larval_habitat, 1, log_lambda_adults, "+")

log_lambda <- sweep(x %*% beta, 2, alpha, FUN = "+")

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

# plot(m)




inits <- function(){

  nsp <- 4
  ncv <- 19
  n_a <- nsp
  n_b <- nsp * ncv
  n_g <- nsp

  ina <- #-1e-2
  inb <- #-1e-1
  ing <- #0
  ind <- #1e-4

  initials(
    alpha = rep(ina, n_a),
    beta = matrix(
      data = rep(inb, n_b),
      ncol = ,
      nrow = ncv
    ),
    gamma = rep(ing, n_g),
    delta = ind
  )

}



# calculate estimates based on initials and compare with data
p_inits <- calculate(p, values = inits())

#library(tidyr)
initplotdatt <- pa |>
  as_tibble() |>
  mutate(
    id = row_number()
  ) |>
  pivot_longer(
    cols = -id,
    names_to = "sp",
    values_to = "p"
  ) |>
  left_join(
    y = tibble(
      arabiensis = p_inits$p[,1],
      funestus = p_inits$p[,2],
      coluzzii = p_inits$p[,3],
      gambiae = p_inits$p[,4]
    )  |>
      mutate(
        id = row_number()
      ) |>
      pivot_longer(
        cols = -id,
        names_to = "sp",
        values_to = "p_init"
      ),
    by = c("id", "sp")
  )

library(ggplot2)
ggplot(initplotdatt) +
  geom_violin(
    aes(x = as.factor(p), y = p_init)
  ) +
  facet_wrap(~sp)


draws <- mcmc(m, warmup = 5000, n_samples = 5000, initial_values = inits())

#draws <- mcmc(m, warmup = 1000, n_samples = 3000)

r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
max(r_hats$psrf[, 2])
r_hats

mcmc_trace(draws)

mcmc_intervals(draws)


# update with better inits and run again

posterior <- calculate(alpha, beta, gamma, delta, values = draws, nsim = 1000)

inits_alpha <- apply(posterior$alpha, MARGIN = 2, FUN = mean)
inits_beta <- apply(posterior$beta, MARGIN = c(2,3), FUN = mean)
inits_gamma <- apply(posterior$gamma, MARGIN = 2, FUN = mean)
inits_delta <- mean(posterior$delta)



inits_2 <- initials(
  alpha = inits_alpha,
  beta = inits_beta,
  gamma = inits_gamma,
  delta = inits_delta
)


p_inits <- calculate(p, values = inits_2)

#library(tidyr)
initplotdatt <- pa |>
  as_tibble() |>
  mutate(
    id = row_number()
  ) |>
  pivot_longer(
    cols = -id,
    names_to = "sp",
    values_to = "p"
  ) |>
  left_join(
    y = tibble(
      arabiensis = p_inits$p[,1],
      funestus = p_inits$p[,2],
      coluzzii = p_inits$p[,3],
      gambiae = p_inits$p[,4]
    )  |>
      mutate(
        id = row_number()
      ) |>
      pivot_longer(
        cols = -id,
        names_to = "sp",
        values_to = "p_init"
      ),
    by = c("id", "sp")
  )

library(ggplot2)
ggplot(initplotdatt) +
  geom_violin(
    aes(x = as.factor(p), y = p_init)
  ) +
  facet_wrap(~sp)



draws <- mcmc(m, warmup = 5000, n_samples = 5000, initial_values = inits_2)

#draws <- mcmc(m, warmup = 1000, n_samples = 3000)

r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
max(r_hats$psrf[, 2])
r_hats


## predict

model_layers

library("terra")
all_layer_values <- values(model_layers)
naidx <- is.na(all_layer_values[,1])


x_predict <- all_layer_values[
  !naidx,
  c(
    "arid",
    "built_volume",
    "cropland",
    "elevation",
    "evi_mean",
    "footprint",
    "lst_day_mean",
    "lst_night_mean",
    "pop",
    "pressure_mean",
    "rainfall_mean",
    "soil_clay",
    "solrad_mean",
    "surface_water",
    "tcb_mean",
    "tcw_mean",
    "windspeed_mean",
    "easting",
    "northing"
    )
]

log_lambda_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")

p_predict <- icloglog(log_lambda_predict + log(area_pa))

preds <- calculate(p_predict, values = draws, nsim = 1)


# add in rel abund data where available as an additional likelihood
# use a multinomial obs model where p is from predicted abundances
#

mod_pred_aa <- mod_pred_af <- mod_pred_ac <- mod_pred_ag <- model_layers[[1]]

mod_pred_aa[!naidx] <- preds$p_predict[,,1]
mod_pred_af[!naidx] <- preds$p_predict[,,2]
mod_pred_ac[!naidx] <- preds$p_predict[,,3]
mod_pred_ag[!naidx] <- preds$p_predict[,,4]


