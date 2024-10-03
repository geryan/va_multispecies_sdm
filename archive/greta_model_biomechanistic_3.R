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


# pred abundances for all three then sum for gambiae complex]
# likelihood has extra level for complex
# po


#mppdat <- readRDS("mpp_data_mech.Rds")
#model_layers_mech <- rast("outputs/model_layers_mech.tif")

# run data_pipeline.R

model_layers_mech <- modlyr

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
  pa_covs |>
    select(
      ag_microclim,
      research_tt_by_country,
      cropland,
      footprint,
      lst_day_mean,
      rainfall_mean
    ),
  bg |>
    select(
      ag_microclim,
      research_tt_by_country,
      cropland,
      footprint,
      lst_day_mean,
      rainfall_mean
    ),
  tibble(po = po_covs) |>
    unnest(po) |>
    as.data.frame() |>
    select(
      ag_microclim,
      research_tt_by_country,
      cropland,
      footprint,
      lst_day_mean,
      rainfall_mean
    )
)

mech_dat <- all_locations$ag_microclim

idx <- which(mech_dat <= exp(-30))

mech_dat[idx] <- exp(-30)

log_mech_dat <- log(mech_dat) |>
  matrix(data = _, ncol = 1)


npa <- nrow(pa_covs)
nbg <- nrow(bg)
npo <- tibble(po = mppdat$po) |>
  unnest(po) |>
  nrow()

#pa.samp <- 1:npa
bg.samp <- (npa + 1):(npa + nbg)
po.samp <- (npa + nbg +1):(npa + npo + nbg)

#area_bg <- sum(!is.na(values(model_layers_mech[[1]])))/nbg
#area_bg <- 825.1676 # this is the above with 30k bg points
area_bg <- 3713.069 # with 6kish points

npospp <- sapply(po_covs, nrow)

x <- all_locations |>
  select(
    cropland,
    footprint,
    lst_day_mean,
    rainfall_mean
  ) |>
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
pa <-pa_filled |>
  select(arabiensis, funestus, coluzzii, gambiae) |>
  as.matrix()

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

# for relabund
# log_lambda ~ unscaled abundance
# need in scaling factor plus random effect of trap type
# per covid model to get size and prob for negbin
# expected_abundance <- exp(log_lambda[abund,] + trap_effect + species_scaling_factor)
#
# sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf), dim = 1)
# size <- 1 / sqrt(sqrt_inv_size)
# prob <- 1 / (1 + expected_abundance / size)
# distribution(abundance) <- negative_binomial(size = size, prob = prob)

# predict for each species lambda + species_scaling_factor
# mod to per relative abundance paper also for output
# i.e. lambda / rowsums(lambda)

# p <- icloglog(log_lambda[pa.samp, ] + log(area_pa))
# distribution(pa) <- bernoulli(p)

p <- icloglog(log_lambda[pa_not_na_idx] + log(area_pa))
distribution(pa[pa_not_na_idx]) <- bernoulli(p)


## compute (biased) expected numbers of presence-only observations across all
## presence and background sites, assuming presence-only count aggregation area
## of 1 (as in multispeciesPP definition). Not that when these are all the same,
## this value only changes all the gamma parameters by a fixed amount, and these
## are not the parameters of interest


area_po <- 1/nbg # 1/10000 # very small
po_idx <- which(po.count == 1, arr.ind = TRUE)
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
  ncv <- 4
  n_a <- nsp
  n_b <- nsp * ncv
  n_g <- nsp

  ina <- -1e-2
  inb <- -1e-1
  ing <- 0 #1e-4
  ind <- 1e-4

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
# p_inits <- calculate(p, values = inits())
#ll_inits <- calculate(log_lambda[pa.samp,], values = inits())

#library(tidyr)
# initplotdatt <- pa |>
#   as_tibble() |>
#   mutate(
#     id = row_number()
#   ) |>
#   pivot_longer(
#     cols = -id,
#     names_to = "sp",
#     values_to = "p"
#   ) |>
#   left_join(
#     y = tibble(
#       arabiensis = p_inits$p[pa_not_na_idx[which(pa_not_na_idx[,2] == 1),]],
#       funestus = p_inits$p[pa_not_na_idx[which(pa_not_na_idx[,2] == 2),]],
#       coluzzii = p_inits$p[pa_not_na_idx[which(pa_not_na_idx[,2] == 3),]],
#       gambiae = p_inits$p[pa_not_na_idx[which(pa_not_na_idx[,2] == 4),]]
#     )  |>
#       mutate(
#         id = row_number()
#       ) |>
#       pivot_longer(
#         cols = -id,
#         names_to = "sp",
#         values_to = "p_init"
#       ),
#     by = c("id", "sp")
#   )
#
# ggplot(initplotdatt) +
#   geom_violin(
#     aes(x = as.factor(p), y = p_init)
#   ) +
#   facet_wrap(~sp)



draws <- mcmc(m, warmup = 500, n_samples = 500, initial_values = inits())

#draws <- mcmc(m, warmup = 1000, n_samples = 3000)

r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
max(r_hats$psrf[, 2])
r_hats

# sux try harder

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


# p_inits <- calculate(p, values = inits_2)
#
# #library(tidyr)
# initplotdatt <- pa |>
#   as_tibble() |>
#   mutate(
#     id = row_number()
#   ) |>
#   pivot_longer(
#     cols = -id,
#     names_to = "sp",
#     values_to = "p"
#   ) |>
#   left_join(
#     y = tibble(
#       arabiensis = p_inits$p[,1],
#       funestus = p_inits$p[,2],
#       coluzzii = p_inits$p[,3],
#       gambiae = p_inits$p[,4]
#     )  |>
#       mutate(
#         id = row_number()
#       ) |>
#       pivot_longer(
#         cols = -id,
#         names_to = "sp",
#         values_to = "p_init"
#       ),
#     by = c("id", "sp")
#   )
#
# library(ggplot2)
# ggplot(initplotdatt) +
#   geom_violin(
#     aes(x = as.factor(p), y = p_init)
#   ) +
#   facet_wrap(~sp)



draws <- mcmc(m, warmup = 5000, n_samples = 5000, initial_values = inits_2)

r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
max(r_hats$psrf[, 2])
r_hats

## predict




## Predictinos

library(sdmtools)
mlm_split <- split_rast(
  model_layers_mech,
  grain = 10,
  write_temp = TRUE
)



predict_greta_sdm <- function(
    r,
    n,
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    alpha,
    beta,
    draws
){

  nspp <- length(spp)
  layer_values <- values(r)
  naidx <- is.na(layer_values[,1])

  ncells <- length(naidx)

  if(ncells == sum(naidx)){

    preds <- rep(r[[1]], times = nspp)
    preds[] <- NA
    names(preds) <- spp

    writeRaster(
      preds,
      filename = sprintf(
        "outputs/predstemp/pred_%s.tif",
        n
      ),
      overwrite = TRUE
    )
    return(print(n))
  }

  x_predict <- layer_values[
    !naidx,
    c(
      "cropland",
      "footprint",
      "lst_day_mean",
      "rainfall_mean"
    )
  ]

  mech_pred <- layer_values[!naidx, "ag_microclim"]

  lowidx <- which(mech_pred <= exp(-30))

  mech_pred[lowidx] <- exp(-30)

  log_mech_pred <- log(mech_pred) |>
    matrix(data = _, ncol = 1)

  log_lambda_adults_predict <- log_mech_pred

  log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")

  log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")

  p_predict <- icloglog(log_lambda_predict + log(area_pa))

  preds <- calculate(p_predict, values = draws, nsim = 100)

  preds_mean <- apply(preds$p_predict, MARGIN = c(2,3), median, na.rm = TRUE)

  preds_rast <- rep(r[[1]], times = nspp)

  for(i in 1:nspp) {
    preds_rast[[i]][!naidx] <- preds_mean[,i]
  }

  names(preds_rast) <- spp

  writeRaster(
    preds_rast,
    filename = sprintf(
      "outputs/predstemp/pred_%s.tif",
      n
    ),
    overwrite = TRUE
  )
  gc()

  print(n)

}

for(i in 26:28){

  predict_greta_sdm(
    mlm_split[[i]],
    n = i,
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    alpha = alpha,
    beta = beta,
    draws = draws
  )
}


predlist <- mapply(
  FUN = predict_greta_sdm,
  r = mlm_split,
  n = 1:length(mlm_split),
  MoreArgs = list(
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    alpha = alpha,
    beta = beta,
    draws = draws
  )
)


# pred#all_layer_values <- values(model_layers_mech)
# all_layer_values <- values(mlm_split[[51]])
# naidx <- is.na(all_layer_values[,1])
#
#
# x_predict <- all_layer_values[
#   !naidx,
#   c(
#     "arid",
#     "built_volume",
#     "cropland",
#     "elevation",
#     "evi_mean",
#     "footprint",
#     "lst_day_mean",
#     "lst_night_mean",
#     "pop",
#     "pressure_mean",
#     "rainfall_mean",
#     "soil_clay",
#     "solrad_mean",
#     "surface_water",
#     "tcb_mean",
#     "tcw_mean",
#     "windspeed_mean",
#     "easting",
#     "northing"
#   )
# ]
#
# mech_pred <- all_layer_values[!naidx, "ag_microclim"]
#
# lowidx <- which(mech_pred <= exp(-30))
#
# mech_pred[lowidx] <- exp(-30)
#
# log_mech_pred <- log(mech_pred) |>
#   matrix(data = _, ncol = 1)
#
# log_lambda_adults_predict <- log_mech_pred
#
# log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")
#
# log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")
#
# p_predict <- icloglog(log_lambda_predict + log(area_pa))
#
# #rm(all_layer_values)
#
# preds <- calculate(p_predict, values = draws, nsim = 100)
#
# preds_mean <- apply(preds$p_predict, MARGIN = c(2,3), median, na.rm = TRUE)
#
# preds_rast <- rep(mlm_split[[51]][[1]], times = 4)
#
#
#
# preds_rast[[1]][!naidx] <- preds_mean[,1]
# preds_rast[[2]][!naidx] <- preds_mean[,2]
# preds_rast[[3]][!naidx] <- preds_mean[,3]
# preds_rast[[4]][!naidx] <- preds_mean[,4]
#
# preds_rast

library(lubridate)
predictions <- predlist() |>
  sprc() |>
  merge(
    filename = sprintf(
      "outputs/preds_all_%s.tif",
      today() |>
        gsub(
          pattern = "_",
          replacement = "",
          x = _
        )
    )
  )

 # add in rel abund data where available as an additional likelihood
# use a multinomial obs model where p is from predicted abundances
#

