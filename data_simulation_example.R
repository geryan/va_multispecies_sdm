# simlulate data for this model and try to recover it

library(targets.utils)
tl()
library(terra)

set.seed(1066)

# raster of covariate values
covariate_rast

r <- covariate_rast[[target_covariate_names]]

global(r, mean, na.rm = TRUE)
global(r, range, na.rm = TRUE)


# make up some coeffs for intercepts and slopes for three species
alphas <- c(-2, -3, -3.5)

betas <- matrix(
  data = c(
    rep(0.2, 6),
    rep(0.6, 6),
    rep(-0.9, 6)
  ),
  ncol = 3
)

# simulate intensity surfaces for three species
simulate_intensity <- function(r, alpha, beta){
  exp(
    alpha +
      r[[1]] * beta[1] +
      r[[2]] * beta[2] +
      r[[3]] * beta[3] +
      r[[4]] * beta[4] +
      r[[5]] * beta[5] +
      r[[6]] * beta[6]
  )
}

sc1 <- simulate_intensity(r, alphas[1], betas[,1])
sc2 <- simulate_intensity(r, alphas[2], betas[,2])
sc3 <- simulate_intensity(r, alphas[3], betas[,3])

intensity_rast <- c(sc1, sc2, sc3) |>
  sdmtools::set_layer_names(layernames = c("sp1", "sp2", "sp3"))


# plot intensity
plot(intensity_rast)
intensity_rast

# plot as p 1 or more
inverse_cloglog <-function(loglambda){
  1 - exp(-exp(loglambda))
}

plot(inverse_cloglog(log(intensity_rast)))

# and plot together
plot(
  c(
    intensity_rast,
    inverse_cloglog(log(intensity_rast))
  )
)


# simulate bias
# get bias correlate from distance from taveltime
bias_corr <- covariate_rast[["research_tt_by_country"]] |>
  sdmtools::set_layer_names(layernames = "bias")

# just scale it directly with the layer
gammas <- c(-0.01, 0, 0.01)
delta <- 0.99

# log bias
spp_log_bias <- delta*log(bias_corr) + gammas

# effective bias
spp_eff_bias <- exp(spp_log_bias)  |>
  sdmtools::set_layer_names(layernames = c("sp1", "sp2", "sp3"))
plot(spp_eff_bias)


# sample data using bias as weighting

# get samples of intensity in each cell
not_na_idx <- which(!is.na(values(r[[1]])))
data_sample_cells <- sample(
  x = not_na_idx,
  size = 900,
  prob = values(bias_corr)[not_na_idx]
)

bg_sample_cells  <- sample(
  x = not_na_idx,
  size = 100
)

sample_cells <- c(data_sample_cells, rep(bg_sample_cells, times = 3))

intensitiy_samples <- values(intensity_rast)[sample_cells,]|>
  as_tibble() |>
  mutate(n = row_number()) |>
  pivot_longer(
    cols = starts_with("sp"),
    names_to = "species",
    values_to = "intensity"
  )

# also extract the values of bias at those cells
bias_samples <- values(spp_eff_bias)[sample_cells,]|>
  as_tibble() |>
  mutate(n = row_number()) |>
  pivot_longer(
    cols = starts_with("sp"),
    names_to = "species",
    values_to = "bias"
  )

# also extract the covariate values at those cells
cov_values <- values(r)[sample_cells,] |>
  as_tibble()

# match these intensities and simulate data from intensity
area_po <- 1
area_bg <- 1e5



library(purrr)
initial_sim_data <- expand_grid(
  type = c("count", "pa", "po", "bg"),
  species = c("sp1", "sp2", "sp3"),
  n = 1:100
) |>
  mutate(
    n = row_number()
  ) |>
  left_join(
    y = intensitiy_samples,
    by = c("n", "species")
  ) |>
  left_join(
    y = bias_samples,
    by = c("n", "species")
  ) |>
  mutate(
    area = ifelse(type == "bg", area_bg, area_po)
  ) |>
  rowwise() |>
  mutate(
    data_value = pmap(
      .l = list(
        intensity,
        type,
        bias,
        area
      ),
      .f = function(intensity, type, bias, area){

        if(type == "count"){
          rpois(1, intensity)
        } else if (type == "pa") {
          rbernoulli(
            n = 1,
            p = inverse_cloglog(log(intensity))
          )
        } else if(type == "po"){
          rpois(
            n = 1,
            lambda = exp(
              log(intensity) +
                log(bias) +
                log(area)
            )
          )
        } else if(type == "bg"){
          0
        }
      }
    ) |>
      unlist()
  ) |>
  ungroup() |>
  bind_cols(
    cov_values
  ) |>
  filter(
    !((type == "po") & (data_value == 0))
  )
# po > 1 get rid of or leave?

table(
  initial_sim_data$data_value,
  initial_sim_data$species,
  initial_sim_data$type
)


#### extra presence_only samples

extra_sample_cells <- sample(
  x = not_na_idx,
  size = 3000,
  prob = values(bias_corr)[not_na_idx]
)


extra_intensitiy_samples <- values(intensity_rast)[extra_sample_cells,]|>
  as_tibble() |>
  mutate(n = row_number()) |>
  pivot_longer(
    cols = starts_with("sp"),
    names_to = "species",
    values_to = "intensity"
  )

# also extract the values of bias at those cells
extra_bias_samples <- values(spp_eff_bias)[extra_sample_cells,]|>
  as_tibble() |>
  mutate(n = row_number()) |>
  pivot_longer(
    cols = starts_with("sp"),
    names_to = "species",
    values_to = "bias"
  )

# also extract the covariate values at those cells
cov_values <- values(r)[extra_sample_cells,] |>
  as_tibble()


extra_po_samples <- expand_grid(
  type = "po",
  species = c("sp1", "sp2", "sp3"),
  n = 1:1000
) |>
  mutate(
    n = row_number()
  ) |>
  left_join(
    y = extra_intensitiy_samples,
    by = c("n", "species")
  ) |>
  left_join(
    y = extra_bias_samples,
    by = c("n", "species")
  ) |>
  mutate(
    area = ifelse(type == "bg", area_bg, area_po)
  ) |>
  rowwise() |>
  mutate(
    data_value = pmap(
      .l = list(
        intensity,
        type,
        bias,
        area
      ),
      .f = function(intensity, type, bias, area){

        if(type == "count"){
          rpois(1, intensity)
        } else if (type == "pa") {
          rbernoulli(
            n = 1,
            p = inverse_cloglog(log(intensity))
          )
        } else if(type == "po"){
          rpois(
            n = 1,
            lambda = exp(
              log(intensity) +
                log(bias) +
                log(area)
            )
          )
        } else if(type == "bg"){
          0
        }
      }
    ) |>
      unlist()
  ) |>
  ungroup() |>
  bind_cols(
    cov_values
  ) |>
  filter(
    !((type == "po") & (data_value == 0))
  )
# po > 1 get rid of or leave?

table(
  extra_po_samples$data_value,
  extra_po_samples$species,
  extra_po_samples$type
)


# combine_them
sim_data <- bind_rows(
  initial_sim_data,
  extra_po_samples
) |>
  mutate(
    n = row_number(),
    species = sub(
      "sp",
      "",
      species
    ) |>
      as.numeric()
  )

table(
  sim_data$data_value,
  sim_data$species,
  sim_data$type
)

table(
  sim_data$species,
  sim_data$type
)


#################################################
# model

# get covariate values
x <- sim_data |>
  select(
    all_of(target_covariate_names)
  ) |>
  as.matrix() |>
  as_data()

# get bias values
z <- sim_data[,"bias"] |>
  as.matrix() |>
  as_data()


# number of cells in analysis data per Fithian model (not in raster)
n_pixel <- nrow(x)

# numbers of covariates in use
n_cov_abund <- ncol(x)
n_cov_bias <- ncol(z)

# number of species
n_species <- length(unique(sim_data$species))

########### priors

# define parameters with normal priors, matching the ridge regression setup in
# multispeciesPP defaults
# originals
penalty.l2.intercept <- 1e-4
penalty.l2.sdm <- penalty.l2.bias <- 0.1

# trying others
#penalty.l2.intercept <- 1e-2
# penalty.l2.sdm <- penalty.l2.bias <- 100

intercept_sd <- sqrt(1 / penalty.l2.intercept)
beta_sd <- sqrt(1 / penalty.l2.sdm)

# delta_sd <- sqrt(1 / penalty.l2.bias)
# delta <- normal(0, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf)) # constrain to be positive
#
# # intercept and shared slope for selection bias
# gamma <- normal(0, intercept_sd, dim = n_species)

# intercept and slopes for abundance rate
alpha <- normal(0, intercept_sd, dim = n_species)
beta <- normal(0, beta_sd, dim = c(n_cov_abund, n_species))

# informative priors on gamma and delta so exp(log_bias), i.e., bias,
# has range around (0, 1) for z in (0, 1)
# these work really well
# delta_sd <- 0.3
# gamma_sd <- 0.1

delta_sd <- 0.5
gamma_sd <- 0.5


gamma <- normal(0, gamma_sd, dim = n_species)
delta <- normal(0, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf))

# log rates across all sites
# larval habitat based on env covariates
log_lambda_larval_habitat <- sweep(x %*% beta, 2, alpha, FUN = "+")
# log_lambda_larval_habitat <- sweep(zeros(n_pixel, n_species), 2, alpha, FUN = "+")


# offset from calculated gambiae adult survival given habitat
#log_lambda_adults <- log_offset
log_lambda_adults <- rep(0, times = 1) |>
  as_data()

# combine larval habitat and adult life cycle offset
#log_lambda <- sweep(log_lambda_larval_habitat, 1, log_lambda_adults, "+")

log_lambda <- log_lambda_larval_habitat

# can easily replace this model with something more interesting, like a low-rank
# GP on covariate space or something mechanistic

# bias across pixels (shared coefficient) and species (different intercepts)
log_bias_coef <- sweep(zeros(n_pixel, n_species), 1, log(z) %*% delta, FUN = "+")
# log_bias_coef <- zeros(n_pixel, n_species)
log_bias <- sweep(log_bias_coef, 2, gamma, FUN = "+")

# rates across all sites and species
lambda <- exp(log_lambda)

# offset stuff
bias <- exp(log_bias)


########## indices
# count data index

count_data_loc_sp_idx <- sim_data |>
  filter(type == "count") |>
  select(
    location_id = n,
    species_id = species,
  ) |>
  as.matrix()


# pa index
pa_data_loc_sp_idx <- sim_data |>
  filter(type == "pa") |>
  select(
    location_id = n,
    species_id = species,
  ) |>
  as.matrix()

# po / bg index
pobg_data_loc_sp_idx <- sim_data |>
  filter(type %in% c("po", "bg")) |>
  select(
    location_id = n,
    species_id = species,
  ) |>
  as.matrix()

##### likelihood

######################

#### count data likelihood

log_lambda_obs_count <-log_lambda[count_data_loc_sp_idx] #+
#sampling_re[count_data_index$sampling_method_id]

count_data_response <- sim_data |>
  filter(type == "count") |>
  pull(data_value) |>
  as_data()

distribution(count_data_response) <- poisson(exp(log_lambda_obs_count))

#### PA likelihood

log_lambda_obs_pa <-log_lambda[pa_data_loc_sp_idx] #+
#sampling_re[pa_data_index$sampling_method_id]

pa_data_response <- sim_data |>
  filter(type == "pa") |>
  pull(data_value) |>
  as_data()

distribution(pa_data_response) <- bernoulli(icloglog(log_lambda_obs_pa))


#### PO likelihood

#area_po <- 1 # very small

# get weights from either set as 1 for po or weight from k-means clustering for bg
area_pobg <- sim_data |>
  filter(type %in% c("po", "bg")) |>
  pull(area) |>
  as_data()


po_data_response <- sim_data |>
  filter(type %in% c("po", "bg")) |>
  pull(data_value) |>
  as_data()

log_bias_obs_pobg <- log_bias[pobg_data_loc_sp_idx]

log_lambda_obs_pobg <-log_lambda[pobg_data_loc_sp_idx]

distribution(po_data_response) <- poisson(
  exp(
    log_lambda_obs_pobg +
      log_bias_obs_pobg +
      log(area_pobg)
  )
)


## model

m <- model(alpha, beta, gamma, delta)
plot(m)


############
# prior predictive checks
############

# prepare data - it's in greta format so get it out

# convert list to vectors / matrices
po_dat <- po_data_response |>
  as.numeric()
pa_dat <- pa_data_response |>
  as.numeric()
count_dat <- count_data_response |>
  as.numeric()

# make list for passing to checks
dat <- list(
  pa_dat = pa_dat,
  po_dat = po_dat,
  count_dat = count_dat
)

# simulate data from prior
prior_preds_calc <- calculate(
  po_data_response,
  pa_data_response,
  count_data_response,
  nsim = 100
)

# convert into matrices
po_pred_prior <- prior_preds_calc$po_data_response[,,1] |>
  as.matrix()
pa_pred_prior <- prior_preds_calc$pa_data_response[,,1] |>
  as.matrix()
count_pred_prior <- prior_preds_calc$count_data_response[,,1] |>
  as.matrix()

# make list for passing to checks
preds_prior <- list(
  po_pred = po_pred_prior |>
    as.matrix(),
  pa_pred = pa_pred_prior |>
    as.matrix(),
  count_pred = count_pred_prior |>
    as.matrix()
)

# pass data to checks
predictive_checks(
  preds = preds_prior,
  dat = dat,
  output_prefix = "outputs/figures/ppc_sim/sim_prior"
)




###################
# fit model
###################



n_burnin <- 2000
n_samples <- 1000
n_chains <- 50

optim <- opt(m)
optim$par

# initialising on optimum drops initial % bad samples
init_vals <- inits_from_opt(
  optim,
  n_chains = n_chains
)

draws <- greta::mcmc(
  m,
  warmup = n_burnin,
  n_samples = n_samples,
  chains = n_chains,
  initial_values = init_vals,
  #sampler = adaptive_hmc(diag_sd = 1)
)

coda::gelman.diag(draws, autoburnin = FALSE)

summary(draws)

mcmc_trace(
  x = draws,
  regex_pars = "alpha"
)

mcmc_trace(
  x = draws,
  regex_pars = c("delta", "gamma")
)


############
# posterior predictive checks
############

# simulate data from posterior
posterior_calc <- calculate(
  po_data_response,
  pa_data_response,
  count_data_response,
  values = draws,
  nsim = 100
)

# convert into matrices
po_pred_post <- posterior_calc$po_data_response[,,1] |>
  as.matrix()
pa_pred_post <- posterior_calc$pa_data_response[,,1] |>
  as.matrix()
count_pred_post <- posterior_calc$count_data_response[,,1] |>
  as.matrix()

# make list for passing to checks
preds_posterior <- list(
  po_pred = po_pred_post |>
    as.matrix(),
  pa_pred = pa_pred_post |>
    as.matrix(),
  count_pred = count_pred_post |>
    as.matrix()
)

# pass data to checks
predictive_checks(
  preds = preds_posterior,
  dat = dat,
  output_prefix = "outputs/figures/ppc_sim/sim_posterior"
)


####### spatial predictions
prednames <- target_covariate_names

r <- covariate_rast

layer_values <- values(r)
naidx <- is.na(layer_values[,1])

x_predict <- layer_values[!naidx, prednames]

# offset_pred <- layer_values[!naidx, "ag_microclim"]
#
# log_offset_pred <- log(offset_pred)
#
# log_lambda_adults_predict <- log_offset_pred

log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")


#log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")
log_lambda_predict <-log_lambda_larval_habitat_predict

pa_rate_predict <- icloglog(log_lambda_predict)

count_rate_predict <- exp(log_lambda_predict)

# run sims for each cell and take the median


# pa
preds_pa <- calculate(
  pa_rate_predict,
  values = draws,
  nsim = 50
)

preds_mean_pa <- apply(
  preds_pa$pa_rate_predict,
  MARGIN = c(2,3),
  median,
  na.rm = TRUE
)

# rasterise predictions and save them
# pa
preds_rast_pa <- rep(r[[1]], times = n_species)
names(preds_rast_pa) <- c("sp1", "sp2", "sp3")
for(i in 1:n_species) {
  preds_rast_pa[[i]][!naidx] <- preds_mean_pa[,i]
}

plot(preds_rast_pa)



# count
preds_count <- calculate(
  count_rate_predict,
  values = draws,
  nsim = 50
)

preds_mean_count <- apply(
  preds_count$count_rate_predict,
  MARGIN = c(2,3),
  median,
  na.rm = TRUE
)

# rasterise predictions and save them
# count
preds_rast_count <- rep(r[[1]], times = n_species)
names(preds_rast_count) <- c("sp1", "sp2", "sp3")
for(i in 1:n_species) {
  preds_rast_count[[i]][!naidx] <- preds_mean_count[,i]
}

plot(preds_rast_count)


plot()




