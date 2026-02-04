library(targets.utils)
tl()

model_data_spatial <- model_data_spatial |>
  # select(
  #   - evi_mean,
  #   - lst_day_mean
  #   - footprint
  # )
  filter(
    (data_type != "count") |
      (data_type == "count" & count < 1000)
  ) |>
 filter(!(latitude > 14 & latitude < 14.5 & longitude > 38 & longitude < 38.5))
#target_covariate_names <- target_covariate_names[c(1,3)] # works
#target_covariate_names


# index of distinct locations
distinct_idx <- model_data_spatial |>
  mutate(
    rn = row_number(),
    .before = species
  ) |>
  group_by(latitude, longitude) |>
  mutate(
    rnsp = row_number(),
    .before = species
  ) |>
  ungroup() |>
  filter(rnsp == 1) |>
  pull(rn)

distinct_coords <- model_data_spatial[distinct_idx, c("latitude", "longitude")]


# get offset values from gambiae mechanistic model
log_offset <- log(model_data_spatial[distinct_idx,"ag_microclim"])|>
  as.matrix() |>
  as_data()


# get covariate values
x <- model_data_spatial[distinct_idx,] |>
  as_tibble() |>
  select(
    all_of(target_covariate_names)
    #"footprint"
  ) |>
  as.matrix() |>
  as_data()

# get bias values
z <- model_data_spatial[distinct_idx,"research_tt_by_country"] |>
  as.matrix() |>
  as_data()


# number of cells in analysis data per Fithian model (not in raster)
n_pixel <- nrow(x)

# numbers of covariates in use
n_cov_abund <- ncol(x)
n_cov_bias <- ncol(z)

# number of species
n_species_with_bg <- unique(model_data_spatial$species) # includes NA

n_species <- length(n_species_with_bg[!is.na(n_species_with_bg)])


## sampling methods
sm_freq <- table(
  na.omit(model_data_spatial$sampling_method)
) |>
  as.matrix()

sm_prop <- sm_freq/sum(sm_freq)

sampling_methods <- row.names(sm_prop)

n_sampling_methods <- length(sampling_methods)


## impute bg sampling methods and species

model_data_spatial_bg <- model_data_spatial |>
  mutate(
    sampling_method = case_when(
      data_type == "bg" ~ sample(
        x = sampling_methods,
        size = n(),
        replace = TRUE,
        prob = sm_prop
      ),
      .default = sampling_method
    ),
    sampling_method_id = match(
      sampling_method,
      sampling_methods
    )
  )

bg_data <- model_data_spatial_bg |>
  filter(data_type == "bg") |>
  select(-species) |>
  expand_grid(species = target_species) |>
  select(species, everything())

unique_locatenate <- distinct_coords |>
  mutate(locatenate = paste(latitude, longitude)) %>%
  pull(locatenate)

model_data <- bind_rows(
  model_data_spatial_bg |>
    filter(data_type != "bg"),
  bg_data
) |>
  mutate(
    locatenate = paste(latitude, longitude),
    location_id = match(
      x = locatenate,
      table = unique_locatenate
    ),
    species_id = match(
      x = species,
      table = target_species
    )
  ) |>
  select(-locatenate)

# area of background cells

total_area <- expanse(
  project_mask,
  unit = "km"
)$area


n_bg <- model_data_spatial_bg |>
  filter(data_type == "bg") |>
  nrow()

# don't need this with weight
#area_bg <- total_area/n_bg

########### priors

# define parameters with normal priors, matching the ridge regression setup in
# multispeciesPP defaults
# originals
#penalty.l2.intercept <- 1e-4
penalty.l2.sdm <- penalty.l2.bias <- 0.1

# trying others
penalty.l2.intercept <- 1e-2
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
gamma_sd <- 0.1


gamma <- normal(0, gamma_sd, dim = n_species)
delta <- normal(1, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf))

# log rates across all sites
# larval habitat based on env covariates
log_lambda_larval_habitat <- sweep(x %*% beta, 2, alpha, FUN = "+")
# log_lambda_larval_habitat <- sweep(zeros(n_pixel, n_species), 2, alpha, FUN = "+")


# offset from calculated gambiae adult survival given habitat
#log_lambda_adults <- log_offset
log_lambda_adults <- rep(0, times = dim(log_offset)[[1]]) |>
  as_data()

# combine larval habitat and adult life cycle offset
log_lambda <- sweep(log_lambda_larval_habitat, 1, log_lambda_adults, "+")

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

# sampling random effects
# hierarchical decentering
# sampling_re_sd <- normal(0, 1, truncation = c(0, Inf))
# sampling_re_raw <- normal(0, 1, dim = n_sampling_methods)
# sampling_re <- sampling_re_raw * sampling_re_sd


########## indices
# count data index

count_data_index <- model_data |>
  filter(data_type == "count") |>
  select(
    location_id,
    species_id,
    sampling_method_id
  )

count_data_loc_sp_idx <- count_data_index |>
  select(
    location_id,
    species_id
  ) |>
  as.matrix()


# pa index
pa_data_index <- model_data |>
  filter(data_type == "pa") |>
  select(
    location_id,
    species_id,
    sampling_method_id
  )

pa_data_loc_sp_idx <- pa_data_index |>
  select(
    location_id,
    species_id
  ) |>
  as.matrix()

# po / bg index
pobg_data_index <- model_data |>
  filter(data_type %in% c("po", "bg")) |>
  select(
    location_id,
    species_id,
    sampling_method_id
  )

pobg_data_loc_sp_idx <- pobg_data_index |>
  select(location_id, species_id) |>
  as.matrix()


##### likelihood

######################

#### count data likelihood

log_lambda_obs_count <-log_lambda[count_data_loc_sp_idx] #+
#sampling_re[count_data_index$sampling_method_id]

count_data_response <- model_data |>
  filter(data_type == "count") |>
  pull(n) |>
  as_data()

count_data_response_expected <- exp(log_lambda_obs_count)
distribution(count_data_response) <- poisson(count_data_response_expected)

#### PA likelihood

# ADD AN EXTRA INTERCEPT PER SPECIES TO ACCOUNT FOR REPORTING AND SAMPLING
# BIASES
# pa_intercept <- normal(0, 1, dim = n_species)

log_lambda_obs_pa <- log_lambda[pa_data_loc_sp_idx] #+
# pa_intercept[pa_data_index$species_id] #+
#sampling_re[pa_data_index$sampling_method_id]

pa_data_response <- model_data |>
  filter(data_type == "pa") |>
  pull(n) |>
  as_data()

# convert log lambda into a logit probability, to evaluate the pa likelihood in
# a more numerically stable way (see ilogit_stable.R for definition and
# explanation)
# logit_icloglog <- function(eta) {
#   exp_eta <- exp(eta)
#   log1p(-exp(-exp_eta)) + exp_eta
# }

# don't do this, this results in invalid samples for log_lambda_obs_pa >= 3.7
# # pa_data_response_expected <- icloglog(log_lambda_obs_pa)

# do this instead via logit_icloglog
logit_prob_pa <- logit_icloglog(log_lambda_obs_pa)
pa_data_response_expected <- ilogit(logit_prob_pa)
distribution(pa_data_response) <- bernoulli(pa_data_response_expected)

#### PO likelihood

#area_po <- 1 # very small

# get weights from either set as 1 for po or weight from k-means clustering for bg
area_pobg <- model_data |>
  filter(data_type %in% c("po", "bg")) |>
  pull(weight) |>
  as_data()


po_data_response <- model_data |>
  filter(data_type %in% c("po", "bg")) |>
  pull(n) |>
  as_data()

log_bias_obs_pobg <- log_bias[pobg_data_loc_sp_idx]
#
# pp_log_bias_obs_pobg <- calculate(log_bias_obs_pobg, nsim = 1000)

# pp_log_bias_obs_pobg <- calculate(log_bias_obs_pobg, nsim = 1000, values = list(gamma = rep(0, times = 4), delta = 1e-4))

log_lambda_obs_pobg <-log_lambda[pobg_data_loc_sp_idx] #+
#sampling_re[pobg_data_index$sampling_method_id]

po_data_response_expected <-   exp(
  log_lambda_obs_pobg +
    log_bias_obs_pobg +
    log(area_pobg)
)
distribution(po_data_response) <- poisson(po_data_response_expected)

#######################

# define and fit the model by MAP and MCMC
#m <- model(alpha, beta, gamma, delta, sampling_re_raw, sampling_re_sd)
# m <- model(alpha, beta, gamma)
# plot(m)

# ############## simulate from priors and calculate probabilities
# mod <- m
# nsims <- 2000
# nsamples <- 50

# # 1. simulate n times from priors for all parameters (as in calculate)
# variable_nodes <- mod$dag$node_list[mod$dag$node_types == "variable"]
# variable_greta_arrays <- lapply(variable_nodes, greta:::as.greta_array)
# sims <- do.call(greta::calculate, c(variable_greta_arrays, list(nsim = nsims)))
#
# # 2. convert these back to free state values
# inits_list <- lapply(seq_len(nsims),
#                      make_inits,
#                      sims,
#                      variable_greta_arrays)
#
# free_states <- get_free_states(inits_list,
#                                variable_greta_arrays,
#                                mod)
#
#
# library(tensorflow)
# # compute densities and their gradients
# # tf_free_states <- tf$constant(free_states[1, , drop = FALSE])
# tf_free_states <- greta:::tf$constant(free_states)
# log_probs <- mod$dag$tf_log_prob_function_adjusted(tf_free_states)
#
# # determine validity (finite density and grads) and return only the valid free states
# log_probs_np <- log_probs$numpy()
#
# log_probs_np
#
# probs <- exp(log_probs_np)
#
# probs
# probs[which(probs != 0)]

#init_index <- 1:nsims

#sample(init_index, size = nsamples, replace = FALSE, prob = exp(log_probs_np))

m <- model(alpha, beta, gamma, delta)#, sampling_re_raw, sampling_re_sd)
plot(m)


# ############## simulate from priors and calculate probabilities
mod <- m
nsims <- 2000
nsamples <- 50

# 1. simulate n times from priors for all parameters (as in calculate)
variable_nodes <- mod$dag$node_list[mod$dag$node_types == "variable"]
variable_greta_arrays <- lapply(variable_nodes, greta:::as.greta_array)
sims <- do.call(greta::calculate, c(variable_greta_arrays, list(nsim = nsims)))

# 2. convert these back to free state values
inits_list <- lapply(seq_len(nsims),
                     make_inits,
                     sims,
                     variable_greta_arrays)

free_states <- get_free_states(inits_list,
                               variable_greta_arrays,
                               mod)


library(tensorflow)
# compute densities and their gradients
# tf_free_states <- tf$constant(free_states[1, , drop = FALSE])
tf_free_states <- greta:::tf$constant(free_states)
log_probs <- mod$dag$tf_log_prob_function_adjusted(tf_free_states)

# determine validity (finite density and grads) and return only the valid free states
log_probs_np <- log_probs$numpy()

log_probs_np

probs <- exp(log_probs_np)

probs
probs[which(probs != 0)]

init_index <- 1:nsims

sample(init_index, size = nsamples, replace = FALSE, prob = exp(log_probs_np))

# Evaluate likelihoods when simulating from priors, under what prior conditions
# do the likelihood calculations overflow?

n_sims <- 10
sims <- calculate(
  # parameters
  alpha,
  beta,
  gamma,
  delta,

  # predictions
  count_data_response_expected,
  pa_data_response_expected,
  po_data_response_expected,

  # debugging pa
  log_lambda_obs_pa,
  logit_prob_pa,

  values = list(
    beta = matrix(
      data = 0.1,
      ncol = length(target_species),
      nrow = length(target_covariate_names)
     )#,
    # alpha = rep(
    #   0.1,
    #   times = length(target_species)
    # )
  ),

  nsim = n_sims
)

# pa likelihood
# logit_prob_pa <- logit_icloglog(log_lambda_obs_pa)
# pa_data_response_expected <- ilogit(logit_prob_pa)
# distribution(pa_data_response) <- bernoulli(pa_data_response_expected)

softplus <- function(x) {
  log1p(exp(-abs(x))) + pmax(x, 0)
}

# x <- rnorm(100)
# y1 <- softplus(x)
# library(greta)
# y2 <- calculate(log1pe(x))[[1]][, 1]
# max(abs(y2 - y1))


bernoulli_logprob <- function(y, logit_p) {
  ifelse(y == 1,
         -softplus(-logit_p),
         -softplus(logit_p))
}

# p <- runif(10)
# y <- rbinom(10, 1, p)
#
# bernoulli_logprob(y, qlogis(p))
# dbinom(y, 1, p, log = TRUE)




# compute likelihoods for these
for (i in 1:n_sims) {
  lp_count <- sum(dpois(as.numeric(count_data_response),
            sims$count_data_response_expected[i, , 1],
            log = TRUE))
  lp_pa <- sum(bernoulli_logprob(
    as.numeric(pa_data_response),
    logit_p = sims$logit_prob_pa[i, , 1]
  ))
  lp_po <- sum(dpois(as.numeric(po_data_response),
            sims$po_data_response_expected[i, , 1],
            log = TRUE))
  print(
    sprintf("sim %i: count: %s, pa: %s, po: %s, sum %s",
      i,
      lp_count,
      lp_pa,
      lp_po,
      sum(lp_count, lp_pa, lp_po)
      )
  )

  # lp_pa_bern <- bernoulli_logprob(
  #   as.numeric(pa_data_response),
  #   logit_p = sims$logit_prob_pa[i, , 1]
  #   )
  #
  first_inf <- which(!is.finite(lp_pa))[1]
  print(
    sprintf("sim %i: does have %s infinite probs, starting at %s",
            i,
            sum(!is.finite(lp_pa)),
            first_inf
    )
  )

  print(
    sprintf("sim %i: log lambda min: %s at element %s",
            i,
            min(sims$log_lambda_obs_pa[i, , 1]),
            which.min(sims$log_lambda_obs_pa[i, , 1])
    )
  )
  print(
    sprintf("sim %i: log lambda max: %s at element %s",
            i,
            max(sims$log_lambda_obs_pa[i, , 1]),
            which.max(sims$log_lambda_obs_pa[i, , 1])
            )
  )
  print(
   sprintf("sim %i: logit prob pa min: %s at element %s",
            i,
            min(sims$logit_prob_pa[i, , 1]),
            which.min(sims$logit_prob_pa[i, , 1])
    )
  )
  print(
   sprintf("sim %i: logit prob pa max: %s at element %s",
            i,
            max(sims$logit_prob_pa[i, , 1]),
            which.max(sims$logit_prob_pa[i, , 1])
    )
  )

}

# if beta not set then logit_prob_pa min is -Inf
# causing log prob pa -Inf
#
# setting beta to zero
# logit_prob_pa is not -Inf but pa is -Inf
# grrr




########


# fit model
n_burnin <- 500
n_samples <- 100
n_chains <- 50

# init_vals <- generate_valid_inits(
#   mod = m,
#   chains = n_chains
# )
#
#
# optim <- opt(m)
# optim$par
#
# init_vals <- inits(
#   n_chains = n_chains,
#   ncv = dim(x)[2],
#   ina = c(3.1, 0.58, 2.8, 1.8),
#   #ina = optim$par$alpha,
#   #inb = optim$par$beta,
#   #ing = optim$par$gamma,
#   ing = c(-10.5, -10.8, -10.0, -11.7),
#   #ind = optim$par$delta#,
#   ind = 30.9#,
#   #inre = optim$par$sampling_re,
#   #inresd = optim$par$sampling_re_sd
# )
# beta back in - tried footprint and EVI - fuct.
# species-specific offsets from expert maps
# check points outside expert area / offset buffer
# send back to MS/AW

# prior sampling / ppcs using prior simulations
# try to manually shove into right space


# tweak optimiser to converge rapidly and well, to initialise MCMC
optim <- opt(
  m,
  optimiser = adam(learning_rate = 0.5),
  initial_values = initials(
    beta = matrix(
      data = 0.1,
      nrow = length(target_covariate_names),
      ncol = length(target_species)
    ),
    alpha = rep(
      0.1,
      times = length(target_species)
    )
  ),
  max_iterations = 1e5
)

# should be 0 if converged
optim$convergence

# if it gives a numerical error try reducing the learning rate (or just run it
# again)
# if it still doesn't converge, increase the number of iterations
optim$iterations

init_vals <- inits_from_opt(
  optim,
  n_chains = n_chains
)

init_vals <- inits(
  n_chains = n_chains,
  nsp = n_species,
  ncv = length(target_covariate_names),
  inb = 0,
  ina = 0#,
  #ing = 1e-6
)

draws <- greta::mcmc(
  m,
  warmup = n_burnin,
  n_samples = n_samples,
  chains = n_chains,
  initial_values = init_vals
)

coda::gelman.diag(draws, autoburnin = FALSE)
mcmc_trace(draws, regex_pars = "alpha")
mcmc_trace(draws, regex_pars = "beta")
mcmc_trace(draws, regex_pars = c("gamma", "delta"))




########

zplot <- seq(from = 0, to = 1, by = 0.001)

yfun <- function(x)(-9 + 48*x)

yplot <- yfun(zplot)

plot(zplot, yplot)

calculate(po_data_response_expected[1], values = list(alpha = rep(-5,times = 8), beta = rep(0, times = 72), gamma = rep(-6, times = 8), delta = 48))
