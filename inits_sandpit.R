#############

###########


rm(list = ls())

library(targets)
tar_load_globals()
tar_load_everything()

## get data in order for model

model_data_spatial <- model_data_spatial |>
  # select(
  #   - evi_mean,
  #   - lst_day_mean
  #   - footprint
  # )
  filter(count < 1000)
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
  as.matrix()


# get covariate values
x <- model_data_spatial[distinct_idx,] |>
  as_tibble() |>
  select(
    all_of(target_covariate_names)
    #"footprint"
  ) |>
  as.matrix()

# get bias values
z <- model_data_spatial[distinct_idx,"research_tt_by_country"] |>
  as.matrix()


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
penalty.l2.intercept <- 1e-4
penalty.l2.sdm <- penalty.l2.bias <- 0.1

# trying others
# penalty.l2.intercept <- 1e-4
# penalty.l2.sdm <- penalty.l2.bias <- 100

intercept_sd <- sqrt(1 / penalty.l2.intercept)
beta_sd <- sqrt(1 / penalty.l2.sdm)
#delta_sd <- sqrt(1 / penalty.l2.bias)
#delta_sd <- 1 # will need to alter if >1 sources of bias
delta_sd <- 0.3

# intercept and shared slope for selection bias
gamma <- normal(0, intercept_sd, dim = n_species)

# gamma_sd <- 0.1
# gamma <- normal(-3.6, gamma_sd, dim = n_species)

delta <- normal(0, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf)) # constrain to be positive
#delta <- normal(3.8, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf))


# intercept and slopes for abundance rate
alpha <- normal(0, intercept_sd, dim = n_species)
beta <- normal(0, beta_sd, dim = c(n_cov_abund, n_species))


# log rates across all sites
# larval habitat based on env covariates
log_lambda_larval_habitat <- sweep(x %*% beta, 2, alpha, FUN = "+")
# log_lambda_larval_habitat <- sweep(zeros(n_pixel, n_species), 2, alpha, FUN = "+")


# offset from calculated gambiae adult survival given habitat
#log_lambda_adults <- log_offset
log_lambda_adults <- rep(0, times = dim(log_offset)[[1]])

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
sampling_re_sd <- normal(0, 1, truncation = c(0, Inf))
sampling_re_raw <- normal(0, 1, dim = n_sampling_methods)
sampling_re <- sampling_re_raw * sampling_re_sd


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
# count data likelihood

log_lambda_obs_count <-log_lambda[count_data_loc_sp_idx] #+
  #sampling_re[count_data_index$sampling_method_id]

count_data_response <- model_data |>
  filter(data_type == "count") |>
  pull(n)

distribution(count_data_response) <- poisson(exp(log_lambda_obs_count))

# PA likelihood

log_lambda_obs_pa <-log_lambda[pa_data_loc_sp_idx] #+
  #sampling_re[pa_data_index$sampling_method_id]

pa_data_response <- model_data |>
  filter(data_type == "pa") |>
  pull(n)

distribution(pa_data_response) <- bernoulli(icloglog(log_lambda_obs_pa))


## PO likelihood

#area_po <- 1 # very small

# get weights from either set as 1 for po or weight from k-means clustering for bg
area_pobg <- model_data |>
  filter(data_type %in% c("po", "bg")) |>
  pull(weight)


po_data_response <- model_data |>
  filter(data_type %in% c("po", "bg")) |>
  pull(n)

log_bias_obs_pobg <- log_bias[pobg_data_loc_sp_idx]
#
# pp_log_bias_obs_pobg <- calculate(log_bias_obs_pobg, nsim = 1000)

# pp_log_bias_obs_pobg <- calculate(log_bias_obs_pobg, nsim = 1000, values = list(gamma = rep(0, times = 4), delta = 1e-4))

log_lambda_obs_pobg <-log_lambda[pobg_data_loc_sp_idx] #+
  #sampling_re[pobg_data_index$sampling_method_id]

distribution(po_data_response) <- poisson(
  exp(
    log_lambda_obs_pobg +
      log_bias_obs_pobg +
      log(area_pobg)
  )
)
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


######## Prior predictive checks

# hist(count_data_response, breaks = 50)
#
# lloc_list <- calculate(
#   log_lambda_obs_count,
#   nsim = 1
# )
#
# pred_lambda_count <- lloc_list$log_lambda_obs_count |>
#   as.numeric()
#
# pred_count <- sapply(
#   pred_lambda_count,
#   FUN = function(x){
#     rpois(n = 1, lambda = exp(x))
#   }
# )
#
# hist(pred_count, breaks = 50)
#
# plot(log(count_data_response), log(pred_count))

# fit model
n_burnin <- 2000
n_samples <- 1000
n_chains <- 50

# init_vals <- generate_valid_inits(
#   mod = m,
#   chains = n_chains
# )
#
#
optim <- opt(m)
optim$par

init_vals <- inits(
  n_chains = n_chains,
  ncv = dim(x)[2],
  ina = c(3.1, 0.58, 2.8, 1.8),
  #ina = optim$par$alpha,
  #inb = optim$par$beta,
  #ing = optim$par$gamma,
  ing = c(-10.5, -10.8, -10.0, -11.7),
  #ind = optim$par$delta#,
  ind = 30.9#,
  #inre = optim$par$sampling_re,
  #inresd = optim$par$sampling_re_sd
)
# beta back in - tried footprint and EVI - fuct.
# species-specific offsets from expert maps
# check points outside expert area / offset buffer
# send back to MS/AW

# prior sampling / ppcs using prior simulations
# try to manually shove into right space

draws <- mcmc(
  m,
  warmup = n_burnin,
  n_samples = n_samples,
  chains = n_chains#,
  #initial_values = init_vals#,
  #sampler = adaptive_hmc(diag_sd = 1)
)


mcmc_trace(draws)

# plots of data vs each covariate
# swap env space to space-space clustering for bg
# pca on env space of data then predict to covariate layers
#




# lloc_list <- calculate(
#   log_lambda_obs_count,
#   nsim = 1,
#   values = draws
# )
#
# pred_lambda_count <- lloc_list$log_lambda_obs_count |>
#   as.numeric()
#
# pred_count <- sapply(
#   pred_lambda_count,
#   FUN = function(x){
#     rpois(n = 1, lambda = exp(x))
#   }
# )
#
# hist(pred_count, breaks = 50)
#
# plot(pred_count, count_data_response)


# converged estimates for alpha beta gamma model
# count, po/bg, pa
# inits via prior converged estimate for alpha gamma delta
# beta inits zero
# lower beta sd (1.141) from penalty.l2.sdm <- 0.5
#
# 2.5%       25%       50%        75%      97.5%
# alpha[1,1]   3.13005   3.13670   3.14017   3.143615   3.150186
# alpha[2,1]   0.58817   0.60828   0.61920   0.630251   0.650535
# alpha[3,1]   2.84142   2.84828   2.85191   2.855502   2.862446
# alpha[4,1]   1.85669   1.86952   1.87610   1.882664   1.895084
# beta[1,1]   -0.01500  -0.01170  -0.00996  -0.008224  -0.004930
# beta[1,2]   -0.04150  -0.03099  -0.02547  -0.019908  -0.009393
# beta[1,3]   -0.03790  -0.03446  -0.03264  -0.030814  -0.027347
# beta[1,4]   -0.03312  -0.02647  -0.02302  -0.019589  -0.012967
# gamma[1,1]  -9.32882  -9.27808  -9.25138  -9.224810  -9.174528
# gamma[2,1]  -9.73551  -9.59740  -9.52934  -9.460853  -9.335991
# gamma[3,1]  -8.81903  -8.76943  -8.74382  -8.717957  -8.668189
# gamma[4,1] -10.59487 -10.48081 -10.42183 -10.363593 -10.258008
# delta       52.12604  53.06677  53.56655  54.063763  55.010476



# converged estimates for alpha gamma delta model
# with bg / po area
# no offset
# inits via optimisation
#
# alpha[1,1]   3.125 0.003219 4.552e-05      7.231e-05
# alpha[2,1]   0.580 0.009702 1.372e-04      2.131e-04
# alpha[3,1]   2.800 0.003191 4.512e-05      6.916e-05
# alpha[4,1]   1.841 0.006141 8.685e-05      1.545e-04
# gamma[1,1] -10.529 0.029839 4.220e-04      5.786e-04
# gamma[2,1] -10.796 0.102183 1.445e-03      2.420e-03
# gamma[3,1] -10.003 0.028378 4.013e-04      5.337e-04
# gamma[4,1] -11.692 0.083177 1.176e-03      1.886e-03
# delta       30.892 0.462241 6.537e-03      8.401e-03


# converged estimates from count, pa, po model with no bias
# inits generated by greta
# areabg and area po included in bg points
# Mean       SD  Naive SE Time-series SE
# alpha[1,1] -3.2242 0.003618 1.618e-05      7.129e-05
# alpha[2,1] -5.5829 0.009805 4.385e-05      1.778e-04
# alpha[3,1] -3.3594 0.003454 1.545e-05      6.046e-05
# alpha[4,1] -4.7764 0.007250 3.242e-05      2.410e-04
# beta[1,1]  -0.7291 0.003814 1.706e-05      1.893e-04
# beta[1,2]  -0.5030 0.010674 4.774e-05      6.078e-04
# beta[1,3]  -0.5328 0.003682 1.647e-05      1.975e-04
# beta[1,4]  -0.1270 0.006855 3.065e-05      4.323e-04


# converged estimates from count pa po model with only alpha and gamma
# inits from function with alpha = - 4 and beta = 0
# area excluded from po likelihood
# Mean       SD  Naive SE Time-series SE
# alpha[1,1]  3.1248 0.003231 1.445e-05      2.352e-05
# alpha[2,1]  0.5799 0.009929 4.440e-05      5.278e-05
# alpha[3,1]  2.8002 0.003287 1.470e-05      3.271e-05
# alpha[4,1]  1.8412 0.005969 2.669e-05      9.529e-05
# gamma[1,1] -3.5647 0.023874 1.068e-04      1.934e-04
# gamma[2,1] -2.9035 0.096654 4.322e-04      2.378e-03
# gamma[3,1] -3.1733 0.021580 9.651e-05      1.821e-04
# gamma[4,1] -3.8421 0.078160 3.495e-04      1.271e-03

# check po mean abund adds up to alpha and gamma
# model_data_spatial |>
#   mutate(
#     cp = case_when(
#       data_type == "count" ~ count,
#       .default = presence
#     )
#   ) |>
#   group_by(species, data_type) |>
#   summarise(
#     mean_cp = mean(cp),
#     #sumpbgration = (sum(cp)*1e-3)/(1000*area_bg + sum(cp)*1e-3)
#     sumpbgration = sum(cp) / (area_po * sum(cp) + area_bg * n_bg)
#   ) |>
#   ungroup() |>
#   mutate(mcp = ifelse(data_type =="po", sumpbgration, mean_cp)) |>
#   arrange(data_type, species) |>
#   select(-mean_cp, -sumpbgration) |>
#   mutate(lcp = case_when(
#     data_type == "pa" ~ log(-log(1 - mcp)),
#     data_type == "po" ~ log(mcp),
#     data_type == "count" ~ log(mcp)
#   ))
#
# summary(calculate(alpha + gamma, values = draws))

# check points in/outside of offset layer
# model_data_spatial |>
#   mutate(is_low = ifelse(ag_microclim < 0.001, TRUE, FALSE)) |>
#   group_by(data_type, species) |>
#   summarise(
#     p_low = sum(is_low)/n()
#   )
#
#
# agbin <- covariate_rast[["ag_microclim"]]
# agbin[] <- agbin < 0.001
#
# ggplot() +
#   geom_spatraster(data = agbin) +
#   geom_spatvector(
#     data = model_data_spatial|>
#       filter(species =="arabiensis") |>
#       mutate(lon = longitude, lat = latitude) |>
#       as.data.frame() |>
#       select(lon, lat, data_type) |>
#       vect(crs = crs(covariate_rast)),
#     aes(col = data_type)
#   )
#
# arv <- model_data_spatial|>
#   filter(species =="arabiensis") |>
#   mutate(lon = longitude, lat = latitude) |>
#   as.data.frame() |>
#   select(lon, lat, data_type, presence, count) |>
#   vect(crs = crs(covariate_rast))
#
# arvpa <- arv |> filter(data_type == "pa", presence == 1)
# arvpo <- arv |> filter(data_type == "po")
# arvct <- arv |> filter(data_type == "count", count > 0)
#
# par(mfrow = c(2,2))
# plot(agbin)
# plot(agbin)
# plot(arvpo, add = TRUE, col = "grey80")
# plot(agbin)
# plot(arvpa, add = TRUE, col = "grey80")
# plot(agbin)
# plot(arvct, add = TRUE, col = "grey80")
