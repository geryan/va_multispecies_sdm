### PPCs with pca covs

rm(list = ls())

library(targets)
tar_load_globals()
tar_load_everything()

## get data in order for model

# model_data_spatial <- model_data_spatial |>
#   select(
#     - evi_mean,
#     - lst_day_mean
#     - footprint
#   )
# target_covariate_names <- target_covariate_names[1:2]

model_data_spatial_pca <- model_data_spatial_pca |>
  select(
    - names(pca_covariate_layers)[2:4]
  )

# index of distinct locations
distinct_idx <- model_data_spatial_pca |>
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

distinct_coords <- model_data_spatial_pca[distinct_idx, c("latitude", "longitude")]


# get offset values from gambiae mechanistic model
log_offset <- log(model_data_spatial_pca[distinct_idx,"ag_microclim"])|>
  as.matrix() |>
  as_data()


# get covariate values
x <- model_data_spatial_pca[distinct_idx,] |>
  as_tibble() |>
  select(
    starts_with("covariate_")
  ) |>
  as.matrix() |>
  as_data()

# get bias values
z <- model_data_spatial_pca[distinct_idx,"research_tt_by_country"] |>
  as.matrix() |>
  as_data()


# number of cells in analysis data per Fithian model (not in raster)
n_pixel <- nrow(x)

# numbers of covariates in use
n_cov_abund <- ncol(x)
n_cov_bias <- ncol(z)

# number of species
n_species_with_bg <- unique(model_data_spatial_pca$species) # includes NA

n_species <- length(n_species_with_bg[!is.na(n_species_with_bg)])


## sampling methods
sm_freq <- table(
  na.omit(model_data_spatial_pca$sampling_method)
) |>
  as.matrix()

sm_prop <- sm_freq/sum(sm_freq)

sampling_methods <- row.names(sm_prop)

n_sampling_methods <- length(sampling_methods)


## impute bg sampling methods and species

model_data_spatial_bg <- model_data_spatial_pca |>
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

###############################################################################
########### priors

# define parameters with normal priors, matching the ridge regression setup in
# multispeciesPP defaults
# originals
#penalty.l2.intercept <- 1e-4
#penalty.l2.sdm <- penalty.l2.bias <- 0.1

# trying others
# penalty.l2.intercept <- 1
# penalty.l2.sdm <- penalty.l2.bias <- 0.5
#
# intercept_sd <- sqrt(1 / penalty.l2.intercept)
# beta_sd <- sqrt(1 / penalty.l2.sdm)
# #delta_sd <- sqrt(1 / penalty.l2.bias)
# delta_sd <- 5 # will need to alter if >1 sources of bias
#
# # intercept and shared slope for selection bias
# gamma <- normal(0, intercept_sd, dim = n_species)
# delta <- normal(0, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf)) # constrain to be positive
#
# # intercept and slopes for abundance rate
# alpha <- normal(0, intercept_sd, dim = n_species)
# beta <- normal(0, beta_sd, dim = c(n_cov_abund, n_species))

alpha_sd <- 1
beta_sd  <- 1
gamma_sd <- 1
delta_sd <- 1

alpha_mean <- -1
beta_mean  <- 0
delta_mean <- 0
gamma_mean <- 0


# intercept and shared slope for selection bias
gamma <- normal(gamma_mean, gamma_sd, dim = n_species)
delta <- normal(delta_mean, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf)) # constrain to be positive

# intercept and slopes for abundance rate
alpha <- normal(alpha_mean, alpha_sd, dim = n_species)
beta <- normal(beta_mean, beta_sd, dim = c(n_cov_abund, n_species))


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
zero_pixels <- zeros(n_pixel, n_species)
log_bias_coef <- sweep(zero_pixels, 1, log(z) %*% delta, FUN = "+")
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
  pull(n) |>
  as_data()

distribution(count_data_response) <- poisson(exp(log_lambda_obs_count))

# PA likelihood

log_lambda_obs_pa <-log_lambda[pa_data_loc_sp_idx] #+
#sampling_re[pa_data_index$sampling_method_id]

pa_data_response <- model_data |>
  filter(data_type == "pa") |>
  pull(n) |>
  as_data()

distribution(pa_data_response) <- bernoulli(icloglog(log_lambda_obs_pa))


## PO likelihood

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


log_lambda_obs_pobg <-log_lambda[pobg_data_loc_sp_idx] #+
#sampling_re[pobg_data_index$sampling_method_id]

# distribution(po_data_response) <- poisson(
#   exp(
#     log_lambda_obs_pobg +
#       log_bias_obs_pobg +
#       log(area_pobg)
#   )
# )

distribution(pa_data_response) <- bernoilli(
  icloglog(
    log_lambda_obs_pobg +
      log_bias_obs_pobg +
      log(area_pobg)
  )
)


#######################


m <- model(alpha, beta, gamma, delta)#, sampling_re_raw, sampling_re_sd)
plot(m)


######## Prior predictive checks ##########################################

color_scheme_set("green")

# simulate data from prior
prior_preds <- calculate(
  po_data_response,
  pa_data_response,
  count_data_response,
  nsim = 250
)

# convert list to vectors / matrices
po_dat <- po_data_response |>
  as.numeric()

po_pred <- prior_preds$po_data_response[,,1] |>
  as.matrix()

pa_dat <- pa_data_response |>
  as.numeric()

pa_pred <- prior_preds$pa_data_response[,,1] |>
  as.matrix()

count_dat <- count_data_response |>
  as.numeric()

count_pred <- prior_preds$count_data_response[,,1] |>
  as.matrix()


###### PO checks ########
ppc_dens_overlay(
  y = po_dat,
  yrep = po_pred
)

ppc_dens_overlay(
  y = po_dat,
  yrep = po_pred
) +
  xlim(0, 10)

ppc_ecdf_overlay(
  y = po_dat,
  yrep = po_pred
) +
  xlim(0, 10)


ppc_stat(
  y = po_dat,
  yrep = po_pred,
  stat = function(x){mean(x == 0)},
  bins = 50
) +
  xlim(0, 1)


# these are mostly pointless for PO because of the long extended upper tail
# but would be worthwhile once prior is a little better
# ppc_bars(
#   y = pa_dat,
#   yrep = pa_pred
# )
#
# ppc_rootogram(
#   y = pa_dat,
#   yrep = pa_pred,
#   prob = 0.9,
#   style = "standing"
# )


###### PA checks ########

ppc_bars(
  y = pa_dat,
  yrep = pa_pred
)

ppc_rootogram(
  y = pa_dat,
  yrep = pa_pred,
  prob = 0.9,
  style = "standing"
)

ppc_stat(
  y = pa_dat,
  yrep = pa_pred,
  stat = function(x){mean(x == 0)},
  bins = 50
) +
  xlim(0, 1)



###### Count checks ########
ppc_dens_overlay(
  y = count_dat,
  yrep = count_pred
)

ppc_dens_overlay(
  y = count_dat,
  yrep = count_pred
) +
  xlim(0, 20)

ppc_ecdf_overlay(
  y = count_dat,
  yrep = count_pred
) +
  xlim(0, 20)


ppc_stat(
  y = count_dat,
  yrep = count_pred,
  stat = function(x){mean(x == 0)},
  bins = 50
) +
  xlim(0, 1)

# these are mostly pointless for Count because of the long extended upper tail
# but would be worthwhile once prior is a little better
# ppc_bars(
#   y = pa_dat,
#   yrep = pa_pred
# )
#
# ppc_rootogram(
#   y = pa_dat,
#   yrep = pa_pred,
#   prob = 0.9,
#   style = "standing"
# )

## model
#
# n_burnin <- 500
# n_samples <- 100
# n_chains <- 50
#
# draws <- mcmc(
#   m,
#   warmup = n_burnin,
#   n_samples = n_samples,
#   chains = n_chains
# )
