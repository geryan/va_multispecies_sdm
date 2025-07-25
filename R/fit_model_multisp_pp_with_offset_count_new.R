fit_model_multisp_pp_with_offset_count_new <- function(
    model_data_spatial,
    target_species,
    image_name = "outputs/images/multisp_pp_with_offset_count_new.RData",
    n_burnin = 50,
    n_samples = 100,
    n_chains = 4
){


  ## get data in order for model

  #for all points (pa, po, bg):


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
      #ag_microclim,
      #research_tt_by_country,
      #arid,
      # built_volume,
      # cropland,
      elevation,
      evi_mean, # correlates with pressure_mean rainfall_mean and solrad_mean
      footprint, # correlates with built_volume and cropland
      lst_day_mean,
      # lst_night_mean,
      # # pressure_mean,
      # # rainfall_mean,
      # soil_clay,
      # # solrad_mean,
      # # surface_water, remove and replace with distance to surface water
      tcb_mean#, # strongly correlates with tcw
      # # tcw_mean,
      # windspeed_mean,
      #easting,
      #northing
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
  n_species_with_bg <- unique(model_data_spatial$species)

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

  model_data_spatial <- model_data_spatial |>
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

  bg_data <- model_data_spatial |>
    filter(data_type == "bg") |>
    select(-species) |>
    expand_grid(species = target_species) |>
    select(species, everything())

  unique_locatenate <- distinct_coords |>
    mutate(locatenate = paste(latitude, longitude)) %>%
    pull(locatenate)

  model_data <- bind_rows(
    model_data_spatial |>
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
    static_vars_agg_mech_nonzero[[1]],
    unit = "km"
  )$area


  n_bg <- model_data_spatial |>
    filter(data_type == "bg") |>
    nrow()

  area_bg <- total_area/n_bg


  # define parameters with normal priors, matching the ridge regression setup in
  # multispeciesPP defaults
  penalty.l2.sdm <- penalty.l2.bias <- 0.1
  penalty.l2.intercept <- 1e-4

  # trying others
  # penalty.l2.sdm <- penalty.l2.bias <- 0.2
  # penalty.l2.intercept <- 1e-2


  intercept_sd <- sqrt(1 / penalty.l2.intercept)
  beta_sd <- sqrt(1 / penalty.l2.sdm)
  #delta_sd <- sqrt(1 / penalty.l2.bias)
  delta_sd <- 1 # will need to alter if >1 sources of bias

  # intercept and shared slope for selection bias
  gamma <- normal(0, intercept_sd, dim = n_species)
  delta <- normal(0, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf)) # constrain to be positive

  # intercept and slopes for abundance rate
  alpha <- normal(0, intercept_sd, dim = n_species)
  beta <- normal(0, beta_sd, dim = c(n_cov_abund, n_species))


  # log rates across all sites
  # larval habitat based on env covariates
  log_lambda_larval_habitat <- sweep(x %*% beta, 2, alpha, FUN = "+")

  # offset from calculated gambiae adult survival given habitat
  log_lambda_adults <- log_offset

  # combine larval habitat and adult life cycle offset
  log_lambda <- sweep(log_lambda_larval_habitat, 1, log_lambda_adults, "+")

  # can easily replace this model with something more interesting, like a low-rank
  # GP on covariate space or something mechanistic

  # bias across pixels (shared coefficient) and species (different intercepts)
  log_bias_coef <- sweep(zeros(n_pixel, n_species), 1, z %*% delta, FUN = "+")
  log_bias <- sweep(log_bias_coef, 2, gamma, FUN = "+")

  # rates across all sites and species
  lambda <- exp(log_lambda)

  # offset stuff
  bias <- exp(log_bias)

  # random effects
  sampling_re_sd <- normal(0, 1, truncation = c(0, Inf))
  sampling_re_raw <- normal(0, 1, dim = n_sampling_methods)
  sampling_re <- sampling_re_raw * sampling_re_sd

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

  ##### define likelihoods

  # count data likelihood

  log_lambda_obs_count <-log_lambda[count_data_loc_sp_idx] +
    sampling_re[count_data_index$sampling_method_id]

  count_data_response <- model_data |>
    filter(data_type == "count") |>
    pull(n)

  distribution(count_data_response) <- poisson(exp(log_lambda_obs_count))

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


  # PA data likelihood

  # compute probability of presence (and detection) at the PA sites, assuming
  # area/effort of 1 in all these sites, for identifiability

  ### can compute separately for each PA PO and RELABUND
  area_pa <- 1
  #area_pa <- uniform(0,1)

  # predict for each species lambda + species_scaling_factor
  # mod to per relative abundance paper also for output
  # i.e. lambda / rowsums(lambda)

  # p <- icloglog(log_lambda[pa.samp, ] + log(area_pa))
  # distribution(pa) <- bernoulli(p)

  # p <- icloglog(log_lambda[model_notna_idx_pa] + log(area_pa))
  # distribution(data_infilled[model_notna_idx_pa]) <- bernoulli(p)

  log_lambda_obs_pa <-log_lambda[pa_data_loc_sp_idx] +
    sampling_re[pa_data_index$sampling_method_id]

  pa_data_response <- model_data |>
    filter(data_type == "pa") |>
    pull(n)

  distribution(pa_data_response) <- bernoulli(icloglog(log_lambda_obs_pa))
  # above technically should have + log(area_pa inside but it's zero)


  # PO and BG likelihoods

  # compute (biased) expected numbers of presence-only observations across all
  # presence and background sites, assuming presence-only count aggregation area
  # of 1 (as in multispeciesPP definition). Not that when these are all the same,
  # this value only changes all the gamma parameters by a fixed amount, and these
  # are not the parameters of interest

  area_po <- 1e-3 # very small

  area_pobg <- model_data |>
    filter(data_type %in% c("po", "bg")) |>
    mutate(area = case_when(data_type == "po" ~ area_po, data_type == "bg" ~ area_bg)) |>
    pull(area)


  po_data_response <- model_data |>
    filter(data_type %in% c("po", "bg")) |>
       pull(n)

  log_bias_obs_pobg <- log_bias[pobg_data_loc_sp_idx]

  log_lambda_obs_pobg <-log_lambda[pobg_data_loc_sp_idx] +
    sampling_re[pobg_data_index$sampling_method_id]

  distribution(po_data_response) <- poisson(
    exp(
      log_lambda_obs_pobg +
        log_bias_obs_pobg +
        log(area_pobg)
    )
  )


  # define and fit the model by MAP and MCMC
  m <- model(alpha, beta, gamma, delta)

  n_burnin <- 5000
  n_samples <- 1000
  n_chains <- 20

  draws <- mcmc(
    m,
    warmup = n_burnin,
    n_samples = n_samples,
    chains = n_chains,
    initial_values = inits(
      n_chains = n_chains,
      nsp = n_species,
      ncv = n_cov_abund
    )
  )

  # can't use save.image inside function inside targets because it only saves
  # global environment not function env.
  save(
    list = ls(all.names = TRUE),
    file = image_name
  )

  print(image_name)

  return(image_name)

}
