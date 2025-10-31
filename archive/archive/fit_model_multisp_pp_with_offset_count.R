fit_model_multisp_pp_with_offset_count <- function(
    model_data_ragged,
    spatial_values,
    model_notna_idx_pa,
    model_notna_idx_po,
    image_name = "outputs/images/multisp_pp_with_offset_count.RData",
    n_burnin = 50,
    n_samples = 100,
    n_chains = 4
){


  ## get data in order for model

  #for all points (pa, po, bg):

  # get offset values from gambiae mechanistic model
  log_offset <- log(spatial_values$ag_microclim)

  # get covariate values
  x <- spatial_values |>
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
      tcb_mean, # strongly correlates with tcw
      # # tcw_mean,
      # windspeed_mean,
      easting,
      northing
    ) |>
    as.matrix()

  # get bias values
  z <- spatial_values[,"research_tt_by_country"]


  # get pa data matrix and infill NAs with zeroes
  # these will be ignored in model because of indexing with
  # model_notna_idx_pa and model_notna_idx_po
  pa_infilled <- model_data_ragged |>
    #filter(type == "pa") |>
    select(
      -lon,
      -lat,
      -type
    ) |>
    # do not convert counts to ones but infill zeroes
    mutate(
      # across(
      #   everything(),
      #   function(x){ifelse(x > 0, 1, 0)}
      # ),
      across(
        everything(),
        function(x){ifelse(is.na(x), 0, x)}
      )
    ) |>
    as.matrix()

  # number of cells in analysis data per Fithian model (not in raster)
  n.pixel <- nrow(x)

  # numbers of covariates in use
  n_cov_abund <- ncol(x)
  n_cov_bias <- ncol(z)

  # number of species
  n_species <- ncol(pa_infilled)

  # index of background data in x
  bg_idx <- (nrow(model_data_ragged) + 1):nrow(spatial_values)
  # number of background data points
  nbg <- length(bg_idx)
  # create infilled bg matrix of zeroes for bg data
  bg_infilled <- matrix(
    data = 0,
    nrow = nbg,
    ncol = n_species
  )

  # area of background cells
  area_bg <- 825.1676 # this is with 30k bg points

  # create orthogonal dataset of ones and zeroes
  # again, unsampled sites will be ignored by indexing in model
  data_infilled <- rbind(
    pa_infilled,
    bg_infilled
  )


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

  # p <- icloglog(log_lambda[model_notna_idx_pa] + log(area_pa))
  # distribution(data_infilled[model_notna_idx_pa]) <- bernoulli(p)

  pa_rate <- exp(log_lambda[model_notna_idx_pa] + log(area_pa))
  distribution(data_infilled[model_notna_idx_pa]) <- poisson(pa_rate)


  ## compute (biased) expected numbers of presence-only observations across all
  ## presence and background sites, assuming presence-only count aggregation area
  ## of 1 (as in multispeciesPP definition). Not that when these are all the same,
  ## this value only changes all the gamma parameters by a fixed amount, and these
  ## are not the parameters of interest


  area_po <- 1/nbg # very small
  po_rate_po <- exp(log_lambda[model_notna_idx_po] + log_bias[model_notna_idx_po] + log(area_po))
  distribution(data_infilled[model_notna_idx_po]) <- poisson(po_rate_po)

  # area_bg <- 3654.515 <- whole area / nnbg
  po_rate_bg <- exp(log_lambda[bg_idx, ] + log_bias[bg_idx,] + log(area_bg))
  distribution(data_infilled[bg_idx, ]) <- poisson(po_rate_bg)


  # define and fit the model by MAP and MCMC
  m <- model(alpha, beta, gamma, delta)

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
