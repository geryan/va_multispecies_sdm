fit_model_multisp_pp_count_sm <- function(
    model_data_spatial,
    target_covariate_names,
    target_species,
    project_mask,
    image_name = "outputs/images/multisp_pp_count_sm.RData",
    n_burnin = 50,
    n_samples = 100,
    n_chains = 4
){


  model_data_spatial <- model_data_spatial |>
    filter(
      (data_type != "count") |
        (data_type == "count" & count < 1000)
    )

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
  log_offset <- log(model_data_spatial[distinct_idx,"offset"])|>
    as.matrix() |>
    as_data()

  # get covariate values
  x <- model_data_spatial[distinct_idx,] |>
    as_tibble() |>
    select(
      all_of(target_covariate_names)
    ) |>
    as.matrix() |>
    as_data()

  # get bias values
  z <- model_data_spatial[distinct_idx,"travel_time"] |>
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
  penalty.l2.intercept <- 1e-4
  penalty.l2.sdm <- penalty.l2.bias <- 0.1

  # trying others
  penalty.l2.intercept <- 1e-2
  # penalty.l2.sdm <- penalty.l2.bias <- 0.01

  intercept_sd <- sqrt(1 / penalty.l2.intercept)
  beta_sd <- sqrt(1 / penalty.l2.sdm)

  # delta_sd <- sqrt(1 / penalty.l2.bias)
  # delta <- normal(0, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf)) # constrain to be positive
  #
  # # intercept and shared slope for selection bias
  # gamma <- normal(0, intercept_sd, dim = n_species)

  # intercept and slopes for abundance rate
  alpha <- normal(0, intercept_sd, dim = n_species)

  # beta <- normal(0,
  #                beta_sd,
  #                dim = c(n_cov_abund, n_species),
  #                truncation = c(0, Inf))

  # for each covariate, model beta as a positive-constrained spatially-varying
  # covariate. Positive so that abundance is forced to be higher in
  # non-bare-gound landcover types, spatially varying to account for: a)
  # differences in definition of landcover types in different parts of Africa
  # (e.g. 'grassland in the Sahel is likely to be very different from in the
  # southern Congo basin), b) differences in species phenotypes across Africa,
  # and c) residual spatial autocorrelation in distributions (e.g. due to
  # dispersal limitation in the coastal species)

  # # use greta.gp to define independent 2D GPs for spatial variation for each
  # # species/covariate combination

  # transform distinct coordinates to a projected coordinate reference system: equal earth
  # projection
  distinct_coords_proj <- distinct_coords |>
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = sf::st_crs(4326)
    ) |>
    sf::st_transform(
      crs("+proj=eqearth")
    ) |>
    sf::st_coordinates() |>
    as_tibble()

  # do the same with background points, to use for defining knots
  bg_coords_proj <- bg_data |>
    distinct(
      longitude,
      latitude
    ) |>
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = sf::st_crs(4326)
    ) |>
    sf::st_transform(
      crs("+proj=eqearth")
    ) |>
    sf::st_coordinates() |>
    as_tibble()

  # define some knots to do a reduced-rank GP
  n_inducing <- 50
  kmn <- kmeans(bg_coords_proj, centers = n_inducing)
  inducing_points_proj <- kmn$centers
  # plot(inducing_points_proj, asp = 1)
  # points(bg_coords_proj, pch = ".")

  # max_distance <- max(range(distinct_coords_proj))
  # # GP lengthscale, in meters
  # lower <- epiwave.mapping::matern_lengthscale_from_correl(
  #   max_distance / 10,
  #   correlation = 0.1,
  #   nu = 0.5
  # )
  # upper <- epiwave.mapping::matern_lengthscale_from_correl(
  #   max_distance / 2,
  #   correlation = 0.1,
  #   nu = 0.5
  # )
  # gamma_prior_params <- epiwave.mapping::find_gamma_parameters(
  #   upper_value = upper,
  #   upper_prob = 0.1,
  #   lower_value = lower,
  #   lower_prob = 0.1
  # )
  gamma_prior_params <- list(
    shape = 3,
    rate = 5e-6
  )

  # define a spatial kernel
  lengthscale <- gamma(gamma_prior_params$shape, gamma_prior_params$rate)
  lengthscales <- c(lengthscale, lengthscale)
  variance <- normal(0, 1, truncation = c(0, Inf))
  kernel_scv_space <- greta.gp::rbf(lengthscales = lengthscales,
                                variance = variance,
                                columns = 1:2)

  # and an intercept kernel, in order to marginalise the intercepts and
  # hopefully improve sampling
  kernel_scv_int <- greta.gp::bias(variance = beta_sd ^ 2)

  # combine them
  kernel_scv <- kernel_scv_int + kernel_scv_space

  n_scv <- n_species * n_cov_abund

  # # prior simulations to check range of prior spatial correlation
  # beta_spatial <- greta.gp::gp(x = bg_coords_proj,
  #                              kernel = kernel_scv_space, # kernel_scv,
  #                              inducing = inducing_points_proj,
  #                              n = n_scv)
  # sims <- calculate(beta_spatial,
  #                   values = list(variance = 1),
  #                   nsim = 9)
  #
  # par(mfrow = c(3, 3))
  # for(i in seq_len(9)) {
  #   plot(bg_coords_proj,
  #        pch = 16,
  #        cex = 0.1 + 1 * plogis(sims$beta_spatial[i, , 1]),
  #        asp = 0.8)
  # }

  # these are the spatially-varying betas for all n_scv species-covariate
  # combinations. They share hyperparameters lengthscale and variance, but have
  # different spatial patterns
  beta_spatial <- greta.gp::gp(x = distinct_coords_proj,
                               kernel = kernel_scv,
                               inducing = inducing_points_proj,
                               n = n_scv)

  # we need to pull out the untransformed internal variables v, so we can
  # initialise them
  gp_info <- attributes(beta_spatial)$gp_info
  v <- gp_info$v

  # back-calculate the main effects (intercept term)
  beta <- greta.gp::project(
    f = beta_spatial,
    x = distinct_coords_proj[1, ],
    kernel = kernel_scv_int
  )

  # restructure into the same orientation as the old beta - I am pretty sure
  # this is the correct orientation
  dim(beta) <- c(n_cov_abund, n_species)


  # now we need to combine them with the covariates to model
  # log_lambda_larval_habitat spatially

  # duplicate x horizontally, n_species times
  x_tiled <- do.call(
    cbind,
    replicate(n_species, x,
              simplify = FALSE)
  )

  # multiply with beta elementwise
  x_beta <- x_tiled * beta_spatial

  # collapse down into n_pixels by n_species matrix, by
  # matrix multiplying by a block matrix?
  blocks <- matrix(0,
                   n_species,
                   n_species * n_cov_abund)
  for (species in seq_len(n_species)) {
    cols <- (species - 1) * n_cov_abund + seq_len(n_cov_abund)
    blocks[species, cols] <- 1
  }

  # get x * beta, for each species (columns), for each distinct pixel (rows)
  x_beta_species <- x_beta %*% t(blocks)

  # # the code above is doing the following, but faster
  # res <- matrix(NA,
  #               nrow = n_pixels,
  #               ncol = n_species)
  # for (species in seq_len(n_species)) {
  #   cols <- (species - 1) * n_cov_abund + seq_len(n_cov_abund)
  #   res[, species] <- rowSums(x_beta[, cols])
  # }
  # # check
  # identical(res, x_beta_species)


  # add alpha to get log larval habitat for each species across all sites based
  # on env covariates
  log_lambda_larval_habitat <- sweep(x_beta_species, 2, alpha, FUN = "+")
  # log_lambda_larval_habitat <- sweep(zeros(n_pixel, n_species), 2, alpha, FUN = "+")



  # informative priors on gamma and delta so exp(log_bias), i.e., bias,
  # has range around (0, 1) for z in (0, 1)
  # delta_sd <- 0.3
  # gamma_sd <- 0.1

  delta_sd <- 0.5
  gamma_sd <- 0.1


  gamma <- normal(0, gamma_sd, dim = n_species)
  delta <- normal(1, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf))



  # offset from calculated gambiae adult survival given habitat
  log_lambda_adults <- log_offset
  # this turns offset off instead of above line
  # log_lambda_adults <- rep(0, times = dim(log_offset)[[1]]) |>
  #  as_data()

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

  #### count data likelihood

  log_lambda_obs_count <-log_lambda[count_data_loc_sp_idx] +
    sampling_re[count_data_index$sampling_method_id]

  count_data_response <- model_data |>
    filter(data_type == "count") |>
    pull(n) |>
    as_data()

  count_data_response_expected <- exp(log_lambda_obs_count)
  distribution(count_data_response) <- poisson(count_data_response_expected)

  #### PA likelihood

  log_lambda_obs_pa <- log_lambda[pa_data_loc_sp_idx] +
    sampling_re[pa_data_index$sampling_method_id]

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

  log_lambda_obs_pobg <-log_lambda[pobg_data_loc_sp_idx] +
    sampling_re[pobg_data_index$sampling_method_id]

  po_data_response_expected <-   exp(
    log_lambda_obs_pobg +
      log_bias_obs_pobg +
      log(area_pobg)
  )
  distribution(po_data_response) <- poisson(po_data_response_expected)


  #######################

  # define and fit the model by MAP and MCMC

  m <- model(alpha, v, gamma, delta, sampling_re_raw, sampling_re_sd)


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
    output_prefix = "outputs/figures/ppc_sm/prior"
  )

  ###################
  # fit model
  ###################

  # manually set some inits (from previous optimiser runs), then optimise again
  inits_manual <- initials(
    alpha = c(8.19, 5.76, 2.59, 7.67, -64.58, -46.96, 3.95, -8.3),
    # set all beta coefficients to 0
    v = matrix(0, n_inducing, n_scv),
    gamma = c(-9.9, -8.03, -9.24, -8.79, -1.77, -1.42, -7.58, -7.54),
    delta = 42,
    sampling_re_raw = c(-0.12, 0.74, 0.2, 1.55, 1.11, 0.23, 3.27, -8.84, 5.96),
    sampling_re_sd = 0.27
  )


  optim <- opt(
    m,
    optimiser = adam(learning_rate = 0.01),
    max_iterations = 5e4
  )

  # optim <- opt(m, max_iterations = 1e5) # with sinka species plus coluzzii this converges

  # should be 0 if converged
  optim$convergence
  print(
    paste(
      "optimiser value is",
      optim$convergence,
      "; it should be 0 if optimiser converged"
    )
  )


  # optim$par

  init_vals <- inits_from_opt(
    optim,
    n_chains = n_chains
  )

  # get inits using fitian method
  # doesn't work because gets gammas that are outside of range
  # of priors
  # # fixed by setting delta as > 0 but not yet tested
  # inits_fithian <- fithian_inits(
  #   dat = model_data,
  #   target_species = target_species,
  #   n_pixel = n_pixel
  # )
  #
  # init_vals <- inits(
  #   n_chains = n_chains,
  #   nsp = length(target_species),
  #   ncv = length(target_covariate_names),
  #   ina = inits_fithian$alpha,
  #   inb = inits_fithian$beta,
  #   ing = inits_fithian$gamma,
  #   ind = inits_fithian$delta
  # )


  draws <- greta::mcmc(
    m,
    warmup = n_burnin,
    n_samples = n_samples,
    chains = n_chains,
    initial_values = init_vals
  )

  print(summary(draws))

  mcmc_trace(
    x = draws,
    regex_pars = "alpha"
  )
  ggsave(
    "outputs/figures/traceplots/sm_alpha.png"
  )

  mcmc_trace(
    x = draws,
    regex_pars = "beta"
  )
  ggsave(
    "outputs/figures/traceplots/sm_beta.png"
  )

  mcmc_trace(
    x = draws,
    regex_pars = c("delta", "gamma")
  )
  ggsave(
    "outputs/figures/traceplots/sm_delta_gamma.png"
  )

  mcmc_trace(
    x = draws,
    regex_pars = "sampling"
  )
  ggsave(
    "outputs/figures/traceplots/sm_sampling.png"
  )

  coda::gelman.diag(draws, autoburnin = FALSE)

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
    output_prefix = "outputs/figures/ppc_sm/posterior"
  )


  ###### Plot parameter estimates

  interval_plots(
    draws = draws,
    target_species = target_species,
    target_covariate_names = target_covariate_names,
    sampling_methods = sampling_methods
  )



  ############
  # Save image
  ############


  # can't use save.image inside function inside targets
  # because it only saves the global environment not
  # function env.

  save(
    list = ls(all.names = TRUE),
    file = image_name
  )

  return(image_name)

}
