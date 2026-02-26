fit_model_multisp_pp_count_sm <- function(
    model_data_spatial,
    target_covariate_names,
    target_species,
    subrealm_names,
    bioregion_names,
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

  # get subrealm dummy values
  x_subrealm <- model_data_spatial[distinct_idx,] |>
    as_tibble() |>
    select(
      all_of(subrealm_names)
    ) |>
    as.matrix() |>
    as_data()

  # get bioregion dummy values
  x_bioregion <- model_data_spatial[distinct_idx,] |>
    as_tibble() |>
    select(
      all_of(bioregion_names)
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

  # do we need a hierarchical mean for this?

  # sampling_re_raw should be centred around 0 but they are mostly negative.
  # Maybe they are compensating for a restrictive prior structure on these?

  # the intercept should now not be anything like zero-mean, because it encodes
  # abundance in the 'bare' landcover areas! The sampling res make sense to use,
  # since they apply to all species
  alpha_mean <- normal(0, intercept_sd)
  alpha_sd <- normal(0, 1, truncation = c(0, Inf))
  alpha_raw <- normal(0, 1, dim = n_species)
  alpha <- alpha_mean + alpha_raw * alpha_sd
  # alpha <- normal(0, intercept_sd, dim = n_species)





  # for each covariate, model beta as a positive-constrained spatially-varying
  # covariate. Positive so that abundance is forced to be higher in
  # non-bare-gound landcover types, spatially varying to account for: a)
  # differences in definition of landcover types in different parts of Africa
  # (e.g. 'grassland' in the Sahel is likely to be very different from in the
  # southern Congo basin), b) differences in species phenotypes across Africa,
  # and c) residual spatial autocorrelation in distributions (e.g. due to
  # dispersal limitation in the coastal species)

  # define the spatially varying coefficients (for each species and landcover
  # type) with a main effect, plus a subrealm-varying coefficient, plus a
  # bioregion-varying coefficient, with shrinkage applied to these increasing
  # levels of complexity

  # load the matrices of subrealm and bioregion dummy variables at the distinct locations
  n_subrealm <- length(subrealm_names)
  n_bioregion <- length(bioregion_names)
  n_svc <- n_species * n_cov_abund

  # define subrealm weights (one per subrealm x SVC combination)
  subrealm_sd <- normal(0, 1,
                        truncation = c(0, Inf))
  subrealm_svc_coef_raw <- normal(0, 1,
                                  dim = c(n_subrealm, n_svc))
  subrealm_svc_coef <- subrealm_svc_coef_raw * subrealm_sd

  # # define bioregion weights (one per bioregion x SVC combination)
  # bioregion_sd <- normal(0, 1,
  #                        truncation = c(0, Inf))
  # bioregion_svc_coef_raw <- normal(0, 1,
  #                                  dim = c(n_bioregion, n_svc))
  # bioregion_svc_coef <- bioregion_svc_coef_raw * bioregion_sd

  # convert these into the spatial variation in the betas
  beta_eff_subrealm <- x_subrealm %*% subrealm_svc_coef
  # beta_eff_bioregion <- x_bioregion %*% bioregion_svc_coef
  beta_spatial <- beta_eff_subrealm #+ beta_eff_bioregion

  # make these positive (median 0)
  beta_spatial_pos <- exp(beta_spatial)
  # beta_spatial_pos <- ones(nrow(distinct_coords), n_svc)

  # # simulate to check there variation
  # sims <- calculate(beta_spatial[, 1], nsim = 9)
  # par(mfrow = c(3, 3))
  # for(i in seq_len(9)) {
  #   plot(distinct_coords[, 2:1],
  #        pch = 16,
  #        cex = 0.5 * plogis(sims[[1]][i, , 1]),
  #        asp = 0.8)
  # }

  # model the main (species by landcover type) effects separately as
  # positive-constrained coefficients and combine them. Note that we could put
  # the log of these in the earlier model, but it is helpful to be able to
  # specify their positive-constrained values in this matrix orientation when
  # initialising

  # these are constrained to be positive
  beta <- normal(0,
                 beta_sd,
                 dim = c(n_cov_abund, n_species),
                 truncation = c(0, Inf))

  # make them as a matrix for plotting code, but flatten for combining into SVCs
  beta_vec <- beta
  dim(beta_vec) <- c(n_svc, 1)

  # make a matrix of  positive-constrained spatially-varying coefficients
  beta_spatial <- sweep(beta_spatial_pos, 2, beta_vec, FUN = "*")

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
  # log_lambda_larval_habitat <- sweep(x %*% beta, 2, alpha, FUN = "+")
  # log_lambda_larval_habitat <- sweep(zeros(n_pixel, n_species), 2, alpha, FUN = "+")



  # informative priors on gamma and delta so exp(log_bias), i.e., bias,
  # has range around (0, 1) for z in (0, 1)
  # delta_sd <- 0.3
  # gamma_sd <- 0.1

  delta_sd <- 0.5
  # gamma_sd <- 0.1


  # perhaps gamma should have a hierarchical mean? The posteriors are all away
  # from 0, and this would constrain them to be similar.
  gamma_mean <- normal(0, 10)
  gamma_sd <- normal(0, 1, truncation = c(0, Inf))
  gamma_raw <- normal(0, 1, dim = n_species)
  gamma <- gamma_mean + gamma_raw * gamma_sd
  # gamma <- normal(0, gamma_sd, dim = n_species)

  delta <- normal(1, delta_sd,
                  dim = c(n_cov_bias),
                  truncation = c(0, Inf))



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

  log_lambda_obs_count <- log_lambda[count_data_loc_sp_idx] +
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

  po_data_response_expected <- exp(
    log_lambda_obs_pobg +
      log_bias_obs_pobg +
      log(area_pobg)
  )
  distribution(po_data_response) <- poisson(po_data_response_expected)


  #######################

  # define and fit the model by MAP and MCMC
  m <- model(alpha_mean, alpha_sd, alpha_raw,
             gamma_mean, gamma_sd, gamma_raw,
             delta,
             beta,
             subrealm_sd, subrealm_svc_coef_raw,
             # bioregion_sd, bioregion_svc_coef_raw,
             sampling_re_raw, sampling_re_sd,
             sqrt_inv_size)


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

  # # # manually set some inits (from previous runs)
  # inits_manual <- initials(
  #   alpha = c(0.04, -0.87, -0.75, 0.29, -77.66, -46.35, -9.74, -12.99),
  #   beta = array(
  #     c(0.01, 2.52, 4.01, 3.45, 0.02, 1.52, 1.17, 0.03, 0.67,
  #       0.01, 1.19, 3.25, 2.06, 3.96, 5.52, 3.82, 0.07, 0.01, 2.97, 3.98,
  #       2.74, 3.47, 0.05, 0.55, 5.95, 0.03, 0.02, 0.18, 0.01, 3.4, 0.8,
  #       1.91, 2.36, 0.02, 0.07, 0.01, 0.01, 1.72, 0.06, 5.37, 0.1, 0.42,
  #       8.71, 4.51, 76.68, 0.15, 1.79, 3.46, 5.2, 0.82, 4.97, 7.8, 14.91,
  #       41.02, 9.06, 6.91, 0.01, 0.08, 7.56, 8.04, 10.19, 0.29, 2.77,
  #       12.19, 8.64, 13.25, 10.85, 0.85, 0.37, 0.29, 1.12, 2.07),
  #     dim = c(9, 8)
  #   ),
  #   # # set all raw spatial random effects to 0
  #   # subrealm_sd = 0.01,
  #   # subrealm_svc_coef_raw = array(0, dim = dim(subrealm_svc_coef_raw)),
  #   # bioregion_sd = 0.01,
  #   # bioregion_svc_coef_raw = array(0, dim = dim(bioregion_svc_coef_raw)),
  #   gamma = c(-9.83, -8.16, -9.3, -8.94, -1.98, -1.45, -7.58, -7.74),
  #   delta = 39.77,
  #   sampling_re_raw = c(-0.5, 0.13, -0.28, -0.08, -0.17, -0.35, 0.19, -2.43,
  #                       0.89),
  #   sampling_re_sd = 1.2,
  #   sqrt_inv_size = 0.001
  # )
  #
  # optim <- opt(
  #   m,
  #   optimiser = adam(learning_rate = 0.1),
  #   max_iterations = 5e4
  #   # initial_values = inits_manual
  # )

  # # optim <- opt(m, max_iterations = 1e5) # with sinka species plus coluzzii this converges
  #
  # # should be 0 if converged
  # optim$convergence
  # print(
  #   paste(
  #     "optimiser value is",
  #     optim$convergence,
  #     "; it should be 0 if optimiser converged"
  #   )
  # )
  #
  #
  # # optim$par
  #
  # init_vals <- inits_from_opt(
  #   optim,
  #   n_chains = n_chains
  # )

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

  # post_summary_filepath <- "~/Desktop/post_summary.RDS"
  # if (file.exists(post_summary_filepath)) {
  #   post_summary <- readRDS(post_summary_filepath)
  # }

  # post_mean_inits <- do.call(initials, post_summary$post_mean)

  # try using an importance sampling to initialise these:
  # - sample parameters from priors
  # - compute model log-posterior at these parameters (use code in initialiser
  #   code)
  # - compute estimate of the posterior mean as a posterior density-weighted
  #   mean
  # - draw samples from the posterior with a weighted resampling of these prior
  #   draws

  # try making this a tempering algorithm, and code it up based on the model


  # n_sims <- 5000

  # zeros_like <- function(greta_array) {
  #   array(0, dim = dim(greta_array))
  # }
  #
  # normal_like <- function(greta_array, mean = 0, sd = 1) {
  #   dims <- dim(greta_array)
  #   nelem <- prod(dims)
  #   values <- rnorm(nelem, mean = mean, sd = sd)
  #   array(values, dim = dims)
  # }
  #
  # posnormal_like <- function(greta_array, mean = 0, sd = 1) {
  #   dims <- dim(greta_array)
  #   nelem <- prod(dims)
  #   values <- extraDistr::rtnorm(nelem,
  #                                mean = mean,
  #                                sd = sd,
  #                                a = 0,
  #                                b = Inf)
  #   array(values, dim = dims)
  # }
  #
  #
  # # Manually tune the importance sampler (sample parameter values from fixed
  # # distributions) instead. Possibly setting all random effects values to 0, and
  # # informed by posterior densities. Maybe sequential Monte Carlo
  # # # manually set some inits (from previous runs)
  # importance_sims <- replicate(
  #   n_sims,
  #   list(
  #     alpha_mean = normal_like(alpha_mean),
  #     alpha_sd = posnormal_like(alpha_sd),
  #     alpha_raw = zeros_like(alpha_raw),
  #     gamma_mean = normal_like(gamma_mean),
  #     gamma_sd = posnormal_like(gamma_sd),
  #     gamma_raw = zeros_like(gamma_raw),
  #     delta = posnormal_like(delta, mean = 1, sd = delta_sd),
  #     beta = posnormal_like(beta),
  #     subrealm_sd = posnormal_like(subrealm_sd),
  #     subrealm_svc_coef_raw = zeros_like(subrealm_svc_coef_raw),
  #     bioregion_sd = posnormal_like(bioregion_sd),
  #     bioregion_svc_coef_raw = zeros_like(bioregion_svc_coef_raw),
  #     sampling_re_raw = zeros_like(sampling_re_raw),
  #     sampling_re_sd = posnormal_like(sampling_re_sd),
  #     sqrt_inv_size = posnormal_like(sqrt_inv_size, sd = 0.5)
  #   ),
  #   simplify = FALSE
  # )
  #
  # # convert into a list of initials
  # inits_list <- lapply(importance_sims,
  #                      function(x) do.call(initials, x))

  # # get all the variable greta arrays, to convert these back to free states
  # variable_greta_arrays <- m$target_greta_arrays
  #
  # # or use posterior samples from MCMC
  # # n_post_sim <- dim(post_summary$post_sims$alpha_mean)[1]
  # inits_list <- lapply(seq_len(n_sims),
  #                      make_inits,
  #                      post_summary$post_sims,
  #                      variable_greta_arrays)
  #
  # free_states <- get_free_states(inits_list,
  #                                variable_greta_arrays,
  #                                m)

  # # Sample (valid) free states from priors, limiting the memory use with
  # # trace_batch_size
  # free_state_prior <- prior_sample_free_states(
  #   mod = m,
  #   n = 500,
  #   trace_batch_size = 10
  # )

  # # compute the log posterior densities of these, and normalise them (in log
  # # space) to get weights
  # tf_log_densities <- m$dag$tf_log_prob_function_adjusted(free_states)
  # tf <- greta:::tf
  # tf_log_sum_densities <- tf$reduce_logsumexp(tf_log_densities)
  # tf_log_densities_norm <- tf_log_densities - tf_log_sum_densities
  # tf_densities_norm <- tf$exp(tf_log_densities_norm)
  # densities_norm <- as.vector(tf_densities_norm)
  #
  # # compute a weighted mean of these on the free state scale (skipping those
  # # with numerical underflow) to find a relatively high posterior density region
  # # of parameter space
  # valid <- densities_norm > 0
  # sum(valid)
  # valid_free_states <- free_states[valid, , drop = FALSE]
  # free_state_mean <- densities_norm[valid] %*% valid_free_states
  #
  # # get the initial values corresponding to this point
  # importance_inits <- initials_from_free_states(m, free_state_mean)[[1]]

  n_free <- length(unlist(m$dag$example_parameters(free = TRUE)))

  # n_particles_per_param <- 2
  # n_particles <- n_particles_per_param * n_free

  smc_output <- run_smc(m,
                        n_particles = 3000,
                        max_stages = 1000,
                        # n_prior_samples = 5000,
                        compute_batch_size = 100)

  saveRDS(smc_output, "~/Desktop/smc_output_subrealm.RDS")

  smc_mean <- colMeans(smc_output$particles)

  # point mass initial values
  inits_point <- initials_from_free_states(m, t(smc_mean))[[1]]

  # # random inits per chain
  # init_particles <- smc_output$particles[seq_len(n_chains), ]
  # inits_random <- initials_from_free_states(m, init_particles)


  # n_prior_samples is ignored internally and n_particles is used instead - fix
  # that. It's working now that I reduced the model dimension to <1000, but
  # won't in general

  # memory leak that causes a crash when running with 2*n_free particles, even
  # with 50 batch size?

  # only 20% memory when running batches though - increase compute batch size?

  # modify model to be linear (interactions between landcover classes and
  # bioergeions, etc., but not bioregion main effects), and post-hoc mask by the
  # area proportion to remove/downscale those regions

  # add more regularisation to the subrealm/bioregion models, with additional
  # variance components on interaction terms for each main covariate, and for
  # each bioregion?

  # is this creating new greta arrays and extending the dag?

  # turn this into a point estimate of the posterior mean, to initialise the
  # model

  # prior simulation seems to be an issue - resulting in an invalid covariance
  # matrix

  Lmax <- 10
  Lmin <- round(Lmax / 2)
  n_burnin <- 2000
  n_chains <- 50
  draws <- greta::mcmc(
    m,
    warmup = n_burnin,
    sampler = hmc(Lmin = Lmin, Lmax = Lmax),
    n_samples = n_samples,
    chains = n_chains,
    initial_values = inits_point
  )

  # save.image(file = "~/Desktop/image_220226.RData")

  # poor convergence (due to bad tuning?), but save posterior means and sds for
  # future inits
  post_sims <- calculate(alpha_mean, alpha_sd, alpha_raw,
                         gamma_mean, gamma_sd, gamma_raw,
                         delta,
                         beta,
                         subrealm_sd, subrealm_svc_coef_raw,
                         # bioregion_sd, bioregion_svc_coef_raw,
                         sampling_re_raw, sampling_re_sd,
                         sqrt_inv_size,
                         values = draws,
                         nsim = 10000,
                         trace_batch_size = 50)

  post_mean <- function(sims) {
    apply(sims, 2:3, mean)
  }
  post_sd <- function(sims) {
    apply(sims, 2:3, sd)
  }
  post_means <- lapply(post_sims,
                       post_mean)
  post_sds <- lapply(post_sims,
                       post_sd)

  post_sry <- list(
    post_sims = post_sims,
    post_mean = post_means,
    post_sd = post_sds
  )
  saveRDS(post_sry,
          file = "~/Desktop/post_summary.RDS")

  # re-run the sampler (with fewer burnin samples) using the posterior mean as
  # the initial values for all chains


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

  coda::gelman.diag(draws,
                    autoburnin = FALSE,
                    multivariate = FALSE)

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
