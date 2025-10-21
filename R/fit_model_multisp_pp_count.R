fit_model_multisp_pp_count <- function(
    model_data_spatial,
    target_covariate_names,
    target_species,
    project_mask,
    image_name = "outputs/images/multisp_pp_count.RData",
    n_burnin = 50,
    n_samples = 100,
    n_chains = 4
){

  # library(targets.utils)
  # tl()

  model_data_spatial <- model_data_spatial |>
    # select(
    #   - evi_mean,
    #   - lst_day_mean
    #   - footprint
    # )
    filter(
      (data_type != "count") |
        (data_type == "count" & count < 1000)
    )
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
  #delta_sd <- sqrt(1 / penalty.l2.bias)
  #delta <- normal(0, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf)) # constrain to be positive
  #
  # intercept and shared slope for selection bias
  #gamma <- normal(0, intercept_sd, dim = n_species)

  # intercept and slopes for abundance rate
  alpha <- normal(0, intercept_sd, dim = n_species)
  beta <- normal(0, beta_sd, dim = c(n_cov_abund, n_species))

  # informative priors on gamma and delta so exp(log_bias), i.e., bias,
  # has range around (0, 1) for z in (0, 1)
  # these work really well
  delta_sd <- 0.3
  gamma_sd <- 0.1

  # delta_sd <- 0.5
  # gamma_sd <- 0.5


  gamma <- normal(-3.6, gamma_sd, dim = n_species)
  delta <- normal(3.8, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf))

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
    output_prefix = "outputs/figures/ppc/prior"
  )




  ###################
  # fit model
  ###################

  optim <- opt(m)
  optim$par

  init_vals <- inits_from_opt(
    optim,
    n_chains = n_chains
  )


  draws <- greta::mcmc(
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


  summary(draws)

  coda::gelman.diag(draws)

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
    output_prefix = "outputs/figures/ppc/posterior"
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
