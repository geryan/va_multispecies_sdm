# inits prob function

ipf <- function(
    am,
    bm,
    gm,
    dm,
    asd,
    bsd,
    gsd,
    dsd
  ){

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
  # penalty.l2.sdm <- penalty.l2.bias <- 0.1

  # trying others

  penalty.l2.intercept <- 1e-1
  penalty.l2.sdm <- penalty.l2.bias <- 1e-4

  # intercept_sd <- sqrt(1 / penalty.l2.intercept)
  # beta_sd <- sqrt(1 / penalty.l2.sdm)

  # delta_sd <- sqrt(1 / penalty.l2.bias)
  # delta <- normal(0, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf)) # constrain to be positive
  #
  # # intercept and shared slope for selection bias
  # gamma <- normal(0, intercept_sd, dim = n_species)


  intercept_sd <- asd
  beta_sd <- bsd

  # intercept and slopes for abundance rate
  alpha <- normal(am, intercept_sd, dim = n_species)
  beta <- normal(bm, beta_sd, dim = c(n_cov_abund, n_species))

  # informative priors on gamma and delta so exp(log_bias), i.e., bias,
  # has range around (0, 1) for z in (0, 1)
  # these work really well
  # delta_sd <- 0.3
  # gamma_sd <- 0.1

  delta_sd <- dsd
  gamma_sd <- gsd
  #
  #
  gamma <- normal(gm, gamma_sd, dim = n_species)
  delta <- normal(dm, delta_sd, dim = c(n_cov_bias), truncation = c(0, Inf))

  # gamma <- zeros(8)
  # delta <- ones(1)

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
  #plot(m)


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

  ninits <- try(sample(init_index, size = nsamples, replace = FALSE, prob = exp(log_probs_np)), silent = TRUE)

  result <- ifelse(
    class(ninits) == "try-error",
    0,
    length(ninits)
  )

  result
}

options <- expand.grid(
  am = c(-10, 0, 10),
  bm = c(-10, 0, 10),
  gm = c(-10, 0, 10),
  dm = c(0.01, 1, 10),
  asd = c(0.01, 1, 10),
  bsd = c(0.01, 1, 10),
  gsd = c(0.01, 1, 10),
  dsd = c(0.01, 1, 10)
)


library(future)
library(future.apply)
plan(multisession, workers = 8)

init_successes <- future_mapply(
  FUN = ipf,
  am = options$am,
  bm = options$bm,
  gm = options$gm,
  dm = options$dm,
  asd = options$asd,
  bsd = options$bsd,
  gsd = options$gsd,
  dsd = options$dsd
)


mapply(
  FUN = ipf,
  am = options$am[1:2],
  bm = options$bm[1:2],
  gm = options$gm[1:2],
  dm = options$dm[1:2],
  asd = options$asd[1:2],
  bsd = options$bsd[1:2],
  gsd = options$gsd[1:2],
  dsd = options$dsd[1:2]
)


ipf(
  am = options$am[1],
  bm = options$bm[1],
  gm = options$gm[1],
  dm = options$dm[1],
  asd = options$asd[1],
  bsd = options$bsd[1],
  gsd = options$gsd[1],
  dsd = options$dsd[1]
)
