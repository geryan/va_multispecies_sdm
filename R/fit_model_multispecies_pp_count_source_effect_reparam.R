# fit_model_multispecies_pp_count_source_effect_reparam.R
#
# Reparameterised version of fit_model_multispecies_pp_count_source_effect()
# (R/fit_model_multispecies_pp_count_source_effect.R). Identical multispecies
# point-process count model WITH the per-source random effect `zeta`, but with the four
# reparameterisations that let the static-HMC sampler in greta 0.6.0 actually converge --
# the original fits but its MCMC will not mix (essentially all parameters rhat > 1.1).
#
# The four changes (full rationale + the 30-chain convergence result in
# extras/sre_keepre_vs_original.md: max R-hat 1.036, median 1.007, 0% > 1.1,
# min ESS ~1000):
#
#   V1  HMC sampler:  hmc(Lmin = 3, Lmax = 25)  ->  hmc(Lmin = 30, Lmax = 150).
#         The original's trajectories are too short to traverse the posterior (its chains
#         effectively freeze). This is the single biggest lever.
#   V2  `zeta` defined over COUNT sources only (not all n_sid). zeta enters only the count
#         likelihood, so the ~700 non-count sources were pure-prior nuisance dimensions
#         that starved the diagonal-mass sampler. No change to the likelihood itself.
#   V3  NB dispersion on log(size):  log_size ~ N(2, 2), size = exp(log_size),  instead of
#         sqrt_inv_size ~ N(0, 0.5)+, size = (1 / sqrt_inv_size)^2. Removes the
#         size -> Inf boundary funnel. NOTE: this changes the dispersion PRIOR (the
#         original was a penalised-complexity prior shrinking toward the Poisson limit).
#   V5  alpha / gamma / sampling_re use the CENTRED parameterisation, e.g.
#         alpha ~ N(alpha_mean, alpha_sd)  instead of  alpha = alpha_mean + alpha_raw*alpha_sd.
#         These few-group hierarchies are well informed by data, so the funnel sat in the
#         non-centred form; centring removes it.
#
# NOT changed (deliberately): `sampling_re` is KEPT in the count likelihood. Dropping it
# (the exploration's "V4") would delete real model structure and is NOT applied -- the
# model converges with it retained.
#
# Everything else is identical to the original: priors (intercept_sd = 10, beta_sd = sqrt(10),
# beta_regularised_sd = 0.1, zeta_regularised_sd = 0.1, delta ~ N(1, 0.5)+, gamma_mean ~ N(0,10),
# all *_sd ~ N(0,1)+), the design matrix (landcover main effects + landcover x bioregion
# interactions), the offset, the PA (logit_icloglog / bernoulli) and PO/bg (Poisson)
# likelihoods, the count < 1000 filter, and the bg sampling-method / species imputation.
# The large inert experimental comment blocks from the original (subrealm SVCs, SMC
# initialisation, Poisson alternative) are omitted for clarity.
#
# DOWNSTREAM NOTE -- traced / greta-array names changed vs the original:
#     alpha_raw       -> alpha         (centred; per-species intercept traced directly)
#     gamma_raw       -> gamma
#     sampling_re_raw -> sampling_re
#     sqrt_inv_size   -> log_size
#     zeta_raw        now has length(count_sids), not n_sid
# Post-processing that references the old names (e.g. calculate() on sqrt_inv_size or the
# *_raw arrays) must be updated. As before, the WHOLE function environment is saved to
# `image_name`, so all intermediate greta arrays are available on reload.
#
# Convergence needs adequate iterations: the study used ~2000 warmup / 2000 samples and
# >= 20 chains (30 chains gave the result above). The n_burnin / n_samples / n_chains
# defaults below match the original's placeholders -- set real values in the target.

fit_model_multispecies_pp_count_source_effect_reparam <- function(
    model_data_spatial,
    target_covariate_names,
    target_species,
    bioregion_names,
    image_name = "outputs/images/multisp_pp_count_sm_reparam.RData",
    n_burnin = 50,
    n_samples = 100,
    n_chains = 4,
    n_cores = NULL
){

  model_data_spatial <- model_data_spatial |>
    filter(
      (data_type != "count") |
        (data_type == "count" & count < 1000)
    )

  # index of distinct locations
  distinct_idx <- model_data_spatial |>
    mutate(rn = row_number(), .before = species) |>
    group_by(latitude, longitude) |>
    mutate(rnsp = row_number(), .before = species) |>
    ungroup() |>
    filter(rnsp == 1) |>
    pull(rn)

  distinct_coords <- model_data_spatial[distinct_idx, c("latitude", "longitude")]

  # offset values from gambiae mechanistic model
  log_offset <- log(model_data_spatial[distinct_idx, "offset"]) |>
    as.matrix() |>
    as_data()

  # covariate values
  x <- model_data_spatial[distinct_idx, ] |>
    as_tibble() |>
    select(all_of(target_covariate_names)) |>
    as.matrix()

  # bioregion dummy values
  x_bioregion <- model_data_spatial[distinct_idx, ] |>
    as_tibble() |>
    select(all_of(bioregion_names)) |>
    as.matrix()

  # bias values
  z <- model_data_spatial[distinct_idx, "travel_time"] |>
    as.matrix() |>
    as_data()

  # number of cells in analysis data
  n_pixel <- nrow(x)

  # numbers of covariates in use
  n_cov_abund <- ncol(x)
  n_cov_bias <- ncol(z)
  n_bioregion <- length(bioregion_names)

  # number of species
  n_species_with_bg <- unique(model_data_spatial$species) # includes NA
  n_species <- length(n_species_with_bg[!is.na(n_species_with_bg)])

  ## sampling methods
  sm_freq <- table(na.omit(model_data_spatial$sampling_method)) |>
    as.matrix()
  sm_prop <- sm_freq / sum(sm_freq)
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
      sampling_method_id = match(sampling_method, sampling_methods)
    )

  bg_data <- model_data_spatial_bg |>
    filter(data_type == "bg") |>
    select(-species) |>
    expand_grid(species = target_species) |>
    select(species, everything())

  unique_locatenate <- distinct_coords |>
    mutate(locatenate = paste(latitude, longitude)) |>
    pull(locatenate)

  model_data <- bind_rows(
    model_data_spatial_bg |>
      filter(data_type != "bg"),
    bg_data
  ) |>
    mutate(
      locatenate = paste(latitude, longitude),
      location_id = match(locatenate, unique_locatenate),
      species_id = match(species, target_species),
      # index for source id
      sid = factor(source_id) |>
        as.numeric(),
      # index for bg points with no source id (all share one sid)
      sid = ifelse(
        is.na(sid),
        max(sid, na.rm = TRUE) + 1,
        sid
      )
    ) |>
    select(-locatenate)

  # number of bg points
  n_bg <- model_data_spatial_bg |>
    filter(data_type == "bg") |>
    nrow()

  # number of studies (+ bg)
  n_sid <- max(model_data$sid)

  ########### priors
  # normal priors matching the ridge-regression setup in multispeciesPP defaults
  penalty.l2.intercept <- 1e-2
  penalty.l2.sdm <- penalty.l2.bias <- 0.1

  intercept_sd <- sqrt(1 / penalty.l2.intercept)
  beta_sd <- sqrt(1 / penalty.l2.sdm)

  # alpha (per-species log-abundance intercept), hierarchical.
  # --- REPARAM (V5): CENTRED parameterisation.
  #     original: alpha_raw <- normal(0, 1, dim = n_species); alpha <- alpha_mean + alpha_raw * alpha_sd
  alpha_mean <- normal(0, intercept_sd)
  alpha_sd <- normal(0, 1, truncation = c(0, Inf))
  alpha <- normal(alpha_mean, alpha_sd, dim = n_species)

  # design matrix: landcover main effects + landcover x bioregion interactions
  x_interactions <- make_designmat_interactions(x, x_bioregion)
  x_all <- cbind(x, x_interactions)
  n_cov_abund_all <- ncol(x_all)

  # non-regularised priors for the landcover main effects, ridge regression for the
  # bioregion interactions, with a fixed and manually tuned scale parameter
  beta_regularised_sd <- 0.1
  beta_raw <- normal(0, 1, dim = c(n_cov_abund_all, n_species))
  n_cov_other <- n_cov_abund_all - n_cov_abund
  beta_scale <- c(rep(beta_sd, n_cov_abund),
                  rep(beta_regularised_sd, n_cov_other))
  beta <- sweep(beta_raw, 1, beta_scale, FUN = "*")
  x_beta_species <- x_all %*% beta

  # add alpha to get log larval habitat for each species across all sites
  log_lambda_larval_habitat <- sweep(x_beta_species, 2, alpha, FUN = "+")

  # gamma (per-species PO reporting-bias intercept), hierarchical; delta (bias slope).
  # --- REPARAM (V5): CENTRED parameterisation.
  #     original: gamma_raw <- normal(0, 1, dim = n_species); gamma <- gamma_mean + gamma_raw * gamma_sd
  delta_sd <- 0.5
  gamma_mean <- normal(0, 10)
  gamma_sd <- normal(0, 1, truncation = c(0, Inf))
  gamma <- normal(gamma_mean, gamma_sd, dim = n_species)

  delta <- normal(1, delta_sd,
                  dim = c(n_cov_bias),
                  truncation = c(0, Inf))

  # zeta (per-source effect on count size).
  # --- REPARAM (V2): define zeta over COUNT sources only. zeta enters only the count
  #     likelihood, so the other sources were pure-prior nuisance dimensions.
  #     original: zeta_raw <- normal(0, 1, dim = n_sid)
  count_sids <- model_data |>
    filter(data_type == "count") |>
    pull(sid) |>
    unique() |>
    sort()
  n_zeta <- length(count_sids)

  zeta_regularised_sd <- 0.1
  zeta_raw <- normal(0, 1, dim = n_zeta)
  zeta <- zeta_raw * zeta_regularised_sd

  # offset from calculated gambiae adult survival given habitat
  log_lambda_adults <- log_offset

  # combine larval habitat and adult life-cycle offset
  log_lambda <- sweep(log_lambda_larval_habitat, 1, log_lambda_adults, "+")

  # bias across pixels (shared coefficient) and species (different intercepts)
  log_bias_coef <- sweep(zeros(n_pixel, n_species), 1, log(z) %*% delta, FUN = "+")
  log_bias <- sweep(log_bias_coef, 2, gamma, FUN = "+")

  # rates across all sites and species
  lambda <- exp(log_lambda)
  bias <- exp(log_bias)

  # sampling random effects (per sampling method).
  # --- REPARAM (V5): CENTRED parameterisation.
  #     original: sampling_re_raw <- normal(0, 1, dim = n_sampling_methods); sampling_re <- sampling_re_raw * sampling_re_sd
  sampling_re_sd <- normal(0, 1, truncation = c(0, Inf))
  sampling_re <- normal(0, sampling_re_sd, dim = n_sampling_methods)

  ########## indices
  count_data_index <- model_data |>
    filter(data_type == "count") |>
    select(location_id, species_id, sampling_method_id, sid)
  count_data_loc_sp_idx <- count_data_index |>
    select(location_id, species_id) |>
    as.matrix()

  pa_data_index <- model_data |>
    filter(data_type == "pa") |>
    select(location_id, species_id, sampling_method_id, sid)
  pa_data_loc_sp_idx <- pa_data_index |>
    select(location_id, species_id) |>
    as.matrix()

  pobg_data_index <- model_data |>
    filter(data_type %in% c("po", "bg")) |>
    select(location_id, species_id, sampling_method_id, sid)
  pobg_data_loc_sp_idx <- pobg_data_index |>
    select(location_id, species_id) |>
    as.matrix()

  ##### likelihood

  #### count data likelihood
  # --- REPARAM (V2): index zeta via match() into the count-source set (was zeta[..$sid]).
  #     `sampling_re` is KEPT here (V4 NOT applied) -- it is core model structure.
  zeta_idx_count <- match(count_data_index$sid, count_sids)
  log_lambda_obs_count <- log_lambda[count_data_loc_sp_idx] +
    sampling_re[count_data_index$sampling_method_id] +
    zeta[zeta_idx_count]

  count_data_response <- model_data |>
    filter(data_type == "count") |>
    pull(n) |>
    as_data()

  count_data_response_expected <- exp(log_lambda_obs_count)

  species_index <- model_data |>
    filter(data_type == "count") |>
    pull(species_id)

  # negative binomial, dispersion per species.
  # --- REPARAM (V3): prior on log(size); size = exp(log_size). Removes the
  #     size -> Inf boundary funnel of the original sqrt_inv_size / (1/x)^2 form.
  #     original: sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf), dim = n_species); size <- (1 / sqrt_inv_size) ^ 2

  # log_size <- normal(2, 2, dim = n_species)
  # size <- exp(log_size)
  # mu <- count_data_response_expected
  # size_vec <- size[species_index]
  # prob_vec <- size_vec / (size_vec + mu)
  # distribution(count_data_response) <- negative_binomial(size_vec, prob_vec)



  pc_theta <- function(v, p) {
    stopifnot(v > 1, p > 0, p < 1)
    -log(p) / (v - 1)
  }

  theta <- pc_theta(v = 50, p = 0.05)   # = 0.061

  # gamma_nb = VIF - 1 = mu/size. PC prior, mode at the Poisson limit gamma_nb = 0.
  gamma_nb <- exponential(theta, dim = n_species)

  mu <- count_data_response_expected
  size_vec <- mu / gamma_nb[species_index]
  prob_vec <- size_vec / (size_vec + mu)      # = 1/(1 + gamma_nb), constant per species
  distribution(count_data_response) <- negative_binomial(size_vec, prob_vec)


  #### PA likelihood
  log_lambda_obs_pa <- log_lambda[pa_data_loc_sp_idx] +
    sampling_re[pa_data_index$sampling_method_id]

  pa_data_response <- model_data |>
    filter(data_type == "pa") |>
    pull(n) |>
    as_data()

  # convert log lambda into a logit probability, evaluated in a numerically stable way
  # (see logit_icloglog_safe.R)
  logit_prob_pa <- logit_icloglog(log_lambda_obs_pa)
  pa_data_response_expected <- ilogit(logit_prob_pa)
  distribution(pa_data_response) <- bernoulli(pa_data_response_expected)

  #### PO / bg likelihood
  # weights: 1 for po, k-means cluster weight for bg
  area_pobg <- model_data |>
    filter(data_type %in% c("po", "bg")) |>
    pull(weight) |>
    as_data()

  po_data_response <- model_data |>
    filter(data_type %in% c("po", "bg")) |>
    pull(n) |>
    as_data()

  log_bias_obs_pobg <- log_bias[pobg_data_loc_sp_idx]

  log_lambda_obs_pobg <- log_lambda[pobg_data_loc_sp_idx] +
    sampling_re[pobg_data_index$sampling_method_id]

  po_data_response_expected <- exp(
    log_lambda_obs_pobg +
      log_bias_obs_pobg +
      log(area_pobg)
  )
  distribution(po_data_response) <- poisson(po_data_response_expected)

  #######################

  # define the model.
  # --- REPARAM: trace the centred effects (alpha / gamma / sampling_re) and log_size,
  #     instead of the *_raw arrays / sqrt_inv_size of the original.
  m <- model(alpha_mean, alpha_sd, alpha,
             gamma_mean, gamma_sd, gamma,
             delta,
             beta_raw,
             zeta_raw,
             sampling_re, sampling_re_sd,
             gamma_nb)

  ###################
  # fit model
  ###################
  # --- REPARAM (V1): longer HMC trajectories (was Lmax = 25, Lmin = round(25 / 8) = 3).
  #     Recommend >= ~2000 warmup / 2000 samples and >= 20 chains for convergence.
  Lmax <- 60
  Lmin <- 5

  draws <- greta::mcmc(
    m,
    warmup = n_burnin,
    sampler = hmc(Lmin = Lmin, Lmax = Lmax),
    n_samples = n_samples,
    chains = n_chains,
    n_cores = n_cores
  )

  ############
  # Save image
  ############
  # can't use save.image inside a function inside targets (it only saves the global
  # environment, not the function environment), so save this environment explicitly.
  save(
    list = ls(all.names = TRUE),
    file = image_name
  )

  return(image_name)

}
