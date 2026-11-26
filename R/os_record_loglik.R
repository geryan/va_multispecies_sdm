# The log-likelihood of every record the model was fitted to, at every posterior draw,
# computed in R from a draws file. Written 2026-10-19 to compare the offset-strength
# ("os") model with the current one; works on the draws of either.
#
# `draws_file` is what extract_cv_draws() (current model) or os_extract_cv_draws() (os
# model) saves. If it holds `kappa` (os model) each species' offset is raised to its
# kappa; otherwise kappa = 1, the current model.
#
# The arithmetic is the fit's own (fit_model_multispecies_pp_count.R), rebuilt from the
# parameter draws:
#
#   log lambda[loc, s] = x_all[loc, ] beta[, s] + alpha[s] + kappa[s] * log(offset[loc])
#
#   counts       NB1, mu = exp(log lambda + sampling_re[method] + zeta[source]),
#                size = mu / gamma_nb[s], prob = 1 / (1 + gamma_nb[s])
#   pres-abs     Bernoulli, p = 1 - exp(-exp(log lambda + sampling_re[method])), with
#                log lambda clamped as the fit clamps it (logit_icloglog_safe())
#   PO / bg      Poisson, mu = exp(log lambda + method term + log(travel_time) delta
#                + gamma[s] + log(weight)); the method term is sampling_re[method] for a
#                presence and log(sum_m p_m exp(sampling_re[m])) for a background point
#
# The records are assembled exactly as the fit assembles them: the non-background rows
# in their original order, then every background point copied once per species. Design
# rows come from cv_designmat(), which mirrors the fit's `distinct_idx`. The source
# effect `zeta` is the fitted one (every count source is in the data).
#
# Returns a list with one element per data type ("count", "pa", "pobg"), each holding
#   loglik    [draws x records], the layout loo::loo() takes
#   records   a tibble of the records, in the same order
# and, if `return_expected = TRUE`, `expected` [records x draws]: the count mean, the
# presence probability or the Poisson mean, for checking against greta's calculate().
os_record_loglik <- function(
    draws_file,
    model_data_spatial,
    data_types = c("count", "pa", "pobg"),
    return_expected = FALSE
){

  data_types <- match.arg(
    data_types,
    several.ok = TRUE
  )

  fit <- readRDS(draws_file)

  target_species <- fit$target_species
  n_species <- fit$n_species
  sampling_methods <- fit$sampling_methods
  D <- length(fit$chain)

  kappa <- if (is.null(fit$kappa)) {
    matrix(
      1,
      nrow = D,
      ncol = n_species
    )
  } else {
    fit$kappa
  }

  if (is.null(fit$sm_prop)) {
    stop(
      "os_record_loglik(): the draws file has no `sm_prop`; re-extract it with the current extract_cv_draws()",
      call. = FALSE
    )
  }

  ############
  # the fit's records and design
  ############

  design <- cv_designmat(
    dat = model_data_spatial,
    target_covariate_names = fit$target_covariate_names,
    bioregion_names = fit$bioregion_names
  )

  bg_data <- model_data_spatial |>
    filter(data_type == "bg") |>
    select(-species) |>
    tidyr::expand_grid(species = target_species) |>
    select(species, everything())

  model_data <- bind_rows(
    model_data_spatial |>
      filter(data_type != "bg"),
    bg_data
  ) |>
    mutate(
      location_id = match(
        paste(
          latitude,
          longitude,
          model_date
        ),
        design$unique_locatenate
      ),
      species_id = match(species, target_species),
      sampling_method_id = match(sampling_method, sampling_methods)
    )

  if (anyNA(model_data$location_id) || anyNA(model_data$species_id)) {
    stop(
      "os_record_loglik(): records without a design row or species",
      call. = FALSE
    )
  }

  # per-location linear predictor without the observation process, one matrix per
  # species, [n_loc x D]: x_all beta + alpha + kappa * log(offset)
  log_lambda_loc <- lapply(
    seq_len(n_species),
    function(s) {
      eta <- design$x_all %*% t(fit$beta[, , s])
      eta <- sweep(eta, 2, fit$alpha[, s], FUN = "+")
      eta + outer(design$log_offset[, 1], kappa[, s])
    }
  )

  # log lambda for a set of records, [records x D]
  log_lambda_rec <- function(recs){
    out <- matrix(NA_real_, nrow = nrow(recs), ncol = D)
    for (s in unique(recs$species_id)) {
      in_s <- which(recs$species_id == s)
      out[in_s, ] <- log_lambda_loc[[s]][recs$location_id[in_s], , drop = FALSE]
    }
    out
  }

  # sampling-method effect for records with a method, [records x D]
  method_effect <- function(recs){
    if (anyNA(recs$sampling_method_id)) {
      stop(
        "os_record_loglik(): records with a method the fit did not see",
        call. = FALSE
      )
    }
    t(fit$sampling_re[, recs$sampling_method_id, drop = FALSE])
  }

  # rows x draws -> draws x rows, the layout loo::loo() takes
  pack <- function(loglik, recs, expected){
    out <- list(
      loglik = t(loglik),
      records = recs |>
        select(
          data_type,
          species,
          latitude,
          longitude,
          model_date,
          sampling_method,
          source_id,
          n
        )
    )
    if (return_expected) {
      out$expected <- expected
    }
    out
  }

  out <- list()

  ############
  # counts
  ############

  if ("count" %in% data_types) {

    recs <- model_data |>
      filter(data_type == "count")

    sid <- fit$sid_lookup$sid[match(recs$source_id, fit$sid_lookup$source_id)]
    zeta_col <- match(sid, fit$count_sids)

    if (anyNA(zeta_col)) {
      stop(
        "os_record_loglik(): count records whose source has no fitted zeta",
        call. = FALSE
      )
    }

    log_mu <- log_lambda_rec(recs) +
      method_effect(recs) +
      t(fit$zeta[, zeta_col, drop = FALSE])

    mu <- exp(log_mu)
    gamma_nb_rec <- t(fit$gamma_nb[, recs$species_id, drop = FALSE])

    loglik <- matrix(
      stats::dnbinom(
        x = rep(recs$n, times = D),
        size = mu / gamma_nb_rec,
        prob = 1 / (1 + gamma_nb_rec),
        log = TRUE
      ),
      nrow = nrow(recs)
    )

    out$count <- pack(loglik, recs, mu)

  }

  ############
  # presence-absence
  ############

  if ("pa" %in% data_types) {

    recs <- model_data |>
      filter(data_type == "pa")

    # the fit clamps log lambda to [log(eps), log(max double)] before this likelihood
    # (logit_icloglog_safe()), so a presence where lambda is vanishingly small scores
    # log(eps) at worst; mirrored here so the scores are the fitted likelihood
    log_lambda_pa <- log_lambda_rec(recs) + method_effect(recs)
    log_lambda_pa <- pmin(
      pmax(
        log_lambda_pa,
        log(.Machine$double.eps)
      ),
      log(.Machine$double.xmax)
    )

    lambda <- exp(log_lambda_pa)

    # absent: log(exp(-lambda)); present: log(1 - exp(-lambda)), via expm1 so that small
    # lambdas do not round to log(0)
    present <- recs$n == 1
    loglik <- -lambda
    loglik[present, ] <- log(-expm1(-lambda[present, , drop = FALSE]))

    out$pa <- pack(loglik, recs, -expm1(-lambda))

  }

  ############
  # presence-only and background
  ############

  if ("pobg" %in% data_types) {

    recs <- model_data |>
      filter(data_type %in% c("po", "bg"))

    is_bg <- recs$data_type == "bg"

    # [D] the background points' averaged method effect, as `sampling_re_bg`
    sre_bg <- log(
      exp(fit$sampling_re) %*% fit$sm_prop[sampling_methods]
    )[, 1]

    method_term <- matrix(
      sre_bg,
      nrow = nrow(recs),
      ncol = D,
      byrow = TRUE
    )
    method_term[!is_bg, ] <- method_effect(recs[!is_bg, ])

    log_bias <- outer(
      design$log_z[recs$location_id, 1],
      as.vector(fit$delta)
    ) + t(fit$gamma[, recs$species_id, drop = FALSE])

    mu <- exp(
      log_lambda_rec(recs) +
        method_term +
        log_bias +
        log(recs$weight)
    )

    loglik <- matrix(
      stats::dpois(
        x = rep(recs$n, times = D),
        lambda = mu,
        log = TRUE
      ),
      nrow = nrow(recs)
    )

    out$pobg <- pack(loglik, recs, mu)

  }

  out$chain <- fit$chain

  out

}
