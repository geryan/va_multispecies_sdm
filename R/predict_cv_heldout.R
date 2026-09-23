# Posterior predictive draws for the records a fold held out.
#
# The arithmetic is the fit's own (fit_...reparam.R:239-365), reassembled in R from the
# parameter draws. Three things it must get right, and each is a way to be silently wrong:
#
#   DESIGN. Built by cv_designmat(), which is one row per held-out COORDINATE, not per
#   record. The fit shares a coordinate's first row across every record there, and `offset`
#   varies within a coordinate on two thirds of the count records, by up to six orders of
#   magnitude. Records reach their design row through `location_id`.
#
#   ZETA. The per-source effect. 51% of count sources sit at a single coordinate, so a
#   spatial block usually removes an entire study and the source effect is genuinely
#   unseen. The default marginalises it over its prior -- `zeta_regularised_sd` is a fixed
#   constant, so that is exact, not an approximation -- which asks the question the CV is
#   for: how well does this predict counts from a new survey in a new place. Drawn ONCE PER
#   SOURCE per posterior draw, never per record: per-record draws would decorrelate
#   within-study observations and flatter every calibration diagnostic.
#
#   SAMPLING METHOD. `sampling_methods` is derived from whatever data a fit saw, so indices
#   are fold-specific. Matched by NAME against the fold's own vector; a method the fold
#   never saw is marginalised over the fitted hyperparameter, `N(0, sampling_re_sd)`, which
#   is the exact posterior predictive for a new level of that random effect.
#
# All three data types are predicted even though only counts are scored, because
# predictive_checks() requires po_pred, pa_pred and count_pred together and the extra two
# are the same matmul with a different link.
predict_cv_heldout <- function(
    draws_file,
    model_data_spatial,
    cv_folds,
    fold,
    zeta_treatment = c("prior", "zero", "posterior_where_seen"),
    nsims_ppc = 200,
    seed = 20260909
){

  zeta_treatment <- match.arg(zeta_treatment)

  set.seed(seed + fold)

  fit <- readRDS(draws_file)

  split <- split_cv_fold(
    model_data_spatial = model_data_spatial,
    cv_folds = cv_folds,
    fold = fold,
    target_species = NULL
  )

  test <- split$test

  target_species <- fit$target_species
  sampling_methods <- fit$sampling_methods
  n_species <- fit$n_species

  D <- length(fit$chain)

  ############
  # mirror the fit's background handling: bg rows carry no species and are replicated
  # across all of them, with a sampling method drawn from the observed frequencies
  ############

  sm_freq <- table(na.omit(test$sampling_method))
  sm_prop <- as.numeric(sm_freq) / sum(sm_freq)

  test_bg <- test |>
    filter(data_type == "bg")

  if (nrow(test_bg) > 0) {

    test_bg <- test_bg |>
      select(-species) |>
      tidyr::expand_grid(species = target_species) |>
      mutate(
        sampling_method = sample(
          x = names(sm_freq),
          size = n(),
          replace = TRUE,
          prob = sm_prop
        )
      )

  }

  test_all <- bind_rows(
    test |>
      filter(data_type != "bg"),
    test_bg
  )

  ############
  # design, and the indices into it
  ############

  design <- cv_designmat(
    dat = test_all,
    target_covariate_names = fit$target_covariate_names,
    bioregion_names = fit$bioregion_names
  )

  test_all <- test_all |>
    mutate(
      location_id = design$location_id,
      species_id = match(species, target_species),
      sampling_method_id = match(sampling_method, sampling_methods)
    )

  # the per-coordinate linear predictor, one species at a time: x_all is
  # [n_coord x n_cov_all] and beta[, , s] is [D x n_cov_all]
  eta_coord <- lapply(
    seq_len(n_species),
    function(s) {
      design$x_all %*% t(fit$beta[, , s])
    }
  )

  # a method the fold never saw gets one draw per method per posterior draw, from the
  # fitted hyperparameter -- the posterior predictive for a new level
  unseen_methods <- setdiff(
    unique(test_all$sampling_method),
    sampling_methods
  )

  sre_unseen <- matrix(
    stats::rnorm(
      length(unseen_methods) * D,
      mean = 0,
      sd = rep(fit$sampling_re_sd, each = length(unseen_methods))
    ),
    nrow = max(length(unseen_methods), 1)
  )

  sampling_effect <- function(rows){

    k <- test_all$sampling_method_id[rows]

    out <- t(fit$sampling_re[, k, drop = FALSE])

    miss <- which(is.na(k))

    if (length(miss) > 0) {
      out[miss, ] <- sre_unseen[
        match(test_all$sampling_method[rows][miss], unseen_methods), ,
        drop = FALSE
      ]
    }

    out

  }

  # log_lambda without the observation process: offset + alpha + x_all %*% beta
  linear_predictor <- function(rows){

    out <- matrix(NA_real_, nrow = length(rows), ncol = D)

    for (s in unique(test_all$species_id[rows])) {

      in_s <- which(test_all$species_id[rows] == s)
      locs <- test_all$location_id[rows][in_s]

      eta <- eta_coord[[s]][locs, , drop = FALSE]
      eta <- sweep(eta, 2, fit$alpha[, s], FUN = "+")
      eta <- eta + design$log_offset[locs, 1]

      out[in_s, ] <- eta

    }

    out

  }

  ############
  # counts -- the scored likelihood
  ############

  count_rows <- which(test_all$data_type == "count")

  log_mu <- linear_predictor(count_rows) + sampling_effect(count_rows)

  count_test <- test_all[count_rows, ]

  zeta_rec <- switch(
    zeta_treatment,

    zero = matrix(0, nrow = length(count_rows), ncol = D),

    prior = {
      srcs <- unique(count_test$source_id)
      z <- matrix(
        stats::rnorm(length(srcs) * D, 0, fit$zeta_regularised_sd),
        nrow = length(srcs)
      )
      z[match(count_test$source_id, srcs), , drop = FALSE]
    },

    posterior_where_seen = {
      sid <- fit$sid_lookup$sid[match(count_test$source_id, fit$sid_lookup$source_id)]
      col <- match(sid, fit$count_sids)
      srcs <- unique(count_test$source_id)
      z_prior <- matrix(
        stats::rnorm(length(srcs) * D, 0, fit$zeta_regularised_sd),
        nrow = length(srcs)
      )
      out <- z_prior[match(count_test$source_id, srcs), , drop = FALSE]
      seen <- which(!is.na(col))
      if (length(seen) > 0) {
        out[seen, ] <- t(fit$zeta[, col[seen], drop = FALSE])
      }
      out
    }
  )

  # exp() of a linear predictor that has not been constrained can overflow; count and
  # report rather than letting Inf reach the score
  log_mu <- log_mu + zeta_rec
  n_capped <- sum(log_mu > 700)
  mu <- exp(pmin(log_mu, 700))

  # NB1: var = mu (1 + gamma_nb), so size = mu / gamma_nb and prob = 1 / (1 + gamma_nb)
  gamma_nb_rec <- t(fit$gamma_nb[, count_test$species_id, drop = FALSE])

  ############
  # presence/absence and presence-only/background, for the out-of-sample PPCs
  ############

  ppc_idx <- round(
    seq(
      from = 1,
      to = D,
      length.out = min(nsims_ppc, D)
    )
  )

  pa_rows <- which(test_all$data_type == "pa")
  pobg_rows <- which(test_all$data_type %in% c("po", "bg"))

  pa_pred <- NULL
  po_pred <- NULL

  if (length(pa_rows) > 0) {
    log_lambda_pa <- (linear_predictor(pa_rows) + sampling_effect(pa_rows))[, ppc_idx, drop = FALSE]
    p_pa <- lambda_to_p(exp(pmin(log_lambda_pa, 700)))
    pa_pred <- matrix(
      stats::rbinom(length(p_pa), 1, p_pa),
      nrow = nrow(p_pa)
    )
  }

  if (length(pobg_rows) > 0) {
    log_nu <- (linear_predictor(pobg_rows) + sampling_effect(pobg_rows))[, ppc_idx, drop = FALSE]
    # bias enters this likelihood only, which is what identifies gamma and delta
    bias <- outer(
      log(test_all$travel_time[pobg_rows]),
      fit$delta[ppc_idx]
    ) + t(fit$gamma[ppc_idx, test_all$species_id[pobg_rows], drop = FALSE])
    log_nu <- log_nu + bias + log(test_all$weight[pobg_rows])
    po_pred <- matrix(
      stats::rpois(length(log_nu), exp(pmin(log_nu, 700))),
      nrow = nrow(log_nu)
    )
  }

  count_pred <- matrix(
    stats::rnbinom(
      n = length(mu[, ppc_idx, drop = FALSE]),
      size = (mu / gamma_nb_rec)[, ppc_idx, drop = FALSE],
      mu = mu[, ppc_idx, drop = FALSE]
    ),
    nrow = nrow(mu)
  )

  list(
    fold = fold,
    zeta_treatment = zeta_treatment,
    mu = mu,
    gamma_nb_rec = gamma_nb_rec,
    count_test = count_test,
    n_capped = n_capped,
    n_unseen_methods = length(unseen_methods),
    n_records_unseen_method = sum(is.na(test_all$sampling_method_id)),
    prop_source_seen = mean(
      count_test$source_id %in%
        fit$sid_lookup$source_id[fit$sid_lookup$sid %in% fit$count_sids]
    ),
    # shaped exactly as predictive_checks() and DHARMa::createDHARMa() consume them:
    # simulations in columns, records in rows
    preds = list(
      count_pred = t(count_pred),
      pa_pred = if (is.null(pa_pred)) NULL else t(pa_pred),
      po_pred = if (is.null(po_pred)) NULL else t(po_pred)
    ),
    dat = list(
      count_dat = count_test$n,
      pa_dat = test_all$n[pa_rows],
      po_dat = test_all$n[pobg_rows]
    ),
    convergence = fit$convergence,
    runtime_min = fit$runtime_min
  )

}
