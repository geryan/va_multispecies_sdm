# Pull everything a held-out prediction needs out of a fit image, and throw the image away.
#
# WHY NOT calculate(). The obvious route is
# `calculate(beta, alpha, sampling_re, values = draws, nsim = n)`, which is what
# predict_lambda_reparam() does. It works, but it starts TensorFlow, re-traces the graph,
# and returns a SUBSAMPLE of size nsim rather than the draws themselves. `beta` and `zeta`
# are deterministic rescalings of traced arrays -- `beta = sweep(beta_raw, 1, beta_scale,
# "*")` and `zeta = zeta_raw * zeta_regularised_sd` -- and `beta_scale` is a plain numeric
# vector sitting in the saved environment. So they can be reconstructed in R from the
# traced values with no greta at all, exactly, and with the chain structure intact.
#
# WHY THIN. `beta` is [draws, n_cov_all, n_species]. At 10 chains x 1000 samples and
# 198 x 18 that is 285 MB before anything else, against an 8.2 GB image we are trying not
# to keep. `n_keep` draws are taken at even spacing within each chain, so chains stay
# balanced and the chain index stays meaningful.
#
# Column names in a greta mcmc.list are `name[i,j]`, `name[i]` or bare `name` depending on
# the array's dimension, so indices are parsed off the labels and written into position
# explicitly rather than trusting column order.
extract_cv_draws <- function(
    image_name,
    output_file,
    n_keep = 1000,
    fold = NA_integer_,
    seed = NA_integer_,
    delete_image = FALSE
){

  fit <- new.env(parent = emptyenv())
  load(image_name, envir = fit)

  draws <- fit$draws

  n_chain <- length(draws)
  n_iter <- nrow(draws[[1]])

  per_chain <- min(
    max(n_keep %/% n_chain, 1),
    n_iter
  )

  keep_idx <- unique(
    round(
      seq(
        from = 1,
        to = n_iter,
        length.out = per_chain
      )
    )
  )

  # [D, n_param], with chain identity kept alongside rather than folded away
  mat <- do.call(
    rbind,
    lapply(
      draws,
      function(x) as.matrix(x)[keep_idx, , drop = FALSE]
    )
  )

  chain <- rep(
    seq_len(n_chain),
    each = length(keep_idx)
  )

  iteration <- rep(
    keep_idx,
    times = n_chain
  )

  ############
  # parse `name[i,j]` / `name[i]` / `name` off the column labels
  ############

  nms <- colnames(mat)

  pull_param <- function(param){

    hit <- grepl(
      sprintf("^%s(\\[|$)", param),
      nms
    )

    if (!any(hit)) {
      stop(
        sprintf(
          "extract_cv_draws(): '%s' is not traced in %s",
          param,
          basename(image_name)
        ),
        call. = FALSE
      )
    }

    sub_nms <- nms[hit]
    idx_txt <- sub("^[^\\[]+\\[?", "", sub_nms)
    idx_txt <- sub("\\]$", "", idx_txt)

    if (all(idx_txt == "")) {
      return(mat[, hit, drop = TRUE])
    }

    idx <- do.call(
      rbind,
      lapply(
        strsplit(idx_txt, ","),
        as.integer
      )
    )

    dims <- apply(idx, 2, max)

    # hoisted out of the placement below: `beta_raw` is 3564 columns, and re-subsetting
    # a [1000 x 3564] matrix once per column copies ~28 MB each time
    sub_mat <- mat[, hit, drop = FALSE]

    n_draw <- nrow(mat)
    n_col <- ncol(sub_mat)

    # greta writes two indices even for a vector (`a[1,1]`, `a[2,1]`, ...), so a trailing
    # dimension of 1 is an artefact, not a real dimension -- confirmed against greta 0.6.0
    if (length(dims) == 2 && dims[2] == 1) {
      dims <- dims[1]
      idx <- idx[, 1, drop = FALSE]
    }

    out <- array(NA_real_, dim = c(n_draw, dims))

    # one vectorised assignment through an array index, rather than a loop over columns
    if (length(dims) == 1) {

      ai <- cbind(
        rep(seq_len(n_draw), times = n_col),
        rep(idx[, 1], each = n_draw)
      )

    } else {

      ai <- cbind(
        rep(seq_len(n_draw), times = n_col),
        rep(idx[, 1], each = n_draw),
        rep(idx[, 2], each = n_draw)
      )

    }

    out[ai] <- as.vector(sub_mat)

    if (anyNA(out)) {
      stop(
        sprintf(
          "extract_cv_draws(): '%s' has gaps after parsing -- %i of %i cells unfilled",
          param,
          sum(is.na(out)),
          length(out)
        ),
        call. = FALSE
      )
    }

    out

  }

  alpha <- pull_param("alpha")
  beta_raw <- pull_param("beta_raw")
  sampling_re <- pull_param("sampling_re")
  sampling_re_sd <- pull_param("sampling_re_sd")
  gamma_nb <- pull_param("gamma_nb")
  gamma <- pull_param("gamma")
  delta <- pull_param("delta")
  zeta_raw <- pull_param("zeta_raw")

  # the deterministic rescalings the fit applies (fit_...reparam.R:205, :236)
  beta <- sweep(
    beta_raw,
    2,
    fit$beta_scale,
    FUN = "*"
  )

  zeta <- zeta_raw * fit$zeta_regularised_sd

  if (dim(beta)[2] != ncol(fit$x_all) || dim(beta)[3] != fit$n_species) {
    stop(
      sprintf(
        "extract_cv_draws(): beta is [%s], expected [D, %i, %i]",
        paste(dim(beta), collapse = ", "),
        ncol(fit$x_all),
        fit$n_species
      ),
      call. = FALSE
    )
  }

  ############
  # convergence. NOT coda::gelman.diag() -- it defaults to multivariate = TRUE, which
  # needs a P x P decomposition and is what stalled the 2026-09-08 production run at
  # ~3600 parameters. posterior::rhat() is rank-normalised split-Rhat and is both faster
  # and better. Gated on the scalars the count score actually depends on; beta_raw is
  # summarised but never gated, because 3564 heavily-shrunk interaction coefficients
  # trade off against each other freely and their max Rhat says little about the
  # linear predictor.
  ############

  gate_vars <- c(
    "alpha", "gamma_nb", "sampling_re", "sampling_re_sd",
    "alpha_mean", "alpha_sd", "gamma_mean", "gamma_sd", "gamma", "delta"
  )

  drw <- posterior::as_draws_array(draws)

  is_gate <- grepl(
    sprintf("^(%s)(\\[|$)", paste(gate_vars, collapse = "|")),
    posterior::variables(drw)
  )

  gate <- posterior::summarise_draws(
    posterior::subset_draws(drw, variable = posterior::variables(drw)[is_gate]),
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  )

  is_beta <- grepl("^beta_raw\\[", posterior::variables(drw))

  beta_summary <- posterior::summarise_draws(
    posterior::subset_draws(drw, variable = posterior::variables(drw)[is_beta]),
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk
  )

  # max()/min() with na.rm return -Inf/Inf on an all-NA column, and -Inf silently reads
  # as a passing convergence gate. NA is the honest answer.
  safe <- function(f, x){

    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    f(x)

  }

  convergence <- list(
    gate = gate,
    max_rhat_gate = safe(max, gate$rhat),
    min_ess_gate = safe(min, gate$ess_bulk),
    beta_max_rhat = safe(max, beta_summary$rhat),
    beta_q99_rhat = safe(function(z) stats::quantile(z, 0.99), beta_summary$rhat),
    beta_pct_rhat_gt_101 = mean(beta_summary$rhat > 1.01, na.rm = TRUE),
    beta_min_ess = safe(min, beta_summary$ess_bulk),
    n_chains = n_chain,
    n_iter = n_iter
  )

  ############
  # save, then drop the image
  ############

  out <- list(
    beta = beta,
    alpha = alpha,
    sampling_re = sampling_re,
    sampling_re_sd = sampling_re_sd,
    gamma_nb = gamma_nb,
    gamma = gamma,
    delta = delta,
    zeta = zeta,
    chain = chain,
    iteration = iteration,
    sampling_methods = fit$sampling_methods,
    count_sids = fit$count_sids,
    sid_lookup = fit$model_data |>
      filter(!is.na(source_id)) |>
      distinct(source_id, sid),
    zeta_regularised_sd = fit$zeta_regularised_sd,
    target_species = fit$target_species,
    target_covariate_names = fit$target_covariate_names,
    bioregion_names = fit$bioregion_names,
    n_cov_abund = fit$n_cov_abund,
    n_species = fit$n_species,
    convergence = convergence,
    fold = fold,
    seed = seed,
    image_path = image_name
  )

  dir.create(
    dirname(output_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  saveRDS(
    out,
    output_file
  )

  if (delete_image) {
    unlink(image_name)
  }

  output_file

}
