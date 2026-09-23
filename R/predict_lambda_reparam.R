# predict_lambda_reparam.R
#
# Reparameterised version of predict_lambda() (R/predict_lambda.R), for predicting
# from images saved by fit_model_multispecies_pp_count_source_effect_reparam()
# (R/fit_model_multispecies_pp_count_source_effect_reparam.R) instead of
# fit_model_multispecies_pp_count_source_effect().
#
# Two things differ from predict_lambda():
#
# 1. TRACED-NAME RENAME (from the reparam fit's DOWNSTREAM NOTE). Only one touches this
#    prediction, which builds larval-habitat lambda from `beta` and `alpha`:
#        sampling_re_raw -> sampling_re   (centred; sampled directly as
#                                          normal(0, sampling_re_sd, dim = n_sampling_methods))
#    so the sm (sampling-method) effect is `sampling_re[i]`, not
#    `sampling_re_raw[i] * sampling_re_sd`.  `i` is now looked up by name from the fit's
#    own `sampling_methods` via the `sampling_method` argument, rather than hardcoded to
#    5 -- see the note at the lookup.  (alpha_raw/gamma_raw/sqrt_inv_size/zeta_raw
#    renames don't appear here -- this function references neither gamma, zeta, size, nor
#    the per-source effect.)
#
# 2. MEMORY-SAFE PREDICTION (the substantive change). predict_lambda() calls
#        calculate(lambda_predict[, i], values = draws, nsim = nsims)
#    inside the species loop, where lambda_predict is built from the full greta graph
#        x_all_predict (n_pixel x n_cov_all)  %*%  beta (n_cov_all x n_species).
#    With the landcover x bioregion interaction design, n_cov_all is large (~180) and
#    n_pixel at 10 km is ~370k. To evaluate that matmul for `nsim` posterior draws at once,
#    greta/TensorFlow materialises a single [nsim x n_pixel x n_cov_all] tensor. At
#    nsim = 100 that is ~53 GB, so the calculate() aborts the R/callr subprocess hard
#    ("could not start R ... has crashed or was killed"). It "works" only at tiny nsim
#    (e.g. 3 -> ~1.6 GB). This is NOT out-of-memory in the ordinary sense -- RSS is still
#    small when TF's single-tensor allocation aborts.
#
#    Fix: draw posterior samples of the *parameters* (beta, alpha, sampling_re) in ONE
#    small calculate() (dims ~[nsim x n_cov_all x n_species] -> a few MB), then do the
#    design-matrix multiply in plain R, one species at a time. Peak memory is then just
#    the [n_pixel x nsim] prediction matrix (~0.3 GB at nsim = 100), there is no giant
#    intermediate tensor, and there is no per-species TF graph retracing. Results are
#    numerically identical to the greta path (same posterior draws, same arithmetic).
#
# Everything else -- inputs, design matrix, offset-as-multiplier, bioregion masking, the
# lambda / p / p_cv outputs and their filenames -- matches predict_lambda() exactly.

predict_lambda_reparam <- function(
    image_name,
    prediction_layer,
    target_species,
    output_file_prefix,
    offset,
    sm = FALSE,
    sampling_method = "human_landing_catch_ind",
    nsims = 50,
    bioregion_mask = NULL
){

  # load image with model and draws in it
  load(image_name)

  prednames <- target_covariate_names

  # create values to predict to using covariate layers
  r <- prediction_layer

  layer_values <- values(r)
  naidx <- is.na(layer_values[, 1])

  x_predict <- layer_values[!naidx, prednames]
  x_bioregion_predict <- layer_values[!naidx, bioregion_names]

  # design matrix: landcover main effects + landcover x bioregion interactions
  # (identical to predict_lambda()). Kept as a plain R matrix: the matmul is now done
  # in R rather than in the greta graph (see header note 2).
  x_interactions_predict <- make_designmat_interactions(
    x_predict,
    x_bioregion_predict
  )
  x_all_predict <- cbind(x_predict, x_interactions_predict)

  n_pixel_predict <- nrow(x_all_predict)

  # --- draw posterior samples of the parameters only (small), instead of pushing the
  #     whole design-matrix multiply through greta::calculate() over all pixels at once.
  #     beta:        [nsims, n_cov_all, n_species]
  #     alpha:       [nsims, n_species, 1]
  #     sampling_re: [nsims, n_sampling_methods, 1]
  param_draws <- calculate(
    beta,
    alpha,
    sampling_re,
    values = draws,
    nsim = nsims
  )

  beta_draws  <- param_draws$beta                          # [S, K, n_species]
  alpha_draws <- array(param_draws$alpha,                  # -> [S, n_species]
                       dim = dim(param_draws$alpha)[1:2])

  # sm: predict as if surveyed by `sampling_method`, matching predict_lambda().
  # --- REPARAM: centred sampling_re is sampled directly (was sampling_re_raw[5] * sampling_re_sd)
  #
  # Looked up BY NAME, not by position. `sampling_methods` is saved in the fit image
  # but is derived there from whatever data the model was fitted to
  # (fit_model_multispecies_pp_count_source_effect_reparam.R:118-122), so the index of
  # a given method depends on which methods that fit actually saw. Position 5 is
  # human_landing_catch_ind only when all nine are present; a fit on any subset of the
  # data -- a cross-validation fold, say -- silently shifts it and predicts a different
  # method with no error and no warning.
  if (sm) {

    sm_idx <- match(sampling_method, sampling_methods)

    if (is.na(sm_idx)) {
      stop(
        sprintf(
          "predict_lambda_reparam(): sampling method '%s' was not seen by this fit. Available: %s",
          sampling_method,
          paste(sampling_methods, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    sampling_re_add <- param_draws$sampling_re[, sm_idx, 1]     # length S

  } else {
    sampling_re_add <- rep(0, nsims)
  }

  # optional bioregion mask, applied on the natural (lambda) scale as in predict_lambda()
  if (!is.null(bioregion_mask)) {
    bioreg_mask_vals <- values(bioregion_mask)
    bmv <- bioreg_mask_vals[!naidx]
  }

  # create rasters to put values into
  rast_lambda_no_offset <- rep(r[[1]], times = n_species)
  names(rast_lambda_no_offset) <- target_species
  rast_p <- rast_lambda_no_offset
  rast_p_cv <- rast_lambda_no_offset

  offset_vals <- values(offset)
  offset_vals <- offset_vals[!naidx]

  # iterate by species for memory efficiency
  for (i in 1:n_species) {

    # log larval-habitat lambda for every posterior draw: [n_pixel x nsims].
    # x_all_predict %*% t(beta_draws[, , i]) gives per-pixel, per-draw linear predictor;
    # add the per-draw species intercept alpha and (if sm) the sampling-method effect.
    eta <- x_all_predict %*% t(beta_draws[, , i])
    eta <- sweep(eta, 2, alpha_draws[, i] + sampling_re_add, FUN = "+")

    lambda_no_offset <- exp(eta)                           # [n_pixel x nsims]

    if (!is.null(bioregion_mask)) {
      # bmv is length n_pixel; recycles down each column (column-major) -> per-pixel scale
      lambda_no_offset <- lambda_no_offset * bmv
    }

    # median lambda (no offset) per pixel
    preds_lambda_median <- apply(lambda_no_offset, 1, median, na.rm = TRUE)

    # apply offset as a per-pixel multiplier (offset_vals length n_pixel recycles per column)
    preds_lambda <- lambda_no_offset * offset_vals

    preds_p <- lambda_to_p(preds_lambda)

    preds_p_mean <- rowMeans(preds_p)
    preds_p_sd <- apply(preds_p, 1, sd)
    preds_p_cv <- preds_p_sd / preds_p_mean

    rast_lambda_no_offset[[i]][!naidx] <- preds_lambda_median
    rast_p[[i]][!naidx] <- preds_p_mean
    rast_p_cv[[i]][!naidx] <- preds_p_cv

  }

  output_filename_lambda <- sprintf(
    "%s.tif",
    output_file_prefix
  )

  writeRaster(
    x = rast_lambda_no_offset,
    filename = output_filename_lambda,
    overwrite = TRUE
  )

  output_filename_p <- sprintf(
    "%s_p.tif",
    output_file_prefix
  )

  writeRaster(
    x = rast_p,
    filename = output_filename_p,
    overwrite = TRUE
  )

  output_filename_p_cv <- sprintf(
    "%s_p_cv.tif",
    output_file_prefix
  )

  writeRaster(
    x = rast_p_cv,
    filename = output_filename_p_cv,
    overwrite = TRUE
  )

  list(
    lambda_no_offset = output_filename_lambda,
    p = output_filename_p,
    p_cv = output_filename_p_cv
  )

}
