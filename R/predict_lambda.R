predict_lambda <- function(
    image_name,
    prediction_layer,
    target_species,
    output_file_prefix,
    offset,
    sm = FALSE,
    nsims = 50
){

  # load image with model and draws in it

  load(image_name)

  prednames <- target_covariate_names

  # create values to predict to using covatiate layers
  r <- prediction_layer

  layer_values <- values(r)
  naidx <- is.na(layer_values[,1])

  x_predict <- layer_values[!naidx, prednames]

  log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")


  if(sm){
    # simulate human_landing_catch indoor
    sampling_re_predict <- sampling_re_raw[5] * sampling_re_sd

    #log_lambda_predict <- log_lambda_combine_predict +
    log_lambda_predict <- log_lambda_larval_habitat_predict +
      sampling_re_predict
  } else {
    log_lambda_predict <- log_lambda_larval_habitat_predict
  }


  lambda_predict <- exp(log_lambda_predict)

  # create   rasters to put values into
  #
  rast_lambda_no_offset <- rep(r[[1]], times = n_species)
  names(rast_lambda_no_offset) <- target_species
  rast_p <- rast_lambda_no_offset
  rast_p_cv <- rast_lambda_no_offset

  offset_vals <- values(offset)
  offset_vals <- offset_vals[!naidx]


  # iterate by species for memory efficiency
  for(i in 1:n_species){

    # simulate predictions of lambda without offset from priors
    pred_lambda_no_offset <- calculate(
      lp = lambda_predict[,i],
      values = draws,
      nsim = nsims
    )

    # calculate median, and lower and upper quantiles and sd
    preds_lambda_median <- apply(
      pred_lambda_no_offset$lp,
      MARGIN = c(2,3),
      median,
      na.rm = TRUE
    )


    #preds_lambda <- t(t(pred_lambda_no_offset$lp[,,1]) * offset_vals)

    preds_lambda <- sweep(
      x = pred_lambda_no_offset$lp,
      MARGIN = c(2,3),
      FUN = "*",
      offset_vals
    )

    preds_p <- lambda_to_p(preds_lambda)

    preds_p_mean <- apply(
      preds_p,
      MARGIN = c(2,3),
      FUN = mean
    )

    preds_p_sd <- apply(
      preds_p,
      MARGIN = c(2,3),
      FUN = sd
    )

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
