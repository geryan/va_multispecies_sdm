predict_lambda_no_offset <- function(
    image_name,
    prediction_layer,
    target_species,
    output_file_prefix,
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
  hast_lambda_no_offset <- rast_lambda_no_offset
  last_lambda_no_offset <- rast_lambda_no_offset

  # iterate by species for memory efficiency
  for(i in 1:n_species){

    # simulate predictions of lambda without offset from priors
    pred_lambda <- calculate(
      lambda_predict[,i],
      values = draws,
      nsim = nsims
    )

    # calculate median, and lower and upper quantiles and sd
    preds_med <- apply(
      pred_lambda$lambda_predict,
      MARGIN = c(2,3),
      median,
      na.rm = TRUE
    )

    preds_lwr <- apply(
      pred_lambda$lambda_predict,
      MARGIN = c(2,3),
      function(x){quantile(x, probs = 0.025, na.rm = TRUE)}
    )

    preds_upp <- apply(
      pred_lambda$lambda_predict,
      MARGIN = c(2,3),
      function(x){quantile(x, probs = 0.975,na.rm = TRUE)}
    )

    rast_lambda_no_offset[[i]][!naidx] <- preds_med
    last_lambda_no_offset[[i]][!naidx] <- preds_lwr
    hast_lambda_no_offset[[i]][!naidx] <- preds_upp

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

  output_filename_l <- sprintf(
    "%s_lower.tif",
    output_file_prefix
  )

  writeRaster(
    x = last_lambda_no_offset,
    filename = output_filename_l,
    overwrite = TRUE
  )

  output_filename_h <- sprintf(
    "%s_upper.tif",
    output_file_prefix
  )

  writeRaster(
    x = hast_lambda_no_offset,
    filename = output_filename_h,
    overwrite = TRUE
  )


  list(
    lambda_no_offset = output_filename_lambda,
    lower = output_filename_l,
    upper = output_filename_h
  )


}
