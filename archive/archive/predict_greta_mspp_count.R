predict_greta_mspp_count <- function(
    image_filename,
    prediction_layer,
    target_species,
    output_file
){

  load(image_filename)

  prednames <- colnames(x)

  r <- prediction_layer

  layer_values <- values(r)
  naidx <- is.na(layer_values[,1])

  x_predict <- layer_values[!naidx, prednames]

  # offset_pred <- layer_values[!naidx, "ag_microclim"]
  #
  # log_offset_pred <- log(offset_pred)
  #
  # log_lambda_adults_predict <- log_offset_pred

  log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")


  #log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")
  log_lambda_predict <-log_lambda_larval_habitat_predict

  pa_rate_predict <- exp(log_lambda_predict + log(area_pa))

  # run 100 sims for each cell and take the mean

  preds <- calculate(pa_rate_predict, values = draws, nsim = 100)

  preds_mean <- apply(preds$pa_rate_predict, MARGIN = c(2,3), median, na.rm = TRUE)

  # rasterise predictions and save them
  preds_rast <- rep(r[[1]], times = n_species)
  names(preds_rast) <- target_species
  for(i in 1:n_species) {
    preds_rast[[i]][!naidx] <- preds_mean[,i]
  }

  writeRaster(
    x = preds_rast,
    filename = output_file,
    overwrite = TRUE
  )

  output_file

}
