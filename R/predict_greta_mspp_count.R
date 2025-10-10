predict_greta_mspp_count <- function(
    image_filename,
    prediction_layer,
    target_species,
    output_file_prefix
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

  pa_rate_predict <- icloglog(log_lambda_predict)

  count_rate_predict <- exp(log_lambda_predict)

  # run 100 sims for each cell and take the mean

  preds_pa <- calculate(
    pa_rate_predict,
    values = draws,
    nsim = 50
  )

  preds_mean_pa <- apply(
    preds_pa$pa_rate_predict,
    MARGIN = c(2,3),
    median,
    na.rm = TRUE
  )

  # rasterise predictions and save them
  # pa
  preds_rast_pa <- rep(r[[1]], times = n_species)
  names(preds_rast_pa) <- target_species
  for(i in 1:n_species) {
    preds_rast_pa[[i]][!naidx] <- preds_mean_pa[,i]
  }

  output_filename_pa <- sprintf(
    "%s_pa.tif",
    output_file_prefix
  )

  writeRaster(
    x = preds_rast_pa,
    filename = output_filename_pa,
    overwrite = TRUE
  )

  rm(preds_pa, preds_mean_pa, preds_rast_pa)

  # count
  preds_count <- calculate(
    count_rate_predict,
    values = draws,
    nsim = 50
  )

  preds_mean_count <- apply(
    preds_count$count_rate_predict,
    MARGIN = c(2,3),
    median,
    na.rm = TRUE
  )

  preds_rast_count <- rep(r[[1]], times = n_species)
  names(preds_rast_count) <- target_species
  for(i in 1:n_species) {
    preds_rast_count[[i]][!naidx] <- preds_mean_count[,i]
  }

  output_filename_count <- sprintf(
    "%s_count.tif",
    output_file_prefix
  )

  writeRaster(
    x = preds_rast_count,
    filename = output_filename_count,
    overwrite = TRUE
  )

  # output filenames
  list(
    pa = output_filename_pa,
    count = output_filename_count
  )

}
