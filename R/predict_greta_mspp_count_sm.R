predict_greta_mspp_count_sm <- function(
    image_filename,
    prediction_layer,
    offset,
    target_species,
    target_covariate_names = target_covariate_names,
    # subrealm_names = subrealm_names,
    bioregion_names = bioregion_names,
    output_file_prefix
){

  load(image_filename)

  prednames <- target_covariate_names

  r <- prediction_layer

  layer_values <- values(r)
  naidx <- is.na(layer_values[,1])

  x_predict <- layer_values[!naidx, prednames]
  offset_values <- values(offset)

  offset_pred <- offset_values[!naidx]

  log_offset_pred <- log(offset_pred)

  log_lambda_adults_predict <- log_offset_pred

  # x_subrealm_predict <- layer_values[!naidx, subrealm_names]
  x_bioregion_predict <- layer_values[!naidx, bioregion_names]


  # # model bioregion effects as additive to landcover, so just expand the
  # # covariate set
  # x_all_predict <- cbind(x_predict, x_bioregion_predict)

  # # model bioregion effects only as interactions with landcover, and expand
  # the covariate set
  x_interactions_predict <- make_designmat_interactions(x_predict,
                                                        x_bioregion_predict)
  x_all_predict <- cbind(x_predict, x_interactions_predict)
  x_all_predict <- as_data(x_all_predict)

  # # or, include main terms and interactions
  # x_interactions_predict <- make_designmat_interactions(x_predict,
  #                                                       x_bioregion_predict)
  # x_all_predict <- cbind(x_predict,
  #                        x_bioregion_predict,
  #                        x_interactions_predict)

  x_predict_beta_species <- x_all_predict %*% beta

  log_lambda_larval_habitat_predict <- sweep(x_predict_beta_species, 2, alpha, FUN = "+")

  # log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")

  log_lambda_combine_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")

  # simulate human_landing_catch indoor
  sampling_re_predict <- sampling_re_raw[5] * sampling_re_sd

  log_lambda_predict <- log_lambda_combine_predict +
    sampling_re_predict

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
