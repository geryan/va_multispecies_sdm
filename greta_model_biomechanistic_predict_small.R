library(greta)
library(tidyverse)
library(terra)
library(sdmtools)

# create a series of extent tiles that mosaic over the whole area
# based on code from sdmtools::split_rast

# raster of predictor variables
# model_layers_mech <- rast("outputs/model_layers_mech.tif")

# load("outputs/drawsetc_large_mod_nonzero_mech_alt_penalties.RData")

# cov_all <- rast("data/raster/static_vars_agg_mech_nonzero.tif")
r <- rast("data/raster/static_vars_agg_mech_nonzero.tif")

r <- r[[
  c(
    "ag_microclim",
    #"arid",
    # built_volume,
    # cropland,
    # "elevation",
    "evi_mean"#, # correlates with pressure_mean rainfall_mean and solrad_mean
    # "footprint", # correlates with built_volume and cropland
    # "lst_day_mean",
    # "lst_night_mean",
    # # pressure_mean,
    # # rainfall_mean,
    # "soil_clay",
    # # solrad_mean,
    # # surface_water, remove and replace with distance to surface water
    # "tcb_mean", # strongly correlates with tcw
    # # tcw_mean,
    # "windspeed_mean",
    # "easting",
    # "northing"
  )
]]

layer_values <- values(r)
naidx <- is.na(layer_values[,1])

nspp <- 4


  # get cell values for predicting

  x_predict <- layer_values[
    !naidx,
    c(
      #"arid",
      # built_volume,
      # cropland,
      # "elevation",
      "evi_mean"#, # correlates with pressure_mean rainfall_mean and solrad_mean
      # "footprint", # correlates with built_volume and cropland
      # "lst_day_mean",
      # "lst_night_mean",
      # # pressure_mean,
      # # rainfall_mean,
      # "soil_clay",
      # # solrad_mean,
      # # surface_water, remove and replace with distance to surface water
      # "tcb_mean", # strongly correlates with tcw
      # # tcw_mean,
      # "windspeed_mean",
      # "easting",
      # "northing"
    )
  ]

  # assemble greta objects for prediction:

  mech_pred <- layer_values[!naidx, "ag_microclim"]

  lowidx <- which(mech_pred <= exp(-30))

  mech_pred[lowidx] <- exp(-30)

  log_mech_pred <- log(mech_pred) |>
    matrix(data = _, ncol = 1)

  log_lambda_adults_predict <- log_mech_pred

  log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")


  log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")

  p_predict <- icloglog(log_lambda_predict + log(area_pa))

  # run 100 sims for each cell and take the mean

  preds <- calculate(p_predict, values = draws, nsim = 100)

  preds_mean <- apply(preds$p_predict, MARGIN = c(2,3), median, na.rm = TRUE)

  # rasterise predictions and save them

  preds_rast <- rep(r[[1]], times = nspp)
  names(preds_rast) <- c("arabiensis", "funestus",  "coluzzii", "gambiae")
  for(i in 1:nspp) {
    preds_rast[[i]][!naidx] <- preds_mean[,i]
  }


