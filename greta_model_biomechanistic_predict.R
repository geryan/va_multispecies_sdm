library(greta)
library(tidyverse)
library(targets)
library(bayesplot)
library(terra)
library(tidyterra)


## predict




## Predictinos

library(sdmtools)
mlm_split <- split_rast(
  model_layers_mech[[
    c(
      "ag_microclim",
      "cropland",
      "footprint",
      "lst_day_mean",
      "rainfall_mean"
    )
  ]],
  grain = 10,
  write_temp = TRUE
)



predict_greta_sdm <- function(
    r,
    n,
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    alpha,
    beta,
    draws
){

  nspp <- length(spp)
  layer_values <- values(r)
  naidx <- is.na(layer_values[,1])

  ncells <- length(naidx)

  if(ncells == sum(naidx)){

    preds <- rep(r[[1]], times = nspp)
    preds[] <- NA
    names(preds) <- spp

    writeRaster(
      preds,
      filename = sprintf(
        "outputs/predstemp/pred_%s.tif",
        n
      ),
      overwrite = TRUE
    )
    return(print(n))
  }

  x_predict <- layer_values[
    !naidx,
    c(
      "cropland",
      "footprint",
      "lst_day_mean",
      "rainfall_mean"
    )
  ]

  mech_pred <- layer_values[!naidx, "ag_microclim"]

  lowidx <- which(mech_pred <= exp(-30))

  mech_pred[lowidx] <- exp(-30)

  log_mech_pred <- log(mech_pred) |>
    matrix(data = _, ncol = 1)

  log_lambda_adults_predict <- log_mech_pred

  log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")

  log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")

  p_predict <- icloglog(log_lambda_predict + log(area_pa))

  preds <- calculate(p_predict, values = draws, nsim = 100)

  preds_mean <- apply(preds$p_predict, MARGIN = c(2,3), median, na.rm = TRUE)

  preds_rast <- rep(r[[1]], times = nspp)

  for(i in 1:nspp) {
    preds_rast[[i]][!naidx] <- preds_mean[,i]
  }

  names(preds_rast) <- spp

  writeRaster(
    preds_rast,
    filename = sprintf(
      "outputs/predstemp/pred_%s.tif",
      n
    ),
    overwrite = TRUE
  )
  gc()

  print(n)

}

for(i in 26){

  predict_greta_sdm(
    mlm_split[[i]],
    n = i,
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    alpha = alpha,
    beta = beta,
    draws = draws
  )
}


predlist <- mapply(
  FUN = predict_greta_sdm,
  r = mlm_split,
  n = 1:length(mlm_split),
  MoreArgs = list(
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    alpha = alpha,
    beta = beta,
    draws = draws
  )
)


# pred#all_layer_values <- values(model_layers_mech)
# all_layer_values <- values(mlm_split[[51]])
# naidx <- is.na(all_layer_values[,1])
#
#
# x_predict <- all_layer_values[
#   !naidx,
#   c(
#     "arid",
#     "built_volume",
#     "cropland",
#     "elevation",
#     "evi_mean",
#     "footprint",
#     "lst_day_mean",
#     "lst_night_mean",
#     "pop",
#     "pressure_mean",
#     "rainfall_mean",
#     "soil_clay",
#     "solrad_mean",
#     "surface_water",
#     "tcb_mean",
#     "tcw_mean",
#     "windspeed_mean",
#     "easting",
#     "northing"
#   )
# ]
#
# mech_pred <- all_layer_values[!naidx, "ag_microclim"]
#
# lowidx <- which(mech_pred <= exp(-30))
#
# mech_pred[lowidx] <- exp(-30)
#
# log_mech_pred <- log(mech_pred) |>
#   matrix(data = _, ncol = 1)
#
# log_lambda_adults_predict <- log_mech_pred
#
# log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")
#
# log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")
#
# p_predict <- icloglog(log_lambda_predict + log(area_pa))
#
# #rm(all_layer_values)
#
# preds <- calculate(p_predict, values = draws, nsim = 100)
#
# preds_mean <- apply(preds$p_predict, MARGIN = c(2,3), median, na.rm = TRUE)
#
# preds_rast <- rep(mlm_split[[51]][[1]], times = 4)
#
#
#
# preds_rast[[1]][!naidx] <- preds_mean[,1]
# preds_rast[[2]][!naidx] <- preds_mean[,2]
# preds_rast[[3]][!naidx] <- preds_mean[,3]
# preds_rast[[4]][!naidx] <- preds_mean[,4]
#
# preds_rast

library(lubridate)
predictions <- predlist() |>
  sprc() |>
  merge(
    filename = sprintf(
      "outputs/preds_all_%s.tif",
      today() |>
        gsub(
          pattern = "_",
          replacement = "",
          x = _
        )
    )
  )

 # add in rel abund data where available as an additional likelihood
# use a multinomial obs model where p is from predicted abundances
#


library(future)
library(future.apply)

plan("multisession", workers = 1)


predlist <- future_mapply(
  FUN = predict_greta_sdm,
  r = mlm_split,
  n = 1:length(mlm_split),
  MoreArgs = list(
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    alpha = alpha,
    beta = beta,
    draws = draws
  )
)



#####################

# new attempt

#####################
library(sdmtools)

model_layers_mech <- rast("outputs/model_layers_mech.tif")

x <- model_layers_mech
grain <- 10
dimx <- dim(x)[1]
dimy <- dim(x)[2]

if(grain > dimx | grain > dimy){
  stop("grain is > x or y dimension.\nCannot split into rasters smaller than cells.")
}

resx <- terra::res(x)[1]
resy <- terra::res(x)[2]

xminx <- terra::xmin(x)
yminx <- terra::ymin(x)

xseq <- seq(
  from = 1,
  to = dimx + 1,
  length.out = grain + 1
) |>
  round()

yseq <- seq(
  from = 1,
  to = dimy + 1,
  length.out = grain + 1
) |>
  round()

xminseq <- xseq[1:grain]
xmaxseq <- xseq[2:(grain + 1)]

yminseq <- yseq[1:grain]
ymaxseq <- yseq[2:(grain + 1)]

extlist <- tidyr::expand_grid(
  tibble::tibble(
    xmin = xminseq,
    xmax = xmaxseq
  ),
  tibble::tibble(
    ymin = yminseq,
    ymax = ymaxseq
  )
) |>
  dplyr::mutate(
    dplyr::across(tidyselect::starts_with("x"), ~ (.x - 1)*resx + xminx),
    dplyr::across(tidyselect::starts_with("y"), ~ (.x - 1)*resy + yminx)
  ) |>
  dplyr::mutate(
    ext = purrr::pmap(
      .l = list(xmin, xmax, ymin, ymax),
      .f = function(xmin, xmax, ymin, ymax){

        terra::ext(c(xmin, xmax, ymin, ymax))

      }
    )
  ) |>
  pull(ext)


predict_greta_sdm_fmapply <- function(
    r = "outputs/model_layers_mech.tif",
    xts,
    n,
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    image = "outputs/drawsetc.RData"
){

  load(file = image)
  nspp <- length(spp)

  r <- rast(r)
  r <- r[[
    c(
      "ag_microclim",
      "cropland",
      "footprint",
      "lst_day_mean",
      "rainfall_mean"
    )
  ]]

  r <- crop(
    x = r,
    y = xts
  )

  layer_values <- values(r)
  naidx <- is.na(layer_values[,1])

  ncells <- length(naidx)

  if(ncells == sum(naidx)){

    preds <- rep(r[[1]], times = nspp)
    preds[] <- NA
    names(preds) <- spp

    writeRaster(
      preds,
      filename = sprintf(
        "outputs/predstemp/pred_%s.tif",
        n
      ),
      overwrite = TRUE
    )
    return(print(n))
  }

  x_predict <- layer_values[
    !naidx,
    c(
      "cropland",
      "footprint",
      "lst_day_mean",
      "rainfall_mean"
    )
  ]

  mech_pred <- layer_values[!naidx, "ag_microclim"]

  lowidx <- which(mech_pred <= exp(-30))

  mech_pred[lowidx] <- exp(-30)

  log_mech_pred <- log(mech_pred) |>
    matrix(data = _, ncol = 1)

  log_lambda_adults_predict <- log_mech_pred

  log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")

  log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")

  p_predict <- icloglog(log_lambda_predict + log(area_pa))

  preds <- calculate(p_predict, values = draws, nsim = 100)

  preds_mean <- apply(preds$p_predict, MARGIN = c(2,3), median, na.rm = TRUE)

  preds_rast <- rep(r[[1]], times = nspp)

  for(i in 1:nspp) {
    preds_rast[[i]][!naidx] <- preds_mean[,i]
  }

  names(preds_rast) <- spp

  writeRaster(
    preds_rast,
    filename = sprintf(
      "outputs/predstemp/pred_%s.tif",
      n
    ),
    overwrite = TRUE
  )
  gc()

  print(n)

}


library(future)
library(future.apply)

plan("multisession", workers = 1)


predlist <- future_mapply(
  FUN = predict_greta_sdm_fmapply,
  xts = extlist[1:100],
  n = 1:100,
  MoreArgs = list(
    r = "outputs/model_layers_mech.tif",
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    image = "outputs/drawsetc.RData"
  ),
  future.seed = 20240903
)


