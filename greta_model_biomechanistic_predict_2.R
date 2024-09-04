library(greta)
library(tidyverse)
library(terra)
library(sdmtools)




# create a series of extent tiles that mosaic over the whole area
# based on code from sdmtools::split_rast

# raster of predictor variables
model_layers_mech <- rast("outputs/model_layers_mech.tif")
x <- model_layers_mech


# grain is the number of vertical and horizontal mosaic slices to create
# number of split rasters = grain^2
grain <- 10

# get dimensions, resolution, and origin points to create series from
dimx <- dim(x)[1]
dimy <- dim(x)[2]

resx <- terra::res(x)[1]
resy <- terra::res(x)[2]

xminx <- terra::xmin(x)
yminx <- terra::ymin(x)


# use these to make sequences of cell edge values for mosaic tiles
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


# take edge sequences and tie together in orthogonal table
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
  # change from generic cell values into coords
  dplyr::mutate(
    dplyr::across(tidyselect::starts_with("x"), ~ (.x - 1)*resx + xminx),
    dplyr::across(tidyselect::starts_with("y"), ~ (.x - 1)*resy + yminx)
  ) |>
  # convert coords into terra extents
  dplyr::mutate(
    ext = purrr::pmap(
      .l = list(xmin, xmax, ymin, ymax),
      .f = function(xmin, xmax, ymin, ymax){

        terra::ext(c(xmin, xmax, ymin, ymax))

      }
    )
  ) |>
  pull(ext)


# function to predict to each mosaic tile

predict_greta_sdm_fmapply <- function(
    # multi-layer raster of predictor variables
    r = "outputs/model_layers_mech.tif",
    # extent of tile to be predicted to
    xts,
    # number for naminge tile output predictions
    # this should be fed into mapply as a sequence
    # of same length as xts
    n,
    # species names for naming prediction layers
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    # image of greta stuff: draws model components
    image = "outputs/drawsetc.RData"
){


  # process raster by cropping it to the extent of the tile
  # and extracting values

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
  nspp <- length(spp)


  # if this tile is all na, don't predict, just return NA raster for
  # mosaic later
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

  # if predicting load in greta things
  # delaying this until we know we are bothering to predict

  load(file = image)

  # get cell values for predicting

  x_predict <- layer_values[
    !naidx,
    c(
      "cropland",
      "footprint",
      "lst_day_mean",
      "rainfall_mean"
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

  # send out proof of life
  print(n)

  # try to clean up?
  # I thought this would happen but seems not to
  rm(list = ls())
  gc()


}

# set up prediction function infrastructure
library(future)
library(future.apply)

# here using this so it destroys the greta mucking about each time
# hopefully
plan("multisession", workers = 1)


predlist <- future_mapply(
  FUN = predict_greta_sdm_fmapply,
  xts = extlist,
  n = 1:100,
  MoreArgs = list(
    r = "outputs/model_layers_mech.tif",
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    image = "outputs/drawsetc.RData"
  ),
  # this gets rid of some annoying messages from future but probably does nothing
  future.seed = 20240903
)


