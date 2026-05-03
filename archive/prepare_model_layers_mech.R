prepare_model_layers_mech <- function(model_layers){


  mech <- model_layers[["ag_microclim"]]

  mvals <- values(mech)

  zeroidx <- which(mvals == 0)

  mechmask <- mech

  mechmask[zeroidx] <- NA

  notnaidx <- which(!is.na(values(mechmask)))

  mechmask[notnaidx] <- 1

  mechmask <- writereadrast(
    mechmask,
    "data/raster/mechmask.tif",
    layernames = "mechmask"
  )

  model_layers_mech <- mask(
    model_layers,
    mechmask
  ) |>
    writereadrast(
      filename = "data/raster/mechmask_africa_covs_std.tif"
    )


}
