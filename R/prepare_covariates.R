prepare_covariates <- function(africa_mask){

  tcw <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/TCW/TCW_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  tcb <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/TCB/TCB_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  built_volume <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/output/rasters/covariates/built_volume.grd", africa_mask)

  landcover <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/Landcover/IGBP_Landcover.2020.Annual.Data.1km.majority-class.tif", africa_mask)

  lst_day <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/LST_Day/LST_Day_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  lst_night <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/LST_Night/LST_Night_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  evi <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/EVI/EVI_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  rainfall <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/Rainfall/chirps-v2-0.2021.Annual.sum.1km.NN.tif", africa_mask)

  mech <- rast("data/raster/An_gambiae_mechanistic_abundance.tif") |>
    crop(africa_mask) |>
    max() |>
    match_ref(africa_mask) |>
    std_rast()

  covs <- c(tcw, tcb, built_volume, landcover, lst_day, lst_night, evi, rainfall, mech)

  names(covs) <- terra::varnames(covs) <- c(
    "tcw",
    "tcb",
    "built_volume",
    "landcover","
    lst_day",
    "lst_night",
    "evi",
    "rainfall",
    "mech"
  )


  new_mask <- africa_mask |>
    mask(covs[[1]]) |>
    mask(covs[[2]]) |>
    mask(covs[[3]]) |>
    mask(covs[[4]]) |>
    mask(covs[[5]]) |>
    mask(covs[[6]]) |>
    mask(covs[[7]]) |>
    mask(covs[[8]]) |>
    mask(covs[[9]])

  cm <- mask(covs, new_mask)

  # mask_nas <- sum(is.na(values(new_mask)))
  #
  # cov_nas <- apply(values(cm), 2, function(x)(sum(is.na(x))))
  #
  # mask_nas - cov_nas

  covs <- sdmtools::writereadrast(
    x = cm,
    filename = "data/covs.tif"
  )

  covs

}
