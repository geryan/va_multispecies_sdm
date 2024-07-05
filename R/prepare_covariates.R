prepare_covariates <- function(africa_mask){

  tcw <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/TCW/TCW_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  tcb <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/TCB/TCB_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  built_volume <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/output/rasters/covariates/built_volume.grd", africa_mask)

  landcover <- rast("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/Landcover/IGBP_Landcover.2020.Annual.Data.1km.majority-class.tif") |>
    crop(africa_mask) |>
    mask(africa_mask)


  landcover_ref <- tribble(
    ~id, ~landcover,
    00, "Unclassified",
    01, "Evergreen_Needleleaf_Forest",
    02, "Evergreen_Broadleaf_Forest",
    03, "Deciduous_Needleleaf_Forest",
    04, "Deciduous_Broadleaf_Forest",
    05, "Mixed_Forest",
    06, "Closed_Shrublands",
    07, "Open_Shrublands",
    08, "Woody_Savannas",
    09, "Savannas",
    10, "Grasslands",
    11, "Permanent_Wetlands",
    12, "Croplands",
    13, "Urban_And_Built_Up",
    14, "Cropland_Natural_Vegetation_Mosaic",
    15, "Snow_And_Ice",
    16, "Barren_Or_Sparsely_Populated",
    17, "Water"
  ) |>
    as.data.frame()

  levels(landcover) <- landcover_ref


  lst_day <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/LST_Day/LST_Day_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  lst_night <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/LST_Night/LST_Night_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  evi <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/EVI/EVI_v6.2021.Annual.mean.1km.Data.tif", africa_mask)

  rainfall <- rcms("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/Rainfall/chirps-v2-0.2021.Annual.sum.1km.NN.tif", africa_mask)

  mech <- rast("data/raster/An_gambiae_mechanistic_abundance.tif") |>
    crop(africa_mask) |>
    max() |>
    match_ref(africa_mask) |>
    std_rast()

  surface_water <- rcms("data/raster/sw.tif", africa_mask)

  #sw_focal <- focal(surface_water, w = 11, fun = mean, na.rm = TRUE, filename = "data/raster/sw_focal.tif", overwrite = TRUE)

  elevation <- global_regions |>
    filter(continent == "Africa") |>
    pull(iso3) |>
    lapply(
      FUN = function(x){
        elevation_30s(
          country = x,
          path = "data/raster/geodata"
        )
      }
    ) |>
    sprc() |>
    merge(filename = "data/raster/elevation30s.tif")

  covs <- c(tcw, tcb, built_volume, landcover, lst_day, lst_night, evi, rainfall, mech, surface_water)

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
