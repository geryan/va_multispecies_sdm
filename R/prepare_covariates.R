prepare_covariates <- function(africa_mask){

  africa_mask <- sdmtools::make_africa_mask()

  pop <- terra::rast(("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/WorldPop/WorldPop_UNAdj_v3_DRC_fix.2020.Annual.Data.1km.Data.tif")) |>
    crop(africa_mask) |>
    mask(africa_mask)

  africa_mask <- mask(africa_mask, pop) |> # cut out large water bodies
    writereadrast("data/raster/africa_mask.tif")

  tcw <- terra::rast("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/TCW/TCW_v6.2021.Annual.mean.1km.Data.tif") |>
    terra::crop(africa_mask) |>
    terra::mask(africa_mask) |>
    sdmtools::standardise_rast()

  tcb <- terra::rast("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/TCB/TCB_v6.2020.Annual.mean.1km.Data.tif") |>
    terra::crop(africa_mask) |>
    terra::mask(africa_mask) |>
    sdmtools::standardise_rast()

  built_volume <- terra::rast("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/GHSL_2023/GHS_BUILT_V_R23A.2020.Annual.Data.1km.Data.tif") |>
    terra::crop(africa_mask) |>
    terra::mask(africa_mask) |>
    sdmtools::standardise_rast()

  covs <- c(tcw, tcb, built_volume)

  names(covs) <- terra::varnames(covs) <- c("tcw", "tcb", "built_volume")

  covs <- sdmtools::writereadrast(
    x = covs,
    filename = "data/covs.tif"
  )

  covs

}
