prepare_mask <- function(file_name){

    africa_mask <- sdmtools::make_africa_mask(
      type = "raster",
      res = "high"
    )

    pop <- terra::rast(("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/WorldPop/WorldPop_UNAdj_v3_DRC_fix.2020.Annual.Data.1km.Data.tif")) |>
      crop(africa_mask) |>
      mask(africa_mask)

    africa_mask <- terra::mask(africa_mask, pop) |> # cut out large water bodies
      sdmtools::writereadrast(file_name)

}
