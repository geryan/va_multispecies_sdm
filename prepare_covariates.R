prepare_covariates <- function(africa_mask){

  tcw <- terra::rast("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/TCW/TCW_v6.2021.Annual.mean.1km.Data.tif") %>%
    mask(mask = africa_mask)

  tcb <- terra::rast("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/TCB/TCB_v6.2020.Annual.mean.1km.Data.tif") %>%
    mask(mask = africa_mask)

  built_volume <- terra::rast("~/Documents/tki_work/vector_atlas/an_stephensi/anopheles_stephensi_expansion/data/MAP_covariates/GHSL_2023/GHS_BUILT_V_R23A.2020.Annual.Data.1km.Data.tif") %>%
    mask(mask = africa_mask)



}
