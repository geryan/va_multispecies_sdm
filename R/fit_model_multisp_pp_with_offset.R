fit_model_multisp_pp_with_offset <- function(
    model_data_ragged,
    spatial_values,
    model_notna_idx_pa,
    model_notna_idx_po,
    image_name = "outputs/images/multisp_pp_with_offset.RData",
    warmup = 500,
    draws = 1000
){

  log_offset <- log(spatial_values$ag_microclim)

  x <- spatial_values |>
    as_tibble() |>
    select(
      #ag_microclim,
      #research_tt_by_country,
      #arid,
      # built_volume,
      # cropland,
      elevation,
      evi_mean, # correlates with pressure_mean rainfall_mean and solrad_mean
      footprint, # correlates with built_volume and cropland
      lst_day_mean,
      # lst_night_mean,
      # # pressure_mean,
      # # rainfall_mean,
      # soil_clay,
      # # solrad_mean,
      # # surface_water, remove and replace with distance to surface water
      tcb_mean, # strongly correlates with tcw
      # # tcw_mean,
      # windspeed_mean,
      easting,
      northing
    ) |>
    as.matrix()

  z <- spatial_values[,"research_tt_by_country"]


  pa <- model_data_ragged |>
    filter(type == "pa") |>
    select(
      -lon,
      -lat,
      -type
    ) |>





}
