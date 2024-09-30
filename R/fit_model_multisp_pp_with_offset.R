fit_model_multisp_pp_with_offset <- function(
    model_data_ragged,
    spatial_values,
    model_notna_idx_pa,
    model_notna_idx_po,
    image_name = "outputs/images/multisp_pp_with_offset.RData",
    warmup = 500,
    draws = 1000
){

  #for all points (pa, po, bg):

  # get offset values from gambiae mechanistic model
  log_offset <- log(spatial_values$ag_microclim)

  # get covariate values
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

  # get bias values
  z <- spatial_values[,"research_tt_by_country"]

  # get pa data matrix and infill NAs with zeroes
  # (these will be ignored in model because of indexing with model_notna_idx_pa)
  pa_infilled <- model_data_ragged |>
    filter(type == "pa") |>
    select(
      -lon,
      -lat,
      -type
    ) |>
    mutate(
      across(
        everything(),
        function(x){ifelse(x > 0, 1, 0)}
      ),
      across(
        everything(),
        function(x){ifelse(is.na(x), 0, x)}
      )
    ) |>
    as.matrix()

  # get pa data matrix and infill NAs with ones
  # again these will be ignored in model because of indexing with
  # model_notna_idx_po
  # NB also, while the above filtered to only PA data this uses all as the
  po_infilled <- model_data_ragged |>
    select(
      -lon,
      -lat,
      -type
    ) |>
    mutate(
      across(
        everything(),
        function(x){1}
      )
    ) |>
    as.matrix()

  bg_idx <- (nrow(model_data_ragged) + 1):nrow(spatial_values)

  nspp <- ncol(pa_infilled)

  area_bg <- 825.1676 # this is with 30k bg points









}
