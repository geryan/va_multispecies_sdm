make_model_data_ragged <- function(
  data_records,
  bg_points,
  target_species
){

  # this function joins them all together in a big long list
  # then in a separate function we extract the indices for the model

  data_records |>
    filter(
      species %in% target_species
    ) |>
    select(
      species,
      lon,
      lat,
      count,
      type = pa
    ) |>
    mutate(
      count = case_when(
        type == "po" ~ 1, # this will need editing if want to use counts for PO data
        !is.na(count) ~ count,
        TRUE ~ 0
      )
    ) |>
    group_by(lon, lat, species, type) |>
    summarise(count = max(count), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = species,
      values_from = count
    ) |>
    arrange(type)

   #  bind_rows(
   #    bg_points |>
   #      as_tibble() |>
   #      rename(
   #        lon = x,
   #        lat = y
   #      ) |>
   #      mutate(
   #        count = 0,
   #        presence = 0,
   #        type = "bg"
   #      )
   # )


}


## model
#
# get rid of aridity predictor
#
#
# plots of covs against pa and po data and against bias
#
# also plots of posteriors against covs
#
#
# https://github.com/goldingn/ir_cube/blob/master/R/fig_internal_validation.R#L13-L49
#
#
# additional bias layer from density of gbif samples
#
#
# plot randomised quantile resids against variables and only predict whatever had a relationship
#
#
# prior on bias as slope of poisson regression or number of records over distance from research centre
#
#
# pa_data <- (
#
#   arabiensis, funestus, coluzzii, gambiae, gambiae_coluzzii,
#   0, 0, 0, 1, 1,
#   0, 0, 0, 0, 1,
#   0, 0, 0, 1, 0,
# )
#
# n_mod_sp <- 4
# n_obs_sp <- 5
#
#
# get shapefiles from sinka for expert maps - rasterise, focal window blur them, use as offset.
#
#
# # combine coluzzi and gambiae modelled abundances
# lambda_coluzzi <- exp(log_lambda[, is_coluzzi])
# lambda_gambiae <- exp(log_lambda[, is_gambiae])
# lambda_gambiae_coluzzi <- lambda_coluzzi + lambda_gambiae
# log_lambda_gambiae_coluzzi <- log(lambda_gambiae_coluzzi)
# log_lambda_observed <- cbind(log_lambda, log_lambda_gambiae_coluzzi)
# # for the po data, need to add in an extra bias intercept for gambiae + coluzzi
# n_obs_species <- n_species + 1
# # use n_obs_species to create the bias coefficients (edited)
