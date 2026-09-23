# Rebuild the abundance design exactly as
# fit_model_multispecies_pp_count_source_effect_reparam() builds it, for an arbitrary
# subset of `model_data_spatial`.
#
# WHY THIS EXISTS
#
# The fit does not use per-record covariates. `distinct_idx` takes the FIRST row at each
# distinct (latitude, longitude) and every record at that coordinate shares that row's
# covariates and its offset. That is not a technicality. Measured on the current
# `model_data_spatial`: `offset` varies within a coordinate at 638 of the 3703
# coordinates, covering 19,359 of 29,476 count records (65.7%), with a within-coordinate
# max/min ratio of median 3.1 and maximum 4,496,005. `footprint` varies at 341
# coordinates and `crop_other` / `tree` at 93 each; `prox_to_sea`, `travel_time` and every
# bioregion dummy are exactly constant within a coordinate.
#
# So scoring a held-out record against its OWN covariate row would evaluate a linear
# predictor the model never fitted, on two thirds of the count data, with offsets wrong by
# up to six orders of magnitude. Anything that predicts from a fit image must come through
# here, and must map records to design rows by `location_id`.
#
# Transcribed from fit_model_multispecies_pp_count_source_effect_reparam.R:73-106 and
# :145-147, :194-195. Kept as one function so there is a single place where the fit's
# convention lives, and one thing to re-check if that fit ever changes.
#
# Returns plain R matrices, not greta data arrays: the prediction path does the
# design-matrix multiply in R, because pushing it through greta::calculate() materialises
# a tensor big enough to abort the process (see predict_lambda_reparam.R:19-36).
cv_designmat <- function(
    dat,
    target_covariate_names,
    bioregion_names
){

  distinct_idx <- dat |>
    mutate(rn = row_number(), .before = species) |>
    group_by(latitude, longitude) |>
    mutate(rnsp = row_number(), .before = species) |>
    ungroup() |>
    filter(rnsp == 1) |>
    pull(rn)

  distinct_coords <- dat[distinct_idx, c("latitude", "longitude")]

  unique_locatenate <- distinct_coords |>
    mutate(locatenate = paste(latitude, longitude)) |>
    pull(locatenate)

  # offset values from gambiae mechanistic model
  log_offset <- log(dat[distinct_idx, "offset"]) |>
    as.matrix()

  # covariate values
  x <- dat[distinct_idx, ] |>
    as_tibble() |>
    select(all_of(target_covariate_names)) |>
    as.matrix()

  # bioregion dummy values
  x_bioregion <- dat[distinct_idx, ] |>
    as_tibble() |>
    select(all_of(bioregion_names)) |>
    as.matrix()

  # bias values
  z <- dat[distinct_idx, "travel_time"] |>
    as.matrix()

  x_interactions <- make_designmat_interactions(
    x,
    x_bioregion
  )

  x_all <- cbind(x, x_interactions)

  # the fit silently propagates any NA here into NaN log_lambda rather than failing, so
  # it is caught at the point it can still be attributed to a column
  na_cols <- colnames(x_all)[apply(is.na(x_all), 2, any)]

  if (length(na_cols) > 0) {
    stop(
      sprintf(
        "cv_designmat(): NA in design columns: %s",
        paste(na_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (anyNA(log_offset) || anyNA(z)) {
    stop(
      "cv_designmat(): NA in offset or travel_time",
      call. = FALSE
    )
  }

  # every row of `dat` addressed to its design row, using the fit's own paste()
  # convention so the two can never drift apart
  location_id <- match(
    paste(dat$latitude, dat$longitude),
    unique_locatenate
  )

  list(
    x_all = x_all,
    log_offset = log_offset,
    log_z = log(z),
    location_id = location_id,
    unique_locatenate = unique_locatenate,
    distinct_idx = distinct_idx,
    n_pixel = nrow(x),
    n_cov_abund = ncol(x)
  )

}
