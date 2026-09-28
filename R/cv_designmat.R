# Rebuild the abundance design exactly as
# fit_model_multispecies_pp_count_source_effect_reparam() builds it, for an arbitrary
# subset of `model_data_spatial`.
#
# WHY THIS EXISTS
#
# The fit does not build one design row per record. `distinct_idx` takes one row per
# distinct (latitude, longitude, model_date), and every record with that key reaches it
# through `location_id`. `model_date` has to be in the key: it is what the offset (matched
# on year-month), land cover and footprint (matched on year) were joined on by, so every
# record sharing the key shares every design value. Keyed on coordinate alone, as it once
# was, the offset differed within a design row on two thirds of the count records.
#
# Anything that predicts from a fit image must come through here and address design rows
# by `location_id`, so that it uses the same rows the fit did.
#
# Transcribed from fit_model_multispecies_pp_count_source_effect_reparam.R:72-112 and
# :151-159, :210-211. Kept as one function so there is a single place where the fit's
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
    group_by(
      latitude,
      longitude,
      model_date
    ) |>
    mutate(rnsp = row_number(), .before = species) |>
    ungroup() |>
    filter(rnsp == 1) |>
    pull(rn)

  distinct_coords <- dat[distinct_idx, c("latitude", "longitude", "model_date")]

  unique_locatenate <- distinct_coords |>
    mutate(
      locatenate = paste(
        latitude,
        longitude,
        model_date
      )
    ) |>
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
    paste(
      dat$latitude,
      dat$longitude,
      dat$model_date
    ),
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
