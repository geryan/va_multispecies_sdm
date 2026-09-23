#' One year of the Human Footprint series, as a model covariate layer
#'
#' Pulls a single year out of the stack written by `stack_mu_hfp()` and returns
#' it as a one-layer SpatRaster named for the covariate rather than for the
#' year, so the layer name is the model covariate name. The land cover
#' counterpart is `esa_landcover_model_layers()`, and this behaves the same way
#' for the same reasons.
#'
#' This is the static, single-year face of the time-varying Human Footprint: it
#' is what goes into `covariate_rast_5_all`, and so what the model predicts to
#' and what the background points are drawn against. Records get their own
#' year's value instead, through `match_footprint_data()`. Both faces come off
#' the same file, so they are on the same scale by construction.
#'
#' The year is found by matching the stack's own time axis, not by arithmetic
#' on layer position, matching `extract_year_indexed_layer_data()`.
#'
#' @param hfp_path path to the stacked series, i.e. the `mu_hfp_all` target
#' @param year the year to take, e.g. 2024
#' @param varname the layer name to give the result, i.e. the covariate name
#' @param project_mask optional SpatRaster to mask to and check against, i.e.
#'   `project_mask_5`. NA inside the mask is an error here rather than a
#'   `mismatched_nas` failure three targets downstream
#' @return single-layer SpatRaster named `varname`
#' @author geryan
#' @export
mu_hfp_model_layer <- function(
    hfp_path,
    year,
    varname = "footprint",
    project_mask = NULL
  ) {

  year <- as.integer(year)

  if (length(year) != 1L || is.na(year)) {
    stop(
      "mu_hfp_model_layer(): `year` must be a single year",
      call. = FALSE
    )
  }

  r <- terra::rast(as.character(hfp_path))

  years <- hfp_layer_years(r)

  if (anyNA(years)) {
    stop(
      sprintf(
        "mu_hfp_model_layer(): %s has layers with no year on its time axis",
        basename(as.character(hfp_path))
      ),
      call. = FALSE
    )
  }

  idx <- match(year, years)

  if (is.na(idx)) {
    stop(
      sprintf(
        "mu_hfp_model_layer(): %s holds %d-%d, not %d",
        basename(as.character(hfp_path)),
        min(years),
        max(years),
        year
      ),
      call. = FALSE
    )
  }

  out <- r[[idx]]

  # the year is fixed and recorded by the target that sets it, so drop it from
  # the layer name and the time axis: this is a covariate layer now, and it
  # gets combined with layers that have no time of their own
  names(out) <- varname
  terra::time(out) <- NULL

  if (!is.null(project_mask)) {

    if (!terra::compareGeom(out, project_mask, stopOnError = FALSE)) {
      stop(
        "mu_hfp_model_layer(): footprint is not on the same grid as `project_mask`",
        call. = FALSE
      )
    }

    out <- terra::mask(out, project_mask)

    extra_na <- terra::global(
      is.na(out) & !is.na(project_mask),
      fun = "sum",
      na.rm = TRUE
    )[1, 1]

    if (extra_na > 0) {
      stop(
        sprintf(
          "mu_hfp_model_layer(): %d cell(s) NA inside the project mask",
          extra_na
        ),
        call. = FALSE
      )
    }

  }

  out

}
