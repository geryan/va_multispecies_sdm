#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param oneearth_vect
#' @param nameme1
#' @return
#' @author Nick Golding
#' @export
make_smooth_dummies <- function(oneearth_spatvector,
                                mask,
                                level = c("bioregion", "subrealm", "realm")) {

  level <- match.arg(level)

  # make factor raster of this level of the classification
  factor_rast <- oneearth_spatvector |>
    terra::rasterize(
      mask,
      field = level
    ) |>
    terra::mask(
      mask
    ) |>
    terra::droplevels()

  lookup <- levels(factor_rast)[[1]]

  # segregate this into binary dummy variables
  dummies <- terra::segregate(factor_rast, keep = FALSE)

  # put the names back
  names_idx <- match(names(dummies), lookup$ID)
  new_names <- lookup[[level]][names_idx]
  names(dummies) <- new_names

  # now smooth them all

  # first project the raster so we can model in 2D space
  dummies_proj <- terra::project(
    dummies,
    crs("+proj=eqearth")
  )

  mask_proj <- terra::project(
    mask,
    crs("+proj=eqearth")
  )

  # define a smoothing window
  # sigma parameter of Gaussian window
  sigma_km <- 100
  window <- terra::focalMat(dummies_proj,
                            # define the distance in meters
                            d = sigma_km * 1e3,
                            type = "Gauss")

  # apply this Gaussian smooting window to each layer
  dummies_proj_smoothed <- terra::focal(dummies_proj,
                                        w = window,
                                        fun = "mean",
                                        na.rm = TRUE,
                                        na.policy = "omit",
                                        expand = TRUE)

  # then relevel to make them sum to one
  sums <- app(dummies_proj_smoothed, sum)
  dummies_proj_smoothed_norm <- dummies_proj_smoothed / sums

  # and project back to the original setup
  dummies_smoothed_norm <- terra::project(
    dummies_proj_smoothed_norm,
    crs(mask)
  )

  # return these
  dummies_smoothed_norm


}
