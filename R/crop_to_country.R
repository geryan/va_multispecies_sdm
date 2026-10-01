#' Crop and mask a raster to one country
#'
#' `touches = TRUE` keeps every cell a boundary touches, not only those whose
#' centre falls inside, so small island states (Seychelles, Mauritius, Sao Tome
#' and Principe) keep their 10 km cells.
#'
#' A country entirely outside `r` -- Cabo Verde lies west of the prediction
#' grid -- would make terra::crop() fail, so `r` is first extended to cover it
#' and the result is an all-NA raster on the country's extent.
#'
#' @param r SpatRaster
#' @param v SpatVector of one country
#' @return SpatRaster cropped and masked to `v`
#' @author geryan
#' @export
crop_to_country <- function(
    r,
    v
){

  er <- terra::ext(r)
  ev <- terra::ext(v)

  overlap <- er$xmin < ev$xmax &&
    er$xmax > ev$xmin &&
    er$ymin < ev$ymax &&
    er$ymax > ev$ymin

  if (!overlap) {
    r <- terra::extend(
      r,
      ev
    )
  }

  terra::crop(
    r,
    v,
    mask = TRUE,
    touches = TRUE
  )

}
