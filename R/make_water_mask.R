#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param water
#' @param proj_mask
#' @return
#' @author geryan
#' @export
make_water_mask <- function(water, proj_mask) {

  water <- crop(water, proj_mask)
  water <- aggregate(water, 5, fun = "modal")

  # convert it into a mask
  water_mask <- water
  water_mask[is.na(water_mask)] <- 0
  water_mask[water_mask == 1] <- NA
  water_mask <- terra::mask(water_mask, proj_mask)
  water_mask <- water_mask * 0

}
