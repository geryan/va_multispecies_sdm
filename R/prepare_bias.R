prepare_bias <- function(my_mask, biaspath){

  x <- terra::rast(
   biaspath
  ) |>
    terra::crop(my_mask) |>
    terra::mask(my_mask)

  names(x) <- varnames(x) <- "bias"

  sdmtools::writereadrast(x, "data/bias.tif")

}
