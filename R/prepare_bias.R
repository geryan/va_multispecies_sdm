prepare_bias <- function(my_mask){

  x <- terra::rast("~/Documents/tki_work/vector_atlas/vector_sdm_course/data/downloads/travel_time_to_cities_2.tif") |>
    terra::crop(my_mask) |>
    terra::mask(my_mask)

  names(x) <- varnames(x) <- "bias"

  sdmtools::writereadrast(x, "data/bias.tif")

}
