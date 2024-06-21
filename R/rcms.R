rcms <- function(x, y){
  terra::rast(x) |>
    terra::crop(y) |>
    terra::mask(y) |>
    sdmtools::standardise_rast()
}
